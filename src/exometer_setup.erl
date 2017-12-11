%% -*- mode: erlang; indent-tabs-mode: nil; -*-
%%=============================================================================
%% Copyright 2014 Ulf Wiger
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%=============================================================================

-module(exometer_setup).

-export([find_env_vars/1, get_env/2]).

-include_lib("kernel/include/file.hrl").

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(THROW(E), {'___SETUP_THROW___', E}).

-define(if_verbose(Expr),
        case get(verbose) of
            true -> Expr;
            _    -> ok
        end).

%% @spec home() -> Directory
%% @doc Returns the configured `home' directory, or a best guess (`$CWD')
%% @end
%%
home() ->
    home_([]).

home_(Vis) ->
    case get_env_v(setup, home, Vis) of
        undefined ->
            CWD = cwd(),
            D = filename:absname(CWD),
            application:set_env(setup, home, D),
            D;
        {ok, D} when is_binary(D) ->
            binary_to_list(D);
        {ok, D} when is_list(D) ->
            D;
        {error,_} = Error ->
            Error;
        Other ->
            {error, Other}
    end.

log_dir_(Vis) ->
    setup_dir(log_dir, "log." ++ atom_to_list(node()), Vis).

data_dir_(Vis) ->
    setup_dir(data_dir, "data." ++ atom_to_list(node()), Vis).

setup_dir(Key, Default, Vis) ->
    case get_env_v(setup, Key, Vis) of
        undefined ->
            D = filename:absname(filename:join(home(), Default)),
            application:set_env(setup, Key, D),
            D;
        {ok, D} when is_binary(D) ->
            binary_to_list(D);
        {ok, D} when is_list(D) ->
            D;
        Other ->
            {error, Other}
    end.

%% @spec find_env_vars(Env) -> [{AppName, Value}]
%% @doc Searches all loaded apps for instances of the `Env' environment variable.
%%
%% The environment variables are expanded according to the rules outlined in
%% {@section Variable expansion}
%% @end
find_env_vars(Env) ->
    GEnv = global_env(),
    lists:flatmap(
      fun({A,_,_}) ->
              case app_get_env(A, Env) of
                  {ok, Val} when Val =/= undefined ->
                      NewEnv = private_env(A, GEnv),
                      [{A, expand_env(NewEnv, Val, A, [Env])}];
                  _ ->
                      []
              end
      end, application:loaded_applications()).

get_env(A, Key) ->
    case app_get_env(A, Key) of
        {ok, Val} ->
            {ok, expand_value(A, Val)};
        Other ->
            Other
    end.

get_env_v(A, Key, V) ->
    try get_env_v_(A, Key, V)
    catch
        throw:?THROW(Error) ->
            Error
    end.

get_env_v_(A, Key, V) ->
    case lists:member(Key, V) of
        false ->
            case app_get_env(A, Key) of
                {ok, Val} ->
                    {ok, expand_value_v(A, Key, Val, V)};
                Other ->
                    Other
            end;
        true ->
            throw(?THROW({error, {loop_detected, Key}}))
    end.

-spec expand_value(atom(), any()) -> any().
%% @doc Expand `Value' using global variables and the variables of `App'
%%
%% The variable expansion is performed according to the rules outlined in
%% {@section Variable expansion}. If a loop is detected (a variable ends
%% up referencing itself), an exception is raised.
%% Use of {@link expand_value/3} (also providing the initial key name) is
%% recommended; this function is primarily here for backward compatibility
%% purposes.
%% @end
expand_value(App, Value) ->
    expand_env(private_env(App, global_env()), Value, App, []).

expand_value_v(App, K, Value, V) ->
    expand_env(private_env(App), Value, App, [K|V]).

global_env() ->
    Acc = [{K, fun(V1) -> env_value(K, V1) end} ||
              K <- ["DATA_DIR", "LOG_DIR", "HOME"]],
    custom_global_env(Acc).

custom_global_env(Acc) ->
    lists:foldl(fun(E, Acc1) ->
                        custom_env1(E, Acc1, setup)
                end, Acc,
                [{K,V} || {K,V} <- app_get_env(setup, vars, []),
                          is_list(K)]).

private_env(A) ->
    private_env(A, global_env()).

private_env(A, GEnv) ->
    Acc = [{K, fun(Vis1) -> env_value(K, A, Vis1) end} ||
              K <- ["APP", "PRIV_DIR", "LIB_DIR"]],
    custom_private_env(A, Acc ++ GEnv).

custom_private_env(A, Acc) ->
    lists:foldl(fun(E, Acc1) ->
                        custom_env1(E, Acc1, A)
                end, Acc,
                [{K, V} ||
                    {K,V} <- app_get_env(A, '$setup_vars', []),
                    is_list(K)]).

%% Wrapped for tracing purposes
app_get_env(A, K) ->
    application:get_env(A, K).

%% Wrapped for tracing purposes
app_get_env(A, K, Default) ->
    %% Apparently, some still use setup on R15B ...
    case application:get_env(A, K) of
        {ok, Val} -> Val;
        _ ->
            Default
    end.

custom_env1({K, V}, Acc, A) ->
    [{K, fun(Vis1) -> custom_env_value(K, V, Acc, A, Vis1) end} | Acc].

expand_env(_, {T,"$env(" ++ S} = X, A, Vis)
  when T=='$value'; T=='$string'; T=='$binary' ->
    try Res = case get_env_name_l(S) of
                  false -> undefined;
                  {Name,[]} ->
                      get_env_v_(A, Name, Vis)
              end,
         case {Res, T} of
             {undefined, '$value'} -> undefined;
             {undefined, '$string'} -> "";
             {undefined, '$binary'} -> <<>>;
             {{ok,V}   , '$value'} -> V;
             {{ok,V}   , '$string'} -> binary_to_list(stringify(V));
             {{ok,V}   , '$binary'} -> stringify(V)
         end
    catch
        error:_ -> X
    end;
expand_env(Vs, {T,"$" ++ S}, _, Vis)
  when T=='$value'; T=='$string'; T=='$binary' ->
    case {lists:keyfind(S, 1, Vs), T} of
        {false, '$value'}  -> undefined;
        {false, '$string'} -> "";
        {false, '$binary'} -> <<>>;
        {{_,V}, '$value'}  -> V(Vis);
        {{_,V}, '$string'} -> binary_to_list(stringify(V(Vis)));
        {{_,V}, '$binary'} -> stringify(V(Vis))
    end;
expand_env(Vs, T, A, V) when is_tuple(T) ->
    list_to_tuple([expand_env(Vs, X, A, V) || X <- tuple_to_list(T)]);
expand_env(Vs, L, A, V) when is_list(L) ->
    case is_string(L) of
        true ->
            do_expand_env(L, Vs, A, list, V);
        false ->
            %% [expand_env(Vs, X, A) || X <- L]
            expand_env_l(Vs, L, A, V)
    end;
expand_env(Vs, B, A, V) when is_binary(B) ->
    do_expand_env(B, Vs, A, binary, V);
expand_env(_, X, _, _) ->
    X.

-spec expand_env_l(list(), maybe_improper_list(), any(), any()) ->
                          maybe_improper_list().
expand_env_l(_Vs, [], _A, _V) ->
    [];
expand_env_l(Vs, [H|T], A, V) when is_list(T) ->
    [expand_env(Vs, H, A, V) | expand_env_l(Vs, T, A, V)];
expand_env_l(Vs, [H|T], A, V) ->
    [expand_env(Vs, H, A, V) | expand_env(Vs, T, A, V)].


%% do_expand_env(X, Vs, Type) ->
%%     lists:foldl(fun({K, Val}, Xx) ->
%%                         re:replace(Xx, [$\\, $$ | K],
%%                                    stringify(Val()), [{return,Type}])
%%                 end, X, Vs).

do_expand_env(X, Vs, A, binary, V) ->
    do_expand_env_b(iolist_to_binary(X), Vs, A, V);
do_expand_env(X, Vs, A, list, V) ->
    binary_to_list(do_expand_env_b(iolist_to_binary(X), Vs, A, V)).

do_expand_env_b(<<"$env(", T/binary>>, Vs, A, Vis) ->
    case get_env_name_b(T) of
        {K, T1} ->
            case get_env_v_(A, K, Vis) of
                {ok, V} ->
                    Res = expand_env(Vs, V, A, Vis),
                    <<(stringify(Res))/binary,
                      (do_expand_env_b(T1, Vs, A, Vis))/binary>>;
                undefined ->
                    <<"$env(", (do_expand_env_b(T, Vs, A, Vis))/binary>>
            end;
        false ->
            do_expand_env_b(T, Vs, A, Vis)
    end;
do_expand_env_b(<<"$", T/binary>>, Vs, A, Vis) ->
    case match_var_b(Vs, T, Vis) of
        {Res, T1} ->
            <<Res/binary, (do_expand_env_b(T1, Vs, A, Vis))/binary>>;
        false ->
            <<"$", (do_expand_env_b(T, Vs, A, Vis))/binary>>
    end;
do_expand_env_b(<<H, T/binary>>, Vs, A, Vis) ->
    <<H, (do_expand_env_b(T, Vs, A, Vis))/binary>>;
do_expand_env_b(<<>>, _, _, _) ->
    <<>>.

get_env_name_b(B) ->
    get_env_name_b(B, <<>>).

get_env_name_b(<<")", T/binary>>, Acc) ->
    try {binary_to_existing_atom(Acc, latin1), T}
    catch
        error:_ -> false
    end;
get_env_name_b(<<H, T/binary>>, Acc) ->
    get_env_name_b(T, <<Acc/binary, H>>);
get_env_name_b(<<>>, _) ->
    false.

get_env_name_l(L) ->
    get_env_name_l(L, []).

get_env_name_l(")" ++ T, Acc) ->
    try {list_to_existing_atom(lists:reverse(Acc)), T}
    catch
        error:_ -> false
    end;
get_env_name_l([H|T], Acc) ->
    get_env_name_l(T, [H|Acc]);
get_env_name_l([], _) ->
    false.

match_var_b([{K,V}|T], B, Vis) ->
    case re:split(B, "^" ++ K, [{return, binary}]) of
        [_] ->
            match_var_b(T, B, Vis);
        [<<>>, Rest] ->
            {stringify(V(Vis)), Rest}
    end;
match_var_b([], _, _) ->
    false.

env_value("LOG_DIR" , Vis) -> log_dir_(Vis);
env_value("DATA_DIR", Vis) -> data_dir_(Vis);
env_value("HOME"    , Vis) -> home_(Vis).

env_value("APP"     , A, _Vis) -> A;
env_value("PRIV_DIR", A, _Vis) -> priv_dir(A);
env_value("LIB_DIR" , A, _Vis) -> lib_dir(A).

custom_env_value(_K, {value, V}, _Vs, _A, _Vis) ->
    V;
custom_env_value(K, {expand, V}, Vs, A, Vis) ->
    expand_env(Vs, V, A, [K|Vis]);
custom_env_value(K, {apply, M, F, As}, _Vs, _A, _Vis) ->
    %% Not ideal, but don't want to introduce exceptions in get_env()
    try apply(M, F, As)
    catch
        error:_ ->
            {error, {custom_setup_env, K}}
    end.

%% This function is more general than to_string/1 below
stringify(V) ->
    try iolist_to_binary(V)
    catch
        error:badarg ->
            iolist_to_binary(io_lib:format("~w", [V]))
    end.

priv_dir(A) ->
    case code:priv_dir(A) of
        {error, bad_name} ->
            case is_cur_dir(A) of
                true ->
                    filename:join(cwd(), "priv");
                false ->
                    error({cannot_get_priv_dir, A})
            end;
        D -> D
    end.

lib_dir(A) ->
    case code:lib_dir(A) of
        {error, bad_name} ->
            case is_cur_dir(A) of
                true ->
                    cwd();
                false ->
                    error({cannot_get_lib_dir, A})
            end;
        D -> D
    end.

cwd() ->
    {ok, CWD} = file:get_cwd(),
    CWD.

is_cur_dir(A) ->
    As = atom_to_list(A),
    filename:basename(cwd()) == As.

is_string(L) ->
    lists:all(fun(X) when 0 =< X, X =< 255 -> true;
                 (_) -> false
              end, L).

%% Unit tests
-ifdef(TEST).

setup_test_() ->
    {foreach,
     fun() ->
             application:load(setup)
     end,
     fun(_) ->
             application:stop(setup),
             application:unload(setup)
     end,
     [
      ?_test(t_expand_vars())
     ]}.

t_expand_vars() ->
    %% global env
    application:set_env(setup, vars, [{"PLUS", {apply,erlang,'+',[1,2]}},
                                      {"FOO", {value, {foo,1}}}]),
    %% private env, stdlib
    application:set_env(stdlib, '$setup_vars',
                        [{"MINUS", {apply,erlang,'-',[4,3]}},
                         {"BAR", {value, "bar"}}]),
    application:set_env(setup, envy, 17),
    application:set_env(setup, v1, "/$BAR/$PLUS/$MINUS/$FOO/$env(envy)"),
    application:set_env(setup, v2, {'$value', "$FOO"}),
    application:set_env(setup, v3, {'$string', "$env(envy)"}),
    application:set_env(stdlib, v1, {'$string', "$FOO"}),
    application:set_env(stdlib, v2, {'$binary', "$FOO"}),
    application:set_env(stdlib, v3, {"$PLUS", "$MINUS", "$BAR"}),
    application:set_env(stdlib, v4, [a|b]),
    %% $BAR and $MINUS are not in setup's context
    {ok, "/$BAR/3/$MINUS/{foo,1}/17"} = exometer_setup:get_env(setup, v1),
    {ok, {foo,1}} = exometer_setup:get_env(setup, v2),
    {ok, "17"} = exometer_setup:get_env(setup, v3),
    {ok, "{foo,1}"} = exometer_setup:get_env(stdlib, v1),
    {ok, <<"{foo,1}">>} = exometer_setup:get_env(stdlib,v2),
    {ok, {"3", "1", "bar"}} = exometer_setup:get_env(stdlib,v3),
    {ok, [a|b]} = exometer_setup:get_env(stdlib, v4),
    ok.

-endif.
