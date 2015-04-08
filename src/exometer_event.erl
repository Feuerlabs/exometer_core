-module(exometer_event).
-behaviour(plain_fsm).

%% Used by exometer
-export([start_link/0,
	 notify/3]).

-export([init/1]).

%% User API
-export([subscribe/0,
	 subscribe/1,
	 unsubscribe/0]).

-define(SERVER, ?MODULE).
-define(TAG, ?MODULE).

-record(mon, {pid, ref}).

-record(st, {subs = [],
	     monitors = []}).


subscribe() ->
    subscribe([]).

-spec subscribe(ets:match_spec() | []) -> ok.
subscribe([]) ->
    call({subscribe, []});
subscribe(Pat) ->
    case ets:test_ms({updated, {[a,b,c], 17}}, Pat) of
	{ok, _} ->
	    call({subscribe, Pat});
	{error, Error} ->
	    error({invalid_match_spec, Error})
    end.

unsubscribe() ->
    call(unsubscribe).

call(Req) ->
    exometer_proc_lib:call(?SERVER, ?TAG, Req).

reply(To, Reply) ->
    exometer_proc_lib:reply(To, Reply).

start_link() ->
    proc_lib:start_link(
      ?MODULE, init, [self()], 5000, [{min_heap_size, 10000},
				      {fullsweep_after, 10},
				      {priority, high}]).

notify(updated, Name, Value) ->
    ?SERVER ! {?TAG, {updated, Name, Value, os:timestamp()}},
    ok.

init(Parent) ->
    exometer_proc_lib:save_info(Parent, ?MODULE, ?SERVER),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(#st{}).

loop(S) ->
    receive
	Msg ->
	    handle_msg(Msg, S)
    end.

handle_msg(Msg, S) ->
    case Msg of
	{?TAG, Notification} ->
	    handle_notification(Notification, S);
	{system, From, Req} ->
	    exometer_proc_lib:handle_system_msg(
	      Req, From, S, fun(S1) -> loop(S1) end);
	{?TAG, {_,_} = From, Req} ->
	    handle_call(Req, From, S);
	_ ->
	    handle_info(Msg, S)
    end.

handle_notification({updated, _, _, _} = Msg, #st{subs = Subs} = S) ->
    forward(Subs, Msg),
    loop(S).

handle_call({subscribe, Pat}, {Pid,_} = From, #st{subs = Subs} = S) ->
    try {Reply, #st{} = S1} = subscribe_(Pid, Pat, Subs),
	 reply(From, Reply),
	 loop(S1)
    catch
	error:_ ->
	    reply(From, badarg),
	    loop(S)
    end.

handle_info(_, S) ->
    loop(S).

forward([P|Subs], Msg) when is_pid(P) ->
    P ! Msg,
    forward(Subs, Msg);
forward([{P,MS}|Subs], Msg) ->
    case ets:match_spec_run([Msg], MS) of
	[_] ->
	    P ! Msg;
	[] ->
	    ok
    end,
    forward(Subs, Msg);
forward([], _) ->
    ok.

subscribe_(Pid, Pat, #st{subs = Subs, monitors = Mons} = S) when is_pid(Pid) ->
    PatC = compile_pat_(Pat),
    Subs1 = lists:keystore(Pid, 1, Subs, {Pid, PatC}),
    Mons1 = monitor_(Pid, Mons),
    {ok, S#st{subs = Subs1, monitors = Mons1}}.

compile_pat_([]) ->
    [];
compile_pat_(P) ->
    ets:match_spec_compile(P).

monitor_(Pid, Mons) ->
    case lists:keymember(Pid, #mon.pid, Mons) of
	true ->
	    Mons;
	false ->
	    Ref = erlang:monitor(process, Pid),
	    [#mon{pid = Pid, ref = Ref}|Mons]
    end.
