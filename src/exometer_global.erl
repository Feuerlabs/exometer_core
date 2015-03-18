-module(exometer_global).

-export([status/0]).

-spec status() -> enabled | disabled.
status() ->
    enabled.
