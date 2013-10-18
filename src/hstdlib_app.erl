-module(hstdlib_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ets:new(ets_timer, [set, public, named_table]),
    hstdlib_sup:start_link().

stop(_State) ->
    ok.
