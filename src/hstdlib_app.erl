-module(hstdlib_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ets:new(ets_timer,       [set, public, named_table]),
    ets:new(ets_system_info, [set, public, named_table]),
    ets:new(ets_monitor_pid, [set, public, named_table]),
    ets:new(ets_words,       [set, public, named_table]),
    hstdlib_sup:start_link().

stop(_State) ->
    ok.
