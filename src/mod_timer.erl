%%%----------------------------------------------------------------------
%%% File    : misc_timer.erl
%%% Created : 2010-10-17
%%% Description: 时间生成器
%%%----------------------------------------------------------------------
-module(mod_timer).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("define_logger.hrl").
-include("define_time.hrl").

%% 时间戳更新间隔
-define(CLOCK, 100).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

info() ->
    [
     ets:info(ets_timer), 
     ets:tab2list(ets_timer)
    ].

%% ====================================================================
%% Server functions
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    ets:insert(ets_timer, {timer, {erlang:now(), 0}}),
    erlang:send_after(?CLOCK, self(), {event, clock}),
    {ok, []}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, State, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({event, clock}, State) ->
    %% {_Total_Wallclock_Time, Wallclock_Time_Since_Last_Call}= statistics(wall_clock),
    {_Total_Run_Time, Time_Since_Last_Call} = statistics(runtime),
    ets:insert(ets_timer, {timer, {erlang:now(), Time_Since_Last_Call}}),
    erlang:send_after(?CLOCK, self(), {event, clock}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

