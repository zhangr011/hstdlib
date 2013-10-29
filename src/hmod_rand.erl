%%%-----------------------------------
%%% @Module  : mod_rand
%%% @Created : 2011.06.01
%%% @Description: 随机种子
%%%-----------------------------------
-module(hmod_rand).
-behaviour(gen_server).
-export([
         start_link/0,
         get_seed/0
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {seed}).

%% --- 对外接口 ---------------------------------

%% 启动服务器
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 取得一个随机数种子
get_seed() ->
    gen_server:call(?MODULE, get_seed).

%% --- 服务器内部实现 ---------------------------------
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    hmisc:write_monitor_pid(self(), ?MODULE, {}),
    {ok, #state{}}.

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
%% 返回一个随机数组合做为其它进程的随机数种子
handle_call(get_seed, _From, State) ->
    case State#state.seed of
        undefined ->
            random:seed(now());
        S ->
            random:seed(S)
    end,
    Seed = {random:uniform(99999), random:uniform(999999), random:uniform(999999)},
    {reply, Seed, State#state{seed = Seed}};

handle_call(_Request, _From, State) ->
    {noreply, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    hmisc:delete_monitor_pid(self()),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

