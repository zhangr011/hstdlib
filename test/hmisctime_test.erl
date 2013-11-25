%%%-------------------------------------------------------------------
%%% @author zhangr <zhangr011@gmail.com>
%%% @copyright (C) 2013, zhangr
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2013 by zhangr
%%%-------------------------------------------------------------------
-module(hmisctime_test).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% mod_timer_test
mod_timer_test() ->
    ok = application:start(hstdlib),
    %% 断言ets初始化成功
    ?assertNotEqual(ets:info(ets_timer), undefined),
    %% ?assertEqual(hmisc:is_process_alive(mod_timer), true),
    ?assertNotEqual(0, hmisctime:unixtime()),
    ?assertNotEqual(0, hmisctime:longunixtime()),
    Last = hmisctime:longunixtime(),
    timer:sleep(200),
    Next = hmisctime:longunixtime(),
    ?assertNotEqual(Last, Next),
    application:stop(hstdlib).

cal_begin_end_test_() ->
    hloglevel:set(6),
    [?_assertEqual({1371175200, 1372039200}, 
                   hmisctime:cal_begin_end({2013, 6, 14, 10, 0, 0}, {2013, 6, 24, 10, 0, 0}, range)),
     ?_assertEqual({undefined, undefined}, hmisctime:cal_begin_end(233, 322, al)),
     ?_assertEqual({1371175200, 1372039200}, 
                   hmisctime:cal_begin_end({2013, 6, 14, 10, 0, 0}, {0, 0, 10, 0, 0, 0}, plus))].
