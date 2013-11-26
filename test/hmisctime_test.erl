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
start_test() ->
    application:start(hstdlib).
%% mod_timer_test
mod_timer_test() ->
    application:start(hstdlib),
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
     ?_assertEqual({1371175200, 1372042861}, 
                   hmisctime:cal_begin_end({2013, 6, 14, 10, 0, 0}, {0, 0, 10, 1, 1, 1}, plus))].


cal_begin_end_advance_test_() ->
    [?_assertEqual({1371175200, 1372039200, 1371085139}, 
                   hmisctime:cal_begin_end_advance({2013, 6, 14, 10, 0, 0}, {2013, 6, 24, 10, 0, 0}, {0, 0, 1, 1, 1, 1}, range)),
     ?_assertEqual({undefined, undefined, undefined}, hmisctime:cal_begin_end_advance(233, 322, 1, al)),
     ?_assertEqual({1371175200, 1372042861, 1371085139},
                   hmisctime:cal_begin_end_advance({2013, 6, 14, 10, 0, 0}, {0, 0, 10, 1, 1, 1}, {0, 0, 1, 1, 1, 1}, plus)),
    ?_assertEqual({1371175200, 1372042861, 1371175200},
                  hmisctime:cal_begin_end_advance({2013, 6, 14, 10, 0, 0}, {0, 0, 10, 1, 1, 1}, undefined, plus))].
%% 周的测试是相对本周的起始时间，所以想要重新回归测试，就得修改期望值
%% cal_week_cycle_test_() ->
%%     application:start(hstdlib),
%%     [?_assertEqual({undefined, undefined}, hmisctime:cal_week_cycle({1, {10, 30}, {2, 00}})),
%%      ?_assertEqual({1385373600, 1385452800}, hmisctime:cal_week_cycle({1, {18, 0}, {22, 00}})),
%%      ?_assertEqual({1385373600, 1385452800}, hmisctime:cal_week_cycle([{1, {10, 30}, {2, 00}}, {1, {18, 0}, {22, 00}}])),
%%      ?_assertEqual({undefined, undefined}, hmisctime:cal_week_cycle(errr))].

%% cal_week_cycle_advance_test_() ->
%%     application:start(hstdlib),
%%     [?_assertEqual({undefined, undefined, undefined}, hmisctime:cal_week_cycle_advance({1, {10, 30}, {2, 00}}, {0, 0, 1, 1, 1, 1})),
%%      ?_assertEqual({1385373600, 1385452800, 1385283539}, hmisctime:cal_week_cycle_advance({1, {18, 0}, {22, 00}}, {0, 0, 1, 1, 1, 1})),
%%      ?_assertEqual({1385373600, 1385452800, 1385283539}, hmisctime:cal_week_cycle_advance([{1, {10, 30}, {2, 00}}, {1, {18, 0}, {22, 00}}], {0, 0, 1, 1, 1, 1})),
%%      ?_assertEqual({1385373600, 1385452800, 1385373600}, hmisctime:cal_week_cycle_advance([{1, {10, 30}, {2, 00}}, {1, {18, 0}, {22, 00}}], undefined)),
%%      ?_assertEqual({undefined, undefined, undefined}, hmisctime:cal_week_cycle_advance(errr, eax))].

%% get_timestamp_of_month_start_test_() ->
%%     application:start(hstdlib),
%%     [?_assertEqual(1383235200, hmisctime:get_timestamp_of_month_start())].


%% cal_month_cycle_test_() ->
%%     application:start(hstdlib),
%%     [?_assertEqual({undefined, undefined}, hmisctime:cal_month_cycle({25, {10, 30}, {2, 00}})),
%%      ?_assertEqual({1385373600, 1385452800}, hmisctime:cal_month_cycle({25, {18, 0}, {22, 00}})),
%%      ?_assertEqual({1385373600, 1385452800}, hmisctime:cal_month_cycle([{25, {10, 30}, {2, 00}}, {25, {18, 0}, {22, 00}}])),
%%      ?_assertEqual({undefined, undefined}, hmisctime:cal_month_cycle(errr))].

%% cal_month_cycle_advance_test_() ->
%%     application:start(hstdlib),
%%     [?_assertEqual({undefined, undefined, undefined}, hmisctime:cal_month_cycle_advance({25, {10, 30}, {2, 00}}, {0, 0, 1, 1, 1, 1})),
%%      ?_assertEqual({1385373600, 1385452800, 1385283539}, hmisctime:cal_month_cycle_advance({25, {18, 0}, {22, 00}}, {0, 0, 1, 1, 1, 1})),
%%      ?_assertEqual({1385373600, 1385452800, 1385283539}, hmisctime:cal_month_cycle_advance([{25, {10, 30}, {2, 00}}, {25, {18, 0}, {22, 00}}], {0, 0, 1, 1, 1, 1})),
%%      ?_assertEqual({1385373600, 1385452800, 1385373600}, hmisctime:cal_month_cycle_advance([{25, {10, 30}, {2, 00}}, {25, {18, 0}, {22, 00}}], undefined)),
%%      ?_assertEqual({undefined, undefined, undefined}, hmisctime:cal_month_cycle_advance(errr, eax))].



%% cal_day_cycle_test_() ->
%%     application:start(hstdlib),
%%     [?_assertEqual({1385433000, 1385440200}, hmisctime:cal_day_cycle({{10, 30}, {2, 00}})),
%%      ?_assertEqual({undefined, undefined}, hmisctime:cal_day_cycle({{18, 0}, {22, 00}})),
%%      ?_assertEqual({1385433000, 1385440200}, hmisctime:cal_day_cycle([{{10, 30}, {2, 00}}, {{18, 0}, {22, 00}}])),
%%      ?_assertEqual({undefined, undefined}, hmisctime:cal_day_cycle(errr))].

%% cal_day_cycle_advance_test_() ->
%%     application:start(hstdlib),
%%     [?_assertEqual({1385433000, 1385440200, 1385342939}, hmisctime:cal_day_cycle_advance({{10, 30}, {2, 00}}, {0, 0, 1, 1, 1, 1})),
%%      ?_assertEqual({1385460000, 1385539200, 1385369939}, hmisctime:cal_day_cycle_advance({{18, 0}, {22, 00}}, {0, 0, 1, 1, 1, 1})),
%%      ?_assertEqual({1385433000, 1385440200, 1385429400}, hmisctime:cal_day_cycle_advance([{{10, 30}, {2, 00}}, {{18, 0}, {22, 00}}], {0, 0, 0, 1, 0, 0})),
%%      ?_assertEqual({1385433000, 1385440200, 1385433000}, hmisctime:cal_day_cycle_advance([{{10, 30}, {2, 00}}, {{18, 0}, {22, 00}}], undefined)),
%%      ?_assertEqual({undefined, undefined, undefined}, hmisctime:cal_day_cycle_advance(errr, eax))].
