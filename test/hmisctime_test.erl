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
    hmisctime:unixtime(),
    hmisctime:longunixtime().
