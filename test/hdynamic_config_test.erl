%%%-------------------------------------------------------------------
%%% @author zhangr <zhangr011@gmail.com>
%%% @copyright (C) 2013, zhangr
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2013 by zhangr
%%%-------------------------------------------------------------------
-module(dynamic_config_test).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

dynamic_compile_test() ->
    ok = application:start(hstdlib),
    dynamic_config:start(),
    %% ?assertNotEqual(config:get_tcp_listener_ip(), undefined),
    %% ?assertNotEqual(config:get_tcp_listener_port(), undefined),
    ?assertNotEqual(config:get_data_words_version(), undefined),
    ?assertEqual(config:get_can_gmcmd(), 1),
    application:stop(hstdlib).

