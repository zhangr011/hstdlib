-module(mod_rand_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

mod_rand_test() ->
    ok = application:start(hstdlib),
    mod_rand:start_link(),
    ?assertNot(undefined == whereis(hstdlib_sup)),
    ?assertNotEqual(mod_rand:get_seed(), {1,1,1}).
    
