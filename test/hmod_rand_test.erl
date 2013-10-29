-module(hmod_rand_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

hmod_rand_test() ->
    ok = application:start(hstdlib),
    %%mod_rand:start_link(),
    ?assertNot(undefined == whereis(hstdlib_sup)),
    ?assertNotEqual(hmod_rand:get_seed(), {1,1,1}),
    application:stop(hstdlib).
    
