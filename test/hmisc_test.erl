-module(hmisc_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
%%my_test() ->
  %%  ok = application:start(hstdlib).

to_list_test()->
    ?assertEqual("99", hmisc:to_list(99)),
    ?assertEqual("5.55000000000000000000e+01" , hmisc:to_list(55.5)),
    ?assertEqual("abcd", hmisc:to_list(abcd)),
    ?assertEqual("9", hmisc:to_list(<<12345>>)),
    ?assertEqual("1234", hmisc:to_list("1234")).

to_binary_test() ->
    ?assertEqual(<<123>>, hmisc:to_binary(<<123>>)),
    ?assertEqual(<<"abcd">>, hmisc:to_binary(abcd)),
    %%?assertEqual('_', hmisc:to_binary("12345")),
    ?assertEqual(<<"abcd">>, hmisc:to_binary("abcd")),
    ?assertEqual(<<"125">>, hmisc:to_binary(125)),
    A=list_to_binary(hmisc:f2s(5.5)),
    ?assertEqual(A, hmisc:to_binary(5.5)).

to_integer_test() ->
    ?assertEqual(125, hmisc:to_integer(125)),
    ?assertEqual(1234, hmisc:to_integer("1234")),
    ?assertEqual(123, hmisc:to_integer(123.4)),
    ?assertEqual(68, hmisc:to_integer(<<"68">>)).

to_bool_test() ->
    ?assertEqual(true, hmisc:to_bool(1243)),
    ?assertEqual(false, hmisc:to_bool(0)).
    
