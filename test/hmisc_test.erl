-module(hmisc_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


rand_test() ->
    ok = application:start(hstdlib),
    ?assertEqual(1, hmisc:rand(1, 1)),
    ?assertNotEqual(3, hmisc:rand(1, 2)).

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
    ?assertEqual(false, hmisc:to_bool(0)),
    ?assertEqual(true, hmisc:to_bool("1234")),
    ?assertEqual(false, hmisc:to_bool([])),
    ?assertEqual(true, hmisc:to_bool(binary_to_list(<<"1234">>))),
    ?assertEqual(true, hmisc:to_bool(true)),
    ?assertEqual(false, hmisc:to_bool(false)).
my_test() ->
    ?assertEqual(hmisc:to_tuple({1,2,3}), {1,2,3}),
    ?assertEqual(hmisc:to_tuple([1,2,3]), {[1,2,3]}),
    ?assertEqual(yes, hmisc:is_string([])),
    ?assertEqual(yes, hmisc:is_string([1,2,3,4,5],non_unicode)),
    ?assertEqual(unicode, hmisc:is_string([1,2,3], unicode)),
    ?assertEqual(no, hmisc:is_string([], asdfdf)),
    ?assertEqual(hmisc:ceil(9), 9),
    ?assertEqual(hmisc:ceil(9.9), 10),
    ?assertEqual(hmisc:floor(9), 9),
    ?assertEqual(hmisc:floor(8.5), 8),
    ?assertEqual(hmisc:floor(-1.6), -2),
    ?assertEqual(hmisc:floor(-0.7), -1),
    ?assertEqual(hmisc:subatom(success, 3), suc),
    ?assertEqual(hmisc:int_to_hex(10), "0a"),
    ?assertEqual(hmisc:remove_string_blank("abc s df"), "abcsdf"),
    ?assertEqual(hmisc:max(10,2), 10),
    ?assertEqual(hmisc:max([3,5,7,8,2]), 8),
    ?assertEqual(hmisc:write_binary(bin, <<123>>), <<123>>),
    ?assertEqual(hmisc:write_binary(bit, 1), <<1:1>>),
    ?assertEqual(hmisc:write_binary(byte, 123), <<123:8>>),
    ?assertEqual(hmisc:write_binary(int8, 36475), <<36475:8>>),
    ?assertEqual(hmisc:to_integer(true), 1),
    ?assertEqual(hmisc:to_integer(false), 0),
    ?assertEqual(hmisc:choose_second_value({1,2}), 2),
    ?assertEqual(hmisc:choose_second_value({1,1}), undefined),
    ?assertEqual(hmisc:filter_undefined(undefined), false),
    ?assertEqual(hmisc:filter_undefined(asdf), true),
    ?assertEqual(hmisc:cal_binary_1_count(10), 2),
    ?assertEqual(hmisc:re_escape(jgg), []),
    ?assertEqual(hmisc:re_escape("abcd"), "abcd"),
    ?assertEqual(hmisc:re_escape("abc[cd]"), "abc\\[cd\\]"),
    application:stop(hstdlib).
    
    
