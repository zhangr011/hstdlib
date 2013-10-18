%%%-----------------------------------
%%% @Module  : util
%%% @Created : 2010.10.05
%%% @Description: 公共函数
%%%-----------------------------------

-module(util).

-include("define_logger.hrl").
-include("define_time.hrl").
-include("define_info_0.hrl").
-include("define_operate.hrl").
-include("define_math.hrl").
-include("define_player.hrl").
-include("define_beast.hrl").
-include("define_partner.hrl").
-include("define_location.hrl").

-export([
         atomize/1,
         implode/2,
         explode/2,
         explode/3,
         log/5,
         md5/1,
         hmac_sha1/2,
         rand/1,
         rand/2,
         shuffle/1,
         unixtime/0,
         longunixtime/0,
         filter_zero/1,
         to_integer/1,
         get_random_list/2,
         %% get_sex_by_career/1,
         get_server_start_time/0,
         ceil/1,
         floor/1,
         sleep/1,
         sleep/2,
         get_list/2,
         for/3,
         for/4,
         for_new/4,
         for2/2,
         ele_tail/2,
         to_string/1,
         term_to_string/1,
         term_to_bitstring/1,
         string_to_term/1,
         list_to_string/1,
         bitstring_to_term/1,
         seconds_to_localtime/1,
         is_same_date/2,
         get_week_start/0,
         is_same_week/2,
         get_midnight_seconds/1,
         get_next_day_seconds/1,
         get_diff_days/2,
         get_today_current_second/0,
         get_next_midnight_second/0,
         get_next_midnight_4oclock_second/0,
         get_today_start/0,
         get_date/0,
         get_week_day/0,
         get_hour/0,
         get_minute/0,
         get_pre_week_duringtime/0,
         get_this_week_duringtime/0,
         lnx/1,
         check_same_day/1,
         filter_list/3,
         get_chinese_count/1,
         lists_nth/2,
         lists_nth_replace/3,
         get_pos_num/1,
         get_pos_num2/1,
         get_max_num/2,
         make_sure_list/2,
         compile_base_data/3,
         register_fun/3,
         execute_registered_fun/1,
         get_day_start/1,
         max/2,
         max/1,
         rand1000/1,
         sorted_scequence_minus/1,
         to_record/2,
         write_binary/2,
         write_list_to_binary/1,
         get_container_and_partner_id/1,
         keyfind_first/2,
         get_combat_uid/0,
         rand/3,
         rand_n/2,
         split_and_strip/3,
         check_cooldown/1,
         compare_record/3,
         get_fields_filter/3,
         get_fields_modified/4,
         calc_fight_evaluate/1,
         to_utf8_string/1,
         to_term_list/1,
         bool_to_int/1,
         format/2,
         thing_to_list/1,
         %% test_utf8/0,
         get_tomorrow_start/0,
         get_tomorrow_start_diff/0,
         %% test_rand_power/0
         choose_second_value/1,
         filter_undefined/1,
         datetime_to_timestamp/1,
         datetime_to_timestamp/6,
         timestamp_to_datetime/1,
         datetime_plus/2,
         get_tomorrow_4_start_diff/0,
         is_same_month/2,
         get_month/0,
         get_month/1,
         get_day/0,
         get_day/1,
         lists_min/2,
         lists_max/2,
         cal_binary_1_count/1,
         get_today_pass/0,
         is_8_3_and_8_7/0,
         is_8_12_and_8_14_coin/0,
         is_8_12_and_8_14_exp/0,
         get_server_name/1,
         re_escape/1
        ]).

%%汉字unicode编码范围 0x4e00 - 0x9fa5
-define(UNICODE_CHINESE_BEGIN, (4*16*16*16 + 14*16*16)).
-define(UNICODE_CHINESE_END,   (9*16*16*16 + 15*16*16 + 10*16 + 5)).

%% atomize a string
atomize(Para) when is_binary(Para) ->
    atomize(thing_to_list(Para));
atomize(Para) when is_list(Para) ->
    list_to_atom(string:to_lower(Para)).

%% 在List中的每两个元素之间插入一个分隔符
implode(_S, [])->
	[<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [thing_to_list(H) | NList],
    implode(S, T, [S | L]).

%% 字符->列
explode(S, B)->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [list_to_integer(Str) || Str <- explode(S, B), Str =/= []].

thing_to_list(X) when is_integer(X) -> 
    integer_to_list(X);
thing_to_list(X) when is_float(X) -> 
    float_to_list(X);
thing_to_list(X) when is_atom(X) ->
    atom_to_list(X);
thing_to_list(X) when is_binary(X) ->
    binary_to_list(X);
thing_to_list(X) when is_list(X) ->
    X.

%% 日志记录函数
log(T, F, A, Mod, Line) ->
    {ok, Fl} = file:open("logs/error_log.txt", [write, append]),
    Format = list_to_binary("#" ++ T ++" ~s[~w:~w] " ++ F ++ "\r\n~n"),
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", 
                           integer_to_list(D), " ", integer_to_list(H), ":", 
                           integer_to_list(I), ":", integer_to_list(S)]),
    io:format(Fl, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A),
    file:close(Fl).    


%% 取得当前的unix时间戳
unixtime() ->
    {M, S, _} = misc_timer:now(),
    M * 1000000 + S.


longunixtime() ->
    {M, S, Ms} = misc_timer:now(),
    (M * 1000000000000 + S * 1000000 + Ms) div 1000.


%% 转换成HEX格式的md5
md5(S) ->
    lists:flatten(
      [io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(S))]).

hmac_sha1(Key, S) ->
    lists:flatten(
      [io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(crypto:sha_mac(Key, S))]).



%% 随机选择list中的一个元素
rand([]) ->
    0;
rand([{_, _} | _] = List) 
  when is_list(List) ->
    {NewList, FullPower} = 
        lists:foldl(
          fun({IKey, IPower}, {OldList, OldPower}) ->
                  %% 叠加权重处理
                  {[{IKey, OldPower + IPower} | OldList], 
                   OldPower + IPower}
          end, {[], 0}, List),
    Rand = rand(0, FullPower),
    %% 过滤掉小于随机值的数据
    case lists:filter(fun({_, Weight}) ->
                              if
                                  Weight >= Rand ->
                                      true;
                                  true ->
                                      false
                              end
                      end, lists:reverse(NewList)) of
        [] ->
            %% 没有了，那么返回0
            0;
        [{Key, _} | _Tail] ->
            %% 获取到第一个元素
            Key
    end;
rand(List)
  when is_list(List) ->
    %% 先随机获取一个位置，然后返回对一个的元素
    lists:nth(rand(1, length(List)), List);
rand(_) ->
    0.

shuffle(L) ->        
    List1 = [{rand(1, 10000), X} || X <- L], 
    List2 = lists:keysort(1, List1), 
    [E || {_, E} <- List2]. 


%% 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> 
    Same;
rand(Min, Max) ->
    M = Min - 1,
    if
        Max - M =< 0 ->
            0;
        true ->
            %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
            case get(rand_seed) of
                undefined ->
                    RandSeed = mod_rand:get_seed(),
                    random:seed(RandSeed),
                    put(rand_seed, RandSeed);
                _ ->
                    skip
            end,
            %% random:seed(erlang:now()),
            random:uniform(Max - M) + M
    end.

%% 根据权重选取 n 个目标
inner_rand_n_helper(Number, _, SelectedList)
  when Number =< 0 ->
    SelectedList;
inner_rand_n_helper(Number, RandomList, SelectedList) ->
    Target = rand(RandomList),
    inner_rand_n_helper(Number - 1, 
                        lists:filter(fun({Tar, _Power}) ->
                                        if
                                            Tar =/= Target ->
                                                true;
                                            true -> 
                                                false
                                        end
                                     end, RandomList),
                        [Target | SelectedList]).

%% 根据权重选取指定的 N 个目标对象
rand_n(Number, [{_, _} | _Tail] = RandomList) ->
    %% 从一个候选列表中根据权重选取指定数量的后备
    if
        length(RandomList) =< Number ->
            %% 候选列表不足，返回所有的数据
            lists:map(fun({Tar, _Power}) ->
                              Tar
                      end, RandomList);
        true -> 
            %% 选取指定数量的
            inner_rand_n_helper(Number, RandomList, [])
    end;
rand_n(N, List) ->
    Len = length(List),
    Ns = rand(N, 1, Len),
    lists:map(fun(I) ->
                      lists:nth(I, List)
              end, Ns).

%% 从[min , max] 中取出 N个数，不重复
rand(Count, Min, Max) 
  when (Max - Min) >= Count->
	rand(Count, Min, Max, []);
rand(_Count, Min, Max) ->
    lists:seq(Min, Max).
	
rand(0, _Min, _Max, List) ->
	List;
rand(Count, Min, Max, List) ->
	Num = rand(Min, Max),
	case lists:member(Num, List) of
		false->
			rand(Count - 1, Min, Max, [Num|List]);
		true ->
			rand(Count, Min, Max, List)
	end.


%% 过滤掉 0 数据
filter_zero(Num) ->
    if
        Num =:= 0 ->
            undefined;
        true -> 
            Num
    end.

%% to_integer
to_integer(undefined) ->
    0;
to_integer(Num) when is_integer(Num) ->
    Num;
to_integer(Num) when is_number(Num) ->
    floor(Num);
to_integer(Num) when is_binary(Num) ->
    try
        list_to_integer(binary_to_list(Num))
    catch _:Reason ->
            ?WARNING_MSG("Error ~w~n, Stack~p~n", [Reason, erlang:get_stacktrace()]),
            0
    end;
to_integer(Num) when is_list(Num) ->
    try
        list_to_integer(Num)
    catch _:Reason ->
            ?WARNING_MSG("Error ~w~n, Stack~p~n", [Reason, erlang:get_stacktrace()]),
            0
    end;
to_integer(_Num) ->
    %% ?WARNING_MSG("Unknow Type To Integer ~w~n", [Num]),
    0.

%%随机从集合中选出指定个数的元素length(List) >= Num
%%[1,2,3,4,5,6,7,8,9]中选出三个不同的数字[1,2,4]
get_random_list(List, Num) ->
	ListSize = length(List),
	F = fun(N, List1) ->
				Random = rand(1,(ListSize-N+1)),
				Elem = lists:nth(Random, List1),
				List2 = lists:delete(Elem, List1),
				List2
		end,
	Result = lists:foldl(F, List, lists:seq(1, Num)),
	List -- Result.

%% %% @doc 根据职业获取性别，仅限于主角
%% get_sex_by_career(Career) ->
%%     case Career of
%%         ?CAPTAIN_POWER_HONOR ->
%%             ?MALE;
%%         ?CAPTAIN_SWORDS_MAN ->
%%             ?FEMALE;
%%         ?CAPTAIN_MAGICIAN ->
%%             ?MALE;
%%         ?CAPTAIN_DANCER ->
%%             ?FEMALE;
%%         ?CAPTAIN_FEATHER_SHINE ->
%%             ?MALE;
%%         ?CAPTAIN_TIAN_YU ->
%%             ?FEMALE;
%%         _ ->
%%             ?MALE
%%     end.

%% @doc get server start time
get_server_start_time() ->
    {YY, MM, DD, HH, II, SS} = config:get_server_start_time(),
    util:datetime_to_timestamp(YY, MM, DD, HH, II, SS).

%% 向上取整
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

%%向下取整
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

sleep(T) ->
    receive
    after T -> ok
    end.

sleep(T, F) ->
    receive
    after T -> F()
    end.

get_list([], _) ->
    [];
get_list(X, F) ->
    F(X).

%% for循环
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> 
	{ok, State};
for(Max, Max, F, State) ->
    F(Max, State);
for(I, Max, F, State)   -> 
    {ok, NewState} = F(I, State), for(I+1, Max, F, NewState).


for_new(Min, Max, _F, State) when (Min > Max) -> 
	{ok, State};
for_new(Min, Max, F, State) -> 
	{ok, NewState} = F(Min, State), 
	for_new(Min+1, Max, F, NewState).

for2(F, State) ->
	for2(go_on, F, State).
for2(stop, _F, State) ->
	State;
for2(go_on, F, State) ->
	{IsGoOn, NewState} = F(State),
	for2(IsGoOn, F, NewState).

%% 取列表Ele后面的元素
ele_tail(_Ele, []) ->
	[];
ele_tail(Ele, [Ele|T]) ->
	T;
ele_tail(Ele, [_|T]) ->
	ele_tail(Ele, T).

to_string(Integer) ->
    lists:flatten(io_lib:format("~p", [Integer])).

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~p", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%%将列表转换为string [a,b,c] -> "a,b,c"
list_to_string(List) ->
	case List == [] orelse List == "" of
		true -> "";
		false ->
			F = fun(E) ->
						tool:to_list(E)++","
				end,
			L1 = [F(E)||E <- List] ,
			L2 = lists:concat(L1),
			string:substr(L2,1,length(L2)-1)
	end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).


%% 时间函数
%% -----------------------------------------------------------------
%% 根据1970年以来的秒数获得日期
%% -----------------------------------------------------------------
seconds_to_localtime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(
                 Seconds + ?DIFF_SECONDS_0000_1900),
    calendar:universal_time_to_local_time(DateTime).

%% -----------------------------------------------------------------
%% 判断是否同一天
%% -----------------------------------------------------------------
is_same_date(Seconds1, Seconds2) ->
    %% 需要考虑时区问题
    NDay = (Seconds1 + ?TIME_ZONE_SECONDS) div ?ONE_DAY_SECONDS,
    ODay = (Seconds2 + ?TIME_ZONE_SECONDS) div ?ONE_DAY_SECONDS,
    NDay =:= ODay.

%%     {{Year1, Month1, Day1}, _Time1} = seconds_to_localtime(Seconds1),
%%     {{Year2, Month2, Day2}, _Time2} = seconds_to_localtime(Seconds2),
%% 	%%?DEBUG("_______________Y:~p M:~p d:~p",[Year1,Month1,Day1]),
%% 	%%?DEBUG("_______________Y:~p M:~p d:~p",[Year2,Month2,Day2]),
%%     if ((Year1 == Year2) andalso (Month1 == Month2) andalso (Day1 == Day2)) -> true;
%%         true -> false
%%     end.

%% -----------------------------------------------------------------
%% 判断是否同一月
%% -----------------------------------------------------------------
is_same_month(Seconds1, Seconds2) ->
    {{Year1, Month1, _Day1}, _Time1} = timestamp_to_datetime(Seconds1),
    {{Year2, Month2, _Day2}, _Time2} = timestamp_to_datetime(Seconds2),
    ?DEBUG("is_same_month Y:~p M:~p d:~p",[Year1,Month1,_Day1]),
    ?DEBUG("is_same_month Y:~p M:~p d:~p",[Year2,Month2,_Day2]),
    if 
        (Year1 == Year2) andalso (Month1 == Month2) -> 
            true;
        true -> 
            false
    end.

%% 获取当天0点秒数
get_today_start() ->
    Now = unixtime(),
    Now - ((Now + ?TIME_ZONE_SECONDS) rem ?ONE_DAY_SECONDS) .
get_tomorrow_start() ->
    get_today_start() + ?ONE_DAY_SECONDS.

%% 获取今天已经流逝的秒数
get_today_pass() ->
    Now = unixtime(),
    ((Now + ?TIME_ZONE_SECONDS) rem ?ONE_DAY_SECONDS).

%% 获取距离明天0点的秒数
get_tomorrow_start_diff() ->
    Now = unixtime(),
    get_today_start() + ?ONE_DAY_SECONDS - Now.

%% 获取距离明天4点的秒数
get_tomorrow_4_start_diff() ->
    Now = unixtime(),
    get_today_start() + ?ONE_HOUR_SECONDS * 4 +?ONE_DAY_SECONDS - Now.

%% 获取本周开始的秒数
get_week_start() ->
	Now = unixtime(),
	Now - ((Now + ?TIME_ZONE_SECONDS) rem ?ONE_WEEK_SECONDS).

%% -----------------------------------------------------------------
%% 判断是否同一星期
%% -----------------------------------------------------------------
is_same_week(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, Time1} = seconds_to_localtime(Seconds1),
    % 星期几
    Week1  = calendar:day_of_the_week(Year1, Month1, Day1),
    % 从午夜到现在的秒数
    Diff1  = calendar:time_to_seconds(Time1),
    Monday = Seconds1 - Diff1 - (Week1 - 1) * ?ONE_DAY_SECONDS,
    Sunday = Seconds1 + (?ONE_DAY_SECONDS - Diff1) + 
        (7 - Week1) * ?ONE_DAY_SECONDS,
    if
        ((Seconds2 >= Monday) and (Seconds2 < Sunday)) ->
            true;
        true ->
            false
    end.

%% -----------------------------------------------------------------
%% 获取当天0点和第二天0点
%% -----------------------------------------------------------------
get_midnight_seconds(Seconds) ->
    {{_Year, _Month, _Day}, Time} = seconds_to_localtime(Seconds),
    %% 从午夜到现在的秒数
    Diff   = calendar:time_to_seconds(Time),
    %% 获取当天0点
    Today  = Seconds - Diff,
    %% 获取第二天0点
    NextDay = Seconds + (?ONE_DAY_SECONDS - Diff),
    {Today, NextDay}.

%% 获取下一天开始的时间
get_next_day_seconds(Now) ->
    {{_Year, _Month, _Day}, Time} = util:seconds_to_localtime(Now),
    %% 从午夜到现在的秒数
    Diff = calendar:time_to_seconds(Time),
    Now + (?ONE_DAY_SECONDS - Diff).

%% -----------------------------------------------------------------
%% 计算相差的天数
%% -----------------------------------------------------------------
get_diff_days(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, _} = seconds_to_localtime(Seconds1),
    {{Year2, Month2, Day2}, _} = seconds_to_localtime(Seconds2),
    Days1 = calendar:date_to_gregorian_days(Year1, Month1, Day1),
    Days2 = calendar:date_to_gregorian_days(Year2, Month2, Day2),
	abs(Days2 - Days1).
    %% DiffDays=abs(Days2-Days1),
    %% DiffDays + 1.

%% 获取从午夜到现在的秒数
get_today_current_second() ->
    {_, Time} = calendar:now_to_local_time(misc_timer:now()),
    calendar:time_to_seconds(Time).

%% 获取从现在到第二天午夜的秒数
get_next_midnight_second() ->
	Now = unixtime(),
	get_next_day_seconds(Now) - Now.

%% 获取从现在到第二天凌晨4点的秒数
get_next_midnight_4oclock_second() ->
    get_next_midnight_second() + 4 * ?ONE_HOUR_SECONDS.



%% 判断今天星期几 
get_week_day() ->
    get_date().

%% 1970.1.1 为星期四
get_date() ->
    %%7.
    Now = unixtime(),
        WeekDay = ((((Now + ?TIME_ZONE_SECONDS) rem ?ONE_WEEK_SECONDS) div ?ONE_DAY_SECONDS) + 4) rem 7,
    if
        WeekDay =:= 0 -> 
            7;
        true -> 
            WeekDay
    end.

    %% calendar:day_of_the_week(date()).

get_hour() ->
    ((unixtime() + ?TIME_ZONE_SECONDS) rem ?ONE_DAY_SECONDS) div ?ONE_HOUR_SECONDS.

get_minute() ->
    ((unixtime() + ?TIME_ZONE_SECONDS) rem ?ONE_HOUR_SECONDS) div ?ONE_MINITE_SECONDS.

%% 获取当前天数
get_day() ->
    Now = unixtime(),
    get_day(Now).

%% 获取当前天数
get_day(Now) ->
    {{_Year, _Month, Day}, _Time} = seconds_to_localtime(Now),
    Day.


%% 获取当前月
get_month() ->
    Now = unixtime(),
    get_month(Now).

%% 获取当前月
get_month(Now) ->
    {{_Year, Month, _Day}, _Time} = seconds_to_localtime(Now),
    Month.

%%获取上一周的开始时间和结束时间
get_pre_week_duringtime() ->
    OrealTime = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    {Year, Month, Day} = date(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(
                    {{Year,Month,Day}, {0,0,0}}
                   ) - OrealTime - 8 * 60 * 60,%%从1970开始时间值
    WeekDay = calendar:day_of_the_week(Year,Month,Day),
    Day1 = 
        case WeekDay of %%上周的时间
            1 -> 7;
            2 -> 7+1;
            3 -> 7+2;
            4 -> 7+3;
            5 -> 7+4;
            6 -> 7+5;
            7 -> 7+6
        end,
    StartTime = CurrentTime - Day1*24*60*60,
    EndTime = StartTime + 7 * 24 * 60 * 60,
    {StartTime, EndTime}.
	
%%获取本周的开始时间和结束时间
get_this_week_duringtime() ->
	OrealTime =  calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
	{Year,Month,Day} = date(),
	CurrentTime = calendar:datetime_to_gregorian_seconds({{Year,Month,Day}, {0,0,0}})-OrealTime-8*60*60,%%从1970开始时间值
	WeekDay = calendar:day_of_the_week(Year,Month,Day),
	Day1 = 
	case WeekDay of %%上周的时间
		1 -> 0;
		2 -> 1;
		3 -> 2;
		4 -> 3;
		5 -> 4;
		6 -> 5;
		7 -> 6
	end,
	StartTime = CurrentTime - Day1*24*60*60,
	EndTime = StartTime+7*24*60*60,
	{StartTime,EndTime}.

%% 传入日期时间，返回时间戳
datetime_to_timestamp(undefined) ->
    undefined;
datetime_to_timestamp(all) ->
    all;
datetime_to_timestamp({Year, Month, Day, Hour, Min, Sec}) ->
    datetime_to_timestamp(Year, Month, Day, Hour, Min, Sec).

datetime_to_timestamp(Year, Month, Day, Hour, Min, Sec) ->
    OrealTime =  calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    ZeroSecs = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}),
    ZeroSecs - OrealTime - ?TIME_ZONE_SECONDS.

%% 传入时间戳，返回日期时间
timestamp_to_datetime(Timestamp) ->
    seconds_to_localtime(Timestamp).
    %% io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
    %%               [YY, MM, DD, Hour, Min, Sec]). 

%% 日期叠加
datetime_plus({Year, Month, Day, Hour, Min, Sec}, {DY, DM, DD, DH, DMin, DS}) ->
    Delta = DD * ?ONE_DAY_SECONDS + DH * ?ONE_HOUR_SECONDS + DMin * ?ONE_MINITE_SECONDS + DS,
    TempMon = Month + DM,
    RMon = (TempMon - 1) rem 12 + 1,
    CYear = (TempMon - 1) div 12,
    EndTime = datetime_to_timestamp(Year + DY + CYear, RMon, Day, Hour, Min, Sec) + Delta,
    timestamp_to_datetime(EndTime).
    
%% 以e=2.718281828459L为底的对数
lnx(X) ->
	math:log10(X) / math:log10(?E).

check_same_day(Timestamp)->
	NDay = (util:unixtime()+8*3600) div 86400,
	ODay = (Timestamp+8*3600) div 86400,
	NDay=:=ODay.

%%对list进行去重，排序
%%Replicat 0不去重，1去重
%%Sort 0不排序，1排序
filter_list(List,Replicat,Sort) ->
	if Replicat == 0 andalso Sort == 0 ->
		   List;
	   true ->
		   if Replicat == 1 andalso Sort == 1 ->
				  lists:usort(List);
			  true ->
				   if Sort == 1 ->
						  lists:sort(List);
					  true ->
						  lists:reverse(filter_replicat(List,[]))
				   end
		   end
	end.

%%list去重
filter_replicat([],List) ->
	List;
filter_replicat([H|Rest],List) ->
	Bool = lists:member(H, List),
	List1 = 
	if Bool == true ->
		   [[]|List];
	   true ->
		   [H|List]
	end,
	List2 = lists:filter(fun(T)-> T =/= [] end, List1),
	filter_replicat(Rest,List2).


%% ------------------------------------------------------
%% desc   获取字符串汉字和非汉字的个数  
%% parm   UTF8String  			UTF8编码的字符串
%% return {汉字个数,非汉字个数}
%% -------------------------------------------------------
get_chinese_count(UTF8String)->
	UnicodeList = unicode:characters_to_list(list_to_binary(UTF8String)),
	Fun = fun(Num,{Sum})->
				  case Num >= ?UNICODE_CHINESE_BEGIN  andalso  Num =< ?UNICODE_CHINESE_END of
					  true->
						  {Sum+1};
					  false->
						  {Sum}
				  end
		  end,
	{ChineseCount} = lists:foldl(Fun, {0}, UnicodeList),
	OtherCount = length(UnicodeList) - ChineseCount,
	{ChineseCount,OtherCount}.

%% 与lists:nth一样，不过多了0判断和N>length(List)情况的判断
lists_nth(0, _) -> [];
lists_nth(1, [H|_]) -> H;
lists_nth(_, []) -> [];
lists_nth(N, [_|T]) when N > 1 ->
    lists_nth(N - 1, T).

%% 替换列表第n个元素
lists_nth_replace(N, L, V) ->
	lists_nth_replace(N, L, V, []).
lists_nth_replace(0, L, _V, _OH) -> L;
lists_nth_replace(1, [_H|T], V, OH) -> recover(OH, [V|T]);
lists_nth_replace(_, [], _V, OH) -> recover(OH, []);
lists_nth_replace(N, [H|T], V, OH) when N > 1 ->
    lists_nth_replace(N - 1, T, V, [H|OH]).

recover([], Hold) ->Hold;
recover([H|T], Hold) ->
	recover(T, [H|Hold]).

%% 如果参数小于0，则取0
get_pos_num(Num) ->
	if Num < 0 ->
		   0;
	   true ->
		   Num
	end.

%% 如果参数小于1，则取1
get_pos_num2(Num) ->
	if Num < 1 ->
		   1;
	   true ->
		   Num
	end.

get_max_num(Num, Max) ->
	if Num > Max ->
		   Max;
	   true ->
		   Num
	end.

make_sure_list(List, Where) ->
	if is_list(List) -> 
		   List; 
	   true ->
		   ?ERROR_MSG("List=~p, Where=~p~n", [List, Where]),
		   []
	end.


compile_base_data(Table, ModName, IDPoses) ->
	ModNameString = util:term_to_string(ModName),
	HeadString = 
		"-module("++ModNameString++").
		-compile(export_all).
		",
	BaseDataList = db_base:select_all(Table, "*", []),
	ContentString = 
	lists:foldl(fun(BaseData0, PreString) ->
						FunChange = 
							fun(Field) ->
									 if is_integer(Field) -> Field; 
										true -> 
											case util:bitstring_to_term(Field) of
												undefined ->
													Field;
												Term ->
													Term
											end
									 end
							end,
						BaseData = [FunChange(Item)||Item <- BaseData0],
						Base =list_to_tuple([Table|BaseData]),
						BaseString = util:term_to_string(Base),
						IDs = [element(Pos, Base)||Pos<-IDPoses],
						IDList0 = lists:foldl(
                                    fun(ID, PreString2)->
                                            IdList = 
                                                if erlang:is_integer(ID) ->
                                                        integer_to_list(ID);
                                                   true ->
                                                        ID
                                                end,
                                            PreString2 ++ "," ++ IdList
                                    end, [], IDs),
						[_|IDList] = IDList0,
						PreString ++ 
							"get(" ++ 
							IDList ++ 
							") ->" ++ 
							BaseString ++
							";
							"
				end
				, "", BaseDataList),
	
	_List0 = [",_"||_Pos<-IDPoses],
	[_|_List] = lists:flatten(_List0),
	ErrorString = "get("++_List++") -> undefined.
	",
	FinalString = HeadString++ContentString++ErrorString,
	%% io:format("string=~s~n",[FinalString]),
	try
        {Mod,Code} = dynamic_compile:from_string(FinalString),
        code:load_binary(Mod, ModNameString++".erl", Code)
    catch
        Type:Error -> ?ERROR_MSG("Error compiling (~p): ~p~n", [Type, Error])
    end,
	ok.

%% 注册函数
register_fun(Fun, Times, Key) ->
	case get({register_fun, Key}) of
		[_|_] = RegisteredFuns ->
			put({register_fun, Key}, [{Fun, Times}|RegisteredFuns]);
		_ ->
			put({register_fun, Key}, [{Fun, Times}])
	end.

%% 执行注册函数
execute_registered_fun(Key) ->
	case get({register_fun, Key}) of
		[_|_] = Funs ->
			NewFuns = 
				lists:foldl(fun({Fun, Times}, Pre) ->
									try Fun() of _ -> ok			%% try执行
									catch _:R -> 
                                            ?ERROR_MSG("R=~p, stack=~p~n", 
                                                       [R, erlang:get_stacktrace()]) 
                                    end,	
									case Times of
										1 -> Pre;					%% 已执行完相应次数
										loop -> [{Fun, loop}|Pre];	%% 循环执行
										_ -> [{Fun, Times-1}|Pre]	%% 剩余次数减一
									end
							end, [], Funs),
			put({register_fun, Key}, NewFuns);
		_Other ->
				 skip
	end.

get_day_start(Time) ->
	Time - ((Time + ?TIME_ZONE_SECONDS) rem ?ONE_DAY_SECONDS).

%%返回两个中较大的数
max(Arg1,Arg2) ->
   Res =  if
              Arg1 >= Arg2 -> 
                  Arg1;
              true -> 
                  Arg2
          end,
    Res.

%%从一列数组中找出最大值
max(NumList) ->
    lists:foldl(fun(Num,Max) ->
                       if
                           Num >= Max ->
                               Num;
                           true ->
                               Max
                       end
                end, 0, NumList).
                     
rand1000(Rate) ->
    R = util:rand(1,1000),
    if
        Rate > R-> 
            false;
        true -> 
            true
    end.


%% @spec
%% sorted_scequence_minus([{1, Pid1}, {5, Pid2}, {7, Pid3}]) ->
%%                        [{0, Pid1}, {4, Pid2}, {6, Pid3}]
%% @end
sorted_scequence_minus([]) ->
    {0, []};
sorted_scequence_minus(List) ->
    SortedList = lists:sort(List),
    [{DelayToMinus, _Pid} | _Tail] = SortedList,
    MinusFunc = fun({Delay, Pid}) ->
                    {Delay - DelayToMinus, Pid}
            end,
    {DelayToMinus, lists:map(MinusFunc, SortedList)}.

%% @spec
%% list to record
%% @end
to_record(Record, RecordInfo) 
  when is_list(RecordInfo) ->
    list_to_tuple([Record | RecordInfo]).

%% @spec
%% 按照指定的格式打包指定的数据
%% @end
write_binary(bin, Value)
  when is_binary(Value) ->
    %% for: bin     value
    Value;
write_binary(bit, Value)
  when is_number(Value) ->
    %% for: bit     value
    <<Value:1>>;
write_binary(byte, Value)
  when is_number(Value) ->
    %% for: int:8   value
    <<Value:8>>;
write_binary(int8, Value)
  when is_number(Value) ->
    %% for: int:8   value
    <<Value:8>>;
write_binary(int16, Value)
  when is_number(Value) ->
    %% for: int:16  value
    <<Value:16>>;
write_binary(int32, Value)
  when is_number(Value) ->
    %% for: int:32  value
    <<Value:32>>;
write_binary(int64, Value)
  when is_number(Value) ->
    %% for: int:64  value
    <<Value:64>>;
write_binary(string, [Head | _Tail] = Value)
  when is_list(Value) andalso is_number(Head) ->
    %% for: string  value
    BinString = pt:write_string(Value),
    <<BinString/binary>>;
write_binary(Type, ValueList) 
  when is_atom(Type) andalso is_list(ValueList) ->
    %% for: int:16 repeated_time
    %%      array(
    %%          type value
    %%      )
    Length = length(ValueList),
    NewBinary = tool:to_binary(lists:map(fun(Value) ->
                                                 write_binary(Type, Value)
                                         end,
                                         ValueList)),
    <<Length:16, NewBinary/binary>>;
write_binary(TypeList, ValueTuple) 
  when is_list(TypeList) andalso is_tuple(ValueTuple) ->
    write_binary(TypeList, tuple_to_list(ValueTuple));
write_binary(TypeList, []) 
  when is_list(TypeList) ->
    %%TypeList不为空，value为空是，直接打包为空
    <<0:16>>;
write_binary(TypeList, [Head | _Tail] = ValueList) 
  when is_list(TypeList) andalso is_list(ValueList) andalso 
       not is_list(Head) andalso not is_tuple(Head) ->
    %% [type1, type2, ...], [value1, value2, ...]
    %% for: type1  value1
    %%      type2  value2
    %%      ...
    if
        length(TypeList) =:= length(ValueList) -> 
            %% 根据TypeList和ValueList匹配打包成二进制数据
            tool:to_binary(lists:zipwith(fun(Type, Value) ->
                                                 write_binary(Type, Value)
                                         end, TypeList, ValueList));
        true ->
            ?DEBUG("TypeList's length and ValueList's length not matched.~p", []),
            <<>>
    end;
write_binary(TypeList, [Head | _Tail] = ValueList) 
  when is_list(TypeList) andalso is_list(ValueList) andalso 
       (is_list(Head) orelse is_tuple(Head)) ->
    %% [type1, type2, ...], [[value1, value2, ...], 
    %%                       [value3, value4, ...], 
    %%                       ...]
    %% for: int:16   repeated_time
    %%      array(
    %%      type1    value1
    %%      type2    value2
    %%      ...
    %%      )
    Length = length(ValueList),
    NBinary = tool:to_binary(lists:map(
                               fun(ValueListIn) ->
                                       write_binary(TypeList, ValueListIn)
                               end, ValueList)),
    <<Length:16, NBinary/binary>>.

%% @spec
%% 将数据列表写成二进制串
%% [{byte, Byte}, {int16, Int16} ... {string, String}] -> Binary.
%% @end
write_list_to_binary(ArgList) when is_list(ArgList) ->
    tool:to_binary(lists:map(fun({Type, Value}) ->
                                     %% ?DEBUG("TYPE:~w~nValue:~w~n", [Type, Value]),
                                     write_binary(Type, Value)
                             end, ArgList)).

%% 根据伙伴id获取包裹位置及伙伴id，用于给客户端发消息时的数据处理
get_container_and_partner_id(ParterId) ->
    if
        ParterId =:= 0 ->
            {?PLAYER_EQUIP_LOCATION, undefined};
        true ->
            {?PARTNER_EQUIP_LOCATION, ParterId}
    end.


-spec keyfind_first(Pred, List) -> boolean() when
      Pred :: fun((Elem :: T) -> boolean()),
      List :: [T],
      T :: term().

keyfind_first(F, [H|T]) ->
    Res = F(H),
    if
        Res =:= true -> 
            H;
        true ->
            keyfind_first(F, T)
    end;
keyfind_first(F, []) when is_function(F, 1) -> [].

%% 获取战斗中的唯一Id
get_combat_uid() ->
    mod_id_factory:get_auto_id().

%% @doc 分解并去除字符两端的无用数据
%% @spec
%% @end
split_and_strip(String, SplitChar, StripChar) ->
    lists:map(fun(Word) ->
                      string:strip(Word, both, StripChar)
              end, string:tokens(String, SplitChar)).

%% @doc 检测 cd 时间是否已经到了
%% @spec
%% @end
check_cooldown({BookMark, Timer}) ->
    check_cooldown(BookMark, Timer);
%% check_cooldown(?OPERATE_COOLDOWN_MINOR) ->
%%     check_cooldown(?OPERATE_COOLDOWN_MINOR, ?OPERATE_TIME_DELTA_MINOR);
%% check_cooldown(?OPERATE_COOLDOWN_HUGE) ->
%%     check_cooldown(?OPERATE_COOLDOWN_HUGE, ?OPERATE_TIME_DELTA_HUGE);
%% check_cooldown(?OPERATE_COOLDOWN_COMBAT) ->
%%     check_cooldown(?OPERATE_COOLDOWN_COMBAT, ?OPERATE_TIME_DELTA_COMBAT);
check_cooldown(_) ->
    %% 传入的参数有误，一概拦截
    {fail, ?INFO_OPERATE_TOO_FREQUENTLY}.

%% @doc 检测 cd 时间是否已经到了
%% @spec
%% @end
check_cooldown(BookMark, Timer) ->
    Current = unixtime(),
    OldTimestamp = get(BookMark),
    if
        OldTimestamp =:= undefined ->
            put(BookMark, Current),
            ok;
        Current >= OldTimestamp + Timer ->
            put(BookMark, Current),
            ok;
        true -> 
            %% 操作过快，发送提示信息给客户端
            {fail, ?INFO_OPERATE_TOO_FREQUENTLY}
    end.

%% 比较两个Record值 Func返回True则返回Record中相应位置的B值，否则返回undefined
-spec compare_record(R1, R2, Pred) -> false | tuple() when
      Pred :: fun(({E1 :: T1, E2 :: T2}) -> boolean()),
      R1 :: tuple(),
      R2 :: tuple(),
      T1 :: term(),
      T2 :: term().
compare_record(R1, R2, Func) 
  when is_tuple(R1) andalso 
       is_tuple(R2) andalso
       is_function(Func)->
    L1 = tuple_to_list(R1),
    L2 = tuple_to_list(R2),
    [H1 | Rest1] = L1,
    [H2 | Rest2] = L2,
    if
        is_atom(H1) andalso 
        is_atom(H2) andalso
        H1 =:= H2 -> 
            ZipList = lists:zip(Rest1, Rest2),
            RetList = lists:map(fun({A, B}) ->
                                        Func({A, B})
                                end, ZipList),
            list_to_tuple([H1 | RetList]);
        true -> 
            false
    end;
compare_record(_R1, _R2, _F) ->
    false.

-spec get_fields_modified(R1, R2, FieldsList, ListModified) -> list() when
      R1 :: tuple(),
      R2 :: tuple(),
      FieldsList :: list(),
      ListModified :: list().
get_fields_modified(R1, R2, FieldsList, ListModified)
  when is_tuple(R1) andalso
       is_tuple(R2) ->
    L1 = tuple_to_list(R1),
    L2 = tuple_to_list(R2),
    [H1 | Rest1] = L1,
    [H2 | Rest2] = L2,
    if
        is_atom(H1) andalso 
        is_atom(H2) andalso
        H1 =:= H2 -> 
            RetList = 
                lists:map(fun({A, B}) ->
                                  if
                                      A =:= B ->
                                          undefined;
                                      true -> 
                                          1
                                  end
                          end, lists:zip(Rest1, Rest2)),
            FieldsModified = 
                lists:foldl(
                  fun({Field, Value}, InList) ->
                          if
                              Value =:= undefined ->
                                  %% 被过滤掉了
                                  InList;
                              Field =:= dirty ->
                                  %% 指定的用于标记的field需要过滤掉
                                  InList;
                              true -> 
                                  %% 有变化，加入到列表中
                                  [Field | InList]
                          end
                  end, [], lists:zip(FieldsList, RetList)),
            lists:umerge(
              lists:sort(ListModified), lists:sort(FieldsModified));
        true -> 
            %% 不匹配，那么直接返回原变化列表
            ListModified
    end.

get_fields_filter(Record, Fields, Func) 
  when is_tuple(Record) andalso 
       is_list(Fields) andalso
       is_function(Func) ->
    RList = tuple_to_list(Record),
    [_H | Values] = RList,
    ZipList = lists:zip(Fields, Values),
    lists:filter(fun({_Field, Value}) ->
                         Func(Value)
                 end, ZipList).

%% 计算战斗评价星级
calc_fight_evaluate(Point) ->
    if
        Point > 80 -> 
            5;
        Point > 60 -> 
            4;
        Point > 40 -> 
            3;
        Point > 20 -> 
            2;
        true ->
            1
    end.

to_utf8_string(String)
  when is_list(String) ->
    binary_to_list(unicode:characters_to_binary(String));
to_utf8_string(String) 
  when is_binary(String) ->
    String;
to_utf8_string(_Other) ->
    ?DEBUG("to_utf8_string unknow sting : ~w ~n",[_Other]),
    [].



to_term_list(Ids)
  when is_binary(Ids) ->
    case util:bitstring_to_term(Ids) of
        List 
          when is_list(List) ->
            List;
        _ ->
            []        
    end;
to_term_list(Ids) 
  when is_list(Ids) ->
    Ids;
to_term_list(_other) ->
    ?INFO_MSG("to_id_list unknown ids : ~w ~n",[_other]),
    [].

%% to_list(ListBin)
%%   when is_binary(ListBin) ->
%%     case util:bitstring_to_term(ListBin) of
%%         List 
%%           when is_list(List) ->
%%             List;
%%         _ ->
%%             []        
%%     end;
%% to_list(List) 
%%   when is_list(List) ->
%%     List;
%% to_list(_other) ->
%%     ?INFO_MSG("to_id_list unknown ids : ~w ~n",[_other]),
%%     [].

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0;
bool_to_int(_Other) ->
    ?WARNING_MSG("unknown bool : ~w~n",[_Other]),
    0.


format(Content, Args) 
  when is_list(Args)->
    NewArgs = lists:map(fun(Arg) ->
                                if
                                    is_binary(Arg) -> 
                                        Arg;
                                    true -> 
                                        util:to_string(Arg)
                                end
                        end, Args),
    io_lib:format(Content, NewArgs).

%% test_utf8() ->
%%     S1 = to_utf8_string(<<"中国">>),
%%     S2 = to_utf8_string("中国"),
%%     S3 = to_utf8_string("china"),
%%     S4 = to_utf8_string([20013,22269]),
    
%%     ?DEBUG("S1 : ~w, S2 : ~w, S3 : ~w, S4 : ~w", [S1, S2, S3, S4]),
%%     ?DEBUG("S1 : ~ts, S2 : ~ts, S3 : ~ts, S4 : ~ts", [S1, S2, S3, S4]),
%%     ok.


%% test_rand_power() ->
%%     test_rand_power_inner(1000000,0),
%%     ok.


%% test_rand_power_inner(0, FailedCnt) ->
%%     io:format("RAND_FAILDE Cnt : ~w~n",[FailedCnt]);
%% test_rand_power_inner(N, FailedCnt) ->
%%     RankList = [{1, 750}, {2, 200}, {3, 50}],
%%     case rand(RankList) of
%%         Result when is_integer(Result) andalso Result >=1 andalso Result =< 3 ->
%%             test_rand_power_inner(N-1, FailedCnt);
%%         _Other ->
%%             test_rand_power_inner(N-1, FailedCnt+1)
%%     end.  

%%--------------------------------------------------------------------
%% @doc
%% 将相同的值赋值为undefined，不同值取新的
%% @spec
%% @end
%%--------------------------------------------------------------------
choose_second_value({Old, New}) ->
    if
        Old =:= New -> 
            undefined;
        true -> 
            New
    end.

%%--------------------------------------------------------------------
%% @doc
%% 过滤掉undefined的值用的
%% @spec
%% @end
%%--------------------------------------------------------------------
filter_undefined(Value) ->
    case Value of
        undefined -> 
            false;
        "undefined" -> 
            false;
        <<"undefined">> ->
            false;
        _ -> 
            true
    end.

%%--------------------------------------------------------------------
%% @doc 跟erlang的lists:max有点类似，但是加了自定义的Func函数来比较
%% @spec
%% @end
%%--------------------------------------------------------------------

lists_max(Func, List) 
  when is_function(Func) ->
    lists:foldl(Func, hd(List), tl(List)).   
%%--------------------------------------------------------------------
%% @doc 跟erlang的lists:min有点类似，但是加了自定义的Func函数来比较
%% @spec
%% @end
%% example
%% util:lists_min(fun(A, B) ->
%%                  if A#base_achieve.task_id =< B#base_achieve.task_id ->
%%                          A;
%%                     true ->
%%                          B
%%                  end
%%          end, BaseAchieveList)
%%--------------------------------------------------------------------

lists_min(Func, List) 
  when is_function(Func) ->
    lists:foldl(Func, hd(List), tl(List)).   

%% %% 较高效率的lists ++ 如果lists1 是大数组时使用
%% append(List1, List2) ->
%%     append(lists:reverse(List1), lists:reverse(List2), []).
%% append([], [], Result) ->
%%     Result;
%% append([E|Rest1], [], Result) ->
%%     append(Rest1, [], [E|Result]);
%% append(List1, [E|Rest2], Result) ->
%%     append(List1, Rest2, [E|Result]).

%% %% 较高效率的lists ++ 如果lists1 是大数组时使用
%% append_1(List1, List2) ->
%%     append2(lists:reverse(List1), List2).
%% append2(List1, []) ->
%%     lists:reverse(List1);
%% append2(List1, [E|Rest2]) ->
%%     append2([E|List1], Rest2).
cal_binary_1_count(N) ->
    cal_binary_1_count(N, 0).
cal_binary_1_count(0, Res) ->
    Res;
cal_binary_1_count(N, Res) ->
    cal_binary_1_count(N bsr 1, Res+(N band 1)).


is_8_3_and_8_7() ->
    Now = unixtime(),
    Sn = tool:to_integer(config:get_server_no()), 
%% (shsg1_game1@127.0.0.1)12>  util:datetime_to_timestamp(2013, 8, 3, 0, 0, 0).
%% 1375459200
%% (shsg1_game1@127.0.0.1)14>  util:datetime_to_timestamp(2013, 8, 8, 0, 0, 0).
%% 1375891200
%% (shsg1_game1@127.0.0.1)15>  util:datetime_to_timestamp(2013, 8, 2, 0, 0, 0).
%% 1375372800
    1 =< Sn andalso Sn =< 4 andalso Now >= 1375495200 andalso Now =<1375858800.

is_8_12_and_8_14_coin() ->
    Now = unixtime(),
    Sn = tool:to_integer(config:get_server_no()), 
%% (shsg2_game1@192.168.1.132)1> util:datetime_to_timestamp(2013, 8, 12, 18, 0, 0).
%% 1376301600
%% (shsg2_game1@192.168.1.132)2> util:datetime_to_timestamp(2013, 8, 15, 0, 0, 0).
%% 1376496000
    (1 =:= Sn orelse Sn =:= 3 orelse Sn =:= 5) andalso Now >= 1376301600 andalso Now =<1376496000.

is_8_12_and_8_14_exp() ->
    Now = unixtime(),
    Sn = tool:to_integer(config:get_server_no()), 
    (2 =:= Sn orelse Sn =:= 4 orelse Sn =:= 6) andalso Now >= 1376301600 andalso Now =<1376496000.

get_server_name(Sn) ->
    ServerNameList = case lib_syssetting:get_game_data(server_name_list, config:get_one_server_no()) of
                         List when is_list(List) ->
                             List;
                         _ ->
                             []
                     end,
    %%?DEBUG("~p~n", [ServerNameList]),
    tool:to_binary(proplists:get_value(to_integer(Sn), ServerNameList, "一将功成")).

-define(RE_METACHARACTERS, "()[]{}\|*.+?:!$^<>=").

re_escape(List) when is_list(List)->
    re_escape(List, []);
re_escape(BinList) when is_binary(BinList)->
    re_escape(tool:to_list(BinList), []);
re_escape(_) ->
    [].
re_escape([], Acc) -> 
    lists:reverse(Acc);
re_escape([H|T], Acc) ->
    case lists:member(H, ?RE_METACHARACTERS) of
        true ->
            re_escape(T, [H, $\\|Acc]);
        false ->
            re_escape(T, [H|Acc])
    end.


