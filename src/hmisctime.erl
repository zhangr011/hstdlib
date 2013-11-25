%%%----------------------------------------------------------------------
%%% File    : misc_timer.erl
%%% Created : 2010-10-17
%%% Description: 时间生成器
%%%----------------------------------------------------------------------
-module(hmisctime).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("define_logger.hrl").
-include("define_time.hrl").

%% --------------------------------------------------------------------
%% External exports

-export([
         unixtime/0,
         longunixtime/0,
         seconds_to_localtime/1,
         get_day_second_passed/1,
         get_seconds_to_tomorrow/0,
         get_seconds_to_tomorrow_4/0,
         get_today_second_passed/0,
         get_timestamp_of_today_start/0,
         get_timestamp_of_tomorrow_start/0,
         get_timestamp_of_week_start/0,
         is_same_date/2,
         is_same_month/2,
         is_same_week/2,
         datetime_to_timestamp/1,
         datetime_to_timestamp/6,
         timestamp_to_datetime/1,
         time_format/1,
         get_server_start_time/0,
         get_today_current_second/0,
         get_week_day/0,
         get_midnight_seconds/1,
         get_days_passed/2,
         cal_begin_end/3,
         cal_begin_end_advance/4
        ]).

%% ====================================================================
%% External functions
%% ====================================================================
%% 时间函数
%% -----------------------------------------------------------------
%% 根据1970年以来的秒数获得日期
%% -----------------------------------------------------------------

%% @doc get the time's seconds for integer type
%% @spec get_seconds(Time) -> integer() 
time_format(Now) -> 
    {{Y,M,D},{H,MM,S}} = calendar:now_to_local_time(Now),
    lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D), " ", 
                  one_to_two(H) , ":", one_to_two(MM), ":", one_to_two(S)]).

%% 取得当前的unix时间戳，秒级
unixtime() ->
    {M, S, _} = current(),
    M * 1000000 + S.

%% 取得当前的unix时间戳，毫秒级
longunixtime() ->
    {M, S, Ms} = current(),
    (M * 1000000000000 + S * 1000000 + Ms) div 1000.

%% seconds to localtime
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

%% -----------------------------------------------------------------
%% 判断是否同一月
%% -----------------------------------------------------------------
is_same_month(Seconds1, Seconds2) ->
    {{Year1, Month1, _Day1}, _Time1} = timestamp_to_datetime(Seconds1),
    {{Year2, Month2, _Day2}, _Time2} = timestamp_to_datetime(Seconds2),
    %% ?DEBUG("is_same_month Y:~p M:~p d:~p",[Year1,Month1,_Day1]),
    %% ?DEBUG("is_same_month Y:~p M:~p d:~p",[Year2,Month2,_Day2]),
    if 
        (Year1 == Year2) andalso (Month1 == Month2) -> 
            true;
        true -> 
            false
    end.

%% 获取今天已经流逝的秒数
get_today_second_passed() ->
    ((unixtime() + ?TIME_ZONE_SECONDS) rem ?ONE_DAY_SECONDS).

%% 获取当前已经流逝的时间（秒）
get_day_second_passed(Time) ->
    Time - ((Time + ?TIME_ZONE_SECONDS) rem ?ONE_DAY_SECONDS).

%% 获取当天0点秒数
get_timestamp_of_today_start() ->
    Now = unixtime(),
    Now - ((Now + ?TIME_ZONE_SECONDS) rem ?ONE_DAY_SECONDS).

get_timestamp_of_tomorrow_start() ->
    get_timestamp_of_today_start() + ?ONE_DAY_SECONDS.

%% 获取距离明天0点的秒数
get_seconds_to_tomorrow() ->
    get_timestamp_of_today_start() + ?ONE_DAY_SECONDS - unixtime().

%% 获取距离明天4点的秒数
get_seconds_to_tomorrow_4() ->
    ?ONE_HOUR_SECONDS * 4 + get_seconds_to_tomorrow().

%% 获取本周开始的秒数
get_timestamp_of_week_start() ->
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

%% -----------------------------------------------------------------
%% 计算相差的天数
%% -----------------------------------------------------------------
get_days_passed(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, _} = seconds_to_localtime(Seconds1),
    {{Year2, Month2, Day2}, _} = seconds_to_localtime(Seconds2),
    Days1 = calendar:date_to_gregorian_days(Year1, Month1, Day1),
    Days2 = calendar:date_to_gregorian_days(Year2, Month2, Day2),
    abs(Days2 - Days1).

%% 获取从午夜到现在的秒数
get_today_current_second() ->
    {_, Time} = calendar:now_to_local_time(current()),
    calendar:time_to_seconds(Time).

%% 判断今天星期几 
get_week_day() ->
    get_date().

%% 1970.1.1 为星期四
get_date() ->
    %% 7.
    Now = unixtime(),
    WeekDay = ((((Now + ?TIME_ZONE_SECONDS) rem ?ONE_WEEK_SECONDS) div ?ONE_DAY_SECONDS) + 4) rem 7,
    if
        WeekDay =:= 0 -> 
            7;
        true -> 
            WeekDay
    end.

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
    get_month(unixtime()).

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
%% lnx(X) ->
%%     math:log10(X) / math:log10(?E).
check_same_day(Timestamp)->
    NDay = (unixtime() + 8 * 3600) div 86400,
    ODay = (Timestamp+8*3600) div 86400,
    NDay =:= ODay.

current() -> 
    [{timer, {Now, _}}] = ets:lookup(ets_timer, timer),
    Now.

now_seconds()->
    [{timer, {Now, _}}] = ets:lookup(ets_timer, timer),
    {MegaSecs, Secs, _MicroSecs} = Now,
    lists:concat([MegaSecs, Secs]).

cpu_time() -> 
    [{timer, {_, Wallclock_Time_Since_Last_Call}}] = 
        ets:lookup(ets_timer, timer),
    Wallclock_Time_Since_Last_Call.

%% @doc get server start time
get_server_start_time() ->
    {YY, MM, DD, HH, II, SS} = config:get_server_start_time(),
    datetime_to_timestamp(YY, MM, DD, HH, II, SS).

%% time format
one_to_two(One) ->
    io_lib:format("~2..0B", [One]).




%% {2013, 6, 14, 10, 0, 0} - {2013, 8, 1, 10, 0, 0}
cal_begin_end({_,M1,D1,_,_,_}=BeginTime, {_,M2,D2,_,_,_}=EndTime, range)
    when M1 >= 1, M1 =< 12,
         D1 >= 1, D1 =< 31,
         M2 >= 1, M2 =< 12,
         D2 >= 1, D2 =< 31 ->
    {datetime_to_timestamp(BeginTime),  
     datetime_to_timestamp(EndTime)};
%% 加法不支持配月和年，全部由策划换算成多少天
cal_begin_end({Year, Month, Day, Hour, Min, Sec}=BeginTime, {0, 0, DD, DH, DMin, DS}, plus) 
    when Month >= 1, Month =< 12,
         Day >= 1, Day =< 31 ->
    Delta = DD * ?ONE_DAY_SECONDS + DH * ?ONE_HOUR_SECONDS + DMin * ?ONE_MINITE_SECONDS + DS,
    NewBeginTime = datetime_to_timestamp(BeginTime),
    {NewBeginTime, NewBeginTime + Delta};
cal_begin_end(BeginTime, EndTime, Other) ->
    ?WARNING_MSG("maybe conf error ~p~n", [{BeginTime, EndTime, Other}]),
    {undefined, undefined}.

cal_begin_end_advance(BeginTime, EndTime, undefined, Method) ->
    cal_begin_end_advance(BeginTime, EndTime, {0, 0, 0, 0, 0, 0}, Method);
cal_begin_end_advance(BeginTime, EndTime, {0, 0, DD, DH, DMin, DS}=Advance, Method) ->
    Delta = DD * ?ONE_DAY_SECONDS + DH * ?ONE_HOUR_SECONDS + DMin * ?ONE_MINITE_SECONDS + DS,
    case cal_begin_end(BeginTime, EndTime, Method) of 
        {NewBeginTime, NewEndTime} 
          when is_integer(NewBeginTime),
               is_integer(NewEndTime) ->
            {NewBeginTime, NewEndTime, NewBeginTime-Delta};
        {NewBeginTime, NewEndTime} ->
            {NewBeginTime, NewEndTime, NewBeginTime}
    end;
cal_begin_end_advance(BeginTime, EndTime, Advance, Other) ->
    ?WARNING_MSG("maybe conf error ~p~n", [{BeginTime, EndTime, Advance, Other}]),
    {undefined, undefined, undefined}.
