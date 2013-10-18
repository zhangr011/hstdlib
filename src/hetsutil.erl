-module(hetsutil).

-include("define_logger.hrl").

-export(
   [
    lookup_one/2,
    lookup_all/2,
    match_one/2,
    match_all/2,
    select_one/2,
    select_all/2,
    select_delete/2,
    insert/2,
    match_delete/2,
    update_element/3,
    match/2,
    delete/2,
    first/1,
    sort/2,
    lock/1,
    unlock/1,
    size/1,
    truncate/1,
    safe_truncate/1,
    match_all_one_field/2,
    is_exist/1,
    max/3,
    tab2list/1
   ]
  ).

%% -----------------------------------------------------------------
%% 通用函数
%% -----------------------------------------------------------------
%%
%%@spec   返回一个ETS表的Record值  | no_found
%%@sample lookup_one(?ETS_BASE_PET, GoodsId).
lookup_one(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            [];
        Record ->
            [R|_Rest] = Record,
            R
    end.

%%
%%@spec  返回ETS表的Record List
%%@sample lookup_one(?ETS_BASE_PET, GoodsId). 
lookup_all(Table, Key) ->
    ets:lookup(Table, Key).

%%
%%@spec   返回首个匹配的数量   | no_found
%%@sample match_all(?ETS_PARTER, #ets_parter{player_id=PlayerId, _='_'}).
%% 参数说明 #record_name{
%%                     id = Key,...
%%                     _  = '_'      %%其余字段任意必须  
%%                     }
match_one(Table, Pattern) ->
    case ets:match_object(Table, Pattern) of
        [] ->
            [];
        [Record | _Rest] ->
            Record
    end.

%%
%%@spec  返回所有匹配的Record List
%%@sample match_all(?ETS_PARTER, #ets_parter{player_id=PlayerId, _='_'}).
%% 参数说明 #record_name{
%%                     id = Key,...
%%                     _  = '_'      %%其余字段任意必须  
%%                     }
match_all(Table, Pattern) ->
    ets:match_object(Table, Pattern).


%%
%%@spec   返回首个匹配的数量  | no_found
%%MS = ets:fun2ms( fun ( Beast = #rec_base_beast{
%%                                      id = ID,
%%                                      _  = '_'
%%                                     }) when ID >0 ->
%%                             Beast
%%                     end),
%%    select_all(?ETS_BASE_BEAST,MS).
%%
select_one(Table, MS) ->
    case ets:select(Table, MS) of
        [] ->
            [];
        [R | _Rest] ->
            R
    end.

%%
%%@spec  返回所有匹配的Record List
%%MS = ets:fun2ms( fun ( Beast = #rec_base_beast{
%%                                      id = ID,
%%                                      _  = '_'
%%                                     }) when ID >0 ->
%%                             Beast
%%                     end),
%%    select_all(?ETS_BASE_BEAST,MS).
%%
select_all(Table, MS) ->
    ets:select(Table, MS).

select_delete(Table, MS) ->
    ets:select_delete(Table, MS).

%% select_test() ->
%%     MS = ets:fun2ms( fun ( Beast = #rec_base_beast{
%%                                       id = ID,
%%                                       _  = '_'
%%                                      }) when ID >0 ->
%%                              Beast
%%                      end),
%%     select_all(?ETS_BASE_BEAST,MS).

%%
%% 插入到ETS表中
%% Table  ： 表名
%% Record ： ETS表中对就的Record值
                                                %
insert(Table,Record) ->
    ets:insert(Table,Record).

%% 删除ETS表中的数据
%% Table : 表名
%%
%%@spec  返回所有匹配的Record List
%%@sample match_all(?ETS_PARTER, #ets_parter{player_id=PlayerId, _='_'}).
%% 参数说明 #record_name{
%%                     id = Key,...
%%                     _  = '_'      %%其余字段任意必须  
%%                     }
match_delete(Table, Pattern) ->
    ets:match_delete(Table, Pattern).

delete(Table, Key) ->
    ets:delete(Table, Key).

update_element(Table,Key,Value) ->
    ets:update_element(Table,Key,Value).

match(Table, Pattern) ->
    ets:match(Table, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% ets_util:match_one_field_all(ets_rank_player, #rec_rank_data{type=3, rank='$1', _='_'}).
%% 这个函数返回 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
%% 原生的函数返回 [[1],[2],[3],[4],[5],[6],[7],[8],[9],[10],[11],[12],[13],[14],[15],[16]]
%% @spec
%% @end
%%--------------------------------------------------------------------
match_all_one_field(Table, Pattern) ->
    lists:append(match(Table, Pattern)).

first(Table) ->
    case ets:first(Table) of
        '$end_of_table' ->
            [];
        FirstKey ->
            [Value] = ets:lookup(Table, FirstKey),
            Value
    end.



%% 暂未测试
sort(Table, Fun)
  when is_function(Fun, 2)->
    List = ets:tab2list(Table),
    lists:sort(fun(E1, E2) ->
                       Fun(E1, E2)
               end, List).
%% match之后取最大值
max(Table, Pattern, Fun)   
  when is_function(Fun, 2) ->
    case match_all(Table, Pattern) of
        [] ->
            [];
        List ->
            ?DEBUG("ETS List ~w~n", [List]),
            util:lists_max(Fun, List)
    end.
%% 锁住ETS表
lock(Table) ->
    try
        ets:safe_fixtable(Table, true),
        true
    catch _:R ->
            ?WARNING_MSG("ets_util:lock() error R: ~w, stack :~p~n", [R, erlang:get_stacktrace()]),
            false
    end.

%% 解锁ETS表 与 lock一一对应 否则报错
unlock(Table) ->
    try
        ets:safe_fixtable(Table, false),
        true
    catch _:R ->
            ?WARNING_MSG("ets_util:unlock() error R: ~w, stack :~p~n", [R, erlang:get_stacktrace()]),
            false
    end.
%%--------------------------------------------------------------------
%% @doc
%% 获取ETS长度
%% @spec
%% @end
%%--------------------------------------------------------------------
size(Table) ->
    case  ets:info(Table, size) of
        Size when is_integer(Size) ->
            Size; 
        Other ->
            ?ERROR_MSG("Unkonw Size~w~n", [Other]),
            0
    end.

%% 清空ETS表            
truncate(Table) ->
    ets:delete_all_objects(Table).

safe_truncate(Table) ->
    lock(Table),
    try 
        truncate(Table)
    catch _:R ->
            ?WARNING_MSG("safe_truncate_failed! ~R Stack : ~p~n",[R, erlang:get_stacktrace()])
    after
        unlock(Table)
    end.


%%--------------------------------------------------------------------
%% @doc 判断ETS是否存在
%% @spec
%% @end
%%--------------------------------------------------------------------
is_exist(Table) ->
    case ets:info(Table) of
        undefined ->
            false;
        _EXIST ->
            true
    end.        
tab2list(Table) ->
    ets:tab2list(Table).

