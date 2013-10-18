%%%----------------------------------------
%%% @Module  : misc_admin
%%% @Created : 2010.10.09 
%%% @Description: 系统状态管理和查询
%%%----------------------------------------
-module(misc_admin).
%%
%% Include files
%%
-include_lib("leshulib/include/define_logger.hrl").
-include_lib("leshulib/include/define_chat.hrl").
-include_lib("leshulib/include/define_server.hrl").
-include_lib("leshulib/include/define_player.hrl").
-include_lib("leshulib/include/define_scene.hrl").

%%
%% Exported Functions
%%
-compile(export_all).

-define(SOCKET_TOP, 5000).

%% 处理http请求【需加入身份验证或IP验证】
treat_http_request(Socket, Packet0) ->
    case gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT) of 
        {ok, Packet} -> 
            MyIp = misc:get_ip(Socket),
            case check_ip(MyIp) of
                true ->
                    P = lists:concat([Packet0, tool:to_list(Packet)]),
                    check_http_command(Socket, tool:to_binary(P)),
                    {http_request,  ok};
                _ ->
                    Data  = lists:concat(["no_right (", MyIp, ")"]),
                    lib_send:send_one(Socket,  Data),
                    {http_request,  no_right}       
            end;
        {error, Reason} -> 
            {http_request,  Reason}
    end.

%% 加入http来源IP验证 
check_ip(MyIp) ->
    lists:any(fun(Ip) ->tool:to_binary(MyIp)=:=tool:to_binary(Ip) end,config:get_http_ips()).

get_cmd_parm(Packet) ->
    Packet_list = string:to_lower(tool:to_list(Packet)),
    try
        case string:str(Packet_list, " ") of
            0 -> no_cmd;
            N -> CM = string:substr(Packet_list,2,N-2),
                 case string:str(CM, "?") of
                     0 -> [CM, ""];
                     N1 -> [string:substr(CM,1,N1-1),  string:substr(CM, N1+1)]
                 end
        end
    catch
        _:_ -> no_cmd
    end.


%% 检查分析并处理http指令
check_http_command(Socket, Packet) ->
    %%              ——节点信息查询:         /get_node_status?                                       (ok)
    %%              --节点信息查询:         /get_node_info?t=1[cpu]2[memory]3[queue](ok)
    %%      --进程信息查询          /get_process_info?p=pid                                 (ok)
    %%              --获取进程信息                  /get_proc_info?p=<0,1,2> (ok)
    %%              --关闭节点                       /close_nodes?t=1|2
    %%              ——设置禁言:                     /donttalk?id=stoptime(分钟)                     (ok)
    %%              ——解除禁言:                     /donttalk?id=0                                          (ok)
    %%              ——踢人下线：                    /kickuser?id                                            (ok)
    %%              ——封/开角色：                   /banrole?id=1/0                                         (ok)
    %%              ——通知客户端增减金钱    /notice_change_money?id=parm
    %%              ——GM群发：                      /broadmsg?gmid_content[中文？]          (ok)
    %%              ——安全退出游戏服务器：  /safe_quit?node                                         (ok)
    %%              ——请求加载基础数据：    /load_base_data?                                        (ok)    
    %%              ——禁言列表：                    /donttalklist?  
    %%              ——获取在线人数                  /online_count?                                          (ok)
    %%              ——获取场景人数                  /scene_online_count?                            (ok)
    %%              ——重新加载模块                  /cl?module_name 

    try
        case get_cmd_parm(Packet) of
            ["get_node_status", _] ->
                Data = get_nodes_info(), 
                Data_len = length(tool:to_list(Data)),
                %% io:format("~s_get_nodes_info__~p_~n", [misc:time_format(now()), Data_len]),
                if Data_len == 0 ->
                        lib_send:send_one(Socket, <<"error!">>);
                   true -> 
                        lib_send:send_one(Socket, Data)
                end;
            ["cl", Parm] ->         
                Data = reload_module(Parm), 
                Data_len = length(tool:to_list(Data)),
                if Data_len == 0 ->
                        lib_send:send_one(Socket, <<"error!">>);
                   true -> 
                        lib_send:send_one(Socket, Data)
                end;                    
            ["get_node_info",Parm] ->                       
                [_n, Type] = string:tokens(Parm, "="),
                Data = get_nodes_cmq(1,tool:to_integer(Type)),
                DataFormat = io_lib:format("~p", [Data]),
                Data_len = length(tool:to_list(Data)),
                if Data_len == 0 ->
                        lib_send:send_one(Socket, <<"error!">>);
                   true -> 
                        lib_send:send_one(Socket,DataFormat)
                end;
            ["get_process_info",Parm]->
                [_,PidList] = string:tokens(Parm,"="),
                Data = get_porcess_info(PidList),
                DataFormat = io_lib:format("~p",[Data]),
                Data_len = length(tool:to_list(Data)),
                if Data_len == 0 ->
                        lib_send:send_one(Socket, <<"error!">>);
                   true -> 
                        lib_send:send_one(Socket,DataFormat)
                end;
            ["close_nodes",Parm] ->
                [_n,Type] = string:tokens(Parm, "="),
                Data = close_nodes(tool:to_integer(Type), Socket),
                DataFormat = io_lib:format("~p", [Data]),
                lib_send:send_one(Socket,DataFormat);
            ["donttalk", Parm] ->           
                [Id, Stoptime] = string:tokens(Parm, "="),
                operate_to_player(misc_admin, donttalk, [list_to_integer(Id), list_to_integer(Stoptime)]),
                lib_send:send_one(Socket, <<"ok!">>),
                ok;
            ["kickuser", Parm] ->           
                ?DEBUG("php_kickuser : ~ts~n",[Parm]),
                operate_to_player(misc_admin, kickuser, [list_to_integer(Parm)]),
                lib_send:send_one(Socket, <<"ok!">>),
                ok;     
            ["banrole", Parm] ->    
                [Id0, Action0] = string:tokens(Parm, "="),
                Id = list_to_integer(Id0),
                Action = list_to_integer(Action0),
                Action1 =
                    if Action < 0 orelse Action >1 ->
                            0;
                       true -> Action
                    end,
                db_agent_player:set_player_status(Id, Action1),
                operate_to_player(misc_admin, banrole, [Id, Action1]),
                lib_send:send_one(Socket, <<"ok!">>),
                ok;             
            ["notice_change_money", Parm] ->                
                [Id, Action] = string:tokens(Parm, "="),
                operate_to_player(misc_admin, notice_change_money, [list_to_integer(Id),Action]),
                lib_send:send_one(Socket, <<"ok!">>),
                ok;     
            ["broadmsg", Parm] ->   
                Content = http_lib:url_decode(Parm),
                %%  io:format("broadmsg: ///~p///~p///~p/// ~n",[Parm, Content, tool:to_binary(Content)]),
                lib_chat:broadcast_sys_msg(?SYSMSG_VIP, Content),%%原来?SYSMSG_VIP是2的
                lib_send:send_one(Socket, tool:to_binary(Content)),
                ok;
            ["server_start_time", _] ->
                TimeStamp = util:get_server_start_time(),
                lib_send:send_one(Socket, tool:to_binary(TimeStamp)),
                ok;
            ["safe_quit", Parm] ->
                ?ERROR_MSG("trying to stop server. safe_quit Parm:~w~n", [Parm]),
                %% safe_quit(Parm, Socket),
                %% 10s后关闭服务器
                spawn(fun() ->
                              main:server_stop(10)
                      end),
                %% main:server_stop(10),
                lib_send:send_one(Socket, <<"ok!">>),
                timer:sleep(10000),
                gen_tcp:close(Socket),
                erlang:halt(),
                ok;
            ["server_status", _] ->
                lib_send:send_one(Socket, <<"OK">>),
                gen_tcp:close(Socket),
                ok;
            ["remove_nodes", Parm] ->               
                remove_nodes(Parm),
                lib_send:send_one(Socket, <<"ok!">>),
                ok;
            ["load_base_data", Parm] ->             
                load_base_data(Parm),
                lib_send:send_one(Socket, <<"ok!">>),
                ok;             
            ["online_count", _Parm] ->
                Data = get_online_count(),
                Data_len = length(tool:to_list(Data)),
                if Data_len == 0 ->
                        lib_send:send_one(Socket, <<"0">>);
                   true -> 
                        lib_send:send_one(Socket, Data)
                end;
            ["scene_online_count",_Parm] ->
                Data = get_scene_online_num(),
                lib_send:send_one(Socket,Data);
            ["new_mail", PlayerId] ->
                send_new_mail(PlayerId),
                lib_send:send_one(Socket, <<"ok!">>),
                ok;
            ["pay_success", Parm] ->		
                send_pay_info(Parm),
                lib_send:send_one(Socket, <<"ok@">>),
                ok;
            ["get_line_count", _] ->
                Tab0 = ets:tab2list(?ETS_LINE_COUNT),
                Tab = [[SceneID, Count, Line]||#line_count{scene_id=SceneID, count=Count, line=Line}<-Tab0],
                try
                    Binary = util:term_to_bitstring(Tab),
                    lib_send:send_one(Socket, <<Binary/binary>>)
                of
                    _ -> ok
                catch 
                    _:R ->
                        io:format("~p~n", [{R, erlang:get_stacktrace()}])
                end,
                ok;
            ["new_vip_info_yes", PlayerId] ->
                new_vip_info(vip_value, true, PlayerId),
                lib_send:send_one(Socket, <<"ok!">>),
                ok;
            ["new_vip_info_no", PlayerId] ->
                new_vip_info(vip_value, false, PlayerId),
                lib_send:send_one(Socket, <<"ok!">>),
                ok;
            ["add_white_ip", IP] ->
                ?DEBUG("Add IP ~w~n", [IP]),
                data_config:add_ip(IP),
                lib_send:send_one(Socket, <<"ok!">>),
                ok;
            ["update_server_state", _] ->
                ?WARNING_MSG("Update Server State~n", []),
                update_server_state(),
                lib_send:send_one(Socket, <<"update server state success!">>),
                ok;
            _ ->
                error_cmd
        end
    catch 
        _:_ -> error
    end.

%% 后台修改玩家vip等级和vip值
new_vip_info(Type, Announce, PlayerId) ->
    case lib_player:get_player_pid(PlayerId) of
        [] ->
            skip;
        Pid ->
            gen_server:cast(Pid, {new_vip_info, Type, Announce})
    end.

%% 获取在线人数
get_online_count() ->
    Total_user_count = get_online_count(num),
    lists:concat(['online_count:',Total_user_count]).


get_online_count(num) ->
    %% L = mod_node_interface:server_list(),
    %% Count_list =
    %%     if 
    %%         L == [] ->
    %%             [0];
    %%         true ->
    %%             Info_list =
    %%                 lists:map(
    %%                   fun(S) ->
    %%                           {_Node_name, User_count} = 
    %%                               case rpc:call(S#server.node, mod_node_interface, online_state, []) of
    %%                                   {badrpc, _} ->
    %%                                       {error, 0};
    %%                                   [_State, Num, _] ->
    %%                                       {tool:to_list(S#server.node), Num}
    %%                               end,
    %%                           User_count
    %%                   end,
    %%                   L),
    %%             Info_list
    %%     end,
    Count_list = [online_state()],
    lists:sum(Count_list).

online_state() ->
    ets_util:size(?ETS_ONLINE).


%% 获取场景在线数
get_scene_online_num() ->
    _Total_scene_user_count=get_scene_online_num(num).

%%获取场景在线数
get_scene_online_num(num) ->
    L = mod_node_interface:server_list(),
    Count_list =
        if
            L == [] ->
                [{0,<<>>,0}];
            true ->
                lists:map(
                  fun(S) ->
                          case rpc:call(S#server.node, mod_node_interface,scene_online_num,[]) of
                              {badrpc,_}->
                                  [];
                              GetList ->
                                  GetList
                          end
                  end
                  ,L)
        end,
    FlattenList = lists:flatten(Count_list),
    CountData = lists:foldl(fun count_scene_online_num/2, [], FlattenList),
    F_print = fun({SceneId,SceneName,Num},Str) ->
                      lists:concat([Str,'[',tool:to_list(SceneName),']  [',SceneId,']  [',Num,']\t\n'])       
              end,
    lists:foldl(F_print,[],CountData).

% 场景人数
scene_online_num()->
	BaseScenes = ets:tab2list(?ETS_BASE_SCENE),
	F = fun(Sinfo,SceneInfoList) ->
				Count = lib_scene:get_scene_count(Sinfo#ets_scene.sid),
				[{Sinfo#ets_scene.sid,Sinfo#ets_scene.name, Count}|SceneInfoList]
		end,
	lists:foldl(F, [], BaseScenes).


count_scene_online_num({SceneId,SceneName,Num},CountInfo) ->
    case lists:keysearch(SceneId, 1, CountInfo) of
        false ->
            [{SceneId,SceneName,Num}|CountInfo];
        {value,{_sceneid,SceneName,Total}} ->
            lists:keyreplace(SceneId, 1, CountInfo,{SceneId,SceneName,Num+Total})
    end.


%% 获取节点列表
get_node_info_list() ->
    %% L = lists:usort(mod_node_interface:server_list()),
    %% Info_list =
    %%     lists:map(
    %%       fun(S) ->
    %%               {Node_status, Node_name, User_count} = 
    %%                   case rpc:call(S#server.node, mod_node_interface, online_state, []) of
    %%                       {badrpc, _} ->
    %%                           {fail, tool:to_list(S#server.node), 0};
    %%                       [_State, Num, _] ->
    %%                           {ok,   tool:to_list(S#server.node), Num}
    %%                   end,    
    %%               Node_info = 
    %%                   try 
    %%                       case rpc:call(S#server.node, misc_admin, node_info, []) of
    %%                           {ok, Node_info_0} ->
    %%                               Node_info_0;
    %%                           _ ->
    %%                               error
    %%                       end
    %%                   catch
    %%                       _:_ -> error
    %%                   end,                                                    
    %%               {Node_status, Node_name, User_count, Node_info}
    %%       end,
    %%       L),

    Info_list = [
                 {ok, tool:to_list(node()), online_state(), get_node_info()}
                ],
    {ok, Info_list}.

                                                % 获取各节点状态
get_nodes_info() ->
    case get_node_info_list() of
        {ok, Node_info_list} ->
            Count_list =
                lists:map(
                  fun({_Node_status, _Node_name, User_count, _Node_info})->
                          User_count
                  end,
                  Node_info_list),      
            Total_user_count = lists:sum(Count_list),       

            Node_ok_list =
                lists:map(
                  fun({Node_status, _Node_name, _User_count, _Node_info})->
                          case Node_status of
                              fail -> 0;
                              _ ->  1
                          end
                  end,
                  Node_info_list),      
            Total_node_ok = lists:sum(Node_ok_list),        
            Temp1 = 
                case Total_node_ok =:= length(Node_info_list) of
                    true -> [];
                    _ -> lists:concat(['/',Total_node_ok])
                end,

            All_Connect_count_str = lists:concat(['Nodes_count: [', length(Node_info_list),
                                                  '] ,total_connections: [', Total_user_count, Temp1,
                                                  ']    [',misc:time_format(misc_timer:now()),
                                                  ']']),
            Info_list =
                lists:map(
                  fun({Node_status, Node_name, _User_count, Node_info})->
                          case Node_status of
                              fail ->
                                  lists:concat(['Node: ',Node_name,'[Warning: this node may be crashed or busy.]\t\n']);
                              _ ->
                                  lists:concat(['Node: ',Node_name,'\t\n',Node_info])
                          end
                  end,
                  Node_info_list),      

            lists:concat([All_Connect_count_str, 
                          '\t\n', 
                          '------------------------------------------------------------------------\t\n',
                          Info_list]);            
        _ ->
            ''
    end.

%% 获取本节点的基本信息
node_info() ->
    Info = get_node_info(),
    {ok, Info}.

get_node_info() ->
    Server_no = 
        case ets:match(?ETS_SYSTEM_INFO,{'_',server_no,'$3'}) of
            [[S_no]] -> S_no;
            _ -> undefined
        end,
    Log_level = 
        case ets:match(?ETS_SYSTEM_INFO,{'_',log_level,'$3'}) of
            [[Log_l]] -> Log_l;
            _ -> undefined
        end,    

    Info_log_level = lists:concat(['    Server_no:[', Server_no
                                   ,'], Log_level:[', Log_level
                                   ,'], Process_id:[',os:getpid(),']\t\n']),

    Info_tcp_listener =     
        lists:concat(
          case ets:match(?ETS_SYSTEM_INFO,{'_',tcp_listener,'$3'}) of
              [[{Host_tcp, Port_tcp, Start_time}]] ->
                  ['    Tcp listener? [Yes]. IP:[',Host_tcp,
                   '], Port:[',Port_tcp,
                   '], Connections:[',ets:info(?ETS_ONLINE, size),
                   '], Start_time:[',misc:time_format(Start_time),                                                 
                   ']\t\n'];
              _ ->
                  []
          end),

    Info_stat_socket = 
        try
            case ets:info(?ETS_STAT_SOCKET) of
                undefined ->
                    [];
                _ ->
                    Stat_list_socket_out = ets:match(?ETS_STAT_SOCKET,{'$1', socket_out , '$3','$4'}),
                    Stat_list_socket_out_1 = lists:sort(fun([_,_,Count1],[_,_,Count2]) -> Count1 > Count2 end, Stat_list_socket_out),
                    Stat_list_socket_out_2 = lists:sublist(Stat_list_socket_out_1, ?SOCKET_TOP),
                    Stat_info_socket_out = 
                        lists:map( 
                          fun(Stat_data) ->
                                  case Stat_data of                               
                                      [Cmd, BeginTime, Count] ->
                                          TimeDiff = timer:now_diff(misc_timer:now(), BeginTime) / (1000 * 1000) + 1,
                                          lists:concat(['        ','Cmd:[', Cmd, 
                                                        '], packet_avg/sec:[', Count, 
                                                        '/',round(TimeDiff), ' = ',
                                                        round(Count / TimeDiff),']\t\n']);
                                      _->
                                          ''
                                  end
                          end,
                          Stat_list_socket_out_2),
                    if length(Stat_info_socket_out) > 0 ->
                            lists:concat(['    Tcp socket packet_out statistic_top',
                                          ?SOCKET_TOP,':\t\n', Stat_info_socket_out]);
                       true ->
                            []
                    end                     
            end
        catch
            _:_ -> []
        end,    

    Info_db =       
        case ets:match(?ETS_SYSTEM_INFO,{'_',db,'$3'}) of
            [] ->
                [];
            Db_list ->
                Db_info_list = 
                    lists:map( 
                      fun([{Db, Type, Host, Port, User, DB, Poolsize, Encode}]) ->
                              lists:concat(['    ','    Db:[', Db ,
                                            '], Type:[', Type,
                                            '], Host:[', Host,
                                            '], Port:[', Port,
                                            '], User:[', User,
                                            '], DB:[', DB,
                                            '], Poolsize:[', Poolsize,
                                            '], Encode:[', Encode,
                                            ']\t\n'])
                      end, 
                      Db_list),
                if length(Db_info_list) > 0 ->
                        lists:concat(['    Db config:\t\n'] ++ Db_info_list);
                   true ->
                        []
                end                                    
        end,

    Info_stat_db = 
        try
            case ets:info(?ETS_STAT_DB) of
                undefined ->
                    [];
                _ ->
                    Stat_list_db = ets:match(?ETS_STAT_DB,{'$1', '$2', '$3', '$4', '$5'}),
                    Stat_list_db_1 = lists:sort(fun([_,_,_,_,Count1],[_,_,_,_,Count2]) -> Count1 > Count2 end, Stat_list_db),
                    Stat_list_db_2 = lists:sublist(Stat_list_db_1, 5), 
                    Stat_info_db = 
                        lists:map( 
                          fun(Stat_data) ->
                                  case Stat_data of                               
                                      [_Key, Table, Operation, BeginTime, Count] ->
                                          TimeDiff = timer:now_diff(misc_timer:now(), BeginTime)/(1000*1000)+1,
                                          lists:concat(['        ','Table:[', lists:duplicate(30-length(tool:to_list(Table))," "), Table, 
                                                        '], op:[', Operation,
                                                        '], avg/sec:[', Count, '/',round(TimeDiff),' = ',round(Count / TimeDiff),']\t\n']);
                                      _->
                                          ''
                                  end 
                          end, 
                          Stat_list_db_2),
                    if length(Stat_info_db) > 0 ->
                            lists:concat(['    DB table operation statistic_top5:\t\n', Stat_info_db]);
                       true ->
                            []
                    end                     
            end
        catch
            _:_ -> []
        end,    

    Process_info_detail = 
        try
            get_monitor_process_info_list()
        catch
            _:_ -> {ok, []} 
        end,

    Info_process_queue_top = 
        try
            case get_process_info(Process_info_detail, 5, 2, 0, msglen) of 
                {ok, Process_queue_List, Process_queue_List_len} ->
                    Info_process_queue_list = 
                        lists:map( 
                          fun({Pid, RegName, Mlen, Qlen, Module, Other, Messgaes}) ->
                                  if      is_atom(RegName) -> 
                                          lists:concat(['        ','regname:[', RegName, erlang:pid_to_list(Pid), 
                                                        '],q:[', Qlen ,
                                                        '],m:[', Mlen ,
                                                        '],i:[', Other ,
                                                        Messgaes ,
                                                        ']\t\n']);
                                          is_atom(Module) ->
                                          lists:concat(['        ','module:[', Module, erlang:pid_to_list(Pid), 
                                                        '],q:[', Qlen ,
                                                        '],m:[', Mlen ,
                                                        '],i:[', Other ,
                                                        Messgaes ,
                                                        ']\t\n']);
                                          true ->
                                          lists:concat(['        ','pid:[', erlang:pid_to_list(Pid) ,
                                                        '],q:[', Qlen ,
                                                        '],m:[', Mlen ,
                                                        '],i:[', Other ,
                                                        Messgaes ,
                                                        ']\t\n'])
                                  end     
                          end,
                          Process_queue_List),
                    lists:concat(['    Message_queue_top5: [', Process_queue_List_len, ' only processes being monitored', ']\t\n',Info_process_queue_list]);
                _ ->
                    lists:concat(['    Message_queue_top5: [error_1]\t\n'])
            end
        catch
            _:_ -> lists:concat(['    Message_queue_top5: [error_2]\t\n'])
        end,

    Info_process_memory_top = 
        try
            case get_process_info(Process_info_detail, 20, 0, 0, memory) of 
                {ok, Process_memory_List, Process_memory_List_len} ->
                    Info_process_memory_list = 
                        lists:map( 
                          fun({Pid, RegName, Mlen, Qlen, Module, Other, _Messgaes}) ->
                                  if      is_atom(RegName) -> 
                                          lists:concat(['        ','regname:[', RegName, erlang:pid_to_list(Pid), 
                                                        '],q:[', Qlen ,
                                                        '],m:[', Mlen ,
                                                        '],i:[', Other ,
                                                        ']\t\n']);
                                          is_atom(Module) ->
                                          lists:concat(['        ','module:[', Module, erlang:pid_to_list(Pid), 
                                                        '],q:[', Qlen ,
                                                        '],m:[', Mlen ,
                                                        '],i:[', Other ,
                                                        ']\t\n']);
                                          true ->
                                          lists:concat(['        ','pid:[', erlang:pid_to_list(Pid) ,
                                                        '],q:[', Qlen ,
                                                        '],m:[', Mlen ,
                                                        '],i:[', Other ,
                                                        ']\t\n'])
                                  end     
                          end,
                          Process_memory_List),
                    lists:concat(['    Process_memory_top20: [', Process_memory_List_len,' only processes being monitored', ']\t\n',Info_process_memory_list]);
                _ ->
                    lists:concat(['    Message_memory_top20: [error_1]\t\n'])
            end
        catch
            _:_ -> lists:concat(['    Message_memory_top20: [error_2]\t\n'])
        end,

    System_process_info = 
        try
            lists:concat(["    System process: \t\n",
                          "        ",
                          "process_count:[", erlang:system_info(process_count) ,'],',
                          "processes_limit:[", erlang:system_info(process_limit) ,'],',
                          "ports_count:[", length(erlang:ports()),']',
                          "\t\n"
                         ])
        catch
            _:_ -> []
        end,

    %%                 total = processes + system
    %%         processes = processes_used + ProcessesNotUsed
    %%         system = atom + binary + code + ets + OtherSystem
    %%         atom = atom_used + AtomNotUsed
    %% 
    %%         RealTotal = processes + RealSystem
    %%         RealSystem = system + MissedSystem

    System_memory_info = 
        try
            lists:concat(["    System memory: \t\n",
                          "        ",
                          "total:[", erlang:memory(total) ,'],',
                          "processes:[", erlang:memory(processes) ,'],',
                          "processes_used:[", erlang:memory(processes_used) ,'],\t\n',
                          "        ",
                          "system:[", erlang:memory(system) ,'],',
                          "atom:[", erlang:memory(atom) ,'],',
                          "atom_used:[", erlang:memory(atom_used) ,'],\t\n',
                          "        ",
                          "binary:[", erlang:memory(binary) ,'],',
                          "code:[", erlang:memory(code) ,'],',
                          "ets:[", erlang:memory(ets) ,']',
                          "\t\n"
                         ])
        catch
            _:_ -> []
        end,

    LL1 = string:tokens(binary_to_list(erlang:system_info(info)),"="),
    Atom_tab =
        case lists:filter(fun(I)-> string:str(I,"index_table:atom_tab") == 1 end, LL1) of
            [] -> [];
            [Index_table] -> binary:replace(list_to_binary(Index_table), <<"\n">>, <<", ">>, [global])
        end,

    System_other_info = 
        try
            System_load = get_system_load(),
            {{input,Input},{output,Output}} = statistics(io),

            lists:concat(["    System other: \t\n",
                          "        ",
                          "atom_tab:[", binary_to_list(Atom_tab) ,'],\t\n', 
                          "        ",
                          "run_queue:[", statistics(run_queue) ,'],',
                          "input:[", Input ,'],',
                          "output:[", Output ,'],',
                          "wallclock_time:[", io_lib:format("~.f", [System_load]) ,'],',                                           
                          "\t\n"
                         ])
        catch
            _:_ -> []
        end,            

    Info_global =   
        case ets:match(?ETS_MONITOR_PID, {'$1', node ,'$3'}) of
            [] ->
                [];
            Global_list ->
                Global_info_list = 
                    lists:map( 
                      fun([GPid, P]) ->
                              [ModuleName, WorkerNumber, WeightLoad] = 
                                  case P of
                                      {Module, M_workers, M_load} -> [Module, M_workers, M_load];
                                      {Module, M_load} -> [Module, unknown, M_load];
                                      {Module} -> [Module, unknown, unknown]
                                  end,
                              lists:concat(['        ',ModuleName,' ==> Pid:[',erlang:pid_to_list(GPid),'], WeightLoad:[', WeightLoad,'], WorkerNumber:[', WorkerNumber, ']\t\n'])
                      end, 
                      Global_list),
                if length(Global_info_list) > 0 ->
                        lists:concat(['    node_mod:\t\n'] ++ Global_info_list);
                   true ->
                        []
                end                                    
        end,

    Info_scene = 
        try 
            case ets:info(?ETS_MONITOR_PID) of
                undefined ->
                    [];
                _ ->
                    Stat_list_scene = ets:match(?ETS_MONITOR_PID,{'$1', mod_scene ,'$3'}),
                    Stat_info_scene = 
                        lists:map( 
                          fun(Stat_data) ->
                                  case Stat_data of                               
                                      [_SceneAgentPid, {SceneId, Worker_Number}] ->
                                          MS = ets:fun2ms(fun(T) when T#player.scene == SceneId  -> 
                                                                  [T#player.id] 
                                                          end),
                                          Players = ets:select(?ETS_ONLINE, MS),
                                          lists:concat([SceneId,'(', Worker_Number ,')_', length(Players), ', ']);
                                      _->
                                          ''
                                  end 
                          end, 
                          Stat_list_scene),
                    case Stat_list_scene of
                        [] -> [];
                        _ -> lists:concat(['    Scene here: [',Stat_info_scene,']\t\n'])
                    end
            end
        catch
            _:_ -> []
        end,

    Info_dungeon = 
        try
            case ets:info(?ETS_MONITOR_PID) of
                undefined ->
                    [];
                _ ->
                    Stat_list_dungeon = ets:match(?ETS_MONITOR_PID,{'$1', mod_dungeon ,'$3'}),
                    Stat_info_dungeon = 
                        lists:map( 
                          fun(Stat_data) ->
                                  case Stat_data of                               
                                      [_,{Dungeon_state}] ->
                                          {_, Dungeon_scene_id, Scene_id, _, 
                                           Dungeon_role_list, _ , _, _, _, _, _ , _} = Dungeon_state,
                                          lists:concat([Scene_id,'(',Dungeon_scene_id,')','_', length(Dungeon_role_list), ', ']);
                                      _->
                                          ''
                                  end 
                          end, 
                          Stat_list_dungeon),
                    case Stat_list_dungeon of
                        [] -> [];
                        _ -> lists:concat(['    Dungeon here: [',Stat_info_dungeon,']\t\n'])
                    end                     
            end
        catch
            _:_ -> []
        end,    

    lists:concat([Info_log_level, Info_global,
                  Info_scene, Info_dungeon, 
                  Info_tcp_listener, Info_stat_socket,
                  Info_db, Info_stat_db, 
                  Info_process_queue_top, Info_process_memory_top, 
                  System_process_info, System_memory_info, System_other_info,
                  '------------------------------------------------------------------------\t\n']).

get_process_info(Process_info_detail, Top, MinMsgLen, MinMemSize, OrderKey) ->
    case Process_info_detail of
        {ok, RsList} ->
            Len = erlang:length(RsList),
            FilterRsList = 
                case OrderKey of 
                    msglen ->
                        lists:filter(fun({_,_,_,Qlen,_,_,_}) -> Qlen >= MinMsgLen end, RsList);
                    memory ->
                        lists:filter(fun({_,_,Qmem,_,_,_,_}) -> Qmem >= MinMemSize end, RsList);
                    _ ->
                        lists:filter(fun({_,_,_,Qlen,_,_,_}) -> Qlen >= MinMsgLen end, RsList)
                end,
            RsList2 = 
                case OrderKey of
                    msglen ->
                        lists:sort(fun({_,_,_,MsgLen1,_,_,_},{_,_,_,MsgLen2,_,_,_}) -> MsgLen1 > MsgLen2 end, FilterRsList);
                    memory ->
                        lists:sort(fun({_,_,MemSize1,_,_,_,_},{_,_,MemSize2,_,_,_,_}) -> MemSize1 > MemSize2 end, FilterRsList);
                    _ ->
                        lists:sort(fun({_,_,_,MsgLen1,_,_,_},{_,_,_,MsgLen2,_,_,_}) -> MsgLen1 > MsgLen2 end, FilterRsList)
                end,
            NewRsList = 
                if Top =:= 0 ->
                        RsList2;
                   true ->
                        if erlang:length(RsList2) > Top ->
                                lists:sublist(RsList2, Top);
                           true ->
                                RsList2
                        end
                end,
            {ok,NewRsList, Len};
        _->
            {error,'error'}
    end.

get_process_info_detail_list(Process, NeedModule, Layer) ->
    RootPid =
        if erlang:is_pid(Process) ->
                Process;
           true ->
                case misc:whereis_name({local, Process}) of
                    undefined ->
                        error;
                    ProcessPid ->
                        ProcessPid
                end
        end,
    case RootPid of
        error ->
            {error,lists:concat([Process," is not process reg name in the ", node()])};
        _ ->
            AllPidList = misc:get_process_all_pid(RootPid,Layer),
            RsList = misc:get_process_info_detail(NeedModule, AllPidList,[]),
            {ok, RsList}
    end.

get_monitor_process_info_list() ->
    Monitor_process_info_list =     
        try
            case ets:match(?ETS_MONITOR_PID,{'$1','$2','$3'}) of
                List when is_list(List) ->
                    lists:map(
                      fun([Pid, Module, Pars]) ->
                              get_process_status({Pid, Module, Pars})
                      end,
                      List);   
                _ ->
                    []
            end
        catch
            _:_ -> []
        end,                    
    {ok, Monitor_process_info_list}.

%% get_process_status({Pid, Module, Pars}) when Module =/= mcs_role_send ->
%%      {'', '', -1, -1, '', '', ''};
get_message_queue_len(Pid) ->
    try 
        case erlang:process_info(Pid, [message_queue_len]) of
            [{message_queue_len, Qlen}] ->  Qlen;
            _ -> -1
        end
    catch 
        _:_ -> -2
    end.

get_process_status({Pid, Module, Pars}) ->
    Other = 
        case Module of
            mod_player -> 
                {PlayerId} = Pars,                              
                case erlang:process_info(Pid,[dictionary]) of
                    undefined -> 
                        lists:concat([PlayerId, "__", dead]);
                    [{_, Dic1}] ->
                        case lists:keyfind(last_msg, 1, Dic1) of 
                            {last_msg, Last_msg} -> 
                                Last_msg1 = io_lib:format("~p", Last_msg),
                                lists:concat([PlayerId, "__", Last_msg1]);
                            _->     
                                lists:concat([PlayerId])
                        end
                end;
            mod_pid_send -> 
                case Pars of
                    {PlayerId, _Socket, N} -> 
                        lists:concat([PlayerId, "_Socket_", N ,"_"]);
                    _ -> ""
                end;
            mod_socket -> 
                case Pars of
                    {GroupName, _Socket, PlayerId, _PlayerPid} -> 
                        lists:concat([PlayerId, "_mod_Socket_", GroupName ,"_"]);
                    _ -> ""
                end;                    
            global ->
                case Pars of
                    {M, WorkerNumber, WeightLoad} -> 
                        lists:concat([M, ",", WorkerNumber, ",", WeightLoad]);
                    {M} -> lists:concat([M]);
                    _ -> ""
                end;
            %%                              lists:concat([PlayerId]);

            %%                      
            %%                              {#role_send_state{roleid = Roleid,  client_ip = {P1,P2,P3,P4}, 
            %%                                                      rolepid = RolePid, accountpid = AccountPid, 
            %%                                                      role_status = Role_status, account_status = Account_status,       
            %%                                                      start_time = StartTime, now_time = CheckTime,priority =Priority,
            %%                                                      canStopCount = CanStopCount, lastMsgLen = LastMsgLen,  getMsgError = GetMsgError
            %%                                                      }} = Pars,
            %%                              lists:concat([Roleid,'/',P1,'.',P2,'.',P3,'.',P4,'/',mcs_misc:time_format( StartTime)
            %%                                                      ,'/',mcs_misc:time_format( CheckTime)
            %%                                                      ,'/',CanStopCount
            %%                                                      ,',', LastMsgLen
            %%                                                      ,',', GetMsgError
            %%                                                      ,',', Priority
            %%                                                      ,'/',erlang:is_process_alive(Pid)
            %%                                                      ,'/R', erlang:pid_to_list(RolePid), '[',Role_status,']_', erlang:is_process_alive(RolePid), '_', get_message_queue_len(RolePid)
            %%                                                      ,'/A', erlang:pid_to_list(AccountPid), '[',Account_status,']_', erlang:is_process_alive(AccountPid), '_', get_message_queue_len(AccountPid)
            %%                                                      ]);
            _->
                ''
        end,

    try 
        case erlang:process_info(Pid, [message_queue_len,memory,registered_name, messages]) of
            [{message_queue_len, Qlen},
             {memory, Mlen},
             {registered_name, RegName},
             {messages, _MessageQueue}] ->
                Messages = '',
                %% if length(MessageQueue) > 0, Module == mod_player ->
                %%         Message_Lists = 
                %%             lists:map(
                %%               fun({Mclass, Mbody}) ->
                %%                       if is_tuple(Mbody) ->
                %%                               [Mtype] = lists:sublist(tuple_to_list(Mbody), 1),
                %%                               [Mtype, Module1, Method1] = lists:sublist(tuple_to_list(Mbody),3),
                %%                               lists:concat(['            <'
                %%                                             ,Mclass,
                %%                                             ', ' ,Mtype,
                %%                                             ', ',binary_to_list(Module1), 
                %%                                             ', ',binary_to_list(Method1),
                %%                                             '>\t\n']);
                %%                          true ->
                %%                               lists:concat(['            <',Mclass,'>\t\n'])
                %%                       end
                %%               end,
                %%               lists:sublist(MessageQueue,5)),                              
                %%         lists:concat(['/\t\n',Message_Lists,'            ']);
                %%    true -> ''
                %% end,
                {Pid, RegName, Mlen, Qlen, Module, Other, Messages};
            _ -> 
                {'', '', -1, -1, '', '', ''}
        end
    catch 
        _:_ -> {'', '', -1, -1, '', '', ''}
    end.


get_process_status_old({Pid, Module, _Pars}) ->
    %%      Other = 
    %%              case Module of
    %%                      mod_player -> 
    %%                              {PlayerId} = Pars,
    %%                              lists:concat([PlayerId]);
    %% %%                   
    %% %%                           {#role_send_state{roleid = Roleid,  client_ip = {P1,P2,P3,P4}, 
    %% %%                                                   rolepid = RolePid, accountpid = AccountPid, 
    %% %%                                                   role_status = Role_status, account_status = Account_status,       
    %% %%                                                   start_time = StartTime, now_time = CheckTime,priority =Priority,
    %% %%                                                   canStopCount = CanStopCount, lastMsgLen = LastMsgLen,  getMsgError = GetMsgError
    %% %%                                                   }} = Pars,
    %% %%                           lists:concat([Roleid,'/',P1,'.',P2,'.',P3,'.',P4,'/',mcs_misc:time_format( StartTime)
    %% %%                                                   ,'/',mcs_misc:time_format( CheckTime)
    %% %%                                                   ,'/',CanStopCount
    %% %%                                                   ,',', LastMsgLen
    %% %%                                                   ,',', GetMsgError
    %% %%                                                   ,',', Priority
    %% %%                                                   ,'/',erlang:is_process_alive(Pid)
    %% %%                                                   ,'/R', erlang:pid_to_list(RolePid), '[',Role_status,']_', erlang:is_process_alive(RolePid), '_', get_message_queue_len(RolePid)
    %% %%                                                   ,'/A', erlang:pid_to_list(AccountPid), '[',Account_status,']_', erlang:is_process_alive(AccountPid), '_', get_message_queue_len(AccountPid)
    %% %%                                                   ]);
    %%                      _->
    %%                              ''
    %%              end,
    %%      Other = %%根据刘哥的方案修改,只处理mod_player----xiaomai
    %%              case Module of
    %%                      mod_player -> 
    %%                              {PlayerId} = Pars,
    %%                              Dic = erlang:process_info(Pid,[dictionary]),
    %%                              [{_, Dic1}] = Dic,
    %%                              case lists:keyfind(last_msg, 1, Dic1) of 
    %%                                      {last_msg, Last_msg} -> 
    %%                                              lists:concat([PlayerId, "__", io_lib:format("~p", Last_msg)]);
    %%                                      _-> lists:concat([PlayerId])
    %%                              end;
    %%                      _ ->
    %%                              ''
    %%              end,
    Other = '',%%去掉last_msg的进程字典，此处修改

    try 
        case erlang:process_info(Pid, [message_queue_len,memory,registered_name, messages]) of
            [{message_queue_len,Qlen},{memory,Mlen},{registered_name, RegName},{messages, _MessageQueue}] ->
                Messages = '',
                %%                      if length(MessageQueue) > 0, Module == mod_player ->
                %%                                 Message_Lists = 
                %%                                 lists:map(
                %%                                       fun({Mclass, Mbody}) ->
                %%                                                       if is_tuple(Mbody) ->
                %% %%                                                                   [Mtype] = lists:sublist(tuple_to_list(Mbody),1),
                %%                                                                      [Mtype, Module1, Method1] = lists:sublist(tuple_to_list(Mbody),3),
                %%                                                                      lists:concat(['            <'
                %%                                                                                               ,Mclass,
                %%                                                                                               ', ' ,Mtype,
                %%                                                                                               ', ',binary_to_list(Module1), 
                %%                                                                                               ', ',binary_to_list(Method1),
                %%                                                                                               '>\t\n']);
                %%                                                         true ->
                %%                                                                      lists:concat(['            <',Mclass,'>\t\n'])
                %%                                                       end
                %%                                       end,
                %%                                 lists:sublist(MessageQueue,5)),                                 
                %%                                 lists:concat(['/\t\n',Message_Lists,'            ']);
                %%                         true -> ''
                %%                      end,
                {Pid, RegName, Mlen, Qlen, Module, Other, Messages};
            _ -> 
                {'', '', -1, -1, '', '', ''}
        end
    catch 
        _:_ -> {'', '', -1, -1, '', '', ''}
    end.


%% ===================针对玩家的各类操作=====================================
operate_to_player(Module, Method, Args) ->
    F = fun(S)->  
                %%                      io:format("node__/~p/ ~n",[[S, Module, Method, Args]]), 
                rpc:cast(S#server.node, Module, Method, Args)
        end,
    [F(S) || S <- ets:tab2list(?ETS_SERVER)],
    ok.

%% 重新加载模块
reload_module(Module_name) ->
    try 
        ResultList = lists:map(fun(Node) -> 
                                       case rpc:call(Node, c, l, [tool:to_atom(Module_name)]) of
                                           {module, _Module_name} -> 
                                               lists:concat(["(", Node, ") ok. / "]);
                                           {error, What} -> 
                                               lists:concat(["(", Node, ") error_1[", What , "]. / "]);
                                           _ -> 
                                               lists:concat(["(", Node, ") error_2.  / "])
                                       end
                               end, 
                               [node() | nodes()]),
        lists:concat([misc:time_format(now()), ": " , lists:flatten(ResultList)])
    catch 
        _:_ -> lists:concat([misc:time_format(now()), ": reload (", Module_name, ") error_3."])
    end.

%% 取得本节点的角色状态
get_player_info_local(Id) ->
    case ets:lookup(?ETS_ONLINE, Id) of
        [] -> [];
        [R] ->
            case misc:is_process_alive(R#player.other#player_other.pid) of
                false -> [];            
                true -> R
            end
    end.

%% 设置禁言 或 解除禁言
donttalk(Id, Stop_minutes) ->
    case get_player_info_local(Id) of
        [] -> no_action;
        Player ->
            if Stop_minutes > 0 ->
                    Stop_begin_time = util:unixtime(),
                    Stop_chat_minutes = Stop_minutes,
                    gen_server:cast(Player#player.other#player_other.pid, 
                                    {set_donttalk, Stop_begin_time, Stop_chat_minutes}),
                    db_agent_player:set_donttalk_status(Id, Stop_begin_time, Stop_chat_minutes),
                    ok;
               Stop_minutes == 0 ->
                    gen_server:cast(Player#player.other#player_other.pid, 
                                    {set_donttalk, undefined, undefined}),                          
                    db_agent_player:delete_donttalk(Id)
            end
    end.

%% 踢人下线 
kickuser(Id) ->
    case get_player_info_local(Id) of
        [] -> no_action;
        Player ->       
            mod_login:logout(Player#player.other#player_other.pid, ?STOP_REASON_KICK_OUT)
    end.

%% 封/开角色
banrole(Id, Action) ->
    case get_player_info_local(Id) of
        [] -> no_action;        
        Player ->
            if Action == 1 ->
                    gen_server:cast(Player#player.other#player_other.pid,{update_sg_player, [{status,1}]}),
                    mod_login:logout(Player#player.other#player_other.pid, ?STOP_ACCOUNT_BAND)
            end
    end.    

%% 通知客户端增减金钱
notice_change_money(Id, Action) ->
    try
        case get_player_info_local(Id) of
            [] -> no_action;
            Player ->                       
                [Val1, Val2, Val3, Val4] = string:tokens(Action, "_"),
                Field = case Val1 of
                            "gold"  -> 1;
                            "coin"  -> 2;
                            "cash"  -> 3;
                            "bcoin" -> 4;
                            _ ->0
                        end,
                Optype = case Val2 of
                             "add"  -> 1;
                             "sub"  -> 2;
                             _ ->0
                         end,
                Value = list_to_integer(Val3),
                Source = list_to_integer(Val4),
                if Field =/=0, Optype =/=0, Value =/=0 ->
                        gen_server:cast(Player#player.other#player_other.pid, {cmd_player_change_money, [Field, Optype, Value, Source]});
                   true -> no_action
                end
        end
    catch
        _:_ -> error
    end.

%% 安全退出游戏服务器
safe_quit(Node, Socket) ->
    ?DEBUG("safe_quit Node:~w~n",[Node]),
    case Node of
        [] -> 
            %% Other_servers = mod_node_interface:server_list_broadcast(),
            %%     ?DEBUG("safe_quit Servers:~w~n",[Other_servers]),
            %% mod_node_interface:stop_game_server(Other_servers),
            %% 调整为 单节点关服
            main:server_stop(10),
            lib_send:send_one(Socket, tool:to_binary(lists:concat(Node,["safe quit ok!"]))),
            timer:sleep(1000),
            spawn(fun()->
                          erlang:halt()
                  end),
            ok;
        _ ->
            rpc:cast(tool:to_atom(Node), main, server_stop, [])
    end,
    ok.

%% 动态撤节点
remove_nodes(NodeOrIp) ->
    case NodeOrIp of
        [] -> 
            io:format("You Must Input Ip or NodeName!!!",[]);
        _ ->
            NI_atom = tool:to_atom(NodeOrIp),
            NI_str = tool:to_list(NodeOrIp),
            case string:tokens(NI_str, "@") of
                [_, _] ->
                    rpc:cast(NI_atom, main, server_remove, []);
                _ ->
                    Server = nodes(),
                    F = fun(S) ->
                                case string:tokens(tool:to_list(S), "@") of
                                    [_Left, Right] when Right =:= NI_str ->
                                        rpc:cast(S, main, server_remove, []);
                                    _ ->
                                        ok
                                end
                        end,
                    [F(S) || S <- Server]
            end
    end,
    ok.


%% 请求加载基础数据
load_base_data(Parm) ->
    Parm_1 = 
        case Parm of 
            [] -> [];
            _ ->                
                [_, ModuleName] = string:tokens(Parm, "="),
                tool:to_atom(ModuleName)
        end,
    ?DEBUG("Parm_1 : ~w~n",[Parm_1]),
    %% 关闭全部节点重载
    %% mod_node_interface:load_base_data(ets:tab2list(?ETS_SERVER), Parm_1),
    case Parm_1 of
        all ->
            mod_kernel:init_base_data(Parm_1);
        Parm_1 ->
            mod_kernel:load_base_data(Parm_1)
    end,
    ok.



test_m() ->
    Process_info_detail = 
        try
            get_monitor_process_info_list()
        catch
            _:_ -> {ok, []} 
        end,
    {ok, L} = Process_info_detail,
    Info_process_memory = 
        try
            case get_process_info(Process_info_detail, length(L), 0, 0, memory) of 
                {ok, Process_memory_List, _Process_memory_List_len} ->
                    Info_process_memory_list = 
                        lists:map( 
                          fun({_Pid, _RegName, Mlen, _Qlen, _Module, _Other, _Messgaes}) -> 
                                  Mlen
                          end,
                          Process_memory_List),
                    lists:sum(Info_process_memory_list);
                _ ->
                    -1
            end
        catch
            _:_ -> -2
        end,

    Info_process_memory.

%%获取所有进程的cpu 内存 队列
get_nodes_cmq(_Node,Type)->
    %% L = mod_node_interface:server_list(),
    %% Info_list0 =
    %%     if
    %%         L == [] ->
    %%             [];
    %%         true ->
    %%             lists:map(
    %%               fun(S)  ->
    %%                       case get_nodes_cmq(Type) of
    %%                           {badrpc,_}->
    %%                               [];
    %%                           GetList ->
    %%                               GetList
    %%                       end
    %%               end
    %%               ,L)
    %%     end,

    Info_list0 = [get_nodes_cmq(Type)],

    try
        Info_list = lists:flatten(Info_list0),
        F_sort = fun(A,B)->                                     
                         {_,_,{_K1,V1}}=A,
                         {_,_,{_K2,V2}}=B,
                         V1 > V2
                 end,
        Sort_list = lists:sort(F_sort,Info_list),
        F_print = fun(Ls,Str) ->
                          lists:concat([Str,tuple_to_list(Ls)])
                  end,
        lists:foldl(F_print,[],Sort_list)
    catch _e:_e2 ->
            %%file:write_file("get_nodes_cmq_err.txt",_e2)
            ?DEBUG("_GET_NODES_CMQ_ERR:~p",[[_e,_e2]])
    end.

get_nodes_cmq(Type)->
	A = lists:foldl( 
		  fun(P, Acc0) -> 
                  case Type of
                      1 ->
                          [{P, 
						  	erlang:process_info(P, registered_name), 
							erlang:process_info(P, reductions) }
                           | Acc0] ;
                      2 ->
                          [{P,
							erlang:process_info(P, registered_name), 
							erlang:process_info(P, memory)}
                           | Acc0] ;
                      3 ->
                          [{P, 
							erlang:process_info(P, registered_name), 
							erlang:process_info(P, message_queue_len)} 
                           | Acc0] 
                  end
          end, 
		  [], 
		  erlang:processes()
         ),
	%%B = io_lib:format("~p", [A]),
	A.

%%查进程信息    
get_porcess_info(Pid_list) ->
    L = mod_node_interface:server_list(),
    Info_list0 =
        if
            L == [] ->
                [];
            true ->
                lists:map(
                  fun(S) ->
                          case rpc:call(S#server.node, mod_node_interface,get_process_info,[Pid_list]) of
                              {badrpc,_} ->
                                  [];
                              GetList ->
                                  GetList
                          end
                  end
                  ,
                  L                                              
                 )
        end,
    file:write_file("info_1.txt",Info_list0),
    Info_list = lists:flatten(Info_list0),
    F_print = fun(Ls,Str) ->
                      lists:concat([Str,Ls])
              end,
    lists:foldl(F_print, [],Info_list).

close_nodes(Type, Socket) ->
    case Type of
        2 ->
            safe_quit([], Socket);
        _ ->
            nodes()
    end.

%% 后台通知新发的邮件
send_new_mail(PlayerId) ->
    PlayerStatus = lib_player:get_user_info_by_id(PlayerId),
    case is_record(PlayerStatus, player) of
        true ->
            {ok, BinData} = pt_19:write(19005, 1),
            lib_send:send_to_sid(PlayerStatus#player.other#player_other.pid_send, BinData);
        false ->
            skip
    end,
    ok.

send_pay_info(Parm) ->
    [AccidStr, SnStr] = string:tokens(Parm, "&"),
    [_, AccId] = string:tokens(AccidStr, "="),
    [_, Sn] = string:tokens(SnStr, "="),
    PlayerId = db_agent:check_role_exists(list_to_integer(Sn), AccId, accid),
    ?ERROR_MSG("AccId: ~s, Sn: ~w, Id: ~w~n", [AccId, list_to_integer(Sn), PlayerId]),
    case lib_player:get_player_pid(PlayerId) of
        [] ->
            ?ERROR_MSG("New Pay, Player Not Online", []),
            skip;
        Pid ->
            gen_server:cast(Pid, {new_pay_info})
    end.



%% 获取负载
get_system_load() ->
    %% 全局进程缺省负载权重
    Load_fact = 10,
    Global_load =	
        case ets:match(?ETS_MONITOR_PID, {'_', global ,'$3'}) of
            [] ->
                0;
            Global_list ->
                Global_load_list = 
                    lists:map( 
                      fun([P]) ->
                              case P of
                                  {_Module, _WorkerNumber, WeightLoad} -> 
                                      WeightLoad;
                                  _ ->
                                      Load_fact
                              end
                      end, Global_list),
                case Global_load_list of
                    [] ->
                        0;
                    _ ->
                        lists:sum(Global_load_list)
                end
        end,
	ScenePlayerCount = 
        try 
            ets:info(?ETS_ONLINE, size)
        catch 
            _:_ -> 0
        end,
    ConnectionCount = 			
        try
            ets:info(?ETS_ONLINE, size)
        catch 
            _:_ -> 0
        end,
    Player_Load = 
        try
            ScenePlayerCount/50 + ConnectionCount/10
        catch 
            _:_ -> 0
        end,			
    misc_timer:cpu_time() + Global_load + Player_Load.
update_server_state() ->
    try
        case lib_http_client:get(config:get_server_list_url()) of
            {ok, _Status, _ResponseHeaders, Body} ->                
                ?DEBUG("~ts~n", [Body]),
                Sn = config:get_one_server_no(),                
                ServerList = string:tokens(Body, ";"),
                {ServerState, ServerNameList} = 
                    lists:foldl(fun(Server, {Ans, AccServerNameList}) ->
                                        %% 1,1,一将功成,s1.fhzs.4399sy.com,8802,fhzs-cdnres.me4399.com/resources/20130603/,0;
                                        L = string:tokens(Server, ","),
                                        ServerId = lists:nth(2, L),
                                        TimeToServe = lists:last(L),                                        
                                        NewAns = 
                                            case util:to_integer(ServerId) of
                                                Sn ->
                                                    ?WARNING_MSG("Find Server~n", []),
                                                    util:to_integer(TimeToServe);
                                                _ ->
                                                    Ans
                                            end,
                                        ServerName = lists:nth(3, L),
                                        {NewAns, [{util:to_integer(ServerId), ServerName}|AccServerNameList]}
                                end, {0, []}, ServerList),
                ?WARNING_MSG("ServerState ~w", [ServerState]),
                lib_syssetting:put_game_data(server_state, Sn, ServerState),
                lib_syssetting:put_game_data(server_name_list, Sn, ServerNameList);
            Other ->
                ?WARNING_MSG("update_server_state error, ~w~n", [Other])
        end
    catch _:R ->
            ?DEBUG("http_client request ~p, get_stacktrace ~p~n", [R, erlang:get_stacktrace()]),
            false
    end.

