-module(config_app). 

-include("define_logger.hrl").

-export([
         get_application/0,
         get_log_level/0,
         get_stat_db/0,
         get_infant_ctrl/0,
         get_tcp_listener_ip/0,
         get_tcp_listener_port/0,
         get_db_config/1,
         get_db_type/1,
         get_gateway_node/0,
         get_service_wait_time/0,
         get_scene_here/0,
         get_global_mod_here/0,
         get_can_gmcmd/0,
         get_data_words_version/0,
         get_strict_md5/0,
         get_http_ips/0,
         get_server_no/0,
         get_one_server_no/0,
         get_server_start_time/0,
         get_max_id/0,
         get_platform_name/0,
         get_platform_names_list/0,
         get_client_platform_name/0,
         get_ticket/0,
         get_cid/0,
         get_game_name/0,
         get_card_key/0,
         get_shop_force_coin/0,
         get_scene_virtual_number/1,
         get_scene_virtual/0,
         get_file_path/0,
         get_gateway/0,
         get_gateway_cookie/0,
         get_server_list_url/0
        ]).
%% -compile(export_all).

%% 获取application名称
get_application() ->
    case application:get_application() of
	{ok, App} -> App;
	_ -> undefined
    end.

%% 获取 .config里的配置信息
get_log_level() ->
    case application:get_env(log_level) of
	{ok, Log_level} -> Log_level;
	_ -> 3
    end.

%% 是否统计数据库访问情况 （1：开启; 0: 关闭）   
get_stat_db() ->
    case application:get_env(stat_db) of
	{ok, Stat_db} -> misc:to_integer(Stat_db);
	_ -> 0
    end.

%% 防沉迷开关读取
get_infant_ctrl() ->
    case application:get_env(infant_ctrl) of
        {ok, Mode} ->
            misc:to_integer(Mode);
        _ ->
            0
    end.

get_tcp_listener_ip() ->  
    case application:get_env(tcp_listener_ip) of
 	{ok, false} ->
            throw(undefined);
	{ok, Tcp_listener_ip} -> 
            try
                {_, Ip} =
                    lists:keyfind(ip, 1, Tcp_listener_ip),
                [Ip]
            catch
                _:_ -> exit({bad_config, {server, {tcp_listener_ip, config_error}}})
            end;
 	undefined -> throw(undefined)
    end.

get_gateway() ->  
    case application:get_env(gateway) of
        {ok, false} ->
            throw(undefined);
        {ok, Gateway} -> 
            try
                {_, Node} = lists:keyfind(node, 1, Gateway),
                {_, Cookie} = lists:keyfind(cookie, 1, Gateway),
                {_, Ip}   = lists:keyfind(ip, 1, Gateway),
                {_, Port} = lists:keyfind(port, 1, Gateway),
                {_, Group}= lists:keyfind(group, 1, Gateway),
                [misc:to_atom(Node), misc:to_atom(Cookie), Ip, Port, Group]
            catch
                _:_ -> exit({bad_config, {server, {gateway, config_error}}})
            end;
        undefined -> throw(undefined)
    end.

get_gateway_node() ->
    [Node, _,  _, _, _]= get_gateway(),
    Node.

get_gateway_cookie() ->
    [_, Cookie,  _, _, _]= get_gateway(),
    Cookie.

get_tcp_listener_port() ->
    case application:get_env(tcp_listener_port) of
 	{ok, false} -> throw(undefined);
	{ok, Tcp_listener} -> 
            try
                {_, Port} = lists:keyfind(port, 1, Tcp_listener),
                [Port]
            catch
                _:_ -> exit({bad_config, {server, {tcp_listener_port, config_error}}})
            end;
 	undefined -> throw(undefined)
    end.

get_db_config(Db) ->
    case application:get_env(Db) of
 	{ok, false} -> throw(undefined);
	{ok, DB_config} -> 
            {_, Type} = lists:keyfind(type, 1, DB_config),
            {_, Host} = lists:keyfind(host, 1, DB_config),
            {_, Port} = lists:keyfind(port, 1, DB_config),
            {_, User} = lists:keyfind(user, 1, DB_config),
            {_, Password} = lists:keyfind(password, 1, DB_config),
            {_, DB} = lists:keyfind(db, 1, DB_config),
            {_, Poolsize} = lists:keyfind(poolsize, 1, DB_config),
            {_, Encode} = lists:keyfind(encode, 1, DB_config),
            [Type, Host, Port, User, Password, DB, Poolsize, Encode];		
 	undefined -> throw(undefined)
    end.

get_db_type(Db) ->
    case application:get_env(Db) of
 	{ok, false} -> throw(undefined);
	{ok, DB_config} -> 
            {_, Type} = lists:keyfind(type, 1, DB_config),
            Type;
 	undefined -> throw(undefined)
    end.

%% get_gateway_node() ->
%%     case application:get_env(gateway_node) of
%% 	{ok, Gateway_node} -> Gateway_node;
%% 	_ -> undefined
%%     end.

get_service_wait_time() ->
    case application:get_env(service_wait_time) of
	{ok, Wait_time} -> Wait_time;
	_ -> undefined
    end.

get_scene_here() ->	
    case application:get_env(scene_here) of
        {ok, all} -> all;
        {ok, SL} when is_list(SL) -> SL;
        _ -> []
    end.	

get_global_mod_here() ->
    case application:get_env(global_mod) of
        {ok, all} -> all;
        {ok, Gmod} when is_list(Gmod) -> Gmod;
        _ -> []
    end.

get_can_gmcmd() ->
    case application:get_env(can_gmcmd) of
        {ok, Can_gmcmd} -> Can_gmcmd;
	_ -> 0
    end.  
get_data_words_version() ->
    case application:get_env(data_words_verson) of
        {ok, Data_words_version} ->
            Data_words_version;
        _->
            0
    end.
get_strict_md5() ->
    case application:get_env(strict_md5) of
        {ok, Strict_md5} -> Strict_md5;
	_ -> 1
    end.  	

get_http_ips() -> 
    case application:get_env(http_ips) of
        {ok, Http_ips} ->
            Http_ips;
        _ ->
            []
    end.

%% 获取开服编号(合服情况下可多个，以英文逗号分隔)
get_server_no() ->
    case application:get_env(server_no) of
        {ok, Server_no} ->
            Server_no;
        _ ->
            undefined
    end.

%% Server_no多个时，随机的取一个即可。
get_one_server_no() ->
    try
        Sn = get_server_no(),
        [H|_T] = string:tokens(Sn,","),
        misc:to_integer(H)
    catch 
        _:_ -> 1
    end.

get_server_start_time() ->
    case application:get_env(server_start_timestamp) of
        {ok, TimeStamp} ->
            TimeStamp;
        _ ->
            {1970, 1, 1, 0, 0, 0}
    end.

%% 获取 .config里的配置信息
get_max_id() ->
    case application:get_env(max_id) of
        {ok, Max_id} -> 
            case is_integer(Max_id) == true of
                false ->
                    0;
                true ->
                    Max_id
            end;
        _ ->
            0
    end.

%% 获取平台名称
get_platform_name() ->
    case application:get_env(platform) of
        {ok, Name} ->
            Name;
        _ ->
            undefined
    end.

get_platform_names_list() ->
    try
        PlatformStr = get_platform_name(),
        PlatformList = string:tokens(PlatformStr, ","),
        %% 去掉前后空格
        lists:map(fun (Str) -> string:strip(Str, both, 32) end, PlatformList)
    catch 
        _:_ -> "4399"
    end.
%% 获取客户端平台名称
get_client_platform_name() ->
    case application:get_env(client_platform) of
        {ok, Name} ->
            Name;
        _ ->
            undefined
    end.

%% 获取和平台之间的加密串
get_ticket() ->
    case application:get_env(ticket) of
        {ok, Ticket} ->
            Ticket;
        _ ->
            []
    end.

%% 获取平台入口 cid
get_cid() ->
    case application:get_env(cid) of
        {ok, Cid} ->
            Cid;
        _ ->
            []
    end.

%% 获取游戏名
get_game_name() ->
    case application:get_env(game) of
        {ok, Game} ->
            Game;
        _ ->
            []
    end.

%% 获取加密串号
get_card_key() ->
    case application:get_env(card_key) of
        {ok,Crypto} ->
            Crypto;
        _ ->
            undefined
    end.

%% 商城是否强制成 铜币购买
get_shop_force_coin() ->
    case application:get_env(shop_force_coin) of
	{ok, Force} -> misc:to_integer(Force);
	_ -> 0
    end.

%% 虚拟场景个数
get_scene_virtual_number(Id) ->
    case application:get_env(scene_virtual) of
        {ok, false} -> 0;
        {ok, Scene_virtual} -> 	
            case lists:keyfind(Id, 1, Scene_virtual) of
                false -> 0;
                {_, Num} -> 
                    if (Num > 98)->
                            98;
                       true -> Num
                    end
            end;
        undefined -> 0
    end.

%% 虚拟场景个数
get_scene_virtual() ->
    case application:get_env(scene_virtual) of
        {ok, false} -> [];
        {ok, Scene_virtual} ->
            SL1 = lists:map(fun(S)->
                                    case S of
                                        {Scene, _Num}-> Scene;
                                        _->[]
                                    end
                            end, 
                            Scene_virtual),
            SL2 = lists:filter(fun(N)-> N =/= [] end, SL1),
            SL2;
        undefined -> []
    end.

%% 获取文件路径
get_file_path() ->
    case application:get_env(file_path) of
	{ok, Path} -> Path;
	_ -> undefined
    end.

get_server_list_url()->
    case application:get_env(server_list_url) of
	{ok, Url} -> 
            Url;
	_ -> 
            undefined
    end.

