%% Author: T400
%% Created: 2011-7-22
%% Description: TODO: Add description to dynamic_config
-module(dynamic_config).

%%
%% Include files
%%
-include("define_logger.hrl").

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%
start() ->
    [ModName, Src] = config_src(),
    dynamic_module(ModName, Src).

dynamic_module(ModName, Src) ->
    try
        {Mod, Code} = dynamic_compile:from_string(Src),
        code:load_binary(Mod, ModName ++ ".erl", Code),
        io:format("~n    dynamic_config OK!")
    catch
        Type:Error ->
            io:format("Error compiling ~p (~p): ~p~n", [ModName, Type, Error])
    end.

%%
%% Local Functions
%%
config_src() ->
    Config_funs = [
                   {get_application, []},
                   {get_log_level, []},
                   {get_tcp_listener_ip, []},
                   {get_tcp_listener_port, []},

                   {get_db_config, [db_admin], ";"},
                   %% {get_db_config, [db_center], ";"},
                   {get_db_config, [db_base], ";"},
                   {get_db_config, [db_game], ";"},
                   {get_db_config, [db_log]},

                   {get_db_type, [db_admin], ";"},
                   %% {get_db_type, [db_center], ";"},
                   {get_db_type, [db_base], ";"},
                   {get_db_type, [db_game], ";"},
                   {get_db_type, [db_log]},

                   {get_gateway_node, []},
                   {get_service_wait_time, []},
                   {get_global_mod_here, []},
                   {get_can_gmcmd, []},
                   {get_data_words_version,[]},
                   {get_strict_md5, []},
                   {get_http_ips, []},
                   {get_server_no, []},
                   {get_one_server_no, []},
                   {get_server_start_time, []},
                   {get_stat_db, []},
                   {get_platform_name, []},
                   {get_client_platform_name, []},
                   {get_platform_names_list, []},
                   {get_game_name, []},
                   {get_ticket, []},
                   {get_cid, []},
                   {get_card_key, []},
                   {get_shop_force_coin, []},
                   {get_file_path, []},
                   {get_gateway, []},
                   {get_gateway_cookie, []},
                   {get_server_list_url, []}
                  ],
    Funs = lists:map( 
             fun(Cfg) ->
                     try
                         {F, P, End} =
                             case Cfg of
                                 {F0, P0} -> {F0, P0, "."};
                                 {F0, P0, End0} -> {F0, P0, End0}
                             end,

                         case P of 
                             [] ->
                                 Config_value = util:term_to_string(erlang:apply(config_app, F, [])),
                                 lists:concat([F, '()->', Config_value, End, '\t\n']);
                             [P1] ->
                                 Config_value = util:term_to_string(erlang:apply(config_app, F, [P1])),
                                 lists:concat([F, '(', P1 ,')->', Config_value, End, '\t\n'])
                         end
                     catch
                         _:_ -> io:format("Error____/~p/~n",[Cfg])
                     end
             end,
             Config_funs),   
    Funcs_scene_virtual = get_scene_virtual(),
    Funcs_infant_ctrl = get_infant_ctrl(),
    ["dynamic_config",
     "-module(config). 
     -compile(export_all). "
    ++ lists:concat(Funs)
++ lists:concat(Funcs_scene_virtual)
++ lists:concat(Funcs_infant_ctrl)
].

get_scene_virtual() ->
    Scene_virtual_ids = config_app:get_scene_virtual(),
    Funs = lists:map( 
             fun(Id) ->
                     Num = config_app:get_scene_virtual_number(Id),
                     lists:concat(['get_scene_virtual_number(', Id ,')->', Num, ';\t\n'])
             end,
             Scene_virtual_ids),             
    Funs ++ [lists:concat(['get_scene_virtual_number(_)->0.\t\n'])]
        ++ [lists:concat(['get_scene_virtual()->', util:term_to_string(Scene_virtual_ids),'.\t\n'])].


get_infant_ctrl() ->
    case config_app:get_infant_ctrl() of
        1 ->
            [lists:concat(['get_infant_ctrl(_)->1.\t\n'])];
        _ ->
            [lists:concat(['get_infant_ctrl(AccName)-> \t\n L3 = string:left(tool:to_list(AccName), 3),\t\n if (L3 == "llj") -> \t\n 1; \t\n    true -> 0 \t\n end. \t\n'])]
    end.

