%%%--------------------------------------
%%% @Module  : table_to_record
%%% @Created : 2010.10.27 
%%% @Description: 将mysql数据表 转换成 erl record
%%%            生成文件： "../include/table_to_record.hrl"
%%%--------------------------------------
-module(htable_to_record).

%%
%% Include files
%%
%% -include("define_logger.hrl").

%% -include("record.hrl").
%% -include_lib("leshulib/include/language.hrl").
%% -include_lib("leshulib/include/define_goods.hrl").
%% -include_lib("leshulib/include/define_dungeon.hrl").
%% -include_lib("leshulib/include/define_skill.hrl").
%% -include_lib("leshulib/include/define_player.hrl").
%% -include_lib("leshulib/include/define_manor.hrl").
%% -include_lib("leshulib/include/define_answer.hrl").
%% -include_lib("leshulib/include/define_scene.hrl").
%% -include_lib("leshulib/include/define_battle.hrl").
%% -include_lib("leshulib/include/define_team.hrl").
%% -include_lib("stdlib/include/ms_transform.hrl"). 

%% 数据库分库列表
%% -define(DB_LIST,
%%         [
%%         db_admin,
%%         db_center,
%%         db_base,
%%         db_game,
%%         db_log,
%%         db_game_slave,
%%         db_log_slave        
%%        ]).

-define(CONFIG_FILE, "../config/server.app").

-define(TMP_TABLE_PATH, "./tmptable/").
-define(SRC_TABLE_PATH, "../src/table/").
-define(INCLUDE_PATH,   "../include/").
-define(RECORD_FILENAME, "../include/table_to_record.hrl").
-define(BEAM_PATH, "./"). 

-define(TABLES,
        [
         {server, server},
         {player, player},
         {pay_info, ets_pay_info},
         {goods, goods},
         {mail, mail},
         {base_answer, ets_base_answer},
         {base_artifact, ets_base_artifact},
         {base_career, ets_base_career},
         {base_culture_state, ets_base_culture_state},
         {base_scene, ets_scene},
         {base_mon, ets_mon},
         {base_npc, ets_npc},
         {base_goods, ets_base_goods},
         {base_goods_drop_num, ets_base_goods_drop_num},
         {base_goods_drop_rule, ets_base_goods_drop_rule},
         {base_goods_ore,ets_base_goods_ore},
         {base_player,rec_base_player},
         {base_protocol, ets_base_protocol},
         {base_goods_refine, ets_base_goods_refine},
         {base_equip_strengthen_rule, ets_base_equip_strengthen_rule},
         {base_equip_suit, ets_base_equip_suit},
         {base_equip_suit_attribute, ets_base_equip_suit_attribute},
         {equip_practise_attribute, ets_equip_practise_attribute},
         {base_equip_practise_star, ets_base_practise_star},
         {base_equip_practise_num, ets_base_practise_num},
         {equip_strengthen_cd, equip_strengthen_cd},
         {base_equip_forge, ets_base_equip_forge},
         {base_equip_upgrade, ets_base_equip_upgrade},

         %% 寻宝相关
         {base_treasure, ets_base_treasure},
         {base_treasure_explore, ets_base_treasure_explore},
         {treasure, ets_treasure},
         {treasure_equip, ets_treasure_equip},
         {log_treasure_receive, rec_log_treasure_receive},
         {log_treasure_merge, rec_log_treasure_merge},

         %%beast相关
         {base_beast,rec_base_beast},
         %% {base_beast_skill_rule,rec_base_beast_skill_rule},

         %% {base_beast_keep_attri_rule,rec_base_beast_keep_attri_rule},
         {base_beast_keep_attri,rec_base_beast_keep_attri},
         %% {base_beast_exp,rec_base_beast_exp},
         {beast,rec_beast},         
         %%新版灵兽阵法相关
         {base_beast_camp,rec_base_beast_camp},
         %% {base_beast_camp_rule,rec_base_beast_camp_rule},

         {base_star_power, rec_base_star_power},
         {log_beast, log_beast},

         
         %% 战报数据结构
         {fight_report,rec_fight_report},
         {fight_report_info,rec_fight_report_info},

         {camp, rec_camp},
         
         %% 战斗属性存储结构
         {player_combat_attri, rec_player_combat_attri},
         {player_combat_attribute_ex, ets_player_combat_attribute_ex},

         {base_equip_attribute,      rec_base_equip_attribute},         
         {base_combat_buff,          rec_base_combat_buff},      %% 战斗Buff
         {base_combat_skill,         rec_combat_skill},
         {base_combat_skill_passive, rec_combat_skill_passive},

         %% 武魂系统相关
         {base_fight_soul, rec_base_fight_soul},
         {fight_soul, rec_fight_soul},
         {dungeon_evaluate, rec_dungeon_evaluate},

         %% {base_shop, ets_shop},
         {base_shop_goods, ets_shop_goods},
         {base_shop_panic_goods, ets_shop_panic_goods},
         {shop_log_panic, ets_player_panic_goods},
         {base_talk, talk},
         {base_task, task},
         {task_bag, role_task},
         {task_log, role_task_log},
         {base_skill, ets_skill},
         {base_camp, ets_camp},
         {base_five_elements,ets_five_elements},

         %%帮派相关
         {base_guild, ets_base_guild},
         {base_guild_donate,ets_base_guild_donate},
         {base_guild_position,ets_base_guild_position},
         {base_guild_skill,ets_base_guild_skill},
         {base_guild_magic, ets_base_guild_magic},%%帮派术法基础数据

         {guild, ets_guild},
         %% {view_guild_member, ets_guild_member, "帮派成员"},
         %% {view_guild_apply, ets_guild_apply,   "帮派申请"},
         %% {guild_member, ets_insert_guild_member,"帮派成员，警告：此ets只能用于insert成员数据时用"},
         %% {guild_apply, ets_insert_guild_apply, "帮派失去你请记录，警告：此ets只能用于insert成员数据时用"},
         {guild_member,ets_guild_member},
         {guild_apply,ets_guild_apply},
         {guild_invite, ets_guild_invite},
         %% {guild_skills_attribute, ets_guild_skills_attribute},
         {guild_log, ets_guild_log},
         {guild_skill,ets_guild_skill},    
         {guild_magic,ets_guild_magic},
         {guild_fight_list, rec_guild_fight_list},
         {guild_fight_member, rec_guild_fight_member},
         {guild_fight_gamble, rec_guild_fight_gamble},

         {base_forage, rec_base_forage},
         {forage_guild, rec_forage_guild},
         {forage_player, rec_forage_player},
         
         {player_magic, ets_player_magic},  
         {log_warehouse_flowdir, ets_log_warehouse_flowdir},     
         {base_pet, ets_base_pet},
         {base_pet_collect, ets_base_pet_collect},
         {base_pet_feed_cd, ets_base_pet_feed_cd},
         {base_pet_active_skill, ets_base_pet_active_skill},
         {base_pet_passive_skill, ets_base_pet_passive_skill},
         {base_pet_character_skill, ets_base_pet_character_skill},
         {base_pet_character,ets_base_pet_character},
         {pet, ets_pet},
         {base_dungeon, dungeon},
         {player_dungeon, player_dungeon},
         {master_apprentice, ets_master_apprentice},
         {master_charts, ets_master_charts},
         {meridian,ets_meridian},
         {meridian_log,ets_meridian_log},
         {base_meridian,ets_base_meridian},
         {base_meridian_cd,ets_base_meridian_cd},
         {upcamp,ets_upcamp},
         %% {sale_goods, ets_sale_goods},
         {log_sale_dir, ets_log_sale_dir},
         {online_gift, ets_online_gift},
         {base_online_gift, ets_base_online_gift},
         {base_login_gift, ets_base_login_gift},

         {base_scene_class,ets_base_scene_class},

         {feedback, feedback},
         {base_target_gift,ets_base_target_gift},
         {target_gift,ets_target_gift},
         {task_consign,ets_task_consign},
         {player_sys_setting, player_sys_setting, "玩家游戏系统设置"},
         {arena,             ets_arena},
         {arena_week,        ets_arena_week},
         {arena_member,      ets_arena_member},
         {arena_report,      ets_arena_report},
         {arena_reward,      ets_arena_reward},
         {base_arena_member, ets_base_arena_member},
         {base_arena_reward, ets_base_arena_reward},
         {base_arena_winning_reward, ets_base_arena_winning_reward},
         {consign_task,   ets_consign_task},
         {consign_player, ets_consign_player},
         {carry_item,ets_carry_item},
         {carry_player,ets_carry_player},
         {carry_call,ets_carry_call},
         %% {base_bless,ets_base_bless},
         %% {daily_bless,ets_daily_bless},
         {base_plant, ets_base_plant},         
         {plant_yard, ets_plant_yard},
         {plant_player, ets_plant_player},                  
         {offline_award, ets_offline_award},
         {online_award, ets_online_award},
         {business, ets_business},
         {log_business_robbed, ets_log_robbed},
         {base_business, ets_base_business},
         {online_award_holiday, ets_online_award_holiday},
         {hero_card, ets_hero_card},
         {base_hero_card, ets_base_hero_card},
         {love, ets_love},
         {base_privity, ets_base_privity},
         {base_goods_fashion, ets_base_goods_fashion},
         {base_rank, ets_base_rank},                              %% 军衔系统
         {base_tavern, ets_base_tavern},                          %% 酒馆招募系统
         {base_partner_beyond_level, ets_base_partner_beyond},
         {base_parter, ets_base_parter},                          %% 伙伴模版数据
         {base_partner_grow, ets_base_partner_grow},              %% 伙伴成长属性
         {base_partner_pray, ets_base_partner_pray},              %% 伙伴成长属性
         {base_partner_fate, rec_base_partner_fate},              %% 将缘配置表
         {base_partner_destiny, rec_base_partner_destiny},        %% 天命配置表
         {parter, ets_parter},                                    %% 玩家伙伴
         {partner_rank, rec_partner_rank},                        %% 武将单挑排行
         {player_tavern, ets_player_tavern},                      %% 玩家的酒馆数据
         {base_goods_drop_control, ets_base_goods_drop_control},  %% 物品掉落控制
         {goods_drop_sum_log, goods_drop_sum_log},
         {player_no_drop_log, player_no_drop_log},
         {base_buff, ets_base_buff},
         {player_buff, db_player_buff},
         %% {player_other, db_player_other},
         {player_info, ets_player_info},
         {base_trainer, ets_base_trainer},
         {player_trainer, dic_player_trainer},
         {player_mystery_shop, ets_player_mystery_shop},          %% 神秘商店
         {player_gift, ets_player_gift},

         %% %% 心法相关，删除2012.12.25@zhangr
         %% {base_mind,ets_base_mind},
         %% {base_mind_secret,ets_base_mind_secret},
         %% {mind,ets_mind},
         %% {mind_use,ets_mind_use},

         %% 市场相关表
         {market_sale_info, ets_market_sale_info},
         {market_buy_info, ets_market_buy_info},

         %% 留言板信息
         {message_board, ets_message_board},

         {player_hs_info, dic_player_hs_info},
         {player_devil_tower, dic_player_devil_tower},
         {base_trial_data, base_trial_data},
         {base_trial_award, base_trial_award},
         {player_trial, dic_player_trial},
         %% 玩家成就、称号相关四个表
         {base_achieve, base_achieve},
         {base_achieve_gift, base_achieve_gift},
         {base_title, base_title},
         {player_achieve, dic_player_achieve},
         {player_achieve_times, dic_player_achieve_times},
         
         {base_boss, base_boss},         
         {player_data, dic_player_data},
         {base_pet_exp, ets_base_pet_exp},
         {base_pet_train_cost, ets_base_pet_train_cost},
         {base_activity, base_activity},
         {base_op_activities, ets_base_op_activities},
         {base_boss_upgrade, base_boss_upgrade},
         
         %% 宝箱
         {base_chest, rec_base_chest},

         %% 过关斩将玩家相关ETS表
         {base_kilo_ride, rec_base_kilo_ride},
         %% {base_challenge_reward, rec_base_challenge_reward},
         {kilo_ride, rec_kilo_ride},   

         %% 过关斩将排行榜奖励配置表
         {base_kilo_ride_rank_reward, rec_base_kilo_ride_rank_reward},
         %% 群雄争霸相关ETS表to Record
         {base_warcraft, rec_base_warcraft},
         {warcraft, rec_warcraft},
         %% 玩家奴隶系统数据
         {player_master, ets_player_master_slave},
         
         %% 排行榜相关ETS表to Record
         {rank_data, rec_rank_data},
         %% 南蛮入侵配置数据
         {base_southern_upgrade, rec_base_southern_upgrade},
         
         %% 跨服竞技相关
         {ups_fighter, rec_ups_fighter},
         {ups_fight_report, rec_ups_fight_report},
         
         {sys_acm, rec_sys_acm},
         {base_sys_acm, rec_base_sys_acm},
         %% VIP 配置表
         {base_vip, rec_base_vip},
         %% LOG 表相关
         {log_mail, rec_log_mail},
         {log_goods_receive, rec_log_goods_receive},
         {log_warcraft_occupy, rec_log_warcraft_occupy},
         {log_warcraft_snatch, rec_log_warcraft_snatch},
         {log_arena_combat, rec_log_arena_combat},
         {log_partner_pray, rec_log_partner_pray},
         %% 等级标准表 
         {base_lv, rec_base_lv},
         {guild_fight_report, rec_guild_fight_report},
         {guild_fight_guild_board, rec_guild_fight_guild_board},
         {guild_fight_member_board, rec_guild_fight_member_board},
         {base_eight_fight_rank_reward, rec_base_eight_fight_rank_reward},
         {store_receipt, rec_store_receipt},
         {base_app_store_product, rec_app_store_product},
         %% 新手礼包激活码
         {activation_code, rec_activation_code},
         {base_activity_rate, rec_base_activity_rate},
         {ups_gamble, rec_ups_gamble},
         {base_ups_reward, rec_base_ups_reward}
        ]).

-record(erlydb_field,
        {name, name_str, name_bin, type, modifier, erl_type,
         html_input_type,
         null, key,
         default, extra, attributes}).
%%
%% Exported Functions
%%
-compile(export_all). 

%%
%% API Functions
%%

start() ->
    case mysql_util:init_db(?CONFIG_FILE) of
        ok ->
            io:format("Begin table_to_record......~n"),            
            tables_to_record(),
            io:format("table_to_record,This is OK!~n"),
            ok;
        _ ->
            io:format("table_to_record,This is fail: must mysql!~n"),
            db_config_fail
    end,
    halt(), 
    ok.


%% @doc 生成指定的表名的beam文件
code_gen(Db_poolid, TableList) ->
    %% io:format("B = ~p  /~p/~n",[1, Db_poolid]),    
    TableFiledList = writeTempFile(TableList),
    %% io:format("B = ~p~n",[2]),    
    erlydb:code_gen(TableFiledList,
                    {mysql, 
                     [{allow_unsafe_statements, true},
                      {skip_fk_checks, true}]
                     ,[{Db_poolid, default}]
                    },
                    [debug_info,
                     {skip_fk_checks, true},
                     {outdir,"../ebin/"}
                    ]),
    %% io:format("B = ~p~n",[3]),    
    clearTempFile(),
    %% io:format("B = ~p~n",[4]),    
    ok.


%% @doc 为指定的表名生成module文件，给code_gen/0 使用
%% @spec writeTempFile/0 ->[TableFilePath]
%%    eg: TableFilePath -> "./tmptable/tuser_friend_log.erl"
writeTempFile(TableList) ->
    clearTempFile(),
    ok = file:make_dir(?TMP_TABLE_PATH),
    lists:map(fun(F)-> 
                      Filename =
                          ?TMP_TABLE_PATH ++ atom_to_list(F) ++ ".erl",
                      Bytes = list_to_binary(io_lib:format("-module(~w).", [F]) ),
                      file:write_file(Filename, Bytes),
                      Filename
              end, TableList).

clearTempFile() ->
    case file:list_dir(?TMP_TABLE_PATH) of
        {ok, Filenames} ->
            lists:foreach(fun(F)->
                                  file:delete(?TMP_TABLE_PATH ++ F)
                          end, Filenames);
        {error, _} ->
            ignore
    end,
    file:del_dir(?TMP_TABLE_PATH).

tables_to_record() ->
    _Bakfile = 
        re:replace(
          lists:flatten(lists:concat([?RECORD_FILENAME , "_", time_format(now())])),
          "[ :]","_",[global,{return,list}]),
    %% file:rename(?RECORD_FILENAME, Bakfile), 
    lists:foreach(fun(Table)-> 
                          case Table of 
                              {Table_name, Record_name} -> 
                                  table_to_record(Table_name, Record_name, "");
                              {Table_name, Record_name, TableComment} -> 
                                  table_to_record(Table_name, Record_name, TableComment);
                              _-> no_action
                          end    
                  end, 
                  ?TABLES),
    io:format("finished!~n~n"),    
    ok.

%% %% @doc 显示人可以看得懂的错误信息
%% mysql_halt([Sql, Reason]) ->
%%     %%     io:format("mysql_halt__/~p/ ~n",[Reason]),
%%     catch erlang:error({db_error, [Sql, Reason]}).

%% table_to_record:table_to_record(user, 1).
%% [A,B]=get_row("show create table user;")
%% get_row("select * from base_goods_type;")
table_to_record(Table_name, Record_name, TableComment) ->
    try 
        case mysql_util:get_table_info(Table_name) of 
            [db_error] ->
                %% io:format("Get_row Error = ~p~n~n~n",[1]),                
                error;
            [Pool_id, A]->
                %% io:format("A = ~p~n~n~n",[Pool_id]),
                Create_table_list = re:split(A, "[\n]", [{return, binary}]),
                %% io:format("Create_table_list = ~p~n~n~n",[Create_table_list]), 
                Table_comment =
                    case TableComment of
                        "" ->
                            get_table_comment(Create_table_list, Table_name);
                        _ ->
                            TableComment
                    end,
                %% io:format("A = ~p~n~n~n",[2]),
                code_gen(Pool_id, [Table_name]),
                %% io:format("A = ~p~n~n~n",[3]),
                Table_fields = erlang:apply(Table_name, db_fields, []),
                
                %% io:format("Table_fields = ~p~n~n~n",[Table_fields]),
                {WriteList0, _} = 
                    lists:foldl(
                      fun(Field, {In, Sum}) ->
                              %% io:format("Sum_~p = ~p~n",[Sum, Field]),
                              Field_comment = get_field_comment(Create_table_list, Sum),

                              OutTemp = 
                                  if 
                                      Sum == 1 ->
                                          In ++
                                              [
                                               "%%%------------------------------------------------\t\n",
                                               io_lib:format("%%% File    : db_~s.hrl\t\n", [Table_name]),
                                               "%%% Description: 从mysql表生成的record\t\n",
                                               "%%% Warning:  由程序自动生成，请不要随意修改！\t\n",
                                               "%%%------------------------------------------------    \t\n",
                                               "\t\n",
                                               io_lib:format("-ifndef(DB_~s_HRL).\t\n",
                                                             [string:to_upper(misc:to_list(Table_name))]),
                                               io_lib:format("-define(DB_~s_HRL, true).\t\n",
                                                             [string:to_upper(misc:to_list(Table_name))]),
                                               "\t\n",
                                               list_to_binary(
                                                 io_lib:format("%% ~s\t\n", [Table_comment])),
                                               list_to_binary(
                                                 io_lib:format("%% ~s ==> ~s \t\n", [Table_name, Record_name])),
                                               list_to_binary(
                                                 io_lib:format("-record(~s, {\t\n", [Record_name]))
                                              ];
                                      true -> 
                                          In
                                  end,
                              %% io:format("Field_comment = ~p~n",[Field_comment]),
                              Default = 
                                  case Field#erlydb_field.default of
                                      undefined ->
                                          '';
                                      <<>> -> 
                                          case erlydb_field:get_erl_type(Field#erlydb_field.type) of
                                              binary -> 
                                                  lists:concat([" = \"\""]);
                                              integer -> 
                                                  lists:concat([" = 0"]);
                                              _ -> '' 
                                          end;
                                      <<"[]">> ->
                                          lists:concat([" = ", binary_to_list(Field#erlydb_field.default)]);
                                      Val -> 
                                          case erlydb_field:get_erl_type(Field#erlydb_field.type) of
                                              binary -> 
                                                  lists:concat([" = <<\"", binary_to_list(Val) ,"\">>"]);
                                              %% integer -> 
                                              %%     lists:concat([" = 0"]);
                                              _ -> 
                                                  lists:concat([" = ", binary_to_list(Val)])
                                          end
                                  end,
                              T1 = 
                                  if
                                      Sum == length(Table_fields) -> 
                                          '';
                                      true ->
                                          ','
                                  end,
                              %% io:format("Field[~p] = ~p / ~p ~n",[Sum, Field, Field_comment]),
                              T2 = io_lib:format("~s~s~s",
                                                 [Field#erlydb_field.name, Default, T1]),
                              %% io:format("T2_len= ~p/~p ~n",[length(T2), T2]),
                              T3 = lists:duplicate(40 - length(lists:flatten(T2)), " "),
                              {OutTemp ++
                                   [list_to_binary(io_lib:format("      ~s~s%% ~s\t\n",
                                                                 [T2, T3, Field_comment]))],
                               Sum + 1}
                      end,
                      {[], 1},
                      Table_fields),

                file_tools:writelines_new(
                  ?INCLUDE_PATH ++ io_lib:format("db_~s.hrl", [Table_name]),
                  WriteList0 ++ 
                      [list_to_binary(io_lib:format("    }).\t\n",[]))] ++
                      ["\t\n-endif.\t\n"]),
                io:format("                [~s] ~s ==> ~s ~n",
                          [Pool_id, Table_name, Record_name]),
                ok
        end
    catch
        _:R ->
            io:format("table_to_record failed ~nR:~w ~n,~n Stack : ~p~n",
                      [R, erlang:get_stacktrace()]),
            error
    end.

get_field_comment(Create_table_list, Loc) ->
    try
%%     L1 = re:split(lists:nth(Loc+1, Create_table_list),"[ ]",[{return, list}]),
        L1 = binary_to_list(lists:nth(Loc+1, Create_table_list)),    
%%   io:format("L1 = ~p ~n", [L1]),        
        Loc1 = string:rstr(L1, "COMMENT "),
%%   io:format("Loc = ~p ~n", [Loc1]),    
        case Loc1 > 0 of
            true -> 
                L2 = string:substr(L1, Loc1 + 8),
                L3 = lists:subtract(L2, [39,44]),
                lists:subtract(L3, [39]);
            _ -> ""
        end
    catch
        _:_ -> ""
    end.

get_table_comment(Create_table_list, Table_name) ->
    try
%%     L1 = re:split(lists:nth(Loc+1, Create_table_list),"[ ]",[{return, list}]),
        Len  = length(Create_table_list),    
        L1 = binary_to_list(lists:nth(Len, Create_table_list)),    
%%   io:format("L1 = ~p ~n", [L1]),        
        Loc1 = string:rstr(L1, "COMMENT="),
%%   io:format("Loc = ~p ~n", [Loc1]),    
        case Loc1 > 0 of
            true -> 
                L2 = string:substr(L1, Loc1 + 8),
                L3 = lists:subtract(L2, [39,44]),
                lists:subtract(L3, [39]);
            _ -> Table_name
        end
    catch
        _:_ -> Table_name
    end.    
  
%% time format
one_to_two(One) -> io_lib:format("~2..0B", [One]).

%% @doc get the time's seconds for integer type
%% @spec get_seconds(Time) -> integer() 
get_seconds(Time)->
    {_MegaSecs, Secs, _MicroSecs} = Time, 
    Secs.
    
time_format(Now) -> 
    {{Y,M,D},{H,MM,S}} = calendar:now_to_local_time(Now),
    lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D), " ", 
                        one_to_two(H) , ":", one_to_two(MM), ":", one_to_two(S)]).
date_format(Now) ->
    {{Y,M,D},{_H,_MM,_S}} = calendar:now_to_local_time(Now),
    lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D)]).
date_hour_format(Now) ->
    {{Y,M,D},{H,_MM,_S}} = calendar:now_to_local_time(Now),
    lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D), " ", one_to_two(H)]).
date_hour_minute_format(Now) ->
    {{Y,M,D},{H,MM,_S}} = calendar:now_to_local_time(Now),
    lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D), " ", one_to_two(H) , "-", one_to_two(MM)]).
%% split by -
minute_second_format(Now) ->
    {{_Y,_M,_D},{H,MM,_S}} = calendar:now_to_local_time(Now),
    lists:concat([one_to_two(H) , "-", one_to_two(MM)]).

hour_minute_second_format(Now) ->
    {{_Y,_M,_D},{H,MM,S}} = calendar:now_to_local_time(Now),
    lists:concat([one_to_two(H) , ":", one_to_two(MM), ":", one_to_two(S)]).

