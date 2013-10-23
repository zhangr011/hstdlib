%%%--------------------------------------
%%% @Module  : record_to_code
%%% @Created : 2010.10.27 
%%% @Description: 将record 转换成 erl code
%%%			暂时先处理 player, 以便方便的按所需字段读写player字段值。
%%%			生成文件： "../src/lib/lib_player_rw.erl"
%%%			生成文件： "../src/db/db_mongo_fields.erl"
%%%			生成文件： "../temp/Table_field_short.php"
%%%         生成文件： "../temp/clear_table.sql"
%%%--------------------------------------
-module(hrecord_to_code).

%% %%
%% %% Include files
%% %%
%% -include("leshulib/include/define_database.hrl").
%% -include("leshulib/include/define_player.hrl").

%% %%
%% %% Exported Functions
%% %%

%% -compile(export_all). 

%% -define(CONFIG_FILE, "../config/server.app").

%% %%
%% %% API Functions
%% %%
%% start()->	
%% 	convert_player(),
%% 	case init_db(?CONFIG_FILE) of
%% 		ok ->
%% 			Tables_name = get_tables_name(),
%% 			table_fields_all(Tables_name),
%% 			get_all_tables(Tables_name),
%% 			io:format("record_to_code,This is OK!~n"),
%% 			ok;
%% 		_ ->
%% 			io:format("record_to_code,This is fail: must mysql!~n"),
%% 			db_config_fail
%% 	end,
%% 	halt(),
%% 	ok.

%% init_db(Config_file)->	
%% 	Result = lists:sum( 
%% 				lists:map(
%% 					fun(Db) ->
%% 						init_db(Config_file, Db)
%% 					end, 
%% 					?DB_LIST)
%% 			),
%% 	if Result == 0 ->
%% 		   ok;
%% 	   true ->
%% 		   fail
%% 	end.

%% init_db(Config_file, Db)->	
%% 	case  get_db_config(Config_file, Db) of
%% 		[Type, Host, Port, User, Password, DB, _Poolsize, Encode] ->
%% 			case Type of
%% 				db_mysql ->
%% 					try
%% 						start_erlydb(Db, Host, Port, User, Password, DB),
%%     					mysql:start_link(mysql_dispatcher, 
%%                                          Db, Host, Port, User, Password, DB, 
%%                                          fun(_, _, _, _) ->
%%                                                  ok
%%                                          end, Encode),
%%     					mysql:connect(mysql_dispatcher, Db, Host, Port, User, Password, DB, Encode, true),
%% 						0
%% 					catch
%% 						_:_ ->
%%                             1
%% 					end;
%% 				_ ->
%%                     %% 必须是 mysql
%%                     1
%% 			end;
%% 		_ ->
%%             1
%% 	end.

%% get_db_config(Config_file, Db) ->
%% 	try
%% 		{ok,[{_,_, Conf}]} = file:consult(Config_file),
%% 		{env, L} = lists:keyfind(env, 1, Conf),	
%% 		{_, Db_config} = lists:keyfind(Db, 1, L),
%% 		{_, Type} = lists:keyfind(type, 1, Db_config),
%% 		{_, Host} = lists:keyfind(host, 1, Db_config),
%% 		{_, Port} = lists:keyfind(port, 1, Db_config),
%% 		{_, User} = lists:keyfind(user, 1, Db_config),
%% 		{_, Password} = lists:keyfind(password, 1, Db_config),
%% 		{_, DB} = lists:keyfind(db, 1, Db_config),
%% 		{_, Poolsize} = lists:keyfind(poolsize, 1, Db_config),
%% 		{_, Encode} = lists:keyfind(encode, 1, Db_config),
%% 		[Type, Host, Port, User, Password, DB, Poolsize, Encode]		
%% 	catch
%% 		_:_ -> no_config
%% 	end.	

%% %%
%% %% Local Functions
%% %%
%% start_erlydb(Db_poolid, IP, Port, User, Password, Db) ->
%% 	erlydb:start(mysql, [{pool_id, Db_poolid},
%% 						 {hostname, IP},
%% 						 {port, Port},
%% 						 {username, User}, 
%% 						 {password, Password}, 
%% 						 {database, Db},
%% 						 {encoding, utf8},
%% 						 {pool_size, 1}]).

%% convert_player() ->
%% 	io:format("~n~n~nBegin create ../src/lib/lib_player_rw.erl!~n"),
	
%% 	P_list = record_info(fields, player),
%% 	O_list = record_info(fields, player_other),
%%     %% io:format("P_list: ~p ~n",[P_list]), 
%%     %% io:format("O_list: ~p ~n",[O_list]),
%% 	File = "../src/lib/lib_player_rw.erl",
	
%% 	file:write_file(File, ""),
	
%% 	file:write_file(File, ""),
%% 	file:write_file(File, "%%%------------------------------------------------\t\n",[append]),
%% 	file:write_file(File, "%%% File    : lib_player_rw.erl\t\n",[append]),
%% 	%% Bytes0 = list_to_binary(io_lib:format("%%% Created : ~s\t\n", [time_format(now())])),
%% 	%% file:write_file(File, Bytes0,[append]),
%% 	file:write_file(File, "%%% Description: 从record生成的代码\t\n",[append]),
%% 	file:write_file(File, "%%% Warning:  由程序自动生成，请不要随意修改！\t\n",[append]),	
%% 	file:write_file(File, "%%%------------------------------------------------	\t\n",[append]),
%% 	file:write_file(File, " \t\n",[append]),	
%% 	file:write_file(File, "-module(lib_player_rw).\t\n",[append]),
%% 	file:write_file(File, " \t\n",[append]),
%% 	file:write_file(File, "%%  \t\n",[append]),
%% 	file:write_file(File, "%% Include files  \t\n",[append]),
%% 	%% file:write_file(File, "-include(\"common.hrl\"). \t\n",[append]),
%% 	file:write_file(File, "-include(\"define_player.hrl\"). \t\n",[append]),
%% 	file:write_file(File, "-include(\"define_pet.hrl\"). \t\n",[append]),
%% 	file:write_file(File, "  \t\n", [append]),
%% 	file:write_file(File, "%% \t\n", [append]),
%% 	file:write_file(File, "%% Exported Functions \t\n", [append]),
%% 	file:write_file(File, "%% \t\n",[append]),
%% 	file:write_file(File, "-export([get_player_info_fields/2, set_player_info_fields/2, get_player_info_changed/2, get_all_tables/0]). \t\n", [append]),
%% 	file:write_file(File, "  \t\n",[append]),	

%% 	file:write_file(File, "%%获取用户信息(按[字段1,字段2,...])\t\n",[append]),
%% 	file:write_file(File, "%% handle_call({cmd_player,  [x ,y]}, _from, Status)\t\n",[append]),
%% 	file:write_file(File, "get_player_info_fields(Player, List) ->\t\n",[append]),
%% 	file:write_file(File, "	lists:map(fun(T) ->\t\n",[append]), 
%% 	file:write_file(File, "			case T of\t\n",[append]),
%% 	lists:foreach(fun(Field_name) ->
%% 					Bytes00 = lists:concat(["				",Field_name," -> Player#player.",Field_name,";\t\n"]),
%% 					file:write_file(File, Bytes00,[append])			  
%% 				  end, 
%% 				  P_list),
%% 	lists:foreach(fun(Field_name) ->
%% 					Bytes00 = lists:concat(["				",Field_name," -> Player#player.other#player_other.",Field_name,";\t\n"]),
%% 					file:write_file(File, Bytes00,[append])			  
%% 				  end, 
%% 				  O_list),	
%% 	file:write_file(File, "				_ -> undefined\t\n",[append]),
%% 	file:write_file(File, "			end\t\n",[append]),						
%% 	file:write_file(File, "		end, List).\t\n",[append]),
%% 	file:write_file(File, " \t\n",[append]),
	
%% 	file:write_file(File, "%%设置用户信息(按[{字段1,值1},{字段2,值2, add},{字段3,值3, sub}...])\t\n",[append]),
%% 	file:write_file(File, "%% handle_cast({update_sg_player,[{x, 10} ,{y, 20, add},  ,{hp, 20, sub}]}, Status)\t\n",[append]),
%% 	file:write_file(File, "set_player_info_fields(Player, []) ->\t\n",[append]),
%% 	file:write_file(File, "	Player;\t\n",[append]),
%% 	file:write_file(File, "set_player_info_fields(Player, [H|T]) ->\t\n",[append]),
%% 	file:write_file(File, "	NewPlayer =\t\n",[append]),
%% 	file:write_file(File, "		case H of\t\n",[append]),
%% 	lists:foreach(fun(Field_name) ->
%% 					if Field_name =/= other ->
%% 						Bytes1 = lists:concat(["				{",Field_name,", Val, add} -> Player#player{",Field_name,"=Player#player.",Field_name," + Val};\t\n"]),
%% 						file:write_file(File, Bytes1,[append]),
%% 						Bytes2 = lists:concat(["				{",Field_name,", Val, sub} -> Player#player{",Field_name,"=Player#player.",Field_name," - Val};\t\n"]),
%% 						file:write_file(File, Bytes2,[append]),
%% 						Bytes3 = lists:concat(["				{",Field_name,", Val, _} -> Player#player{",Field_name,"= Val};\t\n"]),
%% 						file:write_file(File, Bytes3,[append]),
%% 						Bytes4 = lists:concat(["				{",Field_name,", Val} -> Player#player{",Field_name,"= Val};\t\n"]),
%% 						file:write_file(File, Bytes4,[append]);
%% 					   true -> no_action
%% 					end
%% 				  end, 
%% 				  P_list),
%% 	lists:foreach(fun(Field_name) ->
%% 					Bytes1 = lists:concat(["				{",Field_name,
%% 										   ", Val, add} -> Player#player{other=Player#player.other#player_other{",Field_name,
%% 										   " = Player#player.other#player_other.",Field_name," + Val}};\t\n"]),
%% 					file:write_file(File, Bytes1,[append]),
%% 					Bytes2 = lists:concat(["				{",Field_name,
%% 										   ", Val, sub} -> Player#player{other=Player#player.other#player_other{",Field_name,
%% 										   " = Player#player.other#player_other.",Field_name," - Val}};\t\n"]),
%% 					file:write_file(File, Bytes2,[append]),
%% 					Bytes3 = lists:concat(["				{",Field_name,
%% 										   ", Val, _} -> Player#player{other=Player#player.other#player_other{",Field_name,
%% 										   " =  Val}};\t\n"]),
%% 					file:write_file(File, Bytes3,[append]),
%% 					Bytes4 = lists:concat(["				{",Field_name,
%% 										   ", Val} -> Player#player{other=Player#player.other#player_other{",Field_name,
%% 										   " =  Val}};\t\n"]),
%% 					file:write_file(File, Bytes4,[append])  
%% 				  end, 
%% 				  O_list),	

%% 	file:write_file(File, "			_ -> Player\t\n",[append]),
%% 	file:write_file(File, "		end,\t\n",[append]),	
%% 	file:write_file(File, "	set_player_info_fields(NewPlayer, T).\t\n",[append]),
%% 	file:write_file(File, " \t\n",[append]),
	
%% 	P_list_2 = P_list,
%% 	%% P_list_2_bak = [
%% %% %% 				id,
%% %% 				scene,
%% %% 				arena,
%% %% 				hp,
%% %% 				hp_lim,
%% %% 				mp,
%% %% 				mp_lim,
%% %% 				x,
%% %% 				y,
%% %% 				max_attack,
%% %% 				min_attack,
%% %%  				def,
%% %% 				hit,
%% %% 				dodge,
%% %% 				crit,
%% %% 				lv,
%% %% 				realm,
%% %% 				career,
%% %% 				guild_id,
%% %% 				status,
%% %% 				evil,
%% %% 				pk_mode,
%% %% 				anti_wind,
%% %% 				anti_water,
%% %% 				anti_thunder,
%% %% 				anti_fire,
%% %% 				anti_soil,
%% %% 				speed,
%% %% 				guild_name,
%% %% 				carry_mark,
%% %% 				task_convoy_npc,
%% %% 				vip,
%% %% 				prestige,
%% %% 				physical_strength ,
%% %% 				physical_strength_lim ,	
%% %% 				experience,	
%% %% 				genius ,
%% %% 				soul_force ,
%% %% 				attack ,
%% %% 				magic_attack ,	
%% %% 				magic_def ,
%% %% 				stunt_attack ,	
%% %% 				stunt_def ,
%% %% 				parry ,
%% %% 				counter ,
%% %% 				morale ,
%% %% 				five_phases ,	
%% %% 				force,
%% %% 				magic ,	
%% %% 				stunt ,	
%% %% 				trained_force ,	
%% %% 				trained_magic ,	
%% %% 				trained_stunt ,	
%% %% 				team_num ,
%% %% 				max_team_num 				
%% 				%% ],
%% 	O_list_2 = O_list,
%% 	%% O_list_2_bak = [
%% 	%% 			battle_limit,
%% 	%% 			leader,
%%     %%         	battle_status,
%%     %%         	pid_team,
%% 	%% 			pid_dungeon,
%% 	%% 			pid_scene,
%% 	%% 			equip_current,
%% 	%% 			out_pet,
%% 	%% 			stren,
%% 	%% 			suitid		
%% 	%% 			],	
	
%% 	file:write_file(File, "%%获取用户信息改变值对\t\n",[append]),
%% 	file:write_file(File, "get_player_info_changed(Player, NewPlayer) ->\t\n",[append]),
%% 	file:write_file(File, "	List=[\t\n",[append]), 
%% 	lists:foreach(fun(Field_name) ->
%% 					Bytes00 = lists:concat(["			",Field_name,",\t\n"]),
%% 					file:write_file(File, Bytes00,[append])			  
%% 				  end, 
%% 				  P_list_2 -- [other]),
%% 	lists:foreach(fun(Field_name) ->
%% 					Bytes00 = lists:concat(["			",Field_name,",\t\n"]),
%% 					file:write_file(File, Bytes00,[append])			  
%% 				  end, 
%% 				  O_list_2),
%% 	file:write_file(File, "			undefined],\t\n",[append]),
%% 	file:write_file(File, "	Ret = lists:map(fun(T) ->\t\n",[append]), 
%% 	file:write_file(File, "			case T of\t\n",[append]),
%% 	lists:foreach(fun(Field_name) ->
%% 					Bytes00 = lists:concat(["				",Field_name," -> if Player#player.",Field_name," =/= NewPlayer#player.",Field_name," ->{",Field_name,",NewPlayer#player.",Field_name,"}; true -> [] end;\t\n"]),
%% 					file:write_file(File, Bytes00,[append])			  
%% 				  end, 
%% 				  P_list_2 -- [other]),
%% 	lists:foreach(fun(Field_name) ->
%% 					Bytes00 = if Field_name =/= out_pet ->
%% 									 lists:concat(["				",Field_name," -> if Player#player.other#player_other.",Field_name," =/= NewPlayer#player.other#player_other.",Field_name," ->{",Field_name,",NewPlayer#player.other#player_other.",Field_name,"}; true -> [] end;\t\n"]);
%% 								 true ->
%% 									 lists:concat(["				",Field_name," -> Out_pet1 = Player#player.other#player_other.",Field_name,",Out_pet2 = NewPlayer#player.other#player_other.",Field_name,",
%% 							  			if is_record(Out_pet1, ets_pet) andalso is_record(Out_pet2, ets_pet) -> 
%% 												Out_pet10 = Out_pet1#ets_pet{time =0},
%% 												Out_pet20 = Out_pet2#ets_pet{time =0}, 
%% 												if Out_pet10 =/= Out_pet20 ->
%% 													{out_pet, Out_pet2}; 
%% 												true ->
%% 													[]
%% 												end; 
%% 								 			true ->
%% 												if Out_pet1 =/= Out_pet2 ->
%% 													{out_pet, Out_pet2}; 
%% 												true ->
%% 													[]
%% 												end
%% 										 end;"
%% 										,"\t\n"])
%% 							end,
%% 					file:write_file(File, Bytes00,[append])			  
%% 				  end, 
%% 				  O_list_2),	
%% 	file:write_file(File, "				_ -> []\t\n",[append]),
%% 	file:write_file(File, "			end\t\n",[append]),						
%% 	file:write_file(File, "		end, List),\t\n",[append]),
%% 	file:write_file(File, "		lists:filter(fun(T)-> T =/= [] end, Ret).\t\n",[append]),
%% 	file:write_file(File, " \t\n",[append]),
	
%% 	io:format("~nEnd create ../src/lib/lib_player_rw.erl!~n"),
%% 	ok.

%% %% 取出查询结果中的第一行
%% get_row(Pool_id, Sql) ->
%% 	case mysql:fetch(Pool_id, Sql) of
%% 		{data, {_, _, [], _, _}} -> [];
%% 		{data, {_, _, [R], _, _}} -> R;
%% 		{error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason]);
%% 		_ -> mysql_halt([Sql, "get_row error"])
%% 	end.

%% %% 取出查询结果中的所有行
%% get_all(Pool_id, Sql) ->
%%     case mysql:fetch(mysql_dispatcher, Pool_id, Sql, undefined) of
%%         {data, {_, _, R, _, _}} -> R;
%%         {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
%%     end.

%% %% @doc 显示人可以看得懂的错误信息
%% mysql_halt([Sql, Reason]) ->
%% %% 	io:format("mysql_halt__/~p/ ~n",[Reason]),
%%     catch erlang:error({db_error, [Sql, Reason]}).

%% get_tables_name() ->
%% 	put(tables, []),
%% 	put(tableds, []),
%% 	put(dbnames, []),
%% 	put(maxColumnLen, 0),
%% 	lists:map(
%% 		fun(Db) ->
%% 			case  get_db_config(?CONFIG_FILE, Db) of
%% 				[_Type, _Host, _Port, _User, _Password, DB, _Poolsize, _Encode] ->	
%% 					try 
%% 						Sql0 = lists:concat(["SELECT max(length(column_name)) FROM information_schema.columns WHERE table_schema= '", misc:to_list(DB), "'"]), 
%% 						MaxColumnLen = 	
%% 							case  get_all(Db, list_to_binary(Sql0)) of
%% 								[] -> 30;
%% 								[[Ml]] -> Ml+5
%% 							end,	
%% 						case MaxColumnLen > get(maxColumnLen) of
%% 							true -> put(maxColumnLen, MaxColumnLen);
%% 							_-> no_action
%% 						end,
%% 						Sql = lists:concat(["SELECT table_name, table_comment FROM information_schema.tables WHERE table_schema='", misc:to_list(DB), "' and table_type ='BASE TABLE'"]),
%% 						case  get_all(Db, list_to_binary(Sql)) of 
%% 							[] ->
%% %% 								io:format("Error(~p)_1  ~n",[Db]),
%% 								[];
%% 							R ->
%% %% io:format("HH__/~p/~n",[R]),							
%% 								lists:foreach(fun([T, C]) ->
%% 										Tableds = get(tableds),
%% 										case lists:member(T, Tableds) of
%% 										   false ->
%% %% 								io:format("Here__~p ~n",[T]),			   
%% 											   put(tables, get(tables) ++ [{Db, DB, T, C}]),
%% 											   put(tableds, Tableds ++ [T]),
%% 											   Dbnames = get(dbnames),
%% 											   case lists:member(DB, Dbnames) of
%% 												   false ->
%% 													   put(dbnames, Dbnames ++ [DB]);
%% 												   _-> no_action
%% 											   end,
%% 											   ok;
%% 											_ -> no_action
%% 										end
%% 									end, 
%% 									R),
%% %% 								io:format("/~p//////~p/~n",[Db, R]),
%% 								ok
%% 						end
%% 					catch
%% 						_:_ -> []
%% 					end;
%% 				_->	[]
%% 			end
%% 		end, 
%% 	?DB_LIST),
%% %% 	io:format("Result: /~p/~p/ ~n", [get(tables), get(maxColumnLen)]),
%% 	{get(tables), get(maxColumnLen)}.
	
%% %%  根据表名获取其完全字段
%% table_fields_all(Tables_name)->
%% 	io:format("~n~nBegin create ../src/db/db_mongo_fields.erl and ./temp/Table_field_short.php!~n"),
	
%% 	File = "../src/db/db_mongo_fields.erl",
%% 	Filephp = "../temp/Table_field_short.php",

%% 	lists:foreach(fun(DB_name) ->
%% 			FileClear = "../temp/" ++ DB_name ++ "_table_clear.sql",
%% 			file:write_file(FileClear, "")
%% 			end,
%% 		get(dbnames)),
	
%% 	file:write_file(Filephp, ""),
%% 	file:write_file(Filephp, ""),
%% 	file:write_file(Filephp, "<?php ",[append]),
%% 	file:write_file(Filephp, "\t\nclass Table_field_short {",[append]),
%% 	file:write_file(Filephp, "static $table_fields = array(\t\n",[append]),
	
%% 	file:write_file(File, ""),	
%% 	file:write_file(File, ""),
%% 	file:write_file(File, "%%%------------------------------------------------\t\n",[append]),
%% 	file:write_file(File, "%%% File    : db_mongo_fields.erl\t\n",[append]),
%% 	%% Bytes0 = list_to_binary(io_lib:format("%%% Created : ~s\t\n", [time_format(now())])),
%% 	%% file:write_file(File, Bytes0,[append]),
%% 	file:write_file(File, "%%% Description: 从mysql生成的代码\t\n",[append]),
%% 	file:write_file(File, "%%% Warning:  由程序自动生成，请不要随意修改！\t\n",[append]),	
%% 	file:write_file(File, "%%%------------------------------------------------	\t\n",[append]),
%% 	file:write_file(File, "-module(db_mongo_fields).\t\n",[append]),
%% 	file:write_file(File, "-compile(export_all).\t\n",[append]),	
%% 	file:write_file(File, " \t\n",[append]),
	
%% 	try 
%% 		case  Tables_name of 
%% 			[] -> error1;
%% 			{A, MaxColumnLen} ->   %% {db_center, xxm_game, <<"td_multi">>}
%% 				file:write_file(File, 
%% 					list_to_binary(io_lib:format("\t\n%% 根据表名获取其完全字段\t\n",[])), 
%% 					[append]),
				
%% 				file:write_file(File, 
%% 					list_to_binary(io_lib:format("get_table_fields(Table_name) ->\t\n",[])), 
%% 					[append]),
				
%% 				file:write_file(File, 
%% 					list_to_binary(io_lib:format("	Table_fileds = [ \t\n",[])), 
%% 					[append]),
				
%% 				F = fun({Pool_id, DB_name, T, Table_Comment}) ->
%% %% io:format("~p~n",[T]),	
%% 						Index = string:str(binary_to_list(T), "base_"),
%% 						if
%% 							Index =:= 1 orelse T =:= <<"shop">> ->
%% 								skip;
%% 					   		T =:= <<"new_player_card">> ->
%% 								FileClear = "../temp/" ++ DB_name ++ "_table_clear.sql",
%% 								file:write_file(FileClear, 
%% 										list_to_binary(io_lib:format("update ~s set player_id=0, is_used=0, use_time=0;	/*~s*/\t\n",[misc:to_list(T), Table_Comment])),
%% 										[append]);
%% 							true ->
%% 							FileClear = "../temp/" ++ DB_name ++ "_table_clear.sql",
%% 							file:write_file(FileClear, 
%% 								list_to_binary(io_lib:format("delete from ~s;           /*~s*/\t\n",[misc:to_list(T), Table_Comment])), 
%% 								[append])
%% 						end,

%% 						Sql1 = lists:concat(["SELECT column_name, data_type, column_default, column_comment, extra FROM information_schema.columns WHERE table_schema= '", misc:to_list(DB_name), "' AND table_name= '", misc:to_list(T),  "'"]),
%% 						case  get_all(Pool_id, list_to_binary(Sql1)) of
%% 							[] -> error2;
%% 							B -> 
%% 							  {DL, _} =
%% 								lists:mapfoldl(fun([Field, Data_type0, Default0, Comment0, Extra0], Sum) -> 
%% 											Data_type =  misc:to_atom(Data_type0),
%% 											Default = 
%% 												case Default0 of
%% 													undefined -> 
%% 														case erlydb_field:get_erl_type(Data_type) of
%% 															binary -> 
%% 																"";
%% 															integer -> 
%% 																0;
%% 															_ -> 0 
%% 														end;													
%% 													<<>> -> 
%% 														case erlydb_field:get_erl_type(Data_type) of
%% 															binary -> 
%% 																"";
%% 															integer -> 
%% 																0;
%% 															_ -> "" 
%% 														end;
%% 													<<"[]">> ->
%% 															[];
%% 													Val -> 
%% 														case erlydb_field:get_erl_type(Data_type) of
%% 															binary -> 
%% 																lists:concat(["", binary_to_list(Val) ,""]);
%% 															integer -> 
%% 																misc:to_integer(binary_to_list(Val));	
%% 															decimal ->
%% 																misc:to_float(binary_to_list(Val));
%% 															_ -> 
%% 																lists:concat([binary_to_list(Val)])
%% 														end																				
%% 												end,	
%% %% TT = misc:to_atom(T),											
%% %% if TT == player ->
%% %% 	io:format("1___/~p/~p/~p/~p/~p/~p/ ~n", [misc:to_list(T), misc:to_atom(Field), Data_type, erlydb_field:get_erl_type(Data_type), Default0, Default]);
%% %%    true ->
%% %% 	   ok
%% %% end,											
%% 											Shortfield = if Sum < 26 ->
%% 																 misc:to_atom([Sum+97]);
%% 															true ->
%% 																 misc:to_atom(list_to_binary(io_lib:format("z~p", [Sum-25])))
%% 														end,
%% 											S1 = if Sum+1 == length(B) -> 
%% 													io_lib:format("{~s, ~p, \"~p\"}",[misc:to_atom(Field), Default, Shortfield]);
%% 												true -> 
%% 													io_lib:format("{~s, ~p, \"~p\"},",[misc:to_atom(Field), Default, Shortfield])
%% 										 	end,
%% 											TTT = lists:concat([misc:to_atom(Field), "", Sum]),
%% 											S2 = io_lib:format("~s\"~s\" => \"~p\", ~s/* ~s */\t\n",[lists:duplicate(7, "\t"), misc:to_atom(Field), Shortfield, lists:duplicate(MaxColumnLen-length(TTT), " "), Comment0]),
%% 											S3 = io_lib:format("~s\"~p\" => \"~s\", ~s/* ~s */\t\n",[lists:duplicate(7, "\t"), Shortfield, misc:to_atom(Field), lists:duplicate(MaxColumnLen-length(TTT), " "), Comment0]),
%% 											S4 = case Extra0 of
%% 														<<"auto_increment">> -> [Field, Shortfield];
%% 														_ -> []
%% 													end,											
%% 											{[S1, S2, S3, S4], Sum+1}
%% 											end, 
%% 										 0,B), 
%% 								Left5 = string:left(binary_to_list(T),5),
%% 								FieldShortMark = if Left5 == "admin" orelse Left5 == "base_" ->
%% 													0;
%% 												 true -> 1
%% 											  end,
							  
%% 							    DL1 = lists:map(fun([S1, _S2, _S3, _S4])-> S1 end, DL),
%% 								E = io_lib:format('{~s, [~p, [~s]]}', [misc:to_atom(T), FieldShortMark, lists:flatten(DL1)]),
%% 								file:write_file(File, 
%% 										list_to_binary(io_lib:format("		~s,\t\n",[E])), 
%% 										[append]),	
%% 								DL2 = lists:map(fun([_S1, S2, _S3, _S4])-> S2 end, DL),
%% 								DL3 = lists:map(fun([_S1, _S2, S3, _S4])-> S3 end, DL),
%% 							  	DL4 = lists:map(fun([_S1, _S2, _S3, S4])-> S4 end, DL),
%% 							  	DL4_1 = lists:filter(fun(DD)-> DD =/= [] end, DL4),
%% %% 					io:format("Here_1_~p____~p___~p__~n",[T, DL4_1, 0]),								  
%% 							  	AutoIncField = 
%% 							  		case DL4_1 of
%% 										[[AF, _SF]] -> lists:flatten(io_lib:format("~s",[AF]));
%% 										_ -> null
%% 									end,
%% %% 					io:format("Here_2_~p____~p___~p__~n",[T, DL4_1, AutoIncField]),		  
%% 								E1 = io_lib:format('"~s" => array(\t\n\t\t\t\t"FieldShortMark" => ~p, \t\n\t\t\t\t"AutoIncField" => ~p, \t\n~s"FieldShortList" => array(\t\n~s~s), \t\n~s"FieldLongList" =>array(\t\n~s~s)\t\n~s)', 
%% 												   [misc:to_atom(T), FieldShortMark, AutoIncField, lists:duplicate(4, "\t"), lists:flatten(DL2), lists:duplicate(7, "\t"), lists:duplicate(4, "\t"), lists:flatten(DL3),lists:duplicate(7, "\t"), lists:duplicate(4, "\t")]),
%% 								file:write_file(Filephp, 
%% 										list_to_binary(io_lib:format("		~s,\t\n",[E1])), 
%% 										[append]),									
%% 								ok
%% 						end
%% 					end,
%% 				[F(T0) || T0 <- A],
%% 				file:write_file(File, 
%% 					list_to_binary(io_lib:format('		{null, [0, []]}, \t\n',[])), 
%% 					[append]),	
%% 				file:write_file(File, 
%% 					list_to_binary(io_lib:format('		{auto_ids, [0, []]}], \t\n',[])), 
%% 					[append]),	
				
%% 				file:write_file(File, 
%% 					list_to_binary(io_lib:format('	case lists:keysearch(Table_name,1, Table_fileds) of \t\n',[])), 
%% 					[append]),	
%% 				file:write_file(File, 
%% 					list_to_binary(io_lib:format('		{value,{_, Val}} -> Val; \t\n',[])), 
%% 					[append]),	
%% 				file:write_file(File, 
%% 					list_to_binary(io_lib:format('		_ -> undefined \t\n',[])), 
%% 					[append]),	
%% 				file:write_file(File, 
%% 					list_to_binary(io_lib:format('	end. \t\n',[])), 
%% 					[append]),	
				
%% 				file:write_file(Filephp, "\t\t);}\t\n",[append]),
%% 				io:format("End create ../src/db/db_mongo_fields.erl and ./temp/Table_field_short.php!~n"),
%% 				ok
%% 		end
%% 	catch
%% 		_:_ -> io:format("Error create ../src/db/db_mongo_fields.erl and ./temp/Table_field_short.php!~n~n"),
%% 			fail
%% 	end.

%% %%生成所有的表
%% get_all_tables(Tables_name) ->
%% 	io:format("~n~nBegin create get_all_tables() in lib_player_rw.erl!~n"),
	
%% 	Filename = "../src/lib/lib_player_rw.erl",
%% 	try 
%% 		case  Tables_name of 
%% 			[] -> error1;
%% 			{A, _MaxColumnLen} ->   %% {db_center, xxm_game, <<"td_multi">>}
%% 				file:write_file(Filename, 
%% 					list_to_binary(io_lib:format("\t\n\t\n%% 获取所有表名\t\n",[])), 
%% 					[append]),
				
%% 				file:write_file(Filename, 
%% 					list_to_binary(io_lib:format("get_all_tables() ->\t\n",[])), 
%% 					[append]),
				
%% 				file:write_file(Filename, 
%% 					list_to_binary(io_lib:format("	[ \t\n",[])), 
%% 					[append]),
				
%% 				F = fun({_Pool_id, _DB_name, T, _Table_Comment}) ->
%% 							file:write_file(Filename, 
%% 										list_to_binary(io_lib:format("		~s,\t\n",[misc:to_atom(T)])), 
%% 										[append])
%% 					end,
%% 				[F(T0) || T0 <- A],
%% 				file:write_file(Filename, 
%% 					list_to_binary(io_lib:format('		null \t\n',[])), 
%% 					[append]),				
%% 				file:write_file(Filename,
%% 					list_to_binary(io_lib:format("	]. \t\n",[])), 
%% 				    [append]),
%% 				io:format("End create get_all_tables() in lib_player_rw.erl!~n~n"),
%% 				ok
%% 		end
%% 	catch
%% 		_:_ ->
%% 			io:format("Error create get_all_tables() in lib_player_rw.erl!~n~n"),
%% 			fail
%% 	end.
			
%% %% --------------------------------------------------
%% %% time format
%% one_to_two(One) -> io_lib:format("~2..0B", [One]).

%% %% @doc get the time's seconds for integer type
%% %% @spec get_seconds(Time) -> integer() 
%% get_seconds(Time)->
%% 	{_MegaSecs, Secs, _MicroSecs} = Time, 
%% 	Secs.
	
%% time_format(Now) -> 
%% 	{{Y,M,D},{H,MM,S}} = calendar:now_to_local_time(Now),
%% 	lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D), " ", 
%% 						one_to_two(H) , ":", one_to_two(MM), ":", one_to_two(S)]).
%% date_format(Now) ->
%% 	{{Y,M,D},{_H,_MM,_S}} = calendar:now_to_local_time(Now),
%% 	lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D)]).
%% date_hour_format(Now) ->
%% 	{{Y,M,D},{H,_MM,_S}} = calendar:now_to_local_time(Now),
%% 	lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D), " ", one_to_two(H)]).
%% date_hour_minute_format(Now) ->
%% 	{{Y,M,D},{H,MM,_S}} = calendar:now_to_local_time(Now),
%% 	lists:concat([Y, "-", one_to_two(M), "-", one_to_two(D), " ", one_to_two(H) , "-", one_to_two(MM)]).
%% %% split by -
%% minute_second_format(Now) ->
%% 	{{_Y,_M,_D},{H,MM,_S}} = calendar:now_to_local_time(Now),
%% 	lists:concat([one_to_two(H) , "-", one_to_two(MM)]).

%% hour_minute_second_format(Now) ->
%% 	{{_Y,_M,_D},{H,MM,S}} = calendar:now_to_local_time(Now),
%% 	lists:concat([one_to_two(H) , ":", one_to_two(MM), ":", one_to_two(S)]).

