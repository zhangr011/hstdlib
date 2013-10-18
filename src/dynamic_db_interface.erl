%% Author: T400
%% Created: 2011-7-29
%% Description: TODO: Add description to dynamic_db_interface
-module(dynamic_db_interface).

-include_lib("leshulib/include/define_logger.hrl").
-include_lib("leshulib/include/define_database.hrl").

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
         start/0
        ]).

%%
%% API Functions
%%
start() ->
    lists:foreach(
      fun(Db) ->
              [ModName, Src] = db_src(Db),
              %%io:format("db_1__/~p/~p/\t\n~s/ ~n",[Db, ModName,Src]),
              dynamic_module(ModName, Src, Db)
      end,
      ?DB_LIST).

dynamic_module(ModName, Src, Db) ->
    try
        %% io:format("Src: ~ts~n", [Src]),
        {Mod, Code} = dynamic_compile:from_string(Src),
        %% io:format("db_2__/~ts/ ~n",[Src]),		
        code:load_binary(Mod, ModName ++ ".erl", Code),
        io:format("~n    dynamic_db_interface[~p] OK!", [Db])
    catch
        Type:Error -> io:format("Error compiling ~p (~p): ~p stack:~p~n", 
                                [ModName, Type, Error,erlang:get_stacktrace()])
    end.

db_src(Db)->
    Db_1 = lists:concat([Db]),
    Funs = get_funs(Db),
    [Db_1 ++ "_interface",
     "-module(" ++ Db_1 ++ ").\t\n"      
     ++ "-compile(export_all).\t\n" 
     ++ Funs ++ "\t\n"
    ].	

get_stat_db_access(Operation) ->
    case config:get_stat_db() of 
        1 -> lists:concat(["    misc:stat_db_access(P1, ", Operation, "), "]);
        _ -> ""
    end.

get_try_begin() ->
    "try \t\n begin \t\n".

get_try_end(DbName) ->
    "\t\n end \t\n catch _:R -> \t\n logger:error_msg("++DbName++",?LINE,\"" ++ DbName++" error R:~w stack:~p~n\",[R, erlang:get_stacktrace()]),\n\terror_db \t\n end".

get_parms(Pn) ->
    case Pn =< 0 of
        true -> " ";
        _ ->
            List = lists:seq(1, Pn),
            {RL, _} =
                lists:mapfoldl(fun(N, Sum) ->
                                       R =	if Sum < Pn ->			  
                                                    lists:concat(["P", N, ", "]);
                                               true ->
                                                    lists:concat(["P", N])
                                            end,
                                       {R,	Sum+1}				   
                               end, 
                               1, List),
            lists:concat(RL)
    end.
	
get_funs(Db) ->
    DbName = lists:concat([Db]),
    Db_1 = "db_agent:get_db_srv_pid("++ tool:to_list(Db) ++")",
    Db_type = lists:concat([(config:get_db_type(Db))]),
    Config_funs = [
                   {insert, 4},
                   {insert, 3},
                   {insert_or_update, 3},
                   {insert_or_update, 4},
                   {replace, 2},
                   {update, 3},
                   {update, 5},
                   {select_one, 3},
                   {select_one, 5},
                   {select_row, 3},
                   {select_row, 5},
                   {select_count, 2},
                   {select_all, 3},
                   {select_all, 5},
                   {delete, 2},
                   {delete, 4},
                   {transaction, 1},

                   {findAndModify, 3},
                   {select_one, 2},
                   {select_one, 4},	
                   {select_row, 2},
                   {select_row, 4},	
                   {select_all, 2},
                   {select_all, 4},
                   {select_one_from_uniontable, 7},
                   {select_all_from_multtable, 5},
                   {select_row_from_multtable, 5},
                   {column_rename, 4},
                   {column_remove, 3},
                   {coin_sum_process, 3},
                   {gold_sum_process, 3},
                   {sum, 3}							 
                  ],
    Funs = lists:map( 
             fun(Cfg) ->
                     try
                         {F, PN, End} =
                             case Cfg of
                                 {F0, PN0} -> {F0, PN0, "."};
                                 {F0, PN0, End0} -> {F0, PN0, End0}
                             end,
                         F1 = lists:concat([F]),
                         [Parms1, Parms2] = 
                             case PN =< 0 of
                                 true ->
                                     [" ", " "];
                                 _ ->
                                     [get_parms(PN), ", " ++ get_parms(PN)]
                             end,
                         F1 ++ "(" ++ Parms1 ++ ") -> \t\n"
                             ++ get_try_begin()
                             ++ get_stat_db_access(F) ++ "\t\n"
                             ++ "    " ++ Db_type ++ ":"
                             ++ F1 ++ "(" ++ Db_1 ++ Parms2 ++ ")"
                             ++ get_try_end(DbName)                             
                             ++ End ++ "\t\n"
                     catch
                         _:_ -> io:format("Error____/~p/~n",[Cfg])
                     end
             end,
             Config_funs),	
	lists:concat(Funs).

