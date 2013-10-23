%%%--------------------------------------
%%% @Module  : generate_info_header_file
%%% @Created : 2013.01.05
%%% @Description: 将 base_error_list 数据表生成头文件
%%%               生成文件："../include/table_to_record.hrl"
%%%--------------------------------------

-module(hgenerate_info_header_file).

-define(CONFIG_FILE, "../config/server.app").
-define(HEADER_PATH, "../include/").
-define(INFO_RANGE, 1000).

-include("define_logger.hrl").

-export([start/0]).

%% @doc 开始生成文件
%% @spec
%% @end
start() ->
    %% 动态编译application的config配置
    %% dynamic_config:start(),
    %% 动态生成数据库访问接口
    %% dynamic_db_interface:start(),
    %% 初始化数据库
	%% db_agent:init_db(),
    case mysql_util:init_db(?CONFIG_FILE) of
        ok ->
            io:format("Begin generate infos ...~n"),
            generate_info_header_file(),
            io:format("Generate infos OK!~n"),
            ok;
        _ ->
            io:format("Generate info failed: connect mysql failed!~n"),
            db_config_fail
    end,
    halt(), 
    ok.


%% @doc 生成对应的头文件
%% @spec
%% @end
generate_info_header_file() ->
    InfoList = db_mysql:select_all(db_base, base_error_list, "*", []),
    %% io:format("DbInfoList: ~w~n", [InfoList]),
    %% 生成一个 1 - 100 的序列
    Sequence = lists:seq(0, 100),
    lists:map(fun(Key) ->
                      %% 提取符合条件的数据记录
                      case lists:filter(fun([ErrorCode, _, _]) ->
                                                if 
                                                    (ErrorCode div ?INFO_RANGE)
                                                    =:= Key ->
                                                        %% 分离出指定的宏定义段
                                                        true;
                                                    true ->
                                                        false
                                                end
                                        end, InfoList) of
                          [] ->
                              %% 空的数据直接跳过
                              ok;
                          FilteredInfoList ->
                              FileName = 
                                  io_lib:format("define_info_~w.hrl", [Key]),
                              io:format("Generate ~s started ...~n", [FileName]),
                              generate_info_header_file(
                                FileName, FilteredInfoList),
                              io:format("Generate ~s end.~n~n", [FileName])
                      end
              end, Sequence).


%% @doc 根据文件名和内容生成对应的信息
%% @spec
%% @end
generate_info_header_file(FileName, InfoList) ->
    Out = ?HEADER_PATH ++ FileName,
    UpperFileName = inner_normalize_header_file_macro(FileName),
    WriteList0 = 
        ["\n",
         "%%%------------------------------------------------\n",
         list_to_binary(io_lib:format("%%% File : ~s\n", [FileName])),
         list_to_binary(io_lib:format("%%% Generate by code\n", [])),
         "%%% Description: 从 base_error_list 表生成的 hrl 信息。\n",
         "%%% Warning: 由程序自动生成，请不要随意修改！\n",
         "%%%------------------------------------------------\n",
         "\n",
         list_to_binary(io_lib:format("-ifndef(~s).\n-define(~s, true).\n\n",
                                      [UpperFileName, UpperFileName])),
         list_to_binary(io_lib:format("-include(\"define_info_0.hrl\").\n\n\n", []))],

    %% 生成宏定义的内容
    WriteList1 =
        lists:foldl(fun([ErrorCode, ErrorDefine, ErrorDesc], In) ->
                            ErrorString = io_lib:format("~w).", [ErrorCode]),
                            DefineInfo = 
                                io_lib:format("-define(~-40s~-10s%% ~s\n",
                                              [bitstring_to_list(ErrorDefine) ++ ",",
                                               ErrorString,
                                               bitstring_to_list(ErrorDesc)]),
                            In ++ [list_to_binary(DefineInfo)]
                    end, WriteList0, InfoList),
    
    file_tools:writelines_new(Out, WriteList1 ++ ["\n\n-endif.\n\n"]),
    ok.

%% 标准化头文件宏定义名
inner_normalize_header_file_macro(FileName) ->
    NoDotFileName = lists:map(fun(Char) ->
                                      case Char of
                                          $. ->
                                              %% .替换成_
                                              $_;
                                          NonDot ->
                                              NonDot
                                      end
                              end, FileName),
    string:to_upper(NoDotFileName).


