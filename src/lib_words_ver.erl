%% Author: lzz
%% Created: 2010-11-29
%% Description: 敏感词处理
-module(lib_words_ver).

%%
%% Include files
%%
-include("define_logger.hrl").

-export([words_ver/1, 
         words_filter/1, 
         words_ver_name/1,
         get_chinese_string/1, 
         find_data_words_in_string/1, 
         handle_brackets/1,
         update_data_words/0,
         init_data_words/0, 
         %% words_filter_test/1, 
         utf8_len/1,
         new_combine/1]).


%%汉字unicode编码范围 0x4e00 - 0x9fa5
-define(UNICODE_CHINESE_BEGIN, (4*16*16*16 + 14*16*16)).
-define(UNICODE_CHINESE_END,   (9*16*16*16 + 15*16*16 + 10*16 + 5)).

%%
%% API Functions
%%
%% -----------------------------------------------------------------
%% 敏感词处理
%% -----------------------------------------------------------------
%% 敏感词过滤替换
%% words_filter(Words_for_filter) -> 
%% 	Words_List = data_words:get_words_verlist(),
%% 	binary:bin_to_list(lists:foldl(fun(Kword, Words_for_filter0)->
%% 										   re:replace(Words_for_filter0,Kword,"*",[global,caseless,{return, binary}])
%% 								   end,
%% 								   Words_for_filter,Words_List)).

%% words_filter(Words_for_filter) -> 
%%     Words_List = data_words:get_words_verlist(),
%%     Words_for_filter_chinese = [get_chinese_string(Words_for_filter)],
%%     
%% 
%%     case words_ver(Words_for_filter_chinese) of
%%         false->
%%              binary:bin_to_list(lists:foldl(fun(Kword, Words_for_filter0)->
%%                                            re:replace(Words_for_filter0,Kword,"*",[global,caseless,{return, binary}])
%%                                    end,
%%                                    Words_for_filter_chinese,Words_List));
%%         true->
%%             binary:bin_to_list(lists:foldl(fun(Kword, Words_for_filter0)->
%%                                            re:replace(Words_for_filter0,Kword,"*",[global,caseless,{return, binary}])
%%                                    end,
%%                                    Words_for_filter,Words_List))
%%     end.
%% 
%% 
%% 
%%     
%% %% 敏感词检查
%% words_ver(Words_for_ver) ->
%% 	Words_List = data_words:get_words_verlist(),
%% 	words_ver_i(erlang:length(Words_List), Words_List, Words_for_ver).
%% 	
%% words_ver_i(1, Words_List, Words_for_ver) -> 
%%     case re:run(Words_for_ver, lists:nth(1, Words_List), [caseless]) of
%%     	nomatch -> true;
%%     	_-> false
%%     end; 
%% words_ver_i(N, Words_List, Words_for_ver) ->
%%     case re:run(Words_for_ver, lists:nth(N, Words_List), [caseless]) of
%%     	nomatch -> words_ver_i(N-1, Words_List, Words_for_ver);
%%     	_-> false
%%     end.
%% 
%% %% 角色名敏感词检查
%% words_ver_name(Words_for_ver) ->
%% 	Words_List = data_words:get_words_verlist_name(),
%% 	words_ver_i_name(erlang:length(Words_List), Words_List, Words_for_ver).
%% 	
%% words_ver_i_name(1, Words_List, Words_for_ver) -> 
%%     case re:run(Words_for_ver, lists:nth(1, Words_List), [caseless]) of
%%     	nomatch -> true;
%%     	_-> false
%%     end; 
%% words_ver_i_name(N, Words_List, Words_for_ver) ->
%%     case re:run(Words_for_ver, lists:nth(N, Words_List), [caseless]) of
%%     	nomatch -> words_ver_i_name(N-1, Words_List, Words_for_ver);
%%     	_-> false
%%     end.




%% %% 敏感词过滤替换 最新版本 速度较快
%% words_filter(Words_for_filter)->
%%     
%%     Words_List = data_words:get_words_verlist(),
%%     lists:foreach( fun(E)->
%%                        put(E,1) end ,Words_List) ,
%%     
%%     UnicodeList = unicode:characters_to_list(list_to_binary(Words_for_filter)),
%%     
%%     Words_List_1 = new_combine(UnicodeList),
%%     
%%     Words_List_2 = lists:map(fun(E) ->
%%                                      E_1 = tool:to_list(unicode:characters_to_binary(E,utf8)),
%%                                      E_2 = get_chinese_string(E_1),
%%                                      
%%                                      case get(E_2) of
%%                                          undefined->
%%                                              [];
%%                                          1->
%%                                              E_1
%%                                      end
%%                              end, Words_List_1),
%%     Words_List_3 = lists:filter(fun(E) ->
%%                                         E =/= [] end
%%                                ,Words_List_2),
%%     
%%         case Words_List_3 of
%%             [] ->
%%                 Words_for_filter;
%%             
%%              _ ->
%%                 binary:bin_to_list(lists:foldl(fun(Kword, Words_for_filter0)->
%%                                         re:replace(Words_for_filter0,Kword,"*",[global,caseless,{return, binary}])
%%                                 end,
%%                                 Words_for_filter,Words_List_3))
%%         end.


%% 敏感词过滤替换 最新版本 速度较快
words_filter(Words_for_filter)->
    ?DEBUG("Words_for_filter ~w~n", [Words_for_filter]),
    try
        Words_for_filter_new = Words_for_filter,%% lists:map(fun(E) ->
                                         %%         handle_brackets(E) 
                                         %% end, Words_for_filter),
        case find_data_words_in_string([Words_for_filter_new]) of
            [] ->
                %% case find_data_words_in_string([Words_for_filter]) of
                %%     [] ->
                %%         Words_for_filter;
                %%     List ->
                %%         words_filter(Words_for_filter,List)
                %% end;
                Words_for_filter;
            List->
                ?DEBUG("Ver List ~w~n", [List]),
                words_filter(Words_for_filter_new,List)
        end
    catch 
        _:Reason->
            ?ERROR_MSG("~n~p/~p:~p",[?MODULE,?LINE,Reason]),
            ?ERROR_MSG("Stack ~p~n", [erlang:get_stacktrace()]),
            data_sys_msg:get(sys_error)
    end.
    
words_filter(Words_for_filter, List)->
    
    Result0 = 
        binary:bin_to_list(
          lists:foldl(fun(Kword, Words_for_filter0)->
                              NewKWord = hmisc:re_escape(Kword),
                              ?DEBUG("Words_for_filter0 ~w,Kword ~w ~n", [Words_for_filter0, NewKWord]),
                              re:replace(Words_for_filter0, NewKWord, "*", 
                                         [global,caseless,{return, binary}])
                      end, Words_for_filter, List)),
	?DEBUG(" Result0 = ~p",[Result0]),
    %%Result_1 = [lists:filter(fun(E)-> E =/= 42 end, Result0)] ,
    lists:flatten(Result0).
    %% case words_ver(Result_1 ) of
    %%     true->
    %%         [lists:flatten(Result0)];
    %%     false->
    %%         words_filter(Result_1)
    %% end.

%% %% 敏感词检查
words_ver(Words_for_ver) ->
    try
        case find_data_words_in_string(Words_for_ver) of
            []->
                true;
            _List->
                false
        end
    catch 
        _:Reason->
            ?ERROR_MSG("~n~p/~p:~p",[?MODULE,?LINE,Reason])
    end.

%% 角色名敏感词检查
words_ver_name(Words_for_ver) ->
    try
        case lib_words_ver:find_data_words_in_string(Words_for_ver) of
            []->
                true;
            _List->
                false
        end
    catch
        _:Reason ->
            ?ERROR_MSG("~nwords ver name error: ~p~n", [Reason]),
            ?ERROR_MSG("Stack ~p~n", [erlang:get_stacktrace()]),
            false
    end.

%% 获取字符串中含有的敏感词列表
%% UTF8String可以是大于255的，也可以小于255
find_data_words_in_string(UTF8String) ->
    %% ?DEBUG("find_data_words_in_string UTF8String=~w~n", [UTF8String]),
    UnicodeList = unicode:characters_to_list(hmisc:to_binary(UTF8String)),
    %% ?DEBUG("find_data_words_in_string String=~w~n", [UnicodeList]),
    %% ?DEBUG("new_combine(UnicodeList) ~p~n", [new_combine(UnicodeList)]),
    %% Words_List_1 = lists:sort(fun(List1, List2) -> 
    %%                                   length(List1) > length(List2)
    %%                           end,  new_combine(UnicodeList)),
    %% TODO: 没搞懂排序意义所在，故注释掉，By罗立威
    Words_List_1 = new_combine(UnicodeList),
    Words_List_2 = 
        lists:map(
          fun(E) ->
                  %% E_3 = lists:filter(fun(Element) ->
                  %%    Element =/= 40 andalso Element =/=41 
                  %% end,E), 
                  E_1 = hmisc:to_utf8_string(E),
                  E_2 = get_chinese_string(E),
                  case hetsutil:lookup_one(ets_words, E_1) of
                      [] ->
                          case hetsutil:lookup_one(ets_words, E_2) of
                              [] ->
                                  [];
                              _ ->
                                  E_1
                          end;
                      _ ->
                          E_1
                  end
          end, Words_List_1),
    Words_List_3 = 
        lists:filter(fun(E) ->
                             E =/= []
                     end, Words_List_2),
    lists:filter(fun(E) ->
                         E =/= 40 andalso E =/=41
                 end, Words_List_3).

%% lib_words_ver:handle_brackets(
%%     [230,152,160,229,177,177,231,186,162,40,233,135,145,232,190,190,232,142,177,41,233,157,169,229,145,189]).
handle_brackets(List)->
    %% List1 = [239,188,136],  %%中文编码小括号 （
    %% List2 = [239,188,137],  %%中文编码小括号 ）
    Result_1 = 
        lists:flatmap(
          fun(E)->
                  if
                      E =:= 40->
                          [];
                      E =:= 41->
                          [];
                      E =:= 42->
                          [32];
                      E =:= 43->
                          [32];
                      E =:= 44->
                          [32];
                      E =:= 45->
                          [32];
                      E =:= 46->
                          [32];
                      E =:= 47->
                          [32];
                      E =:= 92->
                          [32];
                      E =:= 61->
                          [32];
                      true ->
                          [E]
                  end
          end,List),
    UnicodeList = unicode:characters_to_list(list_to_binary(Result_1)),
	
    Fun = fun(E) ->
                  E =/= 65289  andalso E =/= 65288  andalso E =/= 32
          end,
    ResultStr = lists:filter(Fun, UnicodeList),
    hmisc:to_list(unicode:characters_to_binary(ResultStr,utf8)).

handle_keyword_symbol(List)->
    lists:filter(fun(E)-> E =/= 46 andalso E =/= 43 end, List).
%% 	lists:flatmap(fun(E)-> 
%% 						  if
%% 							  E =:=46 ->
%% 								  [32];
%% 							  E =:=43 ->
%% 								  [32];
%% 							  true->
%% 								  [E]
%% 						  end
%% 				  end, List).



%% ------------------------------------------------------
%% desc   过滤字符串中非汉字字符
%% parm   大于255的字符
%% return [汉字字符串]231,189,151,233,148,161,229,189,170,32,227,128,130,227,128,130,227,128,130,227,128,130
%% -------------------------------------------------------
get_chinese_string(UnicodeList)->
    Fun = fun(E) ->
                  E >= ?UNICODE_CHINESE_BEGIN  andalso
                      E =< ?UNICODE_CHINESE_END
          end,
    ResultStr = lists:filter(Fun, UnicodeList),
    hmisc:to_utf8_string(ResultStr).


%% combine(UnicodeList)->
%%     Len = length(UnicodeList),
%%     Seq = lists:seq(1,Len),
%%     CountTemp = Len + 1,
%%     Fun = fun(Pos,{Result_3})->
%%               Fun_2 = fun(LenTemp,{Result})->   
%%                         {[string:substr(UnicodeList,Pos,LenTemp) | Result]}
%%                       end,
%%               {Result_2} = lists:foldl(Fun_2,{[]},lists:seq(1, CountTemp - Pos)),
%%                {Result_3 ++ Result_2}
%%           end,
%%      {Result} = lists:foldl(Fun,{[]},Seq),
%%     %%     lists:foreach(fun(E)-> E_1 = tool:to_list(unicode:characters_to_binary(E,utf8)),
%%     %%                            ?DEBUG("combine E_1 = ~p",[E_1]) end,Result),
%%    Result.
    
new_combine(List) ->
    new_combine(List, []).
new_combine([], Res) ->
    Res;
new_combine(UnicodeList, Res) ->
    {Result0, _Pre0} =
        lists:foldl(fun(Begin, {Result, Pre}) ->
                            Next = Pre ++ [Begin],
                            {[Next|Result], Next}
                    end, {[], []}, UnicodeList),
    new_combine(tl(UnicodeList), Result0 ++ Res).


%% words_filter_test(Data)->
%%     UTF8String = tool:to_list(unicode:characters_to_binary(Data,utf8)),
%%     S = words_filter([UTF8String]),
%%     Res = unicode:characters_to_list(list_to_binary(S)),
%%     io:format("~ts~n", [Res]),
%%     lists:foreach(fun(X)->
%%                          io:format("~ts~n", [unicode:characters_to_list(list_to_binary(X))])                         
%%                  end, [["乳交 ","乳交"," 乳交 ",
%%                                                 " 乳交",
%%                                                 [232,130,137,231,169,180,32],
%%                                                 [232,130,137,231,169,180],
%%                                                 [32,232,130,137,231,169,180,
%%                                                  32],
%%                                                 [32,232,130,137,231,169,180],
%%                                                 [232,130,137,230,163,141],
%%                                                 [32,232,130,137,230,163,141],
%%                                                 [232,130,137,230,180,158,32],
%%                                                 [232,130,137,230,180,158],
%%                                                 [32,232,130,137,230,180,158,
%%                                                  32],
%%                                                 [32,232,130,137,230,180,158],
%%                                                 [232,130,137,230,163,146,32],
%%                                                 [232,130,137,230,163,146],
%%                                                 [32,232,130,137,230,163,146,
%%                                                  32],
%%                                                 [32,232,130,137,230,163,146]]
%% ]),
%%     S.
	

%% *****************************************************************************
%% 初始化敏感词库进程字典聊天系统要用到的数据，本不应该在这里出现，
%% 但是为了避免对于每一个玩家进程都初始化一个进程字典词库，势必要找一个全局进程来完成这个初始话
%% *****************************************************************************

init_data_words()->
    try
        Words_List = data_words:get_words_verlist(),
        lists:foreach(fun(E)->
                              E_1 = handle_keyword_symbol(E),
                              ets:insert(ets_words, {E, 1}),
                              ets:insert(ets_words, {E_1, 1})
                      end, Words_List)
    catch _:R ->
            ?WARNING_MSG("init_data_words failed R:~w Stack: ~p~n",[R, erlang:get_stacktrace()])
    end.

%% *****************************************************************************
%% 手动即时刷新敏感词库
%% *****************************************************************************
update_data_words() ->
	gen_server:cast(mod_mail, {apply_cast, lib_words_ver, init_data_words, []}).

%% @doc 检测utf8字符串的长度
%% @spec utf8_len(String) -> Length.
%% @end
utf8_len(<<>>) ->
    0;
utf8_len(B) when is_list(B) ->
    utf8_len(hmisc:to_binary(B));
utf8_len(B) when is_binary(B) ->
    {_, _, Rest} = read_codepoint(B),
    1 + utf8_len(Rest).

read_codepoint(Bin = <<2#0:1, C:7, Rest/binary>>) ->
    %% U+0000 - U+007F - 7 bits
    <<B:1/binary, _/binary>> = Bin,
    {C, B, Rest};
read_codepoint(Bin = <<2#110:3, B1:5,
                       2#10:2, B0:6,
                       Rest/binary>>) ->
    %% U+0080 - U+07FF - 11 bits
    case <<B1:5, B0:6>> of
        <<C:11>> when C >= 16#80 ->
            <<B:2/binary, _/binary>> = Bin,
            {C, B, Rest}
    end;
read_codepoint(Bin = <<2#1110:4, B2:4,
                       2#10:2, B1:6,
                       2#10:2, B0:6,
                       Rest/binary>>) ->
    %% U+0800 - U+FFFF - 16 bits (excluding UTC-16 surrogate code points)
    case <<B2:4, B1:6, B0:6>> of
        <<C:16>> when (C >= 16#0800 andalso C =< 16#FFFF) andalso
                      (C < 16#D800 orelse C > 16#DFFF) ->
            <<B:3/binary, _/binary>> = Bin,
            {C, B, Rest}
    end;

read_codepoint(Bin = <<2#11110:5, B3:3,
                       2#10:2, B2:6,
                       2#10:2, B1:6,
                       2#10:2, B0:6,
                       Rest/binary>>) ->
    %% U+10000 - U+10FFFF - 21 bits
    case <<B3:3, B2:6, B1:6, B0:6>> of
        <<C:21>> when (C >= 16#010000 andalso C =< 16#10FFFF) ->
            <<B:4/binary, _/binary>> = Bin,
            {C, B, Rest}
    end.

