%%%-----------------------------------
%%% @Module  : file_tools
%%% @Created : 2010.10.05
%%% @Description: 公共函数
%%%-----------------------------------
-module(hfile).

-export([
         readlines/1,
         writelines/2,
         writelines_new/2
        ]).

%% @doc readlines from file
readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try
        inner_get_all_lines(Device)
    after
        file:close(Device)
    end.

inner_get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof ->
            [];
        Line ->
            Line ++ inner_get_all_lines(Device)
    end.

%% @doc writelines
writelines(FileName, InfoList) ->
    file:write_file(FileName, ""),
    lists:foreach(fun(Info) ->
                          file:write_file(FileName, Info, [append])
                  end, InfoList).

writelines_new(FileName, BinList) when is_binary(BinList) ->
    BinOldInfoList = 
        try
            case file:read_file(FileName) of
                {ok, Bin} ->
                    Bin;
                _ ->
                    <<>>
            end
        catch
            _:_ ->
                io:format("new file ~ts ~n", [FileName]),
                <<>>
        end,
    if
        BinOldInfoList =/= BinList ->
            %% io:format("Old: ~n~p~n", [BinOldInfoList]),
            %% io:format("New: ~n~p~n", [BinList]),
            io:format("rewrite file ~ts ...~n", [FileName]),
            file:write_file(FileName, BinList);
        true ->
            ok
    end;
%% @doc writelines when new
writelines_new(FileName, InfoList) ->
    BinOldInfoList = 
        try
            hmisc:to_binary(readlines(FileName))
        catch
            _:_ ->
                io:format("new file ~ts ~n", [FileName]),
                []
        end,
    BinInfoList = hmisc:to_binary(InfoList),
    if
        BinInfoList =/= BinOldInfoList ->
            io:format("rewrite file ~ts ...~n", [FileName]),
            writelines(FileName, InfoList);
        true ->
            ok
    end.

