%%% @author Roowe <bestluoliwe@gmail.com>
%%% @copyright (C) 2013, Roowe
%%% @doc
%%%
%%% @end
%%% Created :  8 Nov 2013 by Roowe <bestluoliwe@gmail.com>

-module(record_info).

-export([
         parse_transform/2,
         get_record_field/2,
         pr/2]).
%% 剥离lager的record pr代码
%% 还有一个实现是https://github.com/hio/erlang-record_info
%% 不过基本太复杂了，而且抽象性一般，基本是手写AST，蛋疼。lager比较粗暴，所有record往attribute塞。
%% By Roowe
%% @private
parse_transform(AST, _Options) ->
    erlang:put(records, []),
    walk_ast([], AST).

walk_ast(Acc, []) ->    
    insert_record_attribute(Acc);
walk_ast(Acc, [{attribute, _, record, {Name, Fields}}=H|T]) ->
    FieldNames = lists:map(fun({record_field, _, {atom, _, FieldName}}) ->
                FieldName;
            ({record_field, _, {atom, _, FieldName}, _Default}) ->
                FieldName
        end, Fields),
    stash_record({Name, FieldNames}),
    walk_ast([H|Acc], T);
walk_ast(Acc, [H|T]) ->
    walk_ast([H|Acc], T).

stash_record(Record) ->
    Records = case erlang:get(records) of
        undefined ->
            [];
        R ->
            R
    end,
    erlang:put(records, [Record|Records]).

insert_record_attribute(AST) ->
    lists:foldl(fun({attribute, Line, module, _}=E, Acc) ->
                [E, {attribute, Line, record_info, erlang:get(records)}|Acc];
            (E, Acc) ->
                [E|Acc]
        end, [], AST).


%% @doc Print a record lager found during parse transform
pr(Record, Module) when is_tuple(Record), is_atom(element(1, Record)) ->
    try Module:module_info(attributes) of
        Attrs ->
            case lists:keyfind(record_info, 1, Attrs) of
                false ->
                    Record;
                {record_info, Records} ->
                    RecordName = element(1, Record),
                    RecordSize = tuple_size(Record) - 1,
                    case lists:filter(fun({Name, Fields}) when Name == RecordName,
                                length(Fields) == RecordSize ->
                                    true;
                                (_) ->
                                    false
                            end, Records) of
                        [] ->
                            Record;
                        [{RecordName, RecordFields}|_] ->                            
                            lists:zip(RecordFields, tl(tuple_to_list(Record)))
                    end
            end
    catch
        error:undef ->
            Record;
        Err:Reason ->
            error_logger:error_msg("record_info pr ~p, ~p~n", [Err, Reason]),
            Record
    end;
pr(Record, _) ->
    Record.

get_record_field(RecordName, Module) ->
    try Module:module_info(attributes) of
        Attris ->
            case lists:keyfind(record_info, 1, Attris) of
                false ->
                    [];
                {record_info, Records} ->
                    case lists:keyfind(RecordName, 1, Records) of
                        false ->
                            [];
                        {_RecordName, Fields}->
                            Fields
                    end
            end
    catch 
        error:undef ->
            error_logger:error_msg("undef record : ~w~n",[RecordName]),
            [];
          _:R ->
            error_logger:error_msg("get_record_field_failed!! Name:~w, R:~w~n",[RecordName, R]),
            []

    end.
