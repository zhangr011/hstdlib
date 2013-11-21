% Copyright (c) 2008, Travis Vachon
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:
%
%     * Redistributions of source code must retain the above copyright
%       notice, this list of conditions and the following disclaimer.
%
%     * Redistributions in binary form must reproduce the above copyright
%       notice, this list of conditions and the following disclaimer in the
%       documentation and/or other materials provided with the distribution.
%
%     * Neither the name of the author nor the names of its contributors
%       may be used to endorse or promote products derived from this
%       software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
-module(uuid).
-export([
         v4_int128/0,
         v4/0, 
         to_string/1, 
         get_parts/1, 
         to_binary/1, 
         is_uuid/1, 
         get_next_uuid/0
         %% test/0
         %uuid_test/0
        ]).
%%-import(random).

% Generates a random binary UUID.

v4_int128() ->
    <<Int128:128>>  = v4(),
    Int128.

get_next_uuid() ->
    to_string(v4()).

%% 新版使用全局随机种子生成UUID
v4() ->
    v4(
      hmisc:rand(1, (round(math:pow(2, 48) - 1)) ), 
      hmisc:rand(1, (round(math:pow(2, 12) - 1)) ), 
      hmisc:rand(1, (round(math:pow(2, 32) - 1)) ), 
      hmisc:rand(1, (round(math:pow(2, 30) - 1)) ) 
     ).

%% v4() ->
%%   v4(random:uniform(round(math:pow(2, 48))) - 1, random:uniform(round(math:pow(2, 12))) - 1, random:uniform(round(math:pow(2, 32))) - 1, random:uniform(round(math:pow(2, 30))) - 1).
v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

% Returns a string representation of a binary UUID.
to_string(U) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).

% Returns the 32, 16, 16, 8, 8, 48 parts of a binary UUID.
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].

% Converts a UUID string in the format of 550e8400-e29b-41d4-a716-446655440000
% (with or without the dashes) to binary.
to_binary(U)->
    convert(lists:filter(fun(Elem) -> Elem /= $- end, U), []).

% Converts a list of pairs of hex characters (00-ff) to bytes.
convert([], Acc)->
    list_to_binary(lists:reverse(Acc));
convert([X, Y | Tail], Acc)->
    {ok, [Byte], _} = io_lib:fread("~16u", [X, Y]),
    convert(Tail, [Byte | Acc]).

%% 00ab905a-4250-462f-b112-42b21be0af38
is_uuid([_, _, _, _, _, _, _, _, $-,
         _, _, _, _, $-,
         _, _, _, _, $-,
         _, _, _, _, $-,
         _, _, _, _, _, _, _, _, _, _, _, _]) ->
    true;
is_uuid(_) ->
    false.

%% test() ->
%%     Res = is_uuid("00ab905a-4250-462f-b112-42b21be0af38"),
%%     io:format("Res:~w~n",[Res]),
%%     io:format("Res:~w~n",[is_uuid("adfasdf")]).

%% uuid_test() ->
%%     UUID = v4(),
%%     uuid_test(UUID, 10000),
%%     ok.

%% uuid_test(_UUID, 0) ->
%%     uuid_test_done;
%% uuid_test(UUID, N) ->
%%     NextUUID = v4(),
%%     if
%%         UUID =:= NextUUID -> 
%%             io:format("uuid is same uuid:~w~n",[UUID]),
%%             error_same_uuid;
%%         true -> 
%%             uuid_test(UUID, N -1)
%%     end.
