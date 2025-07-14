%%==============================================================================
%% Copyright 2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%%  A BSON library based on:
%%%
%%%  BSON, short for Binary JSON    (https://bsonspec.org/)
%%%
%%%  BSON bson() values are represented as follows:
%%%
%%%  Simple values:
%%%
%%%  False(20)     : false
%%%
%%%  True(21)      : true
%%%
%%%  Null(22)      : null
%%%
%%%  Undefined(23) : undefined
%%%
%%%  (Integer)     : {simple, integer()}
%%%
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_bson).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1]).

%% Exported types
-export_type([]).

%% Types

-type bson() :: _.
-type opts() :: _.


%% Records
-record(opts, {return_type = iolist :: iolist | binary}).

%% Defines
-define(LSW, 32/little-signed).
-define(LSDW, 64/little-signed).

%% Types
-define(FLOAT, 1).
-define(STRING, 2).
-define(OBJECT, 3).
-define(ARRAY, 4).
-define(BINARY, 5).
-define(UNDEFINED, 6).
-define(OID,7).
-define(BOOLEAN, 8).
-define(DATETIME, 9).
-define(NULL, 10).
-define(REGEXP, 11).
-define(DBPOINTER, 12).
-define(JS_CODE, 13).
-define(SYMBOL, 14).
%% Remain
-define(JS_CODE_WITH_SCOPE, 15).
-define(INT, 16).
-define(TIMESTAMP, 17).
-define(LONG, 18).
%% Remain
-define(DECIMAL128, 19).
-define(MIN, -1).
-define(MAX, 127).

%% Binary SubType
-define(GENERIC, 0).
-define(FUNCTION, 1).
-define(OLD, 2).
-define(UUID_OLD, 3).
-define(UUID, 4).
-define(MD5, 5).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Term) -> BSON.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent of encode(Term, []) -> BSON.
%% @end
%%--------------------------------------------------------------------
-spec encode(bson()) -> iodata().
%%--------------------------------------------------------------------
encode(B) -> encode(B, []).

%%--------------------------------------------------------------------
%% Function: encode(Term, [Option]) -> BSON.
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     iolist -> an iolist is returned (default)
%%     {tag_callbacks, [{integer(), nodule()}]} specifies
%%     a module with a bson_encode_tag callback function to decode tags
%%     not supporrted in this module.
%% @end
%%--------------------------------------------------------------------
-spec encode(bson(), opts()) -> binary().
%%--------------------------------------------------------------------
encode(B, Opts) ->
    case parse_opts(Opts, #opts{}) of
        #opts{return_type = iolist} -> do_encode(B);
        #opts{return_type = binary} -> iolist_to_binary(do_encode(B))
    end.

%%--------------------------------------------------------------------
%% Function: decode(BSON) -> {Term, Binary}
%% @doc
%%   Decodes the binary into a tuple of structured Erlang term.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> {bson(), binary()}.
%%--------------------------------------------------------------------
decode(<<5,0,0,0,0, T/binary>>) -> {#{}, T};
decode(<<Size:?LSW, T/binary>>) ->
    <<D:(Size - 5)/binary, 0, T1/binary>> = T,
    {do_decode(D, #{}), T1}.

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

do_encode(Object) ->
    case maps:size(Object) of
        0 -> <<5,0,0,0,0>>;
        _ ->
            Enc = [encode_value(Key, Val) ||
                      {Key, Val} <- maps:to_list(Object)],
            Size = iolist_size(Enc) + 5,
            [<<Size:?LSW>>, Enc, <<0>>]
    end.

encode_value(Key, nan) ->
    <<?FLOAT, Key/binary, 0, 0:48, 16#F8:8, 16#7F:8>>;
encode_value(Key, inf) ->
    <<?FLOAT, Key/binary, 0, 0:48, 16#F0:8, 16#7F:8>>;
encode_value(Key, neg_inf) ->
    <<?FLOAT, Key/binary, 0, 0:48, 16#F0:8, 16#FF:8>>;
encode_value(Key, Val) when is_float(Val) ->
    <<?FLOAT, Key/binary, 0, Val:?LSDW-float>>;
encode_value(Key, Val) when is_binary(Val) ->
    Size = byte_size(Val) + 1,
    <<?STRING, Key/binary, 0, Size:?LSW, Val/binary, 0>>;
encode_value(Key, Val = #{}) ->
    [<<?OBJECT, Key/binary, 0>>, encode(Val)];
encode_value(Key, Val) when is_list(Val) ->
    Array = encode_array(Val, 0, []),
    Size = iolist_size(Array) + 5,
    [<<?ARRAY, Key/binary, 0, Size:?LSW>>, Array, <<0>>];
encode_value(Key, {binary, SubType, Val}) ->
    Size = byte_size(Val),
    case SubType of
        %% TODO add the other subtype cases where relevant
        ?GENERIC ->
            %% Generic binary
            <<?BINARY, Key/binary, 0, Size:?LSW, SubType:8, Val/binary>>;
        ?OLD ->
            %% Old binary
            <<?BINARY, Key/binary, 0,
              (Size + 4):?LSW, 2, Size:?LSW, Val/binary>>;
        _ ->
            %% The rest
            <<?BINARY, Key/binary, 0, Size:?LSW, SubType:8, Val/binary>>
    end;
encode_value(Key, undefined) ->
    <<?UNDEFINED, Key/binary, 0>>;
%% OID Should we support hexstrings?
%% And should we support Timstamp, random, counter
encode_value(Key, {oid, OID}) ->
    <<?OID, Key/binary, 0, OID:96>>;
encode_value(Key, true) ->
    <<?BOOLEAN, Key/binary, 0, 1>>;
encode_value(Key, false) ->
    <<?BOOLEAN, Key/binary, 0, 0>>;
encode_value(Key, {posix, I}) when is_integer(I)->
    <<?DATETIME, Key/binary, 0, I:?LSDW>>;
encode_value(Key, {datetime, T}) when is_binary(T) ->
    I = jhn_timestamp:encode(jhn_timestamp:decode(T), [posix, milli]),
    <<?DATETIME, Key/binary, 0, I:?LSDW>>;
encode_value(Key, null) ->
    <<?NULL, Key/binary, 0>>;
encode_value(Key, {regexp, R, O}) when is_binary(R), is_binary(O) ->
    <<?REGEXP, Key/binary, 0, R/binary, 0, O/binary, 0>>;
encode_value(Key, {db_pointer, Ref, ID}) ->
    <<?DBPOINTER, Key/binary, 0, (byte_size(Ref) + 1):?LSW,Ref/binary,0,ID:96>>;
encode_value(Key, {code, C}) when is_binary(C) ->
    <<?JS_CODE, Key/binary, 0, (byte_size(C) + 1):?LSW, C/binary, 0>>;
encode_value(Key, {symbol, S}) when is_binary(S) ->
    <<?SYMBOL, Key/binary, 0, (byte_size(S) + 1):?LSW, S/binary, 0>>;
encode_value(Key, V) when is_integer(V),
                              V =< 2147483647,
                              V >= -2147483648 ->
    <<?INT, Key/binary, 0, V:?LSW>>;
encode_value(Key, {timestamp, Inc, Time}) when is_integer(Inc),
                                               is_integer(Time) ->
    <<TS:64>> = <<Inc:32, Time:32>>,
    <<?TIMESTAMP, Key/binary, 0, TS:?LSDW>>;
encode_value(Key, {long, Val}) when is_integer(Val) ->
    <<?LONG, Key/binary, 0, Val:?LSDW>>;
encode_value(Key, Val) when is_integer(Val) ->
    <<?LONG, Key/binary, 0, Val:?LSDW>>;
encode_value(Key, min) ->
    <<?MIN, Key/binary, 0>>;
encode_value(Key, max) ->
    <<?MAX, Key/binary, 0>>.

encode_array([], _, Acc) -> lists:reverse(Acc);
encode_array([H | T], N, Acc) ->
    encode_array(T, N + 1, [encode_value(integer_to_binary(N), H) | Acc]).

%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(<<>>, Acc) -> Acc;
do_decode(<<Type:8/little-signed, T/binary>>, Acc) ->
    {Key, T1} = decode_key(T, <<>>),
    {Value, T2} = decode_value(Type, T1),
    do_decode(T2, Acc#{Key => Value}).

decode_key(<<0, T/binary>>, Acc) -> {Acc, T};
decode_key(<<H, T/binary>>, Acc) -> decode_key(T, <<Acc/binary, H>>).

decode_value(?FLOAT, <<0:48, 16#F8:8, 16#7F:8, T/binary>>) -> {nan, T};
decode_value(?FLOAT, <<0:48, 16#F0:8, 16#7F:8, T/binary>>) -> {inf, T};
decode_value(?FLOAT, <<0:48, 16#F0:8, 16#FF:8, T/binary>>) -> {neg_inf, T};
decode_value(?FLOAT, <<V:?LSDW-float, T/binary>>) -> {V, T};
decode_value(?STRING, <<1:?LSW, 0>>) -> {<<>>, <<>>};
decode_value(?STRING, <<Size:?LSW, T/binary>>) ->
    <<V:(Size - 1)/binary, 0, T1/binary>> = T,
    {V, T1};
decode_value(?OBJECT, Val) ->
    decode(Val);
decode_value(?ARRAY, <<Size:?LSW, T/binary>>) ->
    <<V:(Size - 5)/binary, 0, T1/binary>> = T,
    {decode_array(V, []), T1};
%% Binary
decode_value(?BINARY, <<Size:?LSW, ?GENERIC:8, T/binary>>) ->
    <<Val:Size/binary, T1/binary>> = T,
    {{binary, ?GENERIC, Val}, T1};
decode_value(?BINARY, <<_:?LSW, ?OLD, Size:?LSW, T/binary>>) ->
    <<Val:Size/binary, T1/binary>> = T,
    {{binary, ?OLD, Val}, T1};
decode_value(?BINARY, <<Size:?LSW, SubType:8, T/binary>> ) ->
    <<Val:Size/binary, T1/binary>> = T,
    {{binary, SubType, Val}, T1};
decode_value(?UNDEFINED, T) ->
    {undefined, T};
decode_value(?OID, <<OID:96, T/binary>>) ->
    {{oid, OID}, T};
decode_value(?BOOLEAN, <<1, T/binary>>) ->
    {true, T};
decode_value(?BOOLEAN, <<0, T/binary>>) ->
    {false, T};
%% Allow for posix as well? Adding flags.
decode_value(?DATETIME, <<I:?LSDW, T/binary>>) ->
    {{posix, I}, T};
decode_value(?NULL, T) ->
    {null, T};
decode_value(?REGEXP, V) ->
    {R, T} = decode_cstring(V, <<>>),
    {O, T1} = decode_cstring(T, <<>>),
    {{regexp, R, O}, T1};
decode_value(?DBPOINTER, <<Size:?LSW, T/binary>>) ->
    <<Ref:(Size - 1)/binary, 0, OID:96, T1/binary>> = T,
    {{db_pointer, Ref, OID}, T1};
decode_value(?JS_CODE, <<Size:?LSW, T/binary>>) ->
    <<Code:(Size - 1)/binary, 0, T1/binary>> = T,
    {{code, Code}, T1};
decode_value(?SYMBOL, <<Size:?LSW, T/binary>>) ->
    <<Symbol:(Size - 1)/binary, 0, T1/binary>> = T,
    {{symbol, Symbol}, T1};
decode_value(?INT, <<V:?LSW, T/binary>>) ->
    {V, T};
decode_value(?TIMESTAMP, <<TS:?LSDW, T/binary>>) ->
    <<Inc:32, Time:32>> = <<TS:64>>,
    {{timestamp, Inc, Time}, T};
decode_value(?LONG, <<I:?LSDW, T/binary>>) ->
    {I, T};
decode_value(?MIN, T) ->
    {min, T};
decode_value(?MAX, T) ->
    {max, T}.

decode_array(<<>>, Acc) -> lists:reverse(Acc);
decode_array(<<Type:8/little-signed, T/binary>>, Acc) ->
    {Value, T1} = decode_value(Type, skip_key(T)),
    decode_array(T1, [Value | Acc]).

skip_key(<<0, T/binary>>) -> T;
skip_key(<<_, T/binary>>) -> skip_key(T).

decode_cstring(<<0, T/binary>>, Acc) -> {Acc, T};
decode_cstring(<<H, T/binary>>, Acc) -> decode_cstring(T, <<Acc/binary, H>>).

%% ===================================================================
%% Common
%% ===================================================================

parse_opts([], Opts) -> Opts;
parse_opts([binary | T], Opts) ->
    parse_opts(T, Opts#opts{return_type = binary});
parse_opts([iolist | T], Opts) ->
    parse_opts(T, Opts#opts{return_type = iolist}).
