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
%%%   eunit unit tests for the jhn_bson library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_bson_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines

%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Encoding/Decoding
%% ===================================================================

%%--------------------------------------------------------------------
%% Float
%%--------------------------------------------------------------------

-define(FLOATS,
        [#{<<"a">> => 0.0},
         #{<<"a">> => nan},
         #{<<"a">> => inf},
         #{<<"a">> => neg_inf},
         #{<<"a">> => 0.0, <<"b">> =>  1.0},
         #{<<"a">> => 0.0, <<"b">> =>  1.0, <<"c">> => 54843.834843}
        ]).

encode_1_decode_1_float_test_() ->
    [?_test(?assertEqual({Term, <<>>},
                         jhn_bson:decode(
                           iolist_to_binary(jhn_bson:encode(Term))))) ||
        Term <- ?FLOATS].

spec_encode_1_float_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_doubles(B)))))} ||
        {D, B, CB} <- spec(double), D /= <<"NaN with payload">>].

spec_decode_1_object_test_() ->
    [{D, ?_test(?assertEqual({special_doubles(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(double), D /= <<"NaN with payload">>].

special_doubles(Map = #{}) -> maps:map(fun special_doubles/2, Map);
special_doubles(BSON) -> BSON.

special_doubles(_, #{<<"$numberDouble">> := <<"NaN">>}) -> nan;
special_doubles(_, #{<<"$numberDouble">> := <<"Infinity">>}) -> inf;
special_doubles(_, #{<<"$numberDouble">> := <<"-Infinity">>}) -> neg_inf;
special_doubles(_, Map = #{}) -> special_doubles(Map);
special_doubles(_, Value) -> Value.

%%--------------------------------------------------------------------
%% String
%%--------------------------------------------------------------------

spec_encode_1_string_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(B))))} ||
        {D, B, CB} <- spec(string)].

spec_decode_1_string_test_() ->
    [{D, ?_test(?assertEqual({B, <<>>}, jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(string)].

%%--------------------------------------------------------------------
%% Object
%%--------------------------------------------------------------------

-define(OBJECTS,
        [#{},
         #{<<"x">> => #{<<"a">> => 1.1}},
         #{<<"x">> => #{<<"y">> => #{<<"a">> => 1.1}}},
         #{<<"x">> => #{<<"a">> => 1.1, <<"b">> => 123343.343434}}
        ]).

encode_1_decode_1_object_test_() ->
    [?_test(?assertEqual({Term, <<>>},
                         jhn_bson:decode(
                           iolist_to_binary(jhn_bson:encode(Term))))) ||
        Term <- ?OBJECTS].

spec_encode_1_object_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(B))))} ||
        {D, B, CB} <- spec(document)].

%% spec_decode_1_object_test_() ->
%%     [{D, ?_test(?assertEqual({B, <<>>}, jhn_bson:decode(CB)))} ||
%%         {D, B, CB} <- spec(document)].

%%--------------------------------------------------------------------
%% Array
%%--------------------------------------------------------------------

-define(ARRAYS,
        [#{<<"a">> => [0.0]},
         #{<<"a">> => [nan]},
         #{<<"a">> => [0.0, 1.0, 54843.834843]}
        ]).

encode_2_decode_1_float_test_() ->
    [?_test(?assertEqual({Term, <<>>},
                         jhn_bson:decode(
                           iolist_to_binary(jhn_bson:encode(Term,
                                                            [binary]))))) ||
        Term <- ?ARRAYS].

spec_encode_1_array_test_() ->
    %% [?debugFmt("~nInt: ~p,~n~p~n", [B, special_elts(B)]) || 
    %%                {D, B, CB} <- spec(array)],
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_elts(B)))))} ||
        {D, B, CB} <- spec(array)].

special_elts(#{<<"a">> := Array}) ->
    #{<<"a">> => [special_elt(E) || E <- Array]};
special_elts(BSON) -> BSON.

special_elt(#{<<"$numberInt">> := I}) -> binary_to_integer(I);
special_elt(Value) -> Value.

%%--------------------------------------------------------------------
%% Binary
%%--------------------------------------------------------------------

-define(BINARIES,
        [#{<<"x">> => {binary, 0, <<>>}}
        ]).

encode_2_decode_1_binary_test_() ->
    [?_test(?assertEqual({Term, <<>>},
                         jhn_bson:decode(
                           iolist_to_binary(jhn_bson:encode(Term,
                                                            [binary]))))) ||
        Term <- ?BINARIES].

spec_encode_1_binary_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_binary(B)))))} ||
        {D, B, CB} <- spec(binary),
        D /= <<"$type query operator (conflicts with legacy "
               "$binary form with $type field)">>].

spec_decode_1_binary_test_() ->
    [{D, ?_test(?assertEqual({special_binary(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(binary),
        D /= <<"$type query operator (conflicts with legacy "
               "$binary form with $type field)">>].

special_binary(Map = #{}) -> maps:map(fun special_binary/2, Map);
special_binary(BSON) -> BSON.

special_binary(_,
               #{<<"$binary">> :=
                     #{<<"base64">> := B,
                       <<"subType">> := S}}) ->
    {binary, binary_to_integer(S, 16), base64:decode(B)};
special_binary (_, Value) -> Value.

%%--------------------------------------------------------------------
%% undefined
%%--------------------------------------------------------------------

spec_encode_1_undefined_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_undefined(B)))))} ||
        {D, B, CB} <- spec(undefined)].

spec_decode_1_undefined_test_() ->
    [{D, ?_test(?assertEqual({special_undefined(B), <<>>},
                              jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(undefined)].

special_undefined(Map = #{}) -> maps:map(fun special_undefined/2, Map);
special_undefined(BSON) -> BSON.

special_undefined(_, #{<<"$undefined">> := true}) -> undefined;
special_undefined (_, Value) -> Value.

%%--------------------------------------------------------------------
%% OID
%%--------------------------------------------------------------------

spec_encode_1_oid_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_oids(B)))))} ||
        {D, B, CB} <- spec(oid)].

spec_decode_1_oid_test_() ->
    [{D, ?_test(?assertEqual({special_oids(B), <<>>},
                              jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(oid)].

special_oids(Map = #{}) -> maps:map(fun special_oids/2, Map);
special_oids(BSON) -> BSON.

special_oids(_, #{<<"$oid">> := OID}) -> {oid, binary_to_integer(OID, 16)};
special_oids(_, Value) -> Value.

%%--------------------------------------------------------------------
%% Boolean
%%--------------------------------------------------------------------

spec_encode_1_boolean_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(B))))} ||
        {D, B, CB} <- spec(boolean)].

spec_decode_1_boolean_test_() ->
    [{D, ?_test(?assertEqual({B, <<>>}, jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(boolean)].

%%--------------------------------------------------------------------
%% DATETIME
%%--------------------------------------------------------------------

spec_encode_1_datetime_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_datetimes(B)))))} ||
        {D, B, CB} <- spec(datetime)].

spec_decode_1_datetime_test_() ->
    [{D, ?_test(?assertEqual({special_datetimes1(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(datetime)].

special_datetimes(Map = #{}) -> maps:map(fun special_datetimes/2, Map);
special_datetimes(BSON) -> BSON.

special_datetimes(_, #{<<"$date">> := #{<<"$numberLong">> := Posix}}) ->
    {posix, binary_to_integer(Posix)};
special_datetimes(_, #{<<"$date">> := DATETIME}) ->
    {datetime, DATETIME};
special_datetimes(_, Value) ->
    Value.

special_datetimes1(Map = #{}) -> maps:map(fun special_datetimes1/2, Map);
special_datetimes1(BSON) -> BSON.

special_datetimes1(_, #{<<"$date">> := #{<<"$numberLong">> := Posix}}) ->
    {posix, binary_to_integer(Posix)};
special_datetimes1(_, #{<<"$date">> := DATETIME}) ->
    {posix,
     jhn_timestamp:encode(jhn_timestamp:decode(DATETIME), [posix, milli])};
special_datetimes1(_, Value) ->
    Value.

%%--------------------------------------------------------------------
%% NULL
%%--------------------------------------------------------------------

spec_encode_1_null_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(B))))} ||
        {D, B, CB} <- spec(null)].

spec_decode_1_null_test_() ->
    [{D, ?_test(?assertEqual({B, <<>>}, jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(null)].

%%--------------------------------------------------------------------
%% REGEXP
%%--------------------------------------------------------------------

spec_encode_1_regexp_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_regexps(B)))))} ||
        {D, B, CB} <- spec(regex),
        D /= <<"Regular expression as value of $regex query operator "
               "with $options">>].

spec_decode_1_regexp_test_() ->
    [{D, ?_test(?assertEqual({special_regexps(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(regex)].

special_regexps(Map = #{}) -> maps:map(fun special_regexps/2, Map);
special_regexps(BSON) -> BSON.

special_regexps(_, #{<<"$regularExpression">> :=
                         #{<<"pattern">> := P, <<"options">> := O}}) ->
    {regexp, P, O};
special_regexps(_, Value) ->
    Value.

%%--------------------------------------------------------------------
%% DBPOINTER
%%--------------------------------------------------------------------

spec_encode_1_db_pointer_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_db_pointers(B)))))} ||
        {D, B, CB} <- spec(dbpointer)].

spec_decode_1_db_pointer_test_() ->
    [{D, ?_test(?assertEqual({special_db_pointers(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(dbpointer)].

special_db_pointers(Map = #{}) -> maps:map(fun special_db_pointers/2, Map);
special_db_pointers(BSON) -> BSON.

special_db_pointers(_, #{<<"$dbPointer">> :=
                        #{<<"$ref">> := Ref,
                          <<"$id">> := #{<<"$oid">> := OID}}}) ->
    {db_pointer, Ref, binary_to_integer(OID, 16)};
special_db_pointers(_, Value) -> Value.


%%--------------------------------------------------------------------
%% CODE
%%--------------------------------------------------------------------

spec_encode_1_code_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_codes(B)))))} ||
        {D, B, CB} <- spec(code)].

spec_decode_1_code_test_() ->
    [{D, ?_test(?assertEqual({special_codes(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(code)].

special_codes(Map = #{}) -> maps:map(fun special_codes/2, Map);
special_codes(BSON) -> BSON.

special_codes(_, #{<<"$code">> := CODE}) -> {code, CODE};
special_codes(_, Value) -> Value.

%%--------------------------------------------------------------------
%% SYMBOL
%%--------------------------------------------------------------------

spec_encode_1_symbol_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_symbols(B)))))} ||
        {D, B, CB} <- spec(symbol)].

spec_decode_1_symbol_test_() ->
    [{D, ?_test(?assertEqual({special_symbols(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(symbol)].

special_symbols(Map = #{}) -> maps:map(fun special_symbols/2, Map);
special_symbols(BSON) -> BSON.

special_symbols(_, #{<<"$symbol">> := SYMBOL}) -> {symbol, SYMBOL};
special_symbols(_, Value) -> Value.

%%--------------------------------------------------------------------
%% Code with scope
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Int
%%--------------------------------------------------------------------

spec_encode_1_int_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_int(B)))))} ||
        {D, B, CB} <- spec(int32)].

spec_decode_1_int_test_() ->
    [{D, ?_test(?assertEqual({special_int(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(int32)].

special_int(Map = #{}) -> maps:map(fun special_int/2, Map);
special_int(BSON) -> BSON.

special_int(_, #{<<"$numberInt">> := I}) -> binary_to_integer(I);
special_int(_, Value) -> Value.

%%--------------------------------------------------------------------
%% TIMESTAMP
%%--------------------------------------------------------------------

spec_entimestamp_1_timestamp_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_timestamps(B)))))} ||
        {D, B, CB} <- spec(timestamp)].

spec_decode_1_timestamp_test_() ->
    [{D, ?_test(?assertEqual({special_timestamps(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(timestamp)].

special_timestamps(Map = #{}) -> maps:map(fun special_timestamps/2, Map);
special_timestamps(BSON) -> BSON.

special_timestamps(_, #{<<"$timestamp">> := #{<<"t">> := T, <<"i">> := I}}) ->
    {timestamp, T, I};
special_timestamps(_, Value) -> Value.

%%--------------------------------------------------------------------
%% Long
%%--------------------------------------------------------------------

-define(LONG, [-1, 0, 1]).

spec_encode_1_long_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_long(B)))))} ||
        {D, B, CB} <- spec(int64)].

spec_decode_1_long_test_() ->
    [{D, ?_test(?assertEqual({special_long1(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(int64)].

special_long(Map = #{}) -> maps:map(fun special_long/2, Map);
special_long(BSON) -> BSON.

special_long(_, #{<<"$numberLong">> := I}) ->
    special_long(x, binary_to_integer(I));
special_long(_, I) when is_integer(I) ->
    case lists:member(I, ?LONG) of
        true -> {long, I};
        false -> I
    end;
special_long(_, Value) -> Value.

special_long1(Map = #{}) -> maps:map(fun special_long1/2, Map);
special_long1(BSON) -> BSON.

special_long1(_, #{<<"$numberLong">> := I}) ->
    special_long(x, binary_to_integer(I));
special_long1(_, I) when is_integer(I) -> I;
special_long1(_, Value) -> Value.

%%--------------------------------------------------------------------
%% Min
%%--------------------------------------------------------------------

spec_encode_1_min_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_min(B)))))} ||
        {D, B, CB} <- spec(minkey)].

spec_decode_1_min_test_() ->
    [{D, ?_test(?assertEqual({special_min(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(minkey)].

special_min(Map = #{}) -> maps:map(fun special_min/2, Map);
special_min(BSON) -> BSON.

special_min(_, #{<<"$minKey">> := _}) -> min;
special_min(_, Value) -> Value.

%%--------------------------------------------------------------------
%% Max
%%--------------------------------------------------------------------

spec_encode_1_max_test_() ->
    [{D, ?_test(?assertEqual(CB,
                             iolist_to_binary(
                               jhn_bson:encode(special_max(B)))))} ||
        {D, B, CB} <- spec(maxkey)].

spec_decode_1_max_test_() ->
    [{D, ?_test(?assertEqual({special_max(B), <<>>},
                             jhn_bson:decode(CB)))} ||
        {D, B, CB} <- spec(maxkey)].

special_max(Map = #{}) -> maps:map(fun special_max/2, Map);
special_max(BSON) -> BSON.

special_max(_, #{<<"$maxKey">> := _}) -> max;
special_max(_, Value) -> Value.


%%--------------------------------------------------------------------
%% Misc
%%--------------------------------------------------------------------

spec(Part) ->
    File =
        filename:join([code:lib_dir(jhn_stdlib), test, bson,
                       atom_to_list(Part) ++ ".json"]),
    {ok, Bin} = file:read_file(File),
    #{<<"valid">> := Valids} = jhn_json:decode(Bin),
    lists:foldl(fun valid/2, [], Valids).

valid(#{<<"relaxed_extjson">> := Relax,
        <<"description">> := D,
        <<"canonical_bson">> := B},
      Acc) ->
    B1  = << <<(binary_to_integer(I, 16))>> || <<I:2/binary>> <= B >>,
    [{D, json:decode(Relax), B1} | Acc];
valid(#{<<"canonical_extjson">> := Can,
        <<"description">> := D,
        <<"canonical_bson">> := B},
      Acc) ->
    B1  = << <<(binary_to_integer(I, 16))>> || <<I:2/binary>> <= B >>,
    [{D, json:decode(Can), B1} | Acc].

