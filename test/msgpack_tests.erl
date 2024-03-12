%%==============================================================================
%% Copyright 2013-2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the msgpack library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(msgpack_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
%% decode(MSGPACK) = Term, encode(TERM) = MSGPACK
-define(SIMPLE, [true, false, nil]).
-define(INTEGER, [
                  0, 1, 2, 127, 128, 255, 256, 65535, 65536, 4294967295,
                  4294967296, 18446744073709551615,
                  -1, -2, -32, -33, -128,
                  -129, -32768, -32769, -2147483648,
                  -2147483649, -9223372036854775808
                 ]).
-define(FLOAT, [1.0, 2.0]).
-define(MAP, [{[]}, {[{<<"one">>, 1}]},
              {[{<<"one">>, 1}, {1.0, true}]},
              [{[{N1, 2.0} || N1 <- lists:seq(1, N)]} ||
                  N <- [32, 65535, 65536]]
             ]).
-define(ARRAY, [[], [1], [1, 2], [1, 2, 3.0],
                [lists:seq(1, N) || N <- [32, 65535, 65536]]]).
-define(RAW, [<<"a">>, <<"abcd">>, binary:copy(<<"a">>, 33),
              binary:copy(<<"a">>, 65536)]).
-define(REVERSIBLE_TERM,
        ?SIMPLE ++ ?INTEGER ++ ?FLOAT ++ ?MAP ++ ?ARRAY ++ ?RAW).

-define(INTEGER_TYPE,
        [{pos_fixnum, [0, 1, 10, 127]},
         {neg_fixnum, [-1, -10, -32]},
         {uint8,  [0, 10, 255]},
         {uint16, [0, 10, 255, 65535]},
         {uint32, [0, 10, 255, 65535, 4294967295]},
         {uint64, [0, 10, 255, 65535, 4294967295, 18446744073709551615]},
         {int8,  [-128, -10, 0, 10, 127]},
         {int16, [-32768, -128, -10, 0, 10, 127, 32767]},
         {int32, [-2147483648, -32768, -128, -10, 0, 10, 127, 2147483647]},
         {int64, [-9223372036854775808, -2147483648, -32768, -128, -10, 0,
                  10, 127, 9223372036854775807]},
         {double, [1.23e-12, -100.0, 0.0, 100.0, 14.5e10]},
         {float, [-100.0, 0.0, 100.0]}
        ]).

%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Encoding/Decoding
%% ===================================================================

%%--------------------------------------------------------------------
%% encode/1 <-> /decode/1
%%--------------------------------------------------------------------
encode_1_decode_1_test_() ->
    [?_test(?assertEqual(Term,
                         msgpack:decode(
                           iolist_to_binary(
                             msgpack:encode(Term))))) ||
        Term <- ?REVERSIBLE_TERM].

%%--------------------------------------------------------------------
%% encode/1 <-> /decode/2 iolist
%%--------------------------------------------------------------------
encode_1_decode_1_iolist_test_() ->
    [?_test(?assertEqual(Term,
                         msgpack:decode(
                           iolist_to_binary(
                             msgpack:encode(Term, [iolist]))))) ||
        Term <- ?REVERSIBLE_TERM].


%%--------------------------------------------------------------------
%% encode/1 <-> /decode/2 binary
%%--------------------------------------------------------------------
encode_1_decode_2_binary_test_() ->
    [?_test(?assertEqual(Term,
                         msgpack:decode(
                             msgpack:encode(Term, [binary])))) ||
        Term <- ?REVERSIBLE_TERM].

%%--------------------------------------------------------------------
%% encode/2 <-> /decode/2
%%--------------------------------------------------------------------
encode_2_decode_2_test_() ->
    [?_test(?assertEqual(Term,
                         msgpack:decode(
                           msgpack:encode(Term, [binary]),
                           []))) ||
        Term <- ?REVERSIBLE_TERM].

%%--------------------------------------------------------------------
%% encode/2 <-> /decode/2 integer_type
%%--------------------------------------------------------------------
encode_2_decode_2_integer_type_test_() ->
    [[
      ?_test(
         ?assertEqual(Value,
                      msgpack:decode(msgpack:encode({Type, Value},
                                                    [binary])))),
      ?_test(
         ?assertEqual({Type, Value},
                      msgpack:decode(msgpack:encode({Type, Value}, [binary]),
                                     [number_types])))]
     ||
        {Type, Values} <- ?INTEGER_TYPE,
        Value <- Values
    ].

