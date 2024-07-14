%%==============================================================================
%% Copyright 2021-2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the jhn_cbor library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2021-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_cbor_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% jhn_cbor callbacks
-export([cbor_encode_tag/3, cbor_decode_tag/3]).

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(LARGE_STRING, <<"a":524288>>).

-define(SIMPLE, [true, false, null, undefined, {simple, 4}, {simple, 250}]).
-define(INTEGER, [0, 1, 2, 127, 128, 255, 256, 65535, 65536, 4294967295,
                  4294967296, 18446744073709551615,
                  -1, -2, -32, -33, -128,
                  -129, -32768, -32769, -2147483648,
                  -2147483649, -9223372036854775808,
                  -100.0, 0.0, 100.0
                 ]).
-define(FLOAT, [1.0, 2.0,
               1.23e-12, -100.0, 0.0, 100.0, 14.5e10,
                inf, neg_inf, nan
               ]).
-define(MAP, [#{}, #{<<"one">> =>  1}, #{<<"one">> => 1, 1.0 => true} |
              [maps:from_list([{N1, 2.0} || N1 <- lists:seq(1, N)]) ||
                  N <- [32, 65535, 65536]]
             ]).
-define(ARRAY, [[], [1], [1, 2], [1, 2, 3.0] ,
                [lists:seq(1, N) || N <- [32, 65535, 65536]]
               ]).
-define(TEXT, [<<"a">>,
               <<"abcd">>,
               <<"a":(33*8)>>,
               <<"a":(260*8)>>,
               ?LARGE_STRING
              ]).
-define(STRING, [{string, <<"a">>},
                 {string, <<"abcd">>},
                 {string, <<"a":(33*8)>>},
                 {string, <<"a":(260*8)>>},
                 {string, ?LARGE_STRING}
                ]).
-define(TAGSR, [{tag, timestamp, jhn_timestamp:gen([binary])},
                {tag, posix, null},
                {tag, posix, undefined},
                {tag, bignum, 5},
                {tag, bignum, 0},
                {tag, bignum, -5},
                {tag, decimal_fraction, {0, 1}},
                {tag, bigfloat, {0, 1}},
                {tag, uri, jhn_uri:decode(<<"https://foo.bar?epp=peppa">>)},
                {tag, base64url, <<"https://foo.bar?epp=peppa">>},
                {tag, base64, <<"https://foo.bar?epp=peppa">>},
                {tag, mac, <<"00-B0-D0-63-C2-26">>},
                {tag, mac, <<"00-B0-D0-63-C2-26-42-96">>},
                {tag, 6, <<"Bar">>},
                {tag, 42, <<"Foo">>},
                {tag, 256, <<"Bat">>},
                {tag, 65536, <<"Hnu">>},
                {tag, 4294967296, <<"Fluff">>} |
                [{tag, cbor, CBOR} || CBOR <- ?SIMPLE ++ ?INTEGER ++ ?FLOAT]
              ]).

-define(TAGS, [{tag, posix, jhn_timestamp:gen([posix])},
               {tag, posix, jhn_timestamp:gen([binary])}
              ]).

-define(REVERSIBLE_TERM,
        ?SIMPLE ++ ?INTEGER ++ ?FLOAT ++ ?MAP ++ ?ARRAY ++ ?TEXT ++
            ?STRING ++?TAGSR).

%% ===================================================================
%% jhn_cbor callbacks
%% ===================================================================
cbor_encode_tag(43, {string, <<"enc:", Binary/binary>>}, _) ->
    jhn_cbor:encode({string, jhn_blist:reverse(Binary)}).

cbor_decode_tag(43, {string, Binary}, _) ->
    {string, <<"dec:", (jhn_blist:reverse(Binary))/binary>>}.

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
                         jhn_cbor:decode(
                           iolist_to_binary(
                             jhn_cbor:encode(Term))))) ||
        Term <- ?REVERSIBLE_TERM].

%%--------------------------------------------------------------------
%% encode/2 <-> /decode/1
%%--------------------------------------------------------------------

encode_2_decode_1_test_() ->
    [?_test(?assertEqual(Term,
                         jhn_cbor:decode(
                             jhn_cbor:encode(Term, [binary])))) ||
        Term <- ?REVERSIBLE_TERM].

encode_2_decode_1_float_test_() ->
    [?_test(?assertEqual(Float,
                         jhn_cbor:decode(
                             jhn_cbor:encode({Form, Float}, [binary])))) ||
        Form <- [float16, float32, float64],
        Float <- [-1.0, 2.0, 0.0, inf, neg_inf, nan]].

encode_2_decode_1_strings_test_() ->
    Strings = iolist_to_binary([String || {string, String} <- ?STRING]),
    [?_test(?assertEqual({string, Strings},
                         jhn_cbor:decode(
                             jhn_cbor:encode({strings, ?STRING},
                                             [binary]))))].

encode_2_decode_1_texts_test_() ->
    Texts = iolist_to_binary(?TEXT),
    [?_test(?assertEqual({text, Texts},
                         jhn_cbor:decode(
                             jhn_cbor:encode({texts, ?TEXT},
                                             [binary]))))].

encode_2_decode_1_maps_test_() ->
    [?_test(?assertEqual({maps, ?MAP},
                         jhn_cbor:decode(
                             jhn_cbor:encode({maps, ?MAP},
                                             [binary]))))].

encode_2_decode_1_arrays_test_() ->
    [?_test(?assertEqual({arrays, ?ARRAY},
                         jhn_cbor:decode(
                             jhn_cbor:encode({arrays, ?ARRAY},
                                             [binary]))))].

encode_2_decode_1_timstamp_test_() ->
    Posix = jhn_timestamp:gen([posix]),
    TS  = {tag, timestamp, Posix},
    TS1 = jhn_timestamp:encode(jhn_timestamp:decode(Posix), [binary]),
    [?_test(?assertEqual({tag, timestamp, TS1},
                         jhn_cbor:decode( jhn_cbor:encode(TS, [binary]))))].

encode_2_decode_1_posix_test_() ->
    Posix = jhn_timestamp:gen([posix]),
    PosixF = Posix + 0.005,
    PosixFM = Posix + 0.000005,
    PosixFN = 1.000000005,
    PosixFZ = Posix + 0.0,
    TS  = {tag, posix, Posix},
    TSF  = {tag, posix, PosixF},
    TSFM  = {tag, posix, PosixFM},
    TSFN  = {tag, posix, PosixFN},
    TSFZ  = {tag, posix, PosixFZ},
    TS1 = jhn_timestamp:encode(jhn_timestamp:decode(Posix), [binary]),
    TS2 = {tag, posix, TS1},
    <<TSF1:19/binary, _:1/binary>> = TS1,
    [?_test(?assertEqual({tag, posix, TS1},
                         jhn_cbor:decode(jhn_cbor:encode(TS, [binary])))),
     ?_test(?assertEqual({tag, posix, <<TSF1/binary, ".005Z">>},
                         jhn_cbor:decode(jhn_cbor:encode(TSF, [binary])))),
     ?_test(?assertEqual({tag, posix, <<TSF1/binary, ".000005Z">>},
                         jhn_cbor:decode(jhn_cbor:encode(TSFM, [binary])))),
     ?_test(?assertEqual( {tag, posix, <<"1970-01-01T00:00:01.000000005Z">>},
                          jhn_cbor:decode( jhn_cbor:encode(TSFN, [binary])))),
     ?_test(?assertEqual({tag, posix, TS1},
                         jhn_cbor:decode( jhn_cbor:encode(TSFZ, [binary])))),
     ?_test(?assertEqual({tag, posix, TS1},
                         jhn_cbor:decode(jhn_cbor:encode(TS2, [binary]))))
    ].

encode_2_decode_1_embedded_test_() ->
    CBOR = [1.0],
    Embedded = jhn_cbor:encode(CBOR, [binary]),
    [?_test(?assertEqual({tag, embedded, Embedded},
                         jhn_cbor:decode(
                           jhn_cbor:encode({tag, embedded, CBOR}, [binary]))))].

encode_2_decode_1_uri_test_() ->
    URL = <<"https://foo.bar">>,
    [?_test(?assertEqual({tag, uri, jhn_uri:decode(URL)},
                         jhn_cbor:decode(
                           jhn_cbor:encode({tag, uri, URL}, [binary]))))].

encode_2_decode_1_callback_test_() ->
    O = [binary, {tag_callbacks, [{43, ?MODULE}]}],
    Enc = {tag, 43, {string, <<"enc:Foo">>}},
    Dec = {tag, 43, {string, <<"dec:Foo">>}},
    [?_test(?assertEqual(Dec, jhn_cbor:decode(jhn_cbor:encode(Enc, O), O)))].

encode_2_decode_1_int_bignum_test_() ->
    [?_test(?assertEqual({tag, bignum, 18446744073709551616},
                         jhn_cbor:decode(jhn_cbor:encode(18446744073709551616,
                                                         [binary])))),
     ?_test(?assertEqual({tag, bignum, -18446744073709551616},
                         jhn_cbor:decode(jhn_cbor:encode(-18446744073709551616,
                                                         [binary]))))
    ].

encode_2_decode_1_iolist_test_() ->
    [?_test(?assertMatch([_ | _], jhn_cbor:encode([1.0], [iolist])))].

%%--------------------------------------------------------------------
%% encode/2 <-> /decode/2
%%--------------------------------------------------------------------

encode_2_decode_2_continue_test_() ->
    [?_test(?assertMatch({[1.0], <<>>},
                         jhn_cbor:decode(jhn_cbor:encode([1.0], [binary]),
                                         [continue])))].
