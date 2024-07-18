%%==============================================================================
%% Copyright 2020-2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the uuid library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_uuid_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(NIL, <<"00000000-0000-0000-0000-000000000000">>).
-define(MAX, <<"FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF">>).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% Generate
%%--------------------------------------------------------------------
gen_1_test_() ->
    [{"gen(v1)", ?_test(?assertMatch([_ | _], jhn_uuid:gen(v1)))},
     {"gen(v4)", ?_test(?assertMatch([_ | _], jhn_uuid:gen(v4)))},
     {"gen(v6)", ?_test(?assertMatch([_ | _], jhn_uuid:gen(v6)))},
     {"gen(v7)", ?_test(?assertMatch([_ | _], jhn_uuid:gen(v7)))},
     {"gen(nil)", ?_test(?assertMatch(?NIL, jhn_uuid:gen(nil)))},
     {"gen(max)", ?_test(?assertMatch(?MAX, jhn_uuid:gen(max)))},
     {"gen(v2)", ?_test(?assertError(badarg, jhn_uuid:gen(v2)))},
     {"gen(v2)", ?_test(?assertError(badarg, jhn_uuid:gen(v3)))},
     {"gen(v2)", ?_test(?assertError(badarg, jhn_uuid:gen(v5)))}
    ].

gen_2_test_() ->
    [%% list, urn v1, v4, v6, v7, v8, nil, max
     {"gen(v1, [list])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v1, [list])))},
     {"gen(v1, [list, urn])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v1, [list, urn])))},
     {"gen(v1, [list, hex])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v1, [list, hex])))},
     {"gen(v4, [list])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v4, [list])))},
     {"gen(v4, [list, urn])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v4, [list, urn])))},
     {"gen(v4, [list, hex])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v4, [list, hex])))},
     {"gen(v6, [list])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v6, [list])))},
     {"gen(v6, [list])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v6, [list, urn])))},
     {"gen(v7, [list])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v7, [list])))},
     {"gen(v7, [list])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v7, [list, urn])))},
     {"gen(nil, [list])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(nil, [list])))},
     {"gen(nil, [list])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(nil, [list, urn])))},
     {"gen(max, [list])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(max, [list])))},
     {"gen(max, [list])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(max, [list, urn])))},
     %% iolist, urn v1, v4, v6, v7, v8, nil, max
     {"gen(v1, [iolist])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v1, [iolist])))},
     {"gen(v1, [iolist, urn])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v1, [iolist, urn])))},
     {"gen(v4, [iolist])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v4, [iolist])))},
     {"gen(v4, [iolist, urn])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v4, [iolist, urn])))},
     {"gen(v6, [iolist])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v6, [iolist])))},
     {"gen(v6, [iolist, urn])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v6, [iolist, urn])))},
     {"gen(v6, [iolist, hex])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v6, [iolist, hex])))},
     {"gen(v7, [iolist])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v7, [iolist])))},
     {"gen(v7, [iolist, urn])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v7, [iolist, urn])))},
     {"gen(nil, [iolist])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(nil, [iolist])))},
     {"gen(nil, [iolist, urn])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(nil, [iolist, urn])))},
     {"gen(max, [iolist])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(max, [iolist])))},
     {"gen(max, [iolist, urn])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(max, [iolist, urn])))},
     %% binary, urn v1, v4, v6, v7, v8, nil, max
     {"gen(v1, [binary])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(v1, [binary])))},
     {"gen(v1, [binary, urn])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(v1, [binary, urn])))},
     {"gen(v4, [binary])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(v4, [binary])))},
     {"gen(v4, [binary, urn])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(v4, [binary, urn])))},
     {"gen(v6, [binary])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(v6, [binary])))},
     {"gen(v6, [binary, urn])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(v6, [binary, urn])))},
     {"gen(v7, [binary])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(v7, [binary])))},
     {"gen(v7, [binary, urn])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(v7, [binary, urn])))},
     {"gen(v7, [binary, hex])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(v7, [binary, hex])))},
     {"gen(nil, [binary])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(nil, [binary])))},
     {"gen(nil, [binary, urn])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(nil, [binary, urn])))},
     {"gen(max, [binary])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(max, [binary])))},
     {"gen(max, [binary, urn])",
      ?_test(?assertMatch(<<_/binary>>, jhn_uuid:gen(max, [binary, urn])))},
     %% uuid v1, v4, v6, v7, v8, nil, max
     {"gen(v1, [uuid])",
      ?_test(?assertMatch(#{version := 1,
                            node  := _,
                            clock_sequence := _,
                            timestamp := _},
                          jhn_uuid:gen(v1, [uuid])))},
     {"gen(v1, [uuid, human])",
      ?_test(?assertMatch(#{version := 1,
                            node  := _,
                            clock_sequence := _,
                            timestamp := _},
                          jhn_uuid:gen(v1, [uuid, human])))},
     {"gen(v4, [uuid])",
      ?_test(?assertMatch(#{version := 4, random := _},
                          jhn_uuid:gen(v4, [uuid])))},
     {"gen(v6, [uuid])",
      ?_test(?assertMatch(#{version := 6,
                            node  := _,
                            clock_sequence := _,
                            timestamp := _},
                          jhn_uuid:gen(v6, [uuid])))},
     {"gen(v6, [uuid, human])",
      ?_test(?assertMatch(#{version := 6,
                            node  := _,
                            clock_sequence := _,
                            timestamp := _},
                          jhn_uuid:gen(v6, [uuid, human])))},
     {"gen(v7, [uuid])",
      ?_test(?assertMatch(#{timestamp := _, random := _},
                          jhn_uuid:gen(v7, [uuid])))},
     {"gen(v7, [uuid, human])",
      ?_test(?assertMatch(#{timestamp := _, random := _},
                          jhn_uuid:gen(v7, [uuid, human])))},
     {"gen(nil, [uuid])", ?_test(?assertMatch(nil, jhn_uuid:gen(nil, [uuid])))},
     {"gen(max, [uuid])", ?_test(?assertMatch(max, jhn_uuid:gen(max, [uuid])))},
     %% name v3, v5
     {"gen(v3, [{name, \"foo\"}])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v3, [{name, "foo"}])))},
     {"gen(v3, [{name, <<\"foo\">>}])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v3, [{name, <<"foo">>}])))},
     {"gen(v3, [{name, [$f, <<\"oo\">>]}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v3, [{name, [$f, <<"oo">>]}])))},
     {"gen(v5, [{name, \"foo\"}])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v5, [{name, "foo"}])))},
     {"gen(v5, [{name, <<\"foo\">>}])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v5, [{name, <<"foo">>}])))},
     {"gen(v5, [{name, [$f, <<\"oo\">>]}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v5, [{name, [$f, <<"oo">>]}])))},
     %% name namespace v3, v5
     {"gen(v3, [{name, \"foo\"}, {name_space, dns}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v3, [{name, "foo"},
                                            {name_space, dns}])))},
     {"gen(v3, [{name, \"foo\"}, {name_space, url}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v3, [{name, "foo"},
                                            {name_space, url}])))},
     {"gen(v3, [{name, \"foo\"}, {name_space, oid}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v3, [{name, "foo"},
                                            {name_space, oid}])))},
     {"gen(v3, [{name, \"foo\"}, {name_space, x500}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v3, [{name, "foo"},
                                            {name_space, x500}])))},
     {"gen(v3, [{name, \"foo\"}, {name_space, nil}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v3, [{name, "foo"},
                                            {name_space, nil}])))},
     {"gen(v3, [{name, \"foo\"}, {name_space, max}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v3, [{name, "foo"},
                                            {name_space, max}])))},
     {"gen(v3, [{name, \"foo\"}, {name_space, \"bar\"}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v3, [{name, "foo"},
                                            {name_space, "bar"}])))},
     {"gen(v3, [{name, \"foo\"}, {name_space, <<\"bar\">>}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v3, [{name, "foo"},
                                            {name_space, <<"bar">>}])))},
     {"gen(v3, [{name, \"foo\"}, {name_space, [$b, <<\"br\">>]}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v3, [{name, "foo"},
                                            {name_space, [$b, <<"ar">>]}])))},
     {"gen(v5, [{name, \"foo\"}, {name_space, dns}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v5, [{name, "foo"},
                                            {name_space, dns}])))},
     {"gen(v5, [{name, \"foo\"}, {name_space, url}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v5, [{name, "foo"},
                                            {name_space, url}])))},
     {"gen(v5, [{name, \"foo\"}, {name_space, oid}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v5, [{name, "foo"},
                                            {name_space, oid}])))},
     {"gen(v5, [{name, \"foo\"}, {name_space, x500}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v5, [{name, "foo"},
                                            {name_space, x500}])))},
     {"gen(v5, [{name, \"foo\"}, {name_space, nil}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v5, [{name, "foo"},
                                            {name_space, nil}])))},
     {"gen(v5, [{name, \"foo\"}, {name_space, max}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v5, [{name, "foo"},
                                            {name_space, max}])))},
     {"gen(v5, [{name, \"foo\"}, {name_space, \"bar\"}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v5, [{name, "foo"},
                                            {name_space, "bar"}])))},
     {"gen(v5, [{name, \"foo\"}, {name_space, <<\"bar\">>}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v5, [{name, "foo"},
                                            {name_space, <<"bar">>}])))},
     {"gen(v5, [{name, \"foo\"}, {name_space, [$b, <<\"br\">>]}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v5, [{name, "foo"},
                                            {name_space, [$b, <<"ar">>]}])))},
     %% node uuid, binary
     {"gen(v1, [{node, undefined}])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v1, [{node, undefined}])))},
     {"gen(v1, [{node, random}])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v1, [{node, random}])))},
     {"gen(v1, [{node, 123456789}])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v1, [{node, 123456789}])))},
     {"gen(v1, [{node, <<\"00-B0-D0-63-C2-26\">>}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v1,
                                       [{node, <<"00-B0-D0-63-C2-26">>}])))},
     {"gen(v1, [uuid, {node, undefined}])",
      ?_test(?assertMatch(#{version := 1},
                          jhn_uuid:gen(v1, [uuid, {node, undefined}])))},
     {"gen(v1, [uuid, {node, random}])",
      ?_test(?assertMatch(#{version := 1},
                          jhn_uuid:gen(v1, [uuid, {node, random}])))},
     {"gen(v1, [uuid, {node, 123456789}])",
      ?_test(?assertMatch(#{version := 1},
                          jhn_uuid:gen(v1, [uuid, {node, 123456789}])))},
     {"gen(v1, [uuid, {node, <<\"00-B0-D0-63-C2-26\">>}])",
      ?_test(
         ?assertMatch(#{version := 1},
                      jhn_uuid:gen(v1,
                                   [uuid, {node,  <<"00-B0-D0-63-C2-26">>}])))},
     {"gen(v6, [{node, undefined}])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v6, [{node, undefined}])))},
     {"gen(v6, [{node, random}])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v6, [{node, random}])))},
     {"gen(v6, [{node, 123456789}])",
      ?_test(?assertMatch([_ | _], jhn_uuid:gen(v6, [{node, 123456789}])))},
     {"gen(v6, [{node, <<\"00-B0-D0-63-C2-26\">>}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v6,
                                       [{node, <<"00-B0-D0-63-C2-26">>}])))},
     {"gen(v6, [uuid, {node, undefined}])",
      ?_test(?assertMatch(#{version := 6},
                          jhn_uuid:gen(v6, [uuid, {node, undefined}])))},
     {"gen(v6, [uuid, {node, random}])",
      ?_test(?assertMatch(#{version := 6},
                          jhn_uuid:gen(v6, [uuid, {node, random}])))},
     {"gen(v6, [uuid, {node, 123456789}])",
      ?_test(?assertMatch(#{version := 6},
                          jhn_uuid:gen(v6, [uuid, {node, 123456789}])))},
     {"gen(v6, [uuid, {node, <<\"00-B0-D0-63-C2-26\">>}])",
      ?_test(
         ?assertMatch(#{version := 6},
                      jhn_uuid:gen(v6,
                                   [uuid, {node,  <<"00-B0-D0-63-C2-26">>}])))},
     %% node clock_sequence
     {"gen(v1, [{node, undefined}, {clock_sequence, 9876543}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v1, [{node, undefined},
                                            {clock_sequence, 9876543}])))},
     {"gen(v6, [{node, undefined}, {clock_sequence, 9876543}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v6, [{node, undefined},
                                            {clock_sequence, 9876543}])))},
     %% custom v8
     {"gen(v8, [{custom, Integer}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v8, [{custom, custom(integer)}])))},
     {"gen(v8, [{custom, Binary}])",
      ?_test(?assertMatch([_ | _],
                          jhn_uuid:gen(v8, [{custom, custom(binary)}])))},
     {"gen(v2, [binary])",
      ?_test(?assertError(badarg, jhn_uuid:gen(v2, [binary])))}
    ].

%%--------------------------------------------------------------------
%% Encode
%%--------------------------------------------------------------------

encode_2_test_() ->
    [{"encode(gen(v4), [human])",
      ?_test(?assertError(badarg,
                          jhn_uuid:encode(jhn_uuid:gen(v4), [human])))},
     {"encode(gen(v4), [uuid])",
      ?_test(?assertError(badarg,
                          jhn_uuid:encode(jhn_uuid:gen(v4), [uuid])))},
     {"encode(decode(gen(v1, [binary]), [uuid]), [human])",
      ?_test(?assertMatch(<<_:36/binary>>,
                          jhn_uuid:encode(
                            jhn_uuid:decode(jhn_uuid:gen(v1, [binary]),
                                            [human]),
                            [binary])))},
     {"encode(decode(gen(v6, [binary]), [uuid]), [human])",
      ?_test(?assertMatch(<<_:36/binary>>,
                          jhn_uuid:encode(
                            jhn_uuid:decode(jhn_uuid:gen(v6, [binary]),
                                            [human]),
                            [binary])))},
     {"encode(decode(gen(v7, [binary]), [uuid]), [human])",
      ?_test(?assertMatch(<<_:36/binary>>,
                          jhn_uuid:encode(
                            jhn_uuid:decode(jhn_uuid:gen(v7, [binary]),
                                            [human]),
                            [binary])))}

    ].

%%--------------------------------------------------------------------
%% Decode
%%--------------------------------------------------------------------
decode_2_test_() ->
    [{"decode(gen(v1, [binary], human)",
      ?_test(?assertMatch(#{version := 1},
                          jhn_uuid:decode(
                            jhn_uuid:gen(v1, [binary]), [human])))},
     {"decode(gen(v6, [binary], human)",
      ?_test(?assertMatch(#{version := 6},
                          jhn_uuid:decode(
                            jhn_uuid:gen(v6, [binary]), [human])))},
     {"decode(gen(v7, [binary], human)",
      ?_test(?assertMatch(#{version := 7},
                          jhn_uuid:decode(
                            jhn_uuid:gen(v7, [binary]), [human])))},
     {"decode(gen(v7, [binary, hex], human)",
      ?_test(?assertMatch(#{version := 7},
                          jhn_uuid:decode(
                            jhn_uuid:gen(v7, [binary, hex]), [human])))}
    ].

%%--------------------------------------------------------------------
%% Gen/Encode/Decode/Encode/Decode
%%--------------------------------------------------------------------
gen_enc_dec_enc_dec_test_() ->
    [{"v1", ?_test(?assertMatch(true, gen_enc_dec_enc_dec(v1)))},
     {"v3", ?_test(?assertMatch(true, gen_enc_dec_enc_dec(v3, "foo")))},
     {"v4", ?_test(?assertMatch(true, gen_enc_dec_enc_dec(v4)))},
     {"v5", ?_test(?assertMatch(true, gen_enc_dec_enc_dec(v5, "foo")))},
     {"v6", ?_test(?assertMatch(true, gen_enc_dec_enc_dec(v6)))},
     {"v7", ?_test(?assertMatch(true, gen_enc_dec_enc_dec(v7)))},
     {"v7", ?_test(?assertMatch(true, gen_enc_dec_enc_dec(v8)))},
     {"nil", ?_test(?assertMatch(true, gen_enc_dec_enc_dec(nil)))},
     {"max", ?_test(?assertMatch(true, gen_enc_dec_enc_dec(max)))}
    ].


%% ===================================================================
%% Internal functions.
%% ===================================================================

gen_enc_dec_enc_dec(v8) ->
    <<Custom:122, _:6>> = crypto:strong_rand_bytes(16),
    UUID = jhn_uuid:gen(v8, [uuid, {custom, Custom}]),
    Enc = jhn_uuid:encode(UUID, [binary, urn]),
    Dec = jhn_uuid:decode(Enc, []),
    Enc1 = jhn_uuid:encode(Dec, [binary]),
    Dec == jhn_uuid:decode(Enc1);
gen_enc_dec_enc_dec(Type) ->
    UUID = jhn_uuid:gen(Type, [uuid]),
    Enc = jhn_uuid:encode(UUID, [binary, urn]),
    Dec = jhn_uuid:decode(Enc, []),
    Enc1 = jhn_uuid:encode(Dec, [binary]),
    Dec == jhn_uuid:decode(Enc1).

gen_enc_dec_enc_dec(Type, Name) when Type == v3; Type == v5->
    UUID = jhn_uuid:gen(Type, [uuid, {name, Name}]),
    Enc = jhn_uuid:encode(UUID, [binary, urn]),
    Dec = jhn_uuid:decode(Enc, []),
    Enc1 = jhn_uuid:encode(Dec, [binary]),
    Dec == jhn_uuid:decode(Enc1).


custom(binary) -> <<(custom(integer)):122>>;
custom(integer) ->
    <<Custom:122, _:6>> = crypto:strong_rand_bytes(16),
    Custom.

