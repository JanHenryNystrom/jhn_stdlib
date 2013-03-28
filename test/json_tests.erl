%%==============================================================================
%% Copyright 2013 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the json library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(json_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Includes
-define(CONVERSIONS,
        [{<<"{}">>, {[]}},
         {<<"[]">>, []},
         {<<"{\"one\":1}">>, {[{<<"one">>, 1}]}},
         {<<"{\"one\":1,\"two\":2}">>, {[{<<"one">>, 1}, {<<"two">>, 2}]}},
         {<<"{\"one\":1.0}">>, {[{<<"one">>, 1.0}]}},
         {<<"{\"one\":0.1,\"two\":2.2}">>,
          {[{<<"one">>, 0.1}, {<<"two">>, 2.2}]}},
         {<<"{\"one\":null}">>, {[{<<"one">>, null}]}},
         {<<"{\"one\":true}">>, {[{<<"one">>, true}]}},
         {<<"{\"one\":false}">>, {[{<<"one">>, false}]}},
         {<<"[null,true,false]">>, [null, true, false]}
        ]).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% encode/1
%%--------------------------------------------------------------------
encode_1_test_() ->
    [?_test(?assertEqual(Result, iolist_to_binary(json:encode(Term)))) ||
        {Result, Term} <- ?CONVERSIONS].

%%--------------------------------------------------------------------
%% encode/2
%%--------------------------------------------------------------------
encode_2_test_() ->
    [?_test(?assertEqual(Result, iolist_to_binary(json:encode(Term, [])))) ||
        {Result, Term} <- ?CONVERSIONS].

%%--------------------------------------------------------------------
%% encode/2 with binary
%%--------------------------------------------------------------------
encode_2_binary_test_() ->
    [?_test(?assertEqual(Result, json:encode(Term, [binary]))) ||
        {Result, Term} <- ?CONVERSIONS].

%%--------------------------------------------------------------------
%% encode/2 with iolist
%%--------------------------------------------------------------------
encode_2_iolist_test_() ->
    [?_test(?assertEqual(Result,
                         iolist_to_binary(json:encode(Term, [iolist])))) ||
        {Result, Term} <- ?CONVERSIONS].

%%--------------------------------------------------------------------
%% encode/2 with utf8
%%--------------------------------------------------------------------
encode_2_utf8_test_() ->
    [?_test(?assertEqual(Result,
                         iolist_to_binary(
                           json:encode(Term, [{encoding, utf8}])))) ||
        {Result, Term} <- ?CONVERSIONS].


%%--------------------------------------------------------------------
%% encode/2 with utf16_little
%%--------------------------------------------------------------------
encode_2_utf16_little_test_() ->
    [?_test(
        ?assertEqual(unicode:characters_to_binary(Result, latin1,
                                                  {utf16, little}),
                     iolist_to_binary(
                       json:encode(Term, [{encoding, {utf16, little}}])))) ||
        {Result, Term} <- ?CONVERSIONS].

%%--------------------------------------------------------------------
%% encode/2 with utf16_big
%%--------------------------------------------------------------------
encode_2_utf16_big_test_() ->
    [?_test(
        ?assertEqual(unicode:characters_to_binary(Result, latin1,
                                                  {utf16, big}),
                     iolist_to_binary(
                       json:encode(Term, [{encoding, {utf16, big}}])))) ||
        {Result, Term} <- ?CONVERSIONS].

%%--------------------------------------------------------------------
%% encode/2 with utf32_little
%%--------------------------------------------------------------------
encode_2_utf132_little_test_() ->
    [?_test(
        ?assertEqual(unicode:characters_to_binary(Result, latin1,
                                                  {utf32, little}),
                     iolist_to_binary(
                       json:encode(Term, [{encoding, {utf32, little}}])))) ||
        {Result, Term} <- ?CONVERSIONS].

%%--------------------------------------------------------------------
%% encode/2 with utf32_big
%%--------------------------------------------------------------------
encode_2_utf32_big_test_() ->
    [?_test(
        ?assertEqual(unicode:characters_to_binary(Result, latin1,
                                                  {utf32, big}),
                     iolist_to_binary(
                       json:encode(Term, [{encoding, {utf32, big}}])))) ||
        {Result, Term} <- ?CONVERSIONS].

%%--------------------------------------------------------------------
%% decode/1
%%--------------------------------------------------------------------
decode_1_test_() ->
    [?_test(?assertEqual(Term, json:decode(JSON))) ||
        {JSON, Term} <- ?CONVERSIONS].

%%--------------------------------------------------------------------
%% decode/2
%%--------------------------------------------------------------------
decode_2_test_() ->
    [?_test(?assertEqual(Term, json:decode(JSON, []))) ||
        {JSON, Term} <- ?CONVERSIONS].



%% ===================================================================
%% Internal functions.
%% ===================================================================

