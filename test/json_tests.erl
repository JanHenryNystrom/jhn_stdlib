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

%% Defines
-define(BASE,
        [{<<"{}">>, {[]}},
         {<<"{\"empty\":[]}">>, {[{<<"empty">>, []}]}},
         {<<"{\"empty\":{}}">>, {[{<<"empty">>, {[]}}]}},
         {<<"[]">>, []},
         {<<"[[]]">>, [[]]},
         {<<"[{}]">>, [{[]}]},
         {<<"{\"one\":1}">>, {[{<<"one">>, 1}]}},
         {<<"{\"one\":1,\"two\":2}">>, {[{<<"one">>, 1}, {<<"two">>, 2}]}},
         {<<"{\"one\":1.0}">>, {[{<<"one">>, 1.0}]}},
         {<<"{\"one\":0.1,\"two\":2.2}">>,
          {[{<<"one">>, 0.1}, {<<"two">>, 2.2}]}},
         {<<"{\"one\":null}">>, {[{<<"one">>, null}]}},
         {<<"{\"one\":true}">>, {[{<<"one">>, true}]}},
         {<<"{\"one\":false}">>, {[{<<"one">>, false}]}},
         {<<"[null,true,false]">>, [null, true, false]},
         {<<"[1,-1,1.0,-1.0]">>, [1, -1, 1.0, -1.0]}
        ]).

-define(ESCAPE,
        [{<<"[\"\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\"]">>,
          [<<0, 1, 2, 3, 4, 5, 6>>]},
         {<<"[\"\\u0007\\b\\t\\n\\u000B\\f\\r\"]">>,
          [<<7, 8, 9, 10, 11, 12, 13>>]},
         {<<"[\"\\u000E\\u000F\\u0010\\u0011\\u0012\\u0013\\u0014\"]">>,
          [<<14, 15, 16, 17, 18, 19, 20>>]},
         {<<"[\"\\u0015\\u0016\\u0017\\u0018\\u0019\\u001A\\u001B\"]">>,
          [<<21, 22, 23, 24, 25, 26, 27>>]},
         {<<"[\"\\u001C\\u001D\\u001E\\u001F\"]">>,
          [<<28, 29, 30, 31>>]},
         {<<"[\"\\\\\\/\\\"\"]">>,
          [<<"\\/\"">>]},
         {<<"[\"\\\"ABBA\\\"\"]">>,
          [<<"\"ABBA\"">>]}
        ]).

-define(STRING,
        [{<<"[\"one\"]">>, [<<"one">>]},
         {<<"[\"a\"]">>, [<<"a">>]},
         {<<"[\"one two\"]">>, [<<"one two">>]},
         {<<"[\" one\"]">>, [<<" one">>]},
         {<<"[\"two \"]">>, [<<"two ">>]},
         {<<"[\" one two \"]">>, [<<" one two ">>]}
        ]).

-define(STRING_ESCAPE, ?STRING ++ ?ESCAPE).

%% decode(JSON) = Term, encode(TERM) = JSON
-define(REVERSIBLE, ?BASE ++ ?STRING_ESCAPE).

-define(PLAIN_FORMATS,
        [utf8, {utf16, little}, {utf16, big}, {utf32, little}, {utf32, big}]).

-define(ENCODINGS,
        [utf8, {utf16, little}, {utf16, big}, {utf32, little}, {utf32, big}]).



%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

%%--------------------------------------------------------------------
%% encode/1
%%--------------------------------------------------------------------
encode_1_test_() ->
    [?_test(?assertEqual(Result, iolist_to_binary(json:encode(Term)))) ||
        {Result, Term} <- ?REVERSIBLE].

%%--------------------------------------------------------------------
%% encode/2
%%--------------------------------------------------------------------
encode_2_test_() ->
    [?_test(?assertEqual(Result, iolist_to_binary(json:encode(Term, [])))) ||
        {Result, Term} <- ?REVERSIBLE].

%%--------------------------------------------------------------------
%% encode/2 with binary
%%--------------------------------------------------------------------
encode_2_binary_test_() ->
    [?_test(?assertEqual(Result, json:encode(Term, [binary]))) ||
        {Result, Term} <- ?REVERSIBLE].

%%--------------------------------------------------------------------
%% encode/2 with iolist
%%--------------------------------------------------------------------
encode_2_iolist_test_() ->
    [?_test(?assertEqual(Result,
                         iolist_to_binary(json:encode(Term, [iolist])))) ||
        {Result, Term} <- ?REVERSIBLE].

%%--------------------------------------------------------------------
%% encode/2 with different encodings
%%--------------------------------------------------------------------
encode_2_encodings_test_() ->
    [?_test(?assertEqual(unicode:characters_to_binary(Result, latin1, Encoding),
                         iolist_to_binary(
                           json:encode(Term, [{encoding, Encoding}])))) ||
        {Result, Term} <- ?REVERSIBLE,
        Encoding <- ?ENCODINGS
    ].


%%--------------------------------------------------------------------
%% encode/2 with encoding = utf8 different plain
%%--------------------------------------------------------------------
encode_2_encodings_plains_test_() ->
    [
     ?_test(
        ?assertEqual(unicode:characters_to_binary(Result, latin1, Encoding),
                     iolist_to_binary(
                       json:encode([unicode:characters_to_binary(String,
                                                                 latin1,
                                                                 Plain)],
                                   [{encoding, Encoding},
                                    {plain_string, Plain}])))) ||
        {Result, [String]} <- ?STRING_ESCAPE,
        Plain <- ?PLAIN_FORMATS,
        Encoding <- ?ENCODINGS
    ].

%% ===================================================================
%% Decoding
%% ===================================================================

%%--------------------------------------------------------------------
%% decode/1
%%--------------------------------------------------------------------
decode_1_test_() ->
    [?_test(
        ?assertEqual(
           Term,
           json:decode(unicode:characters_to_binary(JSON, latin1, Encoding))))||
        {JSON, Term} <- ?REVERSIBLE,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% decode/2
%%--------------------------------------------------------------------
decode_2_test_() ->
    [?_test(
        ?assertEqual(
           Term,
           json:decode(unicode:characters_to_binary(JSON, latin1, Encoding),
                       []))) ||
        {JSON, Term} <- ?REVERSIBLE,
        Encoding <- ?ENCODINGS
    ].

%% ===================================================================
%% Internal functions.
%% ===================================================================

