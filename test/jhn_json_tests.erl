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
%%%   eunit unit tests for the json library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2021-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_json_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(BASE,
        [{<<"true">>, true},
         {<<"false">>, false},
         {<<"null">>, null},
%%         {<<"1">>, 1},
%%         {<<"1.0">>, 1.0},
         {<<"\"Foo\"">>, <<"Foo">>},
         {<<"{}">>, #{}},
         {<<"{\"empty\":[]}">>, #{<<"empty">> => []}},
         {<<"{\"empty\":{}}">>, #{<<"empty">> => #{}}},
         {<<"[]">>, []},
         {<<"[[]]">>, [[]]},
         {<<"[{}]">>, [#{}]},
         {<<"{\"one\":1}">>, #{<<"one">> => 1}},
%%         {<<"{\"one\":1,\"two\":2}">>, #{<<"one">> => 1, <<"two">> => 2}},
         {<<"{\"one\":1.0}">>, #{<<"one">> => 1.0}},
         %% {<<"{\"one\":0.1,\"two\":2.2}">>,
         %%  #{<<"one">> => 0.1, <<"two">> => 2.2}},
         {<<"{\"one\":null}">>, #{<<"one">> => null}},
         {<<"{\"one\":true}">>, #{<<"one">> => true}},
         {<<"{\"one\":false}">>, #{<<"one">> => false}},
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

-define(FLOATS_RAW,
        [0.0,
         0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001,
         0.1, 0.11, 0.101, 0.1001, 0.10001, 0.100001, 0.1000001,
         0.11, 0.011, 0.0011, 0.00011, 0.00011, 0.000011, 0.0000011,
         1.1e1, 1.1e2, 1.1e3, 1.1e4, 1.1e5, 1.1e6, 1.1e7, 1.1e8,
         0.1e1, 0.1e2, 0.1e3, 0.1e4, 0.1e5, 0.1e6, 0.1e7, 0.1e8,
         1.23456, 12.3456, 123.456, 1234.56, 12345.6, 123456.0,
         123456.0, 12345.6, 1234.56, 123.456, 12.3456,
         12345678919393939393.0, 4503599627370496.0, 1.0e-308,
         1/7
        ]).

-define(FLOATS_PLUS,
        [{iolist_to_binary([$[, hd(io_lib:format("~p", [F])), $]]), [F]} ||
            F <- ?FLOATS_RAW]).

-define(FLOATS_MINUS,
        [{iolist_to_binary([$[, hd(io_lib:format("~p", [-F])), $]]), [-F]} ||
            F <- ?FLOATS_RAW]).

-define(FLOATS, ?FLOATS_PLUS ++ ?FLOATS_MINUS).

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
-define(REVERSIBLE, ?BASE ++ ?STRING_ESCAPE ++ ?FLOATS).

-define(ATOM_STRINGS,
        [{<<"[\"foo\"]">>, [foo]},
         {<<"[\"foo bar\"]">>, ['foo bar']},
         {<<"[\"foo\\tbar\"]">>, ['foo\tbar']},
         {<<"[\"foo\\nbar\"]">>, ['foo\nbar']}
        ]).

-define(NON_LATIN,
        %% UTF-8 JSON                  UTF-8 string
        [{<<91,34,224,164,132,34,93>>, [<<224,164,132>>]}]).

-define(BAD_OPTS, [non_option]).

-define(BAD_JSON, [{[true]}, {[<<"Foo">>]}, {[{<<"one">>, 1} | true]},
                   [1 | 1],
                   {[{1, 1}]},
                   [{foo, bar}]
                  ]).

-define(WS_JSON, [{<<" {}">>, #{}}, {<<" { }">>, #{}}, {<<" { } ">>, #{}},
                  {<<" { \"one\" : 1 } ">>, #{<<"one">> => 1}},
                  {<<" []">>, []}, {<<" [ ]">>, []}, {<<" [ ] ">>, []},
                  {<<" [ 1e17 ] ">>, [1.0e17]},
                  {<<"[\"\\0\\a\\v\\s\\q\"]">>, [<<0, 7, 11, 32, $\\, $q>>]}
                 ]).

-define(BAD_BINARY, [
                     <<"<true>">>, <<"{\"one\":1]">>, <<"{\"one\":1">>,
                     <<"{\"one\":1 \"two\":2}">>, <<"{\"one\":1,\"two\":2,}">>,
                     <<"{,\"one\":1,\"two\":2}">>,
                     <<"{true:1}">>, <<"{\"one\" 1}">>,
                     <<"[true}">>, <<"[true">>, <<"[gnuba]">>, <<"[tru]">>,
                     <<"[1,]">>, <<"[,]">>, <<"[,1]">>,
                     <<"[1.]">>, <<"[\"\\u00\"]">>, <<"[\"\\u00q\"]">>]).

-define(POINTERS, [{<<>>, top},
                   {<<"/foo">>, [<<"foo">>]},
                   {<<"/foo/0">>, [<<"foo">>, 0]},
                   {<<"/">>, [<<>>]},
                   {<<"/a~1b">>, [<<"a/b">>]},
                   {<<"/c%d">>, [<<"c%d">>]},
                   {<<"/e^f">>, [<<"e^f">>]},
                   {<<"/g|h">>, [<<"g|h">>]},
                   {<<"/i\\\\j">>, [<<"i\\\\j">>]},
                   {<<"/k\"l">>, [<<"k\"l">>]},
                   {<<"/ ">>, [<<" ">>]},
                   {<<"/m~0n">>, [<<"m~n">>]},
                   {<<"/-">>, ['-']}
                  ]).

-define(POINTERS_ATOMS, [{<<"/foo">>, [foo]},
                         {<<"/foo/0">>, [foo, 0]}
                        ]).

-define(IS_BADARG(X), ?assertMatch({'EXIT', _}, catch X)).

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
    [?_test(?assertEqual(Result, iolist_to_binary(jhn_json:encode(Term)))) ||
        {Result, Term} <- ?REVERSIBLE].

%%--------------------------------------------------------------------
%% encode/2
%%--------------------------------------------------------------------
encode_2_test_() ->
    [?_test(?assertEqual(Result,
                         iolist_to_binary(jhn_json:encode(Term, [])))) ||
        {Result, Term} <- ?REVERSIBLE].

%%--------------------------------------------------------------------
%% encode/2 with binary
%%--------------------------------------------------------------------
encode_2_binary_test_() ->
    [?_test(?assertEqual(Result, jhn_json:encode(Term, [binary]))) ||
        {Result, Term} <- ?REVERSIBLE].

%%--------------------------------------------------------------------
%% encode/2 with iolist
%%--------------------------------------------------------------------
encode_2_iolist_test_() ->
    [?_test(?assertEqual(Result,
                         iolist_to_binary(jhn_json:encode(Term, [iolist])))) ||
        {Result, Term} <- ?REVERSIBLE].

%%--------------------------------------------------------------------
%% encode/2 with atom strings
%%--------------------------------------------------------------------
encode_2_atoms_test_() ->
    [?_test(?assertEqual(Result, iolist_to_binary(jhn_json:encode(Term)))) ||
        {Result, Term} <- ?ATOM_STRINGS
    ].


%%--------------------------------------------------------------------
%% encode/2 non latin
%%--------------------------------------------------------------------
encode_2_encodings_non_latin_test_() ->
    [
     ?_test(?assertEqual(Result,
                         iolist_to_binary(jhn_json:encode(String)))) ||
        {Result, String} <- ?NON_LATIN
    ].

%% ===================================================================
%% Bad json
%% ===================================================================
encode_1_bad_json_test_() ->
    [?_test(?IS_BADARG(jhn_json:encode(JSON))) || JSON <- ?BAD_JSON].

encode_2_bad_json_test_() ->
    [?_test(?IS_BADARG(jhn_json:encode(JSON, [binary]))) || JSON <- ?BAD_JSON].

%% %% ===================================================================
%% %% Decoding
%% %% ===================================================================

%%--------------------------------------------------------------------
%% decode/1
%%--------------------------------------------------------------------
decode_1_test_() ->
    [{JSON, ?_test(?assertEqual(Term, jhn_json:decode(JSON)))} ||
        {JSON, Term} <- ?REVERSIBLE
    ].

%%--------------------------------------------------------------------
%% decode/1 strings and escaped strings
%%--------------------------------------------------------------------
decode_1_string_escape_test_() ->
    [
     ?_test(?assertEqual(String, jhn_json:decode(JSON))) ||
        {JSON, String} <- ?STRING_ESCAPE
    ].


%%--------------------------------------------------------------------
%% decode/1 non latin
%%--------------------------------------------------------------------
decode_1_non_latin_test_() ->
    [
     ?_test(?assertEqual(String, jhn_json:decode(JSON))) ||
        {JSON, String} <- ?NON_LATIN
    ].

%%--------------------------------------------------------------------
%% decode/1 white space
%%--------------------------------------------------------------------
decode_1_ws_test_() ->
    [
     ?_test(?assertEqual(Term, jhn_json:decode(JSON))) ||
        {JSON, Term} <- ?WS_JSON
    ].

%%--------------------------------------------------------------------
%% decode/2
%%--------------------------------------------------------------------
decode_2_test_() ->
    [{JSON, ?_test(?assertEqual({Term, <<>>}, chunk(JSON)))} ||
        {JSON, Term} <- ?REVERSIBLE].

%%--------------------------------------------------------------------
%% decode/2 strings and escaped strings
%%--------------------------------------------------------------------
decode_2_string_escape_test_() ->
    [
     ?_test(?assertEqual({String, <<>>}, jhn_json:chunk(JSON))) ||
        {JSON, String} <- ?STRING_ESCAPE
    ].

%%--------------------------------------------------------------------
%% decode/2 non latin
%%--------------------------------------------------------------------
decode_2_non_latin_test_() ->
    [
     ?_test(?assertEqual({String, <<>>}, jhn_json:chunk(JSON))) ||
        {JSON, String} <- ?NON_LATIN
    ].

%%--------------------------------------------------------------------
%% decode/1 white space
%%--------------------------------------------------------------------
decode_2_ws_test_() ->
    [
     ?_test(?assertEqual({Term, <<>>}, jhn_json:chunk(JSON))) ||
        {JSON, Term} <- ?WS_JSON
    ].

%% ===================================================================
%% Bad BINARY
%% ===================================================================
decode_1_binary_test_() ->
    [?_test(?IS_BADARG(jhn_json:decode(B))) ||
        B <- ?BAD_BINARY].


%% ===================================================================
%% Pointer encoding
%% ===================================================================

%%--------------------------------------------------------------------
%% pointer/2
%%--------------------------------------------------------------------
pointer_2_test_() ->
    [
     ?_test(?assertEqual(Pointer,
                         iolist_to_binary(jhn_json:encode(Term, [pointer]))))
     || {Pointer, Term} <- ?POINTERS ++ ?POINTERS_ATOMS
    ].

%%--------------------------------------------------------------------
%% pointer/2 with binary
%%--------------------------------------------------------------------
pointer_2_binary_test_() ->
    [
     ?_test(
        ?assertEqual(Pointer, jhn_json:encode(Term, Opts)))
     || {Pointer, Term} <- ?POINTERS ++ ?POINTERS_ATOMS,
        Opts <- [[pointer, binary], [binary, pointer]]
    ].

%%--------------------------------------------------------------------
%% pointer/2 with iolist
%%--------------------------------------------------------------------
pointer_2_iolist_test_() ->
    [
     ?_test(
        ?assertEqual(Pointer,
                     iolist_to_binary(jhn_json:encode(Term, Opts))))
     || {Pointer, Term} <- ?POINTERS ++ ?POINTERS_ATOMS,
        Opts <- [[pointer, iolist], [iolist, pointer]]
    ].

%% ===================================================================
%% Pointer decoding
%% ===================================================================

%%--------------------------------------------------------------------
%% decode pointer/1
%%--------------------------------------------------------------------
decode_pointer_1_test_() ->
    [
     {Pointer, ?_test(?assertEqual(Term, jhn_json:decode(Pointer)))} ||
        {Pointer, Term} <- ?POINTERS
    ].

%% ===================================================================
%% Next
%% ===================================================================

%%--------------------------------------------------------------------
%% next/1
%%--------------------------------------------------------------------
next_1_test_() ->
    [{JSON, ?_test(?assertEqual({JSON, <<>>}, jhn_json:next(JSON)))} ||
        {JSON, _} <- ?REVERSIBLE
    ].

%%--------------------------------------------------------------------
%% next/1
%%--------------------------------------------------------------------
next_2_test_() ->
    [{JSON, ?_test(?assertEqual({JSON, <<>>}, next_chunk(JSON)))} ||
        {JSON, _} <- ?REVERSIBLE
    ].

%% ===================================================================
%% Eval
%% ===================================================================

-define(RFC6901_TEXT,
        <<"{\"foo\": [\"bar\", \"baz\"],
            \"\": 0,
            \"a/b\": 1,
            \"c%d\": 2,
            \"e^f\": 3,
            \"g|h\": 4,
            \"i\\j\": 5,
            \"k\\\"l\": 6,
            \" \": 7,
            \"m~n\": 8}">>).

-define(RFC6901_PV1,
        [
         {<<>>, jhn_json:decode(?RFC6901_TEXT)},
         {<<"/foo">>, [<<"bar">>, <<"baz">>]},
         {<<"/foo/0">>, <<"bar">>},
         {<<"/">>, 0},
         {<<"/a~1b">>, 1},
         {<<"/c%d">>, 2},
         {<<"/e^f">>, 3},
         {<<"/g|h">>, 4},
         {<<"/i\\j">>, 5},
         {<<"/k\\\"l">>, 6},
         {<<"/ ">>, 7},
         {<<"/m~0n">>, 8}
        ]).

-define(RFC6901_PV2,
        [
         {<<>>, jhn_json:decode(?RFC6901_TEXT)},
         {<<"/foo">>, [<<"bar">>, <<"baz">>]},
         {<<"/foo/0">>, <<"bar">>},
         {<<"/">>, 0},
         {<<"/a~1b">>, 1},
         {<<"/c%d">>, 2},
         {<<"/e^f">>, 3},
         {<<"/g|h">>, 4},
         {<<"/i\\j">>, 5},
         {<<"/k\"l">>, 6},
         {<<"/ ">>, 7},
         {<<"/m~0n">>, 8}
        ]).


%%--------------------------------------------------------------------
%% eval/2 binaries
%%--------------------------------------------------------------------

eval_2_binaries_test_() ->
    [{Pointer,
      ?_test(?assertEqual(Value,
                          jhn_json:eval(Pointer, ?RFC6901_TEXT)))} ||
        {Pointer, Value} <- ?RFC6901_PV1
    ].

%%--------------------------------------------------------------------
%% eval/2 binary pointer
%%--------------------------------------------------------------------

eval_2_binary_pointer_test_() ->
    Text = jhn_json:decode(?RFC6901_TEXT),
    [{Pointer,
      ?_test(?assertEqual(Value,
                          jhn_json:eval(Pointer, Text)))} ||
        {Pointer, Value} <- ?RFC6901_PV2
    ].

%% ===================================================================
%% Bad options
%% ===================================================================
bad_option_test_() ->
    [?_test(?IS_BADARG(jhn_json:encode({}, [Option]))) ||
        Option <- ?BAD_OPTS] ++
        [?_test(?IS_BADARG(jhn_json:decode(<<"{}">>, [Option]))) ||
            Option <- ?BAD_OPTS].

%% %% ===================================================================
%% %% Internal functions.
%% %% ===================================================================

chunk(<<H/utf8, T/binary>>) ->
    case jhn_json:decode(<<H/utf8>>, [stream]) of
        {more, More} -> chunk(T, More);
        X -> X
    end.

chunk(<<H/utf8, T/binary>>, M) ->
    case jhn_json:decode(<<H/utf8>>, M) of
        {more, More} -> chunk(T, More);
        {JSON, Rest} -> {JSON, Rest}
    end.


next_chunk(<<H/utf8, T/binary>>) ->
    case jhn_json:next(<<H/utf8>>) of
        {more, More} -> next_chunk(T, More);
        X -> X
    end.

next_chunk(<<H/utf8, T/binary>>, M) ->
    case jhn_json:next(<<H/utf8>>, M) of
        {more, More} -> next_chunk(T, More);
        {JSON, Rest} -> {JSON, Rest}
    end.
