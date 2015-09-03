%%==============================================================================
%% Copyright 2013-2015 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%% @copyright (C) 2013-2015, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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

-define(BAD_OPTS, [{atom_strings, null},
                   {plain_string, utf99},
                   {encoding, latin44},
                   non_option
                  ]).

-define(BAD_JSON, [true, false, null, foo, 1, 1.0, <<"Foo">>,
                   {[true]}, {[<<"Foo">>]}, {[{<<"one">>, 1} | true]},
                   [1 | 1],
                   {[{1, 1}]},
                   [{foo, bar}]
                  ]).

-define(WS_JSON, [{<<" {}">>, {[]}}, {<<" { }">>, {[]}}, {<<" { } ">>, {[]}},
                  {<<" { \"one\" : 1 } ">>, {[{<<"one">>, 1}]}},
                  {<<" []">>, []}, {<<" [ ]">>, []}, {<<" [ ] ">>, []},
                  {<<" [ 1e17 ] ">>, [1.0e17]},
                  {<<"[\"\\0\\a\\v\\s\\q\"]">>, [<<0, 7, 11, 32, $\\, $q>>]}
                 ]).

-define(BAD_BINARY, [<<"true">>, <<"false">>, <<"null">>, <<"1">>, <<"1.0">>,
                     <<"<true>">>, <<"{\"one\":1]">>, <<"{\"one\":1">>,
                     <<"{\"one\":1 \"two\":2}">>, <<"{\"one\":1,\"two\":2,}">>,
                     <<"{,\"one\":1,\"two\":2}">>,
                     <<"{true:1}">>, <<"{\"one\" 1}">>,
                     <<"[true}">>, <<"[true">>, <<"[gnuba]">>, <<"[tru]">>,
                     <<"[1,]">>, <<"[,]">>, <<"[,1]">>,
                     <<"[1.]">>, <<"[\"\\u00\"]">>, <<"[\"\\u00q\"]">>]).

-define(ATOM_KEYS, [{<<"{}">>, {[]}},
                    {<<"{\"one\":1}">>, {[{one, 1}]}}
                   ]).

-define(EXISTING_ATOM_KEYS, [{<<"{}">>, {[]}},
                             {<<"{\"one\":1}">>, {[{one, 1}]}}
                            ]).

-define(POINTERS, [{<<"">>, []},
                   {<<"/foo">>, [<<"foo">>]},
                   {<<"/foo">>, [foo]},
                   {<<"/foo/0">>, [foo, 0]},
                   {<<"/">>, [<<"">>]},
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

-define(IS_BADARG(X), ?assertMatch({'EXIT', {badarg, _}}, catch X)).

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
    [?_test(?assertEqual(utf(Result, utf8, Encoding),
                         iolist_to_binary(
                           json:encode(Term, [{encoding, Encoding}])))) ||
        {Result, Term} <- ?REVERSIBLE,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% encode/2 with different encodings and bom
%%--------------------------------------------------------------------
encode_2_encodings_bom_test_() ->
    [?_test(?assertEqual(<<(unicode:encoding_to_bom(Encoding))/binary,
                           (utf(Result, utf8, Encoding))/binary>>,
                         iolist_to_binary(
                           json:encode(Term, [bom, {encoding, Encoding}])))) ||
        {Result, Term} <- ?REVERSIBLE,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% encode/2 with different encodings and bom and binary
%%--------------------------------------------------------------------
encode_2_encodings_bom_binary_test_() ->
    [?_test(?assertEqual(<<(unicode:encoding_to_bom(Encoding))/binary,
                           (utf(Result, utf8, Encoding))/binary>>,
                         json:encode(Term,
                                     [bom,
                                      binary,
                                      {encoding, Encoding}]))) ||
        {Result, Term} <- ?REVERSIBLE,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% encode/2 with atom strings
%%--------------------------------------------------------------------
encode_2_atoms_test_() ->
    [?_test(?assertEqual(utf(Result, utf8, Encoding),
                         iolist_to_binary(
                           json:encode(Term, [{encoding, Encoding}])))) ||
        {Result, Term} <- ?ATOM_STRINGS,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% encode/2 with atom strings and the flags set
%%--------------------------------------------------------------------
encode_2_atoms_strings_test_() ->
    [?_test(?assertEqual(utf(Result, utf8, Encoding),
                         iolist_to_binary(
                           json:encode(Term, [{encoding, Encoding},
                                              {atom_strings, true}
                                             ])))) ||
        {Result, Term} <- ?ATOM_STRINGS,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% encode/2 with different encodings and  plains
%%--------------------------------------------------------------------
encode_2_encodings_plains_test_() ->
    [
     ?_test(?assertEqual(utf(Result, utf8, Encoding),
                         iolist_to_binary(
                           json:encode([utf(String, utf8, Plain)],
                                       [{encoding, Encoding},
                                        {plain_string, Plain}])))) ||
        {Result, [String]} <- ?STRING_ESCAPE,
        Plain <- ?PLAIN_FORMATS,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% encode/2 with different encodings and non latin plains
%%--------------------------------------------------------------------
encode_2_encodings_non_latin_plains_test_() ->
    [
     ?_test(?assertEqual(utf(Result, utf8, Encoding),
                         iolist_to_binary(
                           json:encode([utf(String, utf8, Plain)],
                                       [{encoding, Encoding},
                                        {plain_string, Plain}])))) ||
        {Result, [String]} <- ?NON_LATIN,
        Plain <- ?PLAIN_FORMATS,
        Encoding <- ?ENCODINGS
    ].
%% ===================================================================
%% Bad json
%% ===================================================================
encode_1_bad_json_test_() ->
    [?_test(?IS_BADARG(json:encode(JSON))) || JSON <- ?BAD_JSON].

encode_2_bad_json_test_() ->
    [?_test(?IS_BADARG(json:encode(JSON, [binary]))) || JSON <- ?BAD_JSON].

%% ===================================================================
%% Decoding
%% ===================================================================

%%--------------------------------------------------------------------
%% decode/1
%%--------------------------------------------------------------------
decode_1_test_() ->
    [?_test(?assertEqual(Term, json:decode(utf(JSON, utf8, Encoding)))) ||
        {JSON, Term} <- ?REVERSIBLE,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% decode/2
%%--------------------------------------------------------------------
decode_2_test_() ->
    [?_test(?assertEqual(Term, json:decode(utf(JSON, utf8, Encoding), []))) ||
        {JSON, Term} <- ?REVERSIBLE,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% decode/2 with bom
%%--------------------------------------------------------------------
decode_2_bom_test_() ->
    [?_test(
        ?assertEqual(Term,
                     json:decode(<<(unicode:encoding_to_bom(Encoding))/binary,
                                   (utf(JSON, utf8, Encoding))/binary>>,
                                 [bom]))) ||
        {JSON, Term} <- ?REVERSIBLE,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% decode/2 with atom_keys
%%--------------------------------------------------------------------
decode_2_atom_keys_test_() ->
    [?_test(?assertEqual(Term,
                         json:decode(utf(JSON, utf8, Encoding),
                                     [{atom_keys, true}]))) ||
        {JSON, Term} <- ?ATOM_KEYS,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% decode/2 with existing_atom_keys
%%--------------------------------------------------------------------
decode_2_existing_atom_keys_test_() ->
    [?_test(?assertEqual(Term,
                         json:decode(utf(JSON, utf8, Encoding),
                                     [{existing_atom_keys, true}]))) ||
        {JSON, Term} <- ?ATOM_KEYS,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% decode/2 with different encodings and  plains
%%--------------------------------------------------------------------
decode_2_encodings_plains_test_() ->
    [
     ?_test(?assertEqual([utf(String, utf8, Plain)],
                         json:decode(utf(JSON, utf8, Encoding),
                                     [{plain_string, Plain}]))) ||
        {JSON, [String]} <- ?STRING_ESCAPE,
        Plain <- ?PLAIN_FORMATS,
        Encoding <- ?ENCODINGS
    ].


%%--------------------------------------------------------------------
%% decode/2 with different encodings and non latin plains
%%--------------------------------------------------------------------
decode_2_encodings_non_latin_plains_test_() ->
    [
     ?_test(?assertEqual([utf(String, utf8, Plain)],
                         json:decode(utf(JSON, utf8, Encoding),
                                     [{plain_string, Plain}]))) ||
        {JSON, [String]} <- ?NON_LATIN,
        Plain <- ?PLAIN_FORMATS,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% decode/1 with different encodings and white space
%%--------------------------------------------------------------------
decode_1_ws_encodings_test_() ->
    [
     ?_test(?assertEqual(Term, json:decode(utf(JSON, utf8, Encoding)))) ||
        {JSON, Term} <- ?WS_JSON,
        Encoding <- ?ENCODINGS
    ].

%%--------------------------------------------------------------------
%% decode/2 with different encodings and white space
%%--------------------------------------------------------------------
decode_2_ws_encodings_test_() ->
    [
     ?_test(?assertEqual(Term, json:decode(utf(JSON, utf8, Encoding), []))) ||
        {JSON, Term} <- ?WS_JSON,
        Encoding <- ?ENCODINGS
    ].


%% ===================================================================
%% Bad BINARY
%% ===================================================================
decode_1_binary_test_() ->
    [?_test(?IS_BADARG(json:decode(utf(B, utf8, Encoding)))) ||
        B <- ?BAD_BINARY, Encoding <- ?ENCODINGS].

decode_2_binary_test_() ->
    [?_test(?IS_BADARG(json:decode(utf(B, utf8, Encoding), [binary]))) ||
        B <- ?BAD_BINARY, Encoding <- ?ENCODINGS].

%% ===================================================================
%% Pointer encoding
%% ===================================================================

%%--------------------------------------------------------------------
%% pointer/1
%%--------------------------------------------------------------------
pointer_1_test_() ->
    [
     ?_test(?assertEqual(Pointer, iolist_to_binary(json:pointer(Term)))) ||
        {Pointer, Term} <- ?POINTERS
    ].
%%--------------------------------------------------------------------
%% pointer/2
%%--------------------------------------------------------------------
pointer_2_test_() ->
    [
     ?_test(?assertEqual(Pointer, iolist_to_binary(json:pointer(Term, [])))) ||
        {Pointer, Term} <- ?POINTERS
    ].

%%--------------------------------------------------------------------
%% pointer/2 with binary
%%--------------------------------------------------------------------
pointer_2_binary_test_() ->
    [
     ?_test(
        ?assertEqual(Pointer,
                     iolist_to_binary(json:pointer(Term, [binary])))) ||
        {Pointer, Term} <- ?POINTERS
    ].

%%--------------------------------------------------------------------
%% pointer/2 with different plain and encoding
%%--------------------------------------------------------------------
pointer_2_encodings_plains_test_() ->
    [
     ?_test(?assertEqual(utf(Pointer, utf8, Encoding),
                         iolist_to_binary(
                           json:pointer(pointer_term_to_encoding(Term, Plain),
                                        [{pointer, Encoding},
                                         {plain_string, Plain}])))) ||
        {Pointer, Term} <- ?POINTERS,
        Plain <- ?PLAIN_FORMATS,
        Encoding <- ?ENCODINGS
    ].

pointer_term_to_encoding([], _) -> [];
pointer_term_to_encoding([H | T], Plain) when is_binary(H) ->
    [utf(H, utf8, Plain) | pointer_term_to_encoding(T, Plain)];
pointer_term_to_encoding([H | T], Plain) ->
    [H | pointer_term_to_encoding(T, Plain)].

%% ===================================================================
%% Bad options
%% ===================================================================
bad_option_test_() ->
    [?_test(?IS_BADARG(json:encode({}, [Option]))) || Option <- ?BAD_OPTS] ++
        [?_test(?IS_BADARG(json:decode(<<"{}">>, [Option]))) ||
            Option <- ?BAD_OPTS].

%% ===================================================================
%% Internal functions.
%% ===================================================================

utf(String, To, To) -> String;
utf(String, From, To) -> unicode:characters_to_binary(String, From, To).
