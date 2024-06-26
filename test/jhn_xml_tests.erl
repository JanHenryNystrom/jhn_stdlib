%%==============================================================================
%% Copyright 2020 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the JHN XML library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2020, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_xml_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").
-include_lib("jhn_stdlib/include/jhn_xml.hrl").

%%--------
%% Defines
%%--------


%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

%%--------------------------------------------------------------------
%% XML decl
%%--------------------------------------------------------------------
encode_decl_test_() ->
    [{"XML Decl standalone=yes",
      ?_test(
         ?assertMatch(<<"<?xml version=\"1.0\""
                        " encoding=\"UTF-8\" standalone=\"yes\"?>">>,
                      jhn_xml:encode(#decl{standalone = yes})))},
     {"XML Decl standalone=no",
      ?_test(
         ?assertMatch(<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>,
                      jhn_xml:encode(#decl{})))}
    ].

%%--------------------------------------------------------------------
%% CDATA
%%--------------------------------------------------------------------
encode_cdata_test_() ->
    [{"CDATA",
      ?_test(
         ?assertMatch(
            <<"<![CDATA[hallo]]>">>,
            iolist_to_binary(jhn_xml:encode(#cdata{data = <<"hallo">>}))))},
     {"CDATA nothing",
      ?_test(
         ?assertMatch(<<>>,
                      iolist_to_binary(jhn_xml:encode(#cdata{data = <<>>}))))}
    ].

%%--------------------------------------------------------------------
%% Comment
%%--------------------------------------------------------------------
encode_comment_test_() ->
    [{"Comment empty",
      ?_test(
         ?assertMatch(
            <<>>,
            iolist_to_binary(jhn_xml:encode(#comment{text = <<"">>}))))},
     {"Commment",
      ?_test(
         ?assertMatch(
            <<"<!--unimportant-->">>,
            iolist_to_binary(
              jhn_xml:encode(#comment{text = <<"unimportant">>}))))}
    ].

%%--------------------------------------------------------------------
%% PI
%%--------------------------------------------------------------------
encode_pi_test_() ->
    [{"PI empty data",
      ?_test(
         ?assertMatch(
            <<"<?foo?>">>,
            iolist_to_binary(jhn_xml:encode(#pi{target = <<"foo">>}))))},
     {"PI atom target empty data",
      ?_test(
         ?assertMatch(
            <<"<?foo?>">>,
            iolist_to_binary(jhn_xml:encode(#pi{target = foo}))))},
     {"PI",
      ?_test(
         ?assertMatch(
            <<"<?foo bar?>">>,
            iolist_to_binary(
              jhn_xml:encode(#pi{target = foo, data = <<"bar">>}))))}
    ].

%%--------------------------------------------------------------------
%% Doctype decl
%%--------------------------------------------------------------------
encode_doc_test_() ->
    [{"Doc foo empty",
      ?_test(
         ?assertMatch(
            <<"<!DOCTYPE foo>">>,
            iolist_to_binary(jhn_xml:encode(#doc{name = <<"foo">>}))))},
     {"Doc foo system bar empty",
      ?_test(
         ?assertMatch(
            <<"<!DOCTYPE foo SYSTEM 'bar'>">>,
            iolist_to_binary(
              jhn_xml:encode(#doc{name = foo, system = <<"bar">>}))))},
     {"Doc foo public bar bat empty",
      ?_test(
         ?assertMatch(
            <<"<!DOCTYPE foo PUBLIC 'bar' 'bat'>">>,
            iolist_to_binary(
              jhn_xml:encode(#doc{name = foo,
                                  system = <<"bat">>,
                                  public = <<"bar">>}))))},
     {"Doc foo parameter entity ref Bar",
      ?_test(
         ?assertMatch(
            <<"<!DOCTYPE foo [%Bar;]>">>,
            iolist_to_binary(
              jhn_xml:encode(#doc{name = foo,
                                  set = [#pe_ref{name = <<"Bar">>}]}))))},
     {"Doc foo parameter entity ref Bar atom",
      ?_test(
         ?assertMatch(
            <<"<!DOCTYPE foo [%Bar;]>">>,
            iolist_to_binary(
              jhn_xml:encode(#doc{name = foo,
                                  set = [#pe_ref{name = 'Bar'}]}))))}
    ].

%%--------------------------------------------------------------------
%% Element
%%--------------------------------------------------------------------
encode_element_test_() ->
    [{"<foo/>",
      ?_test(
         ?assertMatch(
            <<"<foo/>">>,
            iolist_to_binary(
              jhn_xml:encode(#elt{tag = foo}))))},
     {"<foo attr='value'/>",
      ?_test(
         ?assertMatch(
            <<"<foo attr='value'/>">>,
            iolist_to_binary(
              jhn_xml:encode(#elt{tag = foo,
                                  attrs = [{attr, <<"value">>}]}))))},
     {"<foo attr='value'/>",
      ?_test(
         ?assertMatch(
            <<"<foo attr='value'/>">>,
            iolist_to_binary(
              jhn_xml:encode(#elt{tag = foo,
                                  attrs = [{attr, value}]}))))},
     %% Is this somthing we want ?
     {"<foo attr='value'/>",
      ?_test(
         ?assertMatch(
            <<"<foo/>">>,
            iolist_to_binary(
              jhn_xml:encode(#elt{tag = foo,
                                  attrs = [{attr, undefined}]}))))},
     {"<foo attr1='value1' attr2='value2'/>",
      ?_test(
         ?assertMatch(
            <<"<foo attr1='value1' attr2='value2'/>">>,
            iolist_to_binary(
              jhn_xml:encode(#elt{tag = foo,
                                  attrs = [{attr1, <<"value1">>},
                                           {attr2, <<"value2">>}]}))))},
     {"<foo><bar/></foo>",
      ?_test(
         ?assertMatch(
            <<"<foo><bar/></foo>">>,
            iolist_to_binary(
              jhn_xml:encode(#elt{tag = <<"foo">>,
                                  children = [#elt{tag = bar}]}))))},
     {"<foo><bar><bat/></bar></foo>",
      ?_test(
         ?assertMatch(
            <<"<foo><bar><bat/></bar></foo>">>,
            iolist_to_binary(
              jhn_xml:encode(
                #elt{tag = foo,
                     children = [#elt{tag = bar,
                                      children = [#elt{tag = bat}]}]}))))},
     {"<foo><![CDATA[bar]]></foo>",
      ?_test(
         ?assertMatch(
            <<"<foo><![CDATA[bar]]></foo>">>,
            iolist_to_binary(
              jhn_xml:encode(#elt{tag = <<"foo">>,
                                  children = [#cdata{data = <<"bar">>}]}))))},
     {"<foo>bar</foo>",
      ?_test(
         ?assertMatch(
            <<"<foo>bar</foo>">>,
            iolist_to_binary(
              jhn_xml:encode(#elt{tag = <<"foo">>,
                                  children = [<<"bar">>]}))))},
     {"<foo>barbat</foo>",
      ?_test(
         ?assertMatch(
            <<"<foo>barbat</foo>">>,
            iolist_to_binary(
              jhn_xml:encode(#elt{tag = <<"foo">>,
                                  children = [<<"bar">>,<<"bat">>]}))))},
     {"<foo attr1='value1' attr2='value2'><bar attr='value'/></foo>",
      ?_test(
         ?assertMatch(
            <<"<foo attr1='value1' attr2='value2'><bar attr='value'/></foo>">>,
            iolist_to_binary(
              jhn_xml:encode(
                #elt{tag = <<"foo">>,
                     attrs = [{<<"attr1">>, <<"value1">>},
                              {attr2, <<"value2">>}],
                     children = [#elt{tag = bar,
                                      attrs = [{attr, <<"value">>}]}]}))))},
     {"<",
      ?_test(
         ?assertMatch( <<"&lt;">>,
            iolist_to_binary(jhn_xml:encode(<<"<">>))))},
     {">",
      ?_test(
         ?assertMatch( <<"&gt; ">>,
            iolist_to_binary(jhn_xml:encode(<<"> ">>))))},
     {"\"",
      ?_test(
         ?assertMatch( <<"&quot;">>,
            iolist_to_binary(jhn_xml:encode(<<"\"">>))))},
     {"'",
      ?_test(
         ?assertMatch( <<"&apos;">>,
            iolist_to_binary(jhn_xml:encode(<<"'">>))))},
     {"&",
      ?_test(
         ?assertMatch( <<"&amp;">>,
            iolist_to_binary(jhn_xml:encode(<<"&">>))))}
    ].

%%--------------------------------------------------------------------
%% Stream
%%--------------------------------------------------------------------
encode_eos_test_() ->
    [{"EOS",
      ?_test(
         ?assertMatch(<<"</stream:stream>">>,
                      iolist_to_binary(jhn_xml:encode(eos))))}
    ].

encode_sos_test_() ->
    [{"SOS no attributes",
      ?_test(
         ?assertMatch(<<"<stream:stream>">>,
                      iolist_to_binary(jhn_xml:encode(#sos{}))))},
     {"SOS foo='bar'",
      ?_test(
         ?assertMatch(<<"<stream:stream foo='bar'>">>,
                      iolist_to_binary(
                        jhn_xml:encode(#sos{attrs = [{foo, <<"bar">>}]}))))}
    ].


%% ===================================================================
%% Decoding
%% ===================================================================

%%--------------------------------------------------------------------
%% XML decl
%%--------------------------------------------------------------------
decode_decl_test_() ->
    [{"XML Decl",
      ?_test(
         ?assertMatch(
            {ok, #decl{standalone = no}, <<>>},
            jhn_xml:decode(<<" <?xml version=\"1.0\"?>">>)))},
     {"XML Decl encoding",
      ?_test(
         ?assertMatch(
            {ok, #decl{standalone = no}, <<>>},
            jhn_xml:decode(<<"<?xml version='1.0' encoding=\"UTF-8\"?>">>)))},
     {"XML Decl standalone=yes",
      ?_test(
         ?assertMatch(
            {ok, #decl{standalone = yes}, <<>>},
            jhn_xml:decode(<<"<?xml version=\"1.0\" standalone=\"yes\"?>">>)))},
     {"XML Decl standalone=no",
      ?_test(
         ?assertMatch(
            {ok, #decl{standalone = no}, <<>>},
            jhn_xml:decode(<<"<?xml version=\"1.0\" standalone=\"no\"?>">>)))},
     {"XML Decl encoding, standalone=yes",
      ?_test(
         ?assertMatch(
            {ok, #decl{standalone = yes}, <<>>},
            jhn_xml:decode(<<"<?xml version=\"1.0\""
                             " encoding='UTF-8' standalone='yes' ?>">>)))}
    ].

decode_decl_chunck_test_() ->
    [{"XML Decl",
      ?_test(
         ?assertMatch(
            {ok, #decl{standalone = no}, <<>>},
            chunk(<<" <?xml version=\"1.0\"?>">>)))},
     {"XML Decl encoding",
      ?_test(
         ?assertMatch(
            {ok, #decl{standalone = no}, <<>>},
            chunk(<<"<?xml version='1.0' encoding=\"UTF-8\"?>">>)))},
     {"XML Decl standalone=yes",
      ?_test(
         ?assertMatch(
            {ok, #decl{standalone = yes}, <<>>},
            chunk(<<"<?xml version=\"1.0\" standalone=\"yes\"?>">>)))},
     {"XML Decl standalone=no",
      ?_test(
         ?assertMatch(
            {ok, #decl{standalone = no}, <<>>},
            chunk(<<"<?xml version=\"1.0\" standalone=\"no\"?>">>)))},
     {"XML Decl encoding, standalone=yes",
      ?_test(
         ?assertMatch(
            {ok, #decl{standalone = yes}, <<>>},
            chunk(<<"<?xml version=\"1.0\""
                    " encoding='UTF-8' standalone='yes' ?>">>)))}
    ].

%%--------------------------------------------------------------------
%% CDATA
%%--------------------------------------------------------------------
decode_cdata_test_() ->
    [{"CDATA",
      ?_test(
         ?assertMatch({ok, #cdata{data = <<"hallo">>}, <<>>},
                      jhn_xml:decode(<<"<![CDATA[hallo]]>">>)))},
     {"CDATA ]]",
      ?_test(
         ?assertMatch({ok, #cdata{data = <<"hallo]] ">>}, <<>>},
                      jhn_xml:decode(<<"<![CDATA[hallo]] ]]>">>)))},
     {"CDATA ]]]>",
      ?_test(
         ?assertMatch({ok, #cdata{data = <<"hallo]">>}, <<>>},
                      jhn_xml:decode(<<"<![CDATA[hallo]]]>">>)))}
    ].

decode_cdata_chunk_test_() ->
    [{"CDATA",
      ?_test(
         ?assertMatch({ok, #cdata{data = <<"hallo">>}, <<>>},
                      chunk(<<"<![CDATA[hallo]]>">>)))},
     {"CDATA ]]",
      ?_test(
         ?assertMatch({ok, #cdata{data = <<"hallo]] ">>}, <<>>},
                      chunk(<<"<![CDATA[hallo]] ]]>">>)))},
     {"CDATA ]]]>",
      ?_test(
         ?assertMatch({ok, #cdata{data = <<"hallo]">>}, <<>>},
                      chunk(<<"<![CDATA[hallo]]]>">>)))}
    ].

%%--------------------------------------------------------------------
%% Comment
%%--------------------------------------------------------------------
decode_comment_test_() ->
    [{"Comment Foo",
      ?_test(
         ?assertMatch({ok, #comment{text = <<" Foo ">>}, <<>>},
                      jhn_xml:decode(<<"<!-- Foo -->">>)))},
     {"Comment Foo\nbar",
      ?_test(
         ?assertMatch({ok, #comment{text = <<" Foo\nbar ">>}, <<>>},
                      jhn_xml:decode(<<"<!-- Foo\nbar -->">>)))}
    ].

decode_comment_chunk_test_() ->
    [{"Comment Foo",
      ?_test(
         ?assertMatch({ok, #comment{text = <<" Foo ">>}, <<>>},
                      chunk(<<"<!-- Foo -->">>)))},
     {"Comment Foo\nbar",
      ?_test(
         ?assertMatch({ok, #comment{text = <<" Foo\nbar ">>}, <<>>},
                      chunk(<<"<!-- Foo\nbar -->">>)))}
    ].

%%--------------------------------------------------------------------
%% Comment
%%--------------------------------------------------------------------
decode_pi_test_() ->
    [{"PI foo",
      ?_test(
         ?assertMatch({ok, #pi{target = <<"foo">>, data = <<>>}, <<>>},
                      jhn_xml:decode(<<"<?foo?>">>)))},
     {"PI foo bar",
      ?_test(
         ?assertMatch({ok, #pi{target = <<"foo">>, data = <<"bar">>}, <<>>},
                      jhn_xml:decode(<<"<?foo bar?>">>)))},
     {"PI foo bar bat",
      ?_test(
         ?assertMatch({ok, #pi{target = <<"foo">>, data = <<"bar bat">>}, <<>>},
                      jhn_xml:decode(<<"<?foo bar bat?>">>)))}
    ].

decode_pi_chunk_test_() ->
    [{"PI foo",
      ?_test(
         ?assertMatch({ok, #pi{target = <<"foo">>, data = <<>>}, <<>>},
                      chunk(<<"<?foo?>">>)))},
     {"PI foo bar",
      ?_test(
         ?assertMatch({ok, #pi{target = <<"foo">>, data = <<"bar">>}, <<>>},
                      chunk(<<"<?foo bar?>">>)))},
     {"PI foo bar bat",
      ?_test(
         ?assertMatch({ok, #pi{target = <<"foo">>, data = <<"bar bat">>}, <<>>},
                      chunk(<<"<?foo bar bat?>">>)))}
    ].

%%--------------------------------------------------------------------
%% Doctype decl
%%--------------------------------------------------------------------
decode_doc_test_() ->
    [{"Doc foo empty",
      ?_test(
         ?assertMatch({ok, #doc{name = <<"foo">>}, <<>>},
                      jhn_xml:decode(<<"<!DOCTYPE foo>">>)))},
     {"Doc foo system 'bar' empty",
      ?_test(
         ?assertMatch({ok, #doc{name = <<"foo">>, system = <<"bar">>}, <<>>},
                      jhn_xml:decode(<<"<!DOCTYPE foo SYSTEM 'bar'>">>)))},
     {"Doc foo system \"bar\" empty",
      ?_test(
         ?assertMatch({ok, #doc{name = <<"foo">>, system = <<"bar">>}, <<>>},
                      jhn_xml:decode(<<"<!DOCTYPE foo SYSTEM \"bar\">">>)))},
     {"Doc foo public 'bar' \"bat\" empty",
      ?_test(
         ?assertMatch(
            {ok, #doc{name = <<"foo">>,
                      system = <<"bat">>,
                      public = <<"bar">>},
             <<>>},
            jhn_xml:decode(<<"<!DOCTYPE foo PUBLIC 'bar' \"bat\" >">>)))},
     {"Doc foo public \"bar\" 'bat' empty",
      ?_test(
         ?assertMatch(
            {ok, #doc{name = <<"foo">>,
                      system = <<"bat">>,
                      public = <<"bar">>},
             <<>>},
            jhn_xml:decode(<<"<!DOCTYPE foo PUBLIC \"bar\" 'bat' >">>)))},
     {"Doc foo parameter entity ref",
      ?_test(
         ?assertMatch({ok, #doc{name = <<"foo">>,
                                set = [#pe_ref{name = <<"Bar">>}]},
                       <<>>},
                      jhn_xml:decode(<<"<!DOCTYPE foo [%Bar;]>">>)))}
    ].

decode_doc_chunk_test_() ->
    [{"Doc foo empty",
      ?_test(
         ?assertMatch({ok, #doc{name = <<"foo">>}, <<>>},
                      chunk(<<"<!DOCTYPE foo>">>)))},
     {"Doc foo system 'bar' empty",
      ?_test(
         ?assertMatch({ok, #doc{name = <<"foo">>, system = <<"bar">>}, <<>>},
                      chunk(<<"<!DOCTYPE foo SYSTEM 'bar'>">>)))},
     {"Doc foo system \"bar\" empty",
      ?_test(
         ?assertMatch({ok, #doc{name = <<"foo">>, system = <<"bar">>}, <<>>},
                      chunk(<<"<!DOCTYPE foo SYSTEM \"bar\">">>)))},
     {"Doc foo public 'bar' \"bat\" empty",
      ?_test(
         ?assertMatch(
            {ok, #doc{name = <<"foo">>,
                      system = <<"bat">>,
                      public = <<"bar">>},
             <<>>},
            chunk(<<"<!DOCTYPE foo PUBLIC 'bar' \"bat\" >">>)))},
     {"Doc foo public \"bar\" 'bat' empty",
      ?_test(
         ?assertMatch(
            {ok, #doc{name = <<"foo">>,
                      system = <<"bat">>,
                      public = <<"bar">>},
             <<>>},
            chunk(<<"<!DOCTYPE foo PUBLIC \"bar\" 'bat' >">>)))},
     {"Doc foo parameter entity ref",
      ?_test(
         ?assertMatch({ok, #doc{name = <<"foo">>,
                                set = [#pe_ref{name = <<"Bar">>}]},
                       <<>>},
                      chunk(<<"<!DOCTYPE foo [%Bar;]>">>)))}
    ].

%%--------------------------------------------------------------------
%% Element
%%--------------------------------------------------------------------
deccode_element_test_() ->
    [{"<foo/>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>}, <<>>},
                      jhn_xml:decode(<<"<foo/>">>)))},
     {"<foo></foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>}, <<>>},
                      jhn_xml:decode(<<"<foo></foo>">>)))},
     {"<foo> </foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>}, <<>>},
                      jhn_xml:decode(<<"<foo> </foo>">>)))},
     {"<foo> bar </foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>, children = [<<"bar">>]}, <<>>},
                      jhn_xml:decode(<<"<foo> bar </foo>">>)))},
     {"<foo> bar bat </foo>",
      ?_test(
         ?assertMatch({ok,
                       #elt{tag = <<"foo">>,
                            children = [<<"bar bat">>]},
                       <<>>},
                      jhn_xml:decode(<<"<foo> bar bat </foo>">>)))},
     {"<foo><![CDATA[bar]]></foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>,
                                children = [#cdata{data = <<"bar">>}]}, <<>>},
                      jhn_xml:decode(<<"<foo><![CDATA[bar]]></foo>">>)))},
     {"<foo><![CDATA[ bar ]]></foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>,
                                children = [#cdata{data = <<" bar ">>}]}, <<>>},
                      jhn_xml:decode(<<"<foo><![CDATA[ bar ]]></foo>">>)))},
     {"<foo attr=\"value\"/>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>,
                                attrs = [{<<"attr">>, <<"value">>}]},
                       <<>>},
                      jhn_xml:decode(<<"<foo attr=\"value\"/>">>)))},
     {"<foo attr1 = \"value1\" attr2 = 'value2'/>",
      ?_test(
         ?assertMatch(
            {ok, #elt{tag = <<"foo">>,
                      attrs = [{<<"attr1">>, <<"value1">>},
                               {<<"attr2">>, <<"value2">>}]},
             <<>>},
            jhn_xml:decode(<<"<foo attr1=\"value1\" attr2 = 'value2'/>">>)))},
     {"<foo attr=\"value\"></foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>,
                                attrs = [{<<"attr">>, <<"value">>}]},
                       <<>>},
                      jhn_xml:decode(<<"<foo attr=\"value\"></foo>">>)))}
    ].

deccode_element_chunk_test_() ->
    [{"<foo/>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>}, <<>>},
                      chunk(<<"<foo/>">>)))},
     {"<foo></foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>}, <<>>},
                      chunk(<<"<foo></foo>">>)))},
     {"<foo> </foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>}, <<>>},
                      chunk(<<"<foo> </foo>">>)))},
     {"<foo> bar </foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>, children = [<<"bar">>]}, <<>>},
                      chunk(<<"<foo> bar </foo>">>)))},
     {"<foo> bar bat </foo>",
      ?_test(
         ?assertMatch({ok,
                       #elt{tag = <<"foo">>,
                            children = [<<"bar bat">>]},
                       <<>>},
                      chunk(<<"<foo> bar bat </foo>">>)))},
     {"<foo><![CDATA[bar]]></foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>,
                                children = [#cdata{data = <<"bar">>}]}, <<>>},
                      chunk(<<"<foo><![CDATA[bar]]></foo>">>)))},
     {"<foo><![CDATA[ bar ]]></foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>,
                                children = [#cdata{data = <<" bar ">>}]}, <<>>},
                      chunk(<<"<foo><![CDATA[ bar ]]></foo>">>)))},
     {"<foo attr=\"value\"/>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>,
                                attrs = [{<<"attr">>, <<"value">>}]},
                       <<>>},
                      chunk(<<"<foo attr=\"value\"/>">>)))},
     {"<foo attr1 = \"value1\" attr2 = 'value2'/>",
      ?_test(
         ?assertMatch(
            {ok, #elt{tag = <<"foo">>,
                      attrs = [{<<"attr1">>, <<"value1">>},
                               {<<"attr2">>, <<"value2">>}]},
             <<>>},
            chunk(<<"<foo attr1=\"value1\" attr2 = 'value2'/>">>)))},
     {"<foo attr=\"value\"></foo>",
      ?_test(
         ?assertMatch({ok, #elt{tag = <<"foo">>,
                                attrs = [{<<"attr">>, <<"value">>}]},
                       <<>>},
                      chunk(<<"<foo attr=\"value\"></foo>">>)))}
    ].

%%--------------------------------------------------------------------
%% Stream
%%--------------------------------------------------------------------
decode_eos_test_() ->
    [{"EOS",
      ?_test(?assertMatch({eos, <<>>},
                          jhn_xml:decode(<<"</stream:stream>">>)))},
     {"EOS WS",
      ?_test(?assertMatch({eos, <<>>},
                          jhn_xml:decode(<<"</stream:stream >">>)))},
     {"EOS WS WS",
      ?_test(?assertMatch({eos, <<>>},
                          jhn_xml:decode(<<"</stream:stream  >">>)))}
    ].

decode_eos_chunk_test_() ->
    [{"EOS",?_test(?assertMatch({eos, <<>>}, chunk(<<"</stream:stream>">>)))},
     {"EOS WS ",
      ?_test(?assertMatch({eos, <<>>}, chunk(<<"</stream:stream >">>)))},
     {"EOS WS WS",
      ?_test(?assertMatch({eos, <<>>}, chunk(<<"</stream:stream  >">>)))}
    ].

decode_sos_test_() ->
    [{"SOS",
      ?_test(?assertMatch({ok, #sos{attrs = []}, <<>>},
                          jhn_xml:decode(<<"<stream:stream>">>)))},
     {"SOS WS",
      ?_test(?assertMatch({ok, #sos{attrs = []}, <<>>},
                          jhn_xml:decode(<<"<stream:stream >">>)))},
     {"SOS foo='bar'",
      ?_test(?assertMatch({ok, #sos{attrs = [{<<"foo">>, <<"bar">>}]}, <<>>},
                          jhn_xml:decode(<<"<stream:stream foo='bar'>">>)))}
    ].

decode_sos_chunk_test_() ->
    [{"SOS",
      ?_test(?assertMatch({ok, #sos{attrs = []}, <<>>},
                          chunk(<<"<stream:stream>">>)))},
     {"SOS WS",
      ?_test(?assertMatch({ok, #sos{attrs = []}, <<>>},
                          chunk(<<"<stream:stream >">>)))},
     {"SOS foo='bar'",
      ?_test(?assertMatch({ok, #sos{attrs = [{<<"foo">>, <<"bar">>}]}, <<>>},
                          chunk(<<"<stream:stream foo='bar'>">>)))}
    ].

%% ===================================================================
%% Decoding/Decoding
%% ===================================================================

%%--------------------------------------------------------------------
%% Escape
%%--------------------------------------------------------------------

encode_decode_escape_test_() ->
    Elt = #elt{tag = <<"a">>, children = [<<"<\" '&>">>]},
    EltA = #elt{tag = <<"a">>, attrs = [{<<"bar">>, <<"<\" '&>">>}]},
    [{"All",
      ?_test(
         ?assertMatch({ok, Elt, <<>>},
                      jhn_xml:decode(iolist_to_binary(jhn_xml:encode(Elt)))))},
     {"Value",
      ?_test(
         ?assertMatch({ok, EltA, <<>>},
                      jhn_xml:decode(iolist_to_binary(jhn_xml:encode(EltA)))))}
    ].

encode_decode_escape_chunk_test_() ->
    Elt = #elt{tag = <<"a">>, children = [<<"<\" '&>">>]},
    EltA = #elt{tag = <<"a">>, attrs = [{<<"bar">>, <<"<\" '&>">>}]},
    [{"All",
      ?_test(
         ?assertMatch({ok, Elt, <<>>},
                      chunk(iolist_to_binary(jhn_xml:encode(Elt)))))},
     {"Value",
      ?_test(
         ?assertMatch({ok, EltA, <<>>},
                      chunk(iolist_to_binary(jhn_xml:encode(EltA)))))}
    ].

%% ===================================================================
%% Common functions.
%% ===================================================================

chunk(<<H/utf8, T/binary>>) ->
    More = {more, _} = jhn_xml:decode(<<H/utf8>>),
    chunk(T, More).

chunk(<<H/utf8, T/binary>>, M) ->
    case jhn_xml:decode(<<H/utf8>>, M) of
        More  = {more, _} -> chunk(T, More);
        {ok, Elt, <<>>} -> {ok, Elt, T};
        {eos, <<>>} -> {eos, T}
    end.

%% equal_decode({eos, Bin}, {eos, Bin}) -> true;
%% equal_decode({XML1, Bin}, {ok, XML2, Bin}) -> equal(XML1, XML2).

%% equal(#xml{tag = Tag, attrs = Attrs1, children = Children1},
%%       #xml{tag = Tag, attrs = Attrs2, children = Children2}) ->
%%     equal_attrs(lists:sort(Attrs1), lists:sort(Attrs2)),
%%     equal_children(Children1, Children2);
%% equal(#xml{tag = Tag1}, #xml{tag = Tag2}) ->
%%     erlang:error({different_tags, Tag1, Tag2});
%% equal(Text, Text) when is_binary(Text) ->
%%     true.

%% equal_attrs([], []) -> true;
%% equal_attrs([], Attrs) -> erlang:error({missing_atributes, Attrs});
%% equal_attrs(Attrs, []) -> erlang:error({excessive_atributes, Attrs});
%% equal_attrs([H1 | T1], [H2 | T2]) ->
%%     equal_attr(H1, H2),
%%     equal_attrs(T1, T2).

%% equal_attr({N, V}, Attr) when is_atom(N) ->
%%     equal_attr({atom_to_binary(N, utf8), V}, Attr);
%% equal_attr(Attr, {N, V}) when is_atom(N) ->
%%     equal_attr(Attr, {atom_to_binary(N, utf8), V});
%% equal_attr({N, V}, {N, V}) ->
%%     true;
%% equal_attr({N, V1}, {N, V2}) ->
%%     erlang:error({attribute_values, V1, V2});
%% equal_attr({N1, V}, {N2, V}) ->
%%     erlang:error({missaligned_attribute, N1, N2});
%% equal_attr({N1, V1}, {N2, V2}) ->
%%     erlang:error({missaligned_attribute, {N1, V1}, {N2, V2}}).

%% equal_children([], []) -> true;
%% equal_children([], Children) -> erlang:error({missing_children, Children});
%% equal_children(Children, []) -> erlang:error({excessive_children, Children});
%% equal_children([H1 |T1], [H2 | T2]) ->
%%     equal(H1, H2),
%%     equal_children(T1, T2).

%% step(<<H, T/binary>>) ->
%%     S = {more, _} = xmpp_xml_parse:decode(<<H>>),
%%     step(T, S).

%% step(<<H>>, S) -> xmpp_xml_parse:decode(<<H>>, S);
%% step(<<H, T/binary>>, S) ->
%%     S1 = {more, _} = xmpp_xml_parse:decode(<<H>>, S),
%%     step(T, S1).
