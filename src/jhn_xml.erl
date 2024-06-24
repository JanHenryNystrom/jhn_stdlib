%==============================================================================
%% Copyright 2020-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   Simple xml encoding/decoding restricted to UTF-8.
%%% @end
%%%
%%%  https://www.w3.org/TR/xml
%%%
%%%  Outstanding: Content of Elements:  Reference
%%%               Document Type Definition: markupdecl 
%%%
%%%
%%%
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2020-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_xml).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2]).

%% Includes
-include_lib("jhn_stdlib/include/jhn_xml.hrl").

%% Exported Types
-export_types([]).

%% Records
-record(state, {current = undefined :: elt() | cdata() | binary() | undefined,
                stack = [] :: [elt()],
                func :: func(),
                args = no_args :: no_args | args()
               }).

%% Types
-type state() :: #state{}.
-type func() :: atom().
-type args() :: tuple().

%% Defines
-define(START_CHAR(C),
            C =:= 16#3A; %% :
            C >= 16#41, C =< 16#5A; %% A-Z
            C =:= 16#5F; %% _
            C >= 16#61, C =< 16#7A; %% a-z
            C >= 16#C0, C =< 16#D6; %% À-Ö
            C >= 16#D8, C =< 16#F6; %% Ø-ö
            C >= 16#F8, C =< 16#2FF; %% ø-
            C >= 16#370, C =< 16#37D;
            C >= 16#37F, C =< 16#1FFF;
            C >= 16#200C, C =< 16#200D;
            C >= 16#2070, C =< 16#218F;
            C >= 16#2C00, C =< 16#2FEF;
            C >= 16#3001, C =< 16#D7FF;
            C >= 16#F900, C =< 16#FDCF;
            C >= 16#FDF0, C =< 16#FFFD;
            C >= 16#10000, C =< 16#EFFFF).

-define(NAME_CHAR(C),
            C >= 16#2D, C =< 16#2E; %% --.
            C >= 16#30, C =< 16#39; %% 0-9
            C =:= 16#3A; %% :
            C >= 16#41, C =< 16#5A; %% A-Z
            C =:= 16#5F; %% _
            C >= 16#61, C =< 16#7A; %% a-z
            C =:= 16#B7; %% ·
            C >= 16#C0, C =< 16#D6; %% À-Ö
            C >= 16#D8, C =< 16#F6; %% Ø-ö
            C >= 16#F8, C =< 16#2FF; %% ø-
            C >= 16#300, C =< 16#37D;
            C >= 16#37F, C =< 16#1FFF;
            C >= 16#200C, C =< 16#200D;
            C >= 16#203F, C =< 16#2040;
            C >= 16#2070, C =< 16#218F;
            C >= 16#2C00, C =< 16#2FEF;
            C >= 16#3001, C =< 16#D7FF;
            C >= 16#F900, C =< 16#FDCF;
            C >= 16#FDF0, C =< 16#FFFD;
            C >= 16#10000, C =< 16#EFFFF).

-define(CHAR(C),
        C >= 16#9, C =< 16#A;
        C =:= 16#D;
        C >= 16#20, C =< 16#D7FF;
        C >= 16#E000, C =< 16#FFFD;
        C >= 16#10000, C =< 16#10FFFF).

-define(PUBID_CHAR(C),
        C =:= 16#A; %% \n
        C =:= 16#D; %% \r
        C >= 16#20, C =< 16#25; %% \s ! " # $ %
        C >= 16#27, C =< 16#3B; %% ' ( _ * + , - . / 0-9 : ;
        C =:= 16#3D; %% =
        C >= 16#3F, C =< 16#5A; %% ? @ A-Z
        C =:= 16#5F; %% _
        C >= 16#61, C =< 16#7A). %% a-z

-define(WS(C), C =:= $\s;
               C =:= $\t;
               C =:= $\r;
               C =:= $\n).

-define(STREAM, <<"stream:stream">>).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode() -> .
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec encode(elt()) -> iodata().
%%--------------------------------------------------------------------
encode(ELT) -> encode(ELT, []).

%%--------------------------------------------------------------------
%% Function: encode() -> .
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec encode(elt(), []) -> iodata().
%%--------------------------------------------------------------------
encode(ELT, _) -> do_encode(ELT).

%%--------------------------------------------------------------------
%% Function: decode() -> .
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) ->
                    {ok, elt() | sos(), binary()} |
                    {eos, binary()} |
                    {more, state()}.
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, #state{}).

%%--------------------------------------------------------------------
%% Function: decode() -> .
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), state() | {more, state()}) ->
                    {ok, elt() | sos(), binary()} |
                    {eos, binary()} |
                    {more, state()}.
%%--------------------------------------------------------------------
decode(<<>>, State = #state{}) -> more(decode, State);
decode(Binary, {more, State}) -> call(Binary, State);
decode(<<$<, T/binary>>, State = #state{}) -> decode_start(T, <<>>, State);
decode(<<H, T/binary>>, State = #state{}) when ?WS(H) -> decode(T, State);
decode(<<$&, T/binary>>, State = #state{}) ->
    decode_text_escape(T, <<>>, <<>>, State);
decode(<<H/utf8, T/binary>>, State = #state{}) ->
    decode_text(T, <<H/utf8>>, State).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

do_encode(Text) when is_binary(Text) -> escape(Text);
do_encode(eos) -> <<"</stream:stream>">>;
do_encode(#sos{attrs = []}) -> <<"<stream:stream>">>;
do_encode(#sos{attrs = Attrs}) ->
    [<<"<stream:stream">>, encode_attrs(Attrs), <<">">>];
do_encode(#cdata{data = <<>>}) -> [];
do_encode(#cdata{data = Data}) -> [<<"<![CDATA[">>, Data, <<"]]>">>];
do_encode(#elt{tag = Tag, attrs = [], children = []}) ->
    [<<"<">>, encode_tag(Tag), <<"/>">>];
do_encode(#elt{tag = Tag, attrs = Attrs, children = []}) ->
    [<<"<">>, encode_tag(Tag), encode_attrs(Attrs), <<"/>">>];
do_encode(#elt{tag = Tag, attrs = Attrs, children = Children}) ->
    Tag1 = encode_tag(Tag),
    [<<"<">>, Tag1, encode_attrs(Attrs), <<">">>,
     [do_encode(Child) || Child <- Children],
     <<"</">>, Tag1, <<">">>];
do_encode(#decl{standalone = yes}) ->
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>">>;
do_encode(#decl{standalone = no}) ->
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>;
do_encode(#comment{text = <<>>}) ->
    [];
do_encode(#comment{text = Text}) ->
    [<<"<!--">>, Text, <<"-->">>];
do_encode(PI = #pi{target = Target}) when is_atom(Target) ->
    do_encode(PI#pi{target = atom_to_binary(Target, utf8)});
do_encode(#pi{target = Target, data = <<>>}) ->
    [<<"<?">>, Target, <<"?>">>];
do_encode(#pi{target = Target, data = Data}) ->
    [<<"<?">>, Target, <<" ">>, Data, <<"?>">>];
do_encode(D = #doc{name = Name}) when is_atom(Name) ->
    do_encode(D#doc{name = atom_to_binary(Name, utf8)});
do_encode(#doc{name = Name, system = undefined, public = undefined, set = S}) ->
    [<<"<!DOCTYPE ">>, Name, encode_set(S), <<">">>];
do_encode(#doc{name = Name, system = System, public = undefined, set = Set}) ->
    [<<"<!DOCTYPE ">>, Name, <<" SYSTEM '">>, System, encode_set(Set),<<"'>">>];
do_encode(#doc{name = Name, system = System, public = Public, set = Set}) ->
    [<<"<!DOCTYPE ">>, Name, <<" PUBLIC '">>, Public, <<"' '">>, System,
     encode_set(Set), <<"'>">>].

encode_set([]) -> [];
encode_set(Set) ->
    [[_, E] | T] = [[$\s, encode_elt(M)] || M <- Set],
    [<<$\s, $[>>, [E | T], <<$]>>].

encode_elt(#pe_ref{name = Name}) when is_atom(Name) ->
    encode_elt(#pe_ref{name = atom_to_binary(Name, utf8)});
encode_elt(#pe_ref{name = Name}) ->
    [<<$%>>, Name, <<$;>>];
encode_elt(ED = #elt_decl{name = Name}) when is_atom(Name) ->
    encode_elt(ED#elt_decl{name = atom_to_binary(Name, utf8)});
encode_elt(#elt_decl{name = Name, pcdata = PC, content = Content}) ->
    [<<"<!ELEMENT' ">>, Name, <<$\s>>, encode_content(PC, Content),<<$>>>].

encode_content(false, empty) -> <<"EMPTY">>;
encode_content(false, any) -> <<"ANY">>;
encode_content(true, []) -> <<"(#PCDATA)">>;
encode_content(true, Names) ->
    [<<"(#PCDATA">>, [[$|, Name] || Name <- Names], <<")*">>];
encode_content(false, #child{type = choice, reg = Reg, children = Children}) ->
    ok.

encode_attrs([]) -> <<>>;
encode_attrs(Attrs) -> [encode_attr(Attr) || Attr <- Attrs].

encode_attr({_, <<"undefined">>}) -> [];
encode_attr({Name, Value}) when is_atom(Name) ->
    encode_attr({atom_to_binary(Name, utf8), Value});
encode_attr({Name, Value}) when is_atom(Value) ->
    encode_attr({Name, atom_to_binary(Value, utf8)});
encode_attr({Name, Value}) ->
    [<<"\s">>, Name, <<"='">>, escape(Value), <<"'">>].

encode_tag(Tag) when is_atom(Tag) -> atom_to_binary(Tag, utf8);
encode_tag(Tag) -> Tag.

escape(Value) ->
    case needs_escaping(Value) of
        true -> escape(Value, <<>>);
        false -> Value
    end.

needs_escaping(<<>>) -> false;
needs_escaping(<<$<, _/binary>>) -> true;
needs_escaping(<<$>, _/binary>>) -> true;
needs_escaping(<<$", _/binary>>) -> true;
needs_escaping(<<$', _/binary>>) -> true;
needs_escaping(<<$&, _/binary>>) -> true;
needs_escaping(<<_, T/binary>>) -> needs_escaping(T).

escape(<<>>, Acc) -> Acc;
escape(<<$<, T/binary>>, Acc) -> escape(T, <<Acc/binary, "&lt;">>);
escape(<<$>, T/binary>>, Acc) -> escape(T, <<Acc/binary, "&gt;">>);
escape(<<$", T/binary>>, Acc) -> escape(T, <<Acc/binary, "&quot;">>);
escape(<<$', T/binary>>, Acc) -> escape(T, <<Acc/binary, "&apos;">>);
escape(<<$&, T/binary>>, Acc) -> escape(T, <<Acc/binary, "&amp;">>);
escape(<<H, T/binary>>, Acc) -> escape(T, <<Acc/binary, H>>).

%% ===================================================================
%% Decoding
%% ===================================================================

decode_start(<<>>, Acc, State) -> more(decode_start, Acc, State);
decode_start(<<"!", T/binary>>, <<>>, State) ->
    decode_bang_start(T, State);
decode_start(<<"?", T/binary>>, <<>>, State) ->
    decode_question(T, <<"xml">>, <<>>, State);
decode_start(<<$/, T/binary>>, Acc, State) ->
    decode_empty(T, State#state{current = #elt{tag = Acc}});
decode_start(<<$>, T/binary>>, ?STREAM, _) ->
    stop(#sos{}, T);
decode_start(<<$>, T/binary>>, Acc, State) ->
    decode_child(T, <<>>, State#state{current = #elt{tag = Acc}});
decode_start(<<H, T/binary>>, Acc, State) when ?WS(H) ->
    decode_name(T, <<>>, State#state{current = #elt{tag = Acc}});
decode_start(<<H/utf8, T/binary>>, <<>>, State) when ?START_CHAR(H) ->
    decode_start(T, <<H/utf8>>, State);
decode_start(<<H/utf8, T/binary>>, Acc = <<_, _/binary>>, State)
  when ?NAME_CHAR(H) ->
    decode_start(T, <<Acc/binary, H/utf8>>, State).

%% -------------------------------------------------------------------
%% Prolog
%% -------------------------------------------------------------------

decode_bang_start(<<>>, State) ->
    more(decode_bang_start, State);
decode_bang_start(<<$[, T/binary>>, State) ->
    decode_bang(T, <<"CDATA[">>, cdata, State);
decode_bang_start(<<$D, T/binary>>, State) ->
    decode_bang(T, <<"OCTYPE">>, doctype_name, State);
decode_bang_start(<<$-, T/binary>>, State) ->
    decode_bang(T, <<"-">>, comment, State).

decode_bang(<<>>, Check, F, State) ->
    more(decode_bang, {Check, F}, State);
decode_bang(T, <<>>, cdata, State) ->
    decode_cdata(T, <<>>, State);
decode_bang(T, <<>>, doctype_name, State) ->
    decode_doctype_name(T, <<>>, State);
decode_bang(T, <<>>, comment, State) ->
    decode_comment(T, <<>>, State);
decode_bang(<<H, T/binary>>, <<H, T1/binary>>, F, State) ->
    decode_bang(T, T1, F, State).

%% -------------------------------------------------------------------
%% CDATA
%% -------------------------------------------------------------------

decode_cdata(<<>>, Acc, State) -> more(decode_cdata, Acc, State);
decode_cdata(<<$], T/binary>>, Acc, State) ->
    decode_cdata_end(T, Acc, <<$]>>, State);
decode_cdata(<<H/utf8, T/binary>>, Acc, State) ->
    decode_cdata(T, <<Acc/binary, H/utf8>>, State).

decode_cdata_end(<<>>, Acc, Term, State) ->
    more(decode_cdata_end, {Acc, Term}, State);
decode_cdata_end(<<$], T/binary>>, Acc, <<$]>>, State) ->
    decode_cdata_end(T, Acc, <<"]]">>, State);
decode_cdata_end(<<$], T/binary>>, Acc, <<"]]">>, State) ->
    decode_cdata_end(T, <<Acc/binary, $]>>,  <<"]]">>, State);
decode_cdata_end(<<$>, T/binary>>, Acc, <<"]]">>, State) ->
    pop(T, State#state{current = #cdata{data = Acc}});
decode_cdata_end(T, Acc, Term, State) ->
    decode_cdata(T, <<Acc/binary, Term/binary>>, State).

%% -------------------------------------------------------------------
%% Doctype
%% -------------------------------------------------------------------

decode_doctype_name(<<>>, Acc, State) ->
    more(decode_doctype_name, Acc, State);
decode_doctype_name(<<$>, T/binary>>, Acc, _) ->
    stop(#doc{name = Acc}, T);
decode_doctype_name(<<H/utf8, T/binary>>, <<>>, State) when ?WS(H) ->
    decode_doctype_name(T, <<>>, State);
decode_doctype_name(<<H/utf8, T/binary>>, Acc, State) when ?WS(H) ->
    decode_doctype_rest(T, #doc{name = Acc}, State);
decode_doctype_name(<<$[, T/binary>>, Acc, State) ->
    decode_doctype_sub(T, <<>>, #doc{name = Acc}, State);
decode_doctype_name(<<H/utf8, T/binary>>, Acc, State) when ?NAME_CHAR(H) ->
    decode_doctype_name(T, <<Acc/binary, H/utf8>>, State).

decode_doctype_rest(<<>>, Decl, State) ->
    more(decode_doctype_rest, Decl, State);
decode_doctype_rest(<<$>, T/binary>>, Decl, _) ->
    stop(Decl, T);
decode_doctype_rest(<<$[, T/binary>>, Decl, State) ->
    decode_doctype_sub(T, <<>>, Decl, State);
decode_doctype_rest(<<$S, T/binary>>, Decl, State) ->
    decode_doctype_external(T, <<"YSTEM">>, system, Decl, State);
decode_doctype_rest(<<$P, T/binary>>, Decl, State) ->
    decode_doctype_external(T, <<"UBLIC">>, public, Decl, State);
decode_doctype_rest(<<H/utf8, T/binary>>, Decl, State) when ?WS(H) ->
    decode_doctype_rest(T, Decl, State).

decode_doctype_external(<<>>, Check, Type, Decl, State) ->
    more(decode_doctype_external, {Check, Type, Decl}, State);
decode_doctype_external(<<H/utf8, T/binary>>, <<>>, system, Decl, State)
  when ?WS(H) ->
    decode_doctype_external_system(T, <<>>, undefined, Decl, State);
decode_doctype_external(T, <<>>, public, Decl, State) ->
    decode_doctype_external_public(T, <<>>, undefined, Decl, State);
decode_doctype_external(<<H, T/binary>>, <<H, T1/binary>>, Type, Decl, State) ->
    decode_doctype_external(T, T1, Type, Decl, State).

decode_doctype_external_system(<<>>, Acc, Quote, Decl, State) ->
    more(decode_doctype_external_system, {Acc, Quote, Decl}, State);
decode_doctype_external_system(<<$', T/binary>>, _, undefined, Decl, State) ->
    decode_doctype_external_system(T, <<>>, $', Decl, State);
decode_doctype_external_system(<<$", T/binary>>, _, undefined, Decl, State) ->
    decode_doctype_external_system(T, <<>>, $", Decl, State);
decode_doctype_external_system(<<Quote, T/binary>>, Acc, Quote, Decl, State) ->
    decode_doctype_rest(T, Decl#doc{system = Acc}, State);
decode_doctype_external_system(<<H/utf8, T/binary>>, Acc, Quote, Decl, State) ->
    decode_doctype_external_system(T, <<Acc/binary, H/utf8>>, Quote,Decl,State).

decode_doctype_external_public(<<>>, Acc, Quote, Decl, State) ->
    more(decode_doctype_external_public, {Acc, Quote, Decl}, State);
decode_doctype_external_public(<<H/utf8, T/binary>>, <<>>, undefined,Decl,State)
  when ?WS(H) ->
    decode_doctype_external_public(T, <<>>, undefined,Decl,State);
decode_doctype_external_public(<<$', T/binary>>, _, undefined, Decl, State) ->
    decode_doctype_external_public(T, <<>>, $', Decl, State);
decode_doctype_external_public(<<$", T/binary>>, _, undefined, Decl, State) ->
    decode_doctype_external_public(T, <<>>, $", Decl, State);
decode_doctype_external_public(<<Quote, T/binary>>, Acc, Quote, Decl, State) ->
    Decl1 = Decl#doc{public=Acc},
    decode_doctype_external_system(T, <<>>, undefined, Decl1, State);
decode_doctype_external_public(<<H/utf8, T/binary>>, Acc, Quote, Decl, State)
  when ?PUBID_CHAR(H) ->
    decode_doctype_external_public(T, <<Acc/binary, H/utf8>>, Quote,Decl,State).

decode_doctype_sub(<<>>, Acc, Decl, State) ->
    more(decode_doctype_sub, {Acc, Decl}, State);
decode_doctype_sub(<<$%, T/binary>>, <<>>, Decl, State) ->
    decode_doctype_per(T, <<>>, Decl, State);
decode_doctype_sub(<<$], T/binary>>, <<>>, Decl, State) ->
    decode_doctype_end(T, Decl, State);
decode_doctype_sub(<<H/utf8, T/binary>>, <<>>, Decl, State) when ?WS(H) ->
    decode_doctype_sub(T, <<>>, Decl, State);
decode_doctype_sub(<<$<, T/binary>>, <<>>, Decl, State) ->
    decode_doctype_sub_start(T, <<>>, Decl, State).

decode_doctype_per(<<>>, Acc, Decl, State) ->
    more(decode_doctype_per, {Acc, Decl}, State);
decode_doctype_per(<<$;, T/binary>>, Acc, Decl = #doc{set = Set}, State) ->
    Decl1 = Decl#doc{set = [#pe_ref{name = Acc} | Set]},
    decode_doctype_sub(T, <<>>, Decl1, State);
decode_doctype_per(<<H/utf8, T/binary>>, Acc, Decl, State) when ?NAME_CHAR(H) ->
    decode_doctype_per(T, <<Acc/binary, H/utf8>>, Decl, State).

decode_doctype_sub_start(_, _, _, _) ->
    erlang:error(tbd).

decode_doctype_end(<<>>, Decl, State) ->
    more(decode_doctype_end, Decl, State);
decode_doctype_end(<<$>, T/binary>>, Decl = #doc{set = Set}, _) ->
    stop(Decl#doc{set = lists:reverse(Set)}, T);
decode_doctype_end(<<H/utf8, T/binary>>, Decl, State) when ?WS(H) ->
    decode_doctype_end(T, Decl, State).

%% -------------------------------------------------------------------
%% Comment
%% -------------------------------------------------------------------

decode_comment(<<>>, Acc, State) -> more(decode_comment, Acc, State);
decode_comment(<<$-, T/binary>>, Acc, State) ->
    decode_comment_end(T, <<"->">>, <<>>, Acc, State);
decode_comment(<<H/utf8, T/binary>>, Acc, State) ->
    decode_comment(T, <<Acc/binary, H/utf8>>, State).

decode_comment_end(T, <<>>, _, Acc, _) -> stop(#comment{text = Acc}, T);
decode_comment_end(<<>>, Check, Acc0, Acc, State) ->
    more(decode_comment_end, {Check, Acc0, Acc}, State);
decode_comment_end(<<H, T/binary>>, <<H, T1/binary>>, Acc0, Acc, State) ->
    decode_comment_end(T, T1, <<Acc0/binary, H>>, Acc, State);
decode_comment_end(<<H/utf8, T/binary>>, _, Acc0, Acc, State) ->
    decode_comment(T, <<Acc/binary, Acc0/binary, H/utf8>>, State).

%% -------------------------------------------------------------------
%% ?
%% -------------------------------------------------------------------

decode_question(<<>>, Check, Acc, State) ->
    more(decode_question, {Check, Acc}, State);
decode_question(T, <<>>, _, State) ->
    decode_decl_attr(T, <<"version=">>, version, State);
decode_question(<<H, T/binary>>, <<H, T1/binary>>, Acc, State) ->
    decode_question(T, T1, Acc, State);
decode_question(T, _, Acc, State) ->
    decode_pi_target(T, Acc, State).

%% -------------------------------------------------------------------
%% XML decl
%% -------------------------------------------------------------------

decode_decl_rest(<<>>, Phase, State) ->
    more(decode_decl_rest, Phase, State);
decode_decl_rest(<<H, T/binary>>, Phase, State) when ?WS(H) ->
    decode_decl_rest(T, Phase, State);
decode_decl_rest(<<$e, T/binary>>, encoding, State) ->
    decode_decl_attr(T, <<"ncoding=">>, encoding, State);
decode_decl_rest(<<$s, T/binary>>, encoding, State) ->
    decode_decl_attr(T, <<"tandalone=">>, standalone, State);
decode_decl_rest(<<$s, T/binary>>, standalone, State) ->
    decode_decl_attr(T, <<"tandalone=">>, standalone, State);
decode_decl_rest(<<$?, T/binary>>, _, State) ->
    decode_decl_end(T, <<$>>>, undefined, State).

decode_decl_version_val(<<>>, Quote, Check, State) ->
    more(decode_decl_version_val, {Quote, Check}, State);
decode_decl_version_val(<<$', T/binary>>, undefined, Check, State) ->
    decode_decl_version_val(T, $', Check, State);
decode_decl_version_val(<<$", T/binary>>, undefined, Check, State) ->
    decode_decl_version_val(T, $", Check, State);
decode_decl_version_val(<<Quote, T/binary>>, Quote, <<>>, State) ->
    decode_decl_rest(T, encoding, State);
decode_decl_version_val(<<H, T/binary>>, Quote, <<H, T1/binary>>, State) ->
    decode_decl_version_val(T, Quote, T1, State).

decode_decl_attr(<<>>, Check1, Phase, State) ->
    more(decode_decl_attr, {Check1, Phase}, State);
decode_decl_attr(<<H/utf8, T/binary>>, Check1, Phase, State) when ?WS(H) ->
    decode_decl_attr(T, Check1, Phase, State);
decode_decl_attr(<<H, T/binary>>, <<H, T1/binary>>, Phase, State) ->
    decode_decl_attr(T, T1, Phase, State);
decode_decl_attr(T, <<>>, version, State) ->
    decode_decl_version_val(T, undefined, <<"1.0">>, State);
decode_decl_attr(T, <<>>, encoding, State) ->
    decode_decl_enc_val(T, undefined, <<"UTF-8">>, State);
decode_decl_attr(T, <<>>, standalone, State) ->
    decode_decl_alone_val(T, undefined, <<>>, State).

decode_decl_enc_val(<<>>, Quote, Check, State) ->
    more(decode_decl_enc_val, {Quote, Check}, State);
decode_decl_enc_val(<<$', T/binary>>, undefined, Check, State) ->
    decode_decl_enc_val(T, $', Check, State);
decode_decl_enc_val(<<$", T/binary>>, undefined, Check, State) ->
    decode_decl_enc_val(T, $", Check, State);
decode_decl_enc_val(<<H, T/binary>>, Quote, <<H, T1/binary>>, State) ->
    decode_decl_enc_val(T, Quote, T1, State);
decode_decl_enc_val(<<Quote, T/binary>>, Quote, <<>>, State) ->
    decode_decl_rest(T, standalone, State).

decode_decl_alone_val(<<>>, Quote, Acc, State) ->
    more(decode_decl_alone_val, {Quote, Acc}, State);
decode_decl_alone_val(<<$', T/binary>>, undefined, Acc, State) ->
    decode_decl_alone_val(T, $', Acc, State);
decode_decl_alone_val(<<$", T/binary>>, undefined, Acc, State) ->
    decode_decl_alone_val(T, $", Acc, State);
decode_decl_alone_val(<<Quote, T/binary>>, Quote, Acc, State) ->
    decode_decl_end(T, <<"?>">>, Acc, State);
decode_decl_alone_val(<<H, T/binary>>, Quote, Acc, State) ->
    decode_decl_alone_val(T, Quote, <<Acc/binary, H>>, State).

decode_decl_end(T, <<>>, undefined, _) ->
    stop(#decl{}, T);
decode_decl_end(T, <<>>, <<"yes">>, _) ->
    stop(#decl{standalone = yes}, T);
decode_decl_end(T, <<>>, <<"no">>, _) ->
    stop(#decl{standalone = no}, T);
decode_decl_end(<<>>, Check, Alone, State) ->
    more(decode_decl_end, {Check, Alone}, State);
decode_decl_end(<<H, T/binary>>, Check, Alone, State) when ?WS(H) ->
    decode_decl_end(T, Check, Alone, State);
decode_decl_end(<<H, T/binary>>, <<H, T1/binary>>, Alone, State) ->
    decode_decl_end(T, T1, Alone, State).

%% -------------------------------------------------------------------
%% PI
%% -------------------------------------------------------------------

decode_pi_target(<<>>, Acc, State) ->
    more(decode_pi_target, Acc, State);
decode_pi_target(<<$?, T/binary>>, Acc, State) ->
    decode_pi_end(T, <<>>, Acc, State);
decode_pi_target(<<H/utf8, T/binary>>, Acc, State) when ?WS(H) ->
    decode_pi_parts(T, <<>>, Acc, State);
decode_pi_target(<<H/utf8, T/binary>>, Acc, State) when ?NAME_CHAR(H) ->
    decode_pi_target(T, <<Acc/binary, H/utf8>>, State).

decode_pi_parts(<<>>, Acc, Target, State) ->
    more(decode_pi_parts, {Acc, Target}, State);
decode_pi_parts(<<$?, T/binary>>, Acc, Target, State) ->
    decode_pi_end(T, Acc, Target, State);
decode_pi_parts(<<H/utf8, T/binary>>, Acc, Target, State) when ?WS(H) ->
    decode_pi_parts(T, <<Acc/binary, H/utf8>>, Target, State);
decode_pi_parts(<<H/utf8, T/binary>>, Acc, Target, State) when ?CHAR(H) ->
    decode_pi_parts(T, <<Acc/binary, H/utf8>>, Target, State).

decode_pi_end(<<>>, Parts, Target, State) ->
    more(decode_pi_end, {Parts, Target}, State);
decode_pi_end(<<$>, T/binary>>, Parts, Target, _) ->
    stop(#pi{target = Target, data = Parts}, T).

%% -------------------------------------------------------------------
%% Element
%% -------------------------------------------------------------------

%% Name

decode_name(<<>>, Acc, State) -> more(decode_name, Acc, State);
decode_name(<<$/, T/binary>>, <<>>, State) -> decode_empty(T, State);
decode_name(<<$>, T/binary>>, <<>>, #state{current = E = #elt{tag=?STREAM}}) ->
    stop(#sos{attrs = E#elt.attrs}, T);
decode_name(<<$>, T/binary>>, <<>>, State) ->
    decode_child(T, <<>>, State);
decode_name(<<$=, T/binary>>, Acc = <<_, _/binary>>, State) ->
    decode_value_start(T,Acc,State);
decode_name(<<H, T/binary>>, <<>>, State) when ?WS(H) ->
    decode_name(T, <<>>, State);
decode_name(<<H, T/binary>>, Acc, State) when ?WS(H) ->
    decode_assign(T, Acc, State);
decode_name(<<H/utf8, T/binary>>, <<>>, State) when ?START_CHAR(H) ->
    decode_name(T, <<H/utf8>>, State);
decode_name(<<H/utf8, T/binary>>, Acc = <<_, _/binary>>, State)
  when ?NAME_CHAR(H) ->
    decode_name(T, <<Acc/binary, H/utf8>>, State).

decode_assign(<<>>, Name, State) -> more(decode_assign, Name, State);
decode_assign(<<$=, T/binary>>, Name, State) ->
    decode_value_start(T, Name, State);
decode_assign(<<H, T/binary>>, Name, State) when ?WS(H) ->
    decode_assign(T, Name, State).

%% Value

decode_value_start(<<>>, Name, State) -> more(decode_value_start, Name, State);
decode_value_start(<<$', T/binary>>, Name, State) ->
    decode_value(T, <<>>, $', Name, State);
decode_value_start(<<$", T/binary>>, Name, State) ->
    decode_value(T, <<>>, $", Name, State);
decode_value_start(<<H, T/binary>>, Name, State) when ?WS(H) ->
    decode_value_start(T, Name, State).

decode_value(<<>>, Acc, Del, Name, State) ->
    more(decode_value, {Acc, Del, Name}, State);
decode_value(<<$&, T/binary>>, Acc, Del, Name, State) ->
    decode_value_escape(T, <<>>, Acc, Del, Name, State);
decode_value(<<Del, T/binary>>, Acc, Del, Name, State) ->
    #state{current = ELT = #elt{attrs = Attrs}} = State,
    Attrs1 = lists:reverse([{Name, Acc} | Attrs]),
    State1 = State#state{current = ELT#elt{attrs = Attrs1}},
    decode_name(T, <<>>, State1);
decode_value(<<H, T/binary>>, Acc, Del, Name, State) ->
    decode_value(T, <<Acc/binary, H>>, Del, Name, State).

decode_value_escape(<<>>, Acc, AccValue, Del, Name, State) ->
    more(decode_value_escape, {Acc, AccValue, Del, Name}, State);
decode_value_escape(<<";", T/binary>>, Acc, AccValue, Del, Name, State) ->
    C = unescape(Acc),
    decode_value(T, <<AccValue/binary, C>>, Del, Name, State);
decode_value_escape(<<H, T/binary>>, Acc, AccValue, Del, Name, State) ->
    decode_value_escape(T, <<Acc/binary, H>>, AccValue, Del, Name, State).

%% Child

decode_child(<<>>, Acc, State) -> more(decode_child, Acc, State);
decode_child(<<$<, T/binary>>, <<>>, State) ->
    decode_child(T, <<$<>>, State);
decode_child(<<$/, T/binary>>, <<$<>>, State) ->
    decode_end_tag(T, <<>>, State);
decode_child(<<H, T/binary>>, <<>>, State) when ?WS(H) ->
    decode_child(T, <<>>, State);
decode_child(T, Acc, State) ->
    decode(<<Acc/binary, T/binary>>, push(State)).

%% End

decode_end_tag(<<>>, Acc, State) -> more(decode_end_tag, Acc, State);
decode_end_tag(<<$>, T/binary>>, A, State = #state{current = #elt{tag = A}}) ->
    pop(T, reverse_children(State));
decode_end_tag(<<H, T/binary>>, Acc, State) when ?WS(H) ->
    decode_end(T, Acc, State);
decode_end_tag(<<H, T/binary>>, Acc, State) ->
    decode_end_tag(T, <<Acc/binary, H>>, State).

decode_end(<<$>, T/binary>>, A, State = #state{current = #elt{tag = A}}) ->
    pop(T, reverse_children(State));
decode_end(<<H, T/binary>>, Acc, State) when ?WS(H) ->
    decode_end(T, Acc, State).

%% Empty

decode_empty(<<>>, State) -> more(decode_empty, State);
decode_empty(<<$>, T/binary>>, State) -> pop(T, State);
decode_empty(Binary, State = #state{current = #elt{tag = <<>>}, stack = []}) ->
    decode_eos(Binary, <<>>, State).

%% EOS

decode_eos(<<>>, Acc, State) -> more(decode_eos, Acc, State);
decode_eos(<<$>, T/binary>>, ?STREAM, _) -> {eos, T};
decode_eos(<<H, T/binary>>, ?STREAM, State) when ?WS(H) ->
    decode_eos_end(T, State);
decode_eos(<<H, T/binary>>, Acc, State) ->
    decode_eos(T, <<Acc/binary, H>>, State).

decode_eos_end(<<>>, State) -> more(decode_eos_end, State);
decode_eos_end(<<$>, T/binary>>, _) -> {eos, T};
decode_eos_end(<<H, T/binary>>, State) when ?WS(H) -> decode_eos_end(T, State).

%% Text

decode_text(<<>>, Acc, State) -> more(decode_text, Acc, State);
decode_text(<<H, T/binary>>, Acc, State) when ?WS(H) ->
    decode_text_ws(T, Acc, <<H>>, State);
decode_text(T = <<$<, _/binary>>, Acc, State) ->
    pop(T, State#state{current = Acc});
decode_text(<<$&, T/binary>>, Acc, State) ->
    decode_text_escape(T, <<>>, Acc, State);
decode_text(<<H/utf8, T/binary>>, Acc, State) ->
    decode_text(T, <<Acc/binary, H/utf8>>, State).

decode_text_escape(<<>>, Acc, TextValue, State) ->
    more(decode_text_escape, {Acc, TextValue}, State);
decode_text_escape(<<";", T/binary>>, Acc, TextValue, State) ->
    C = unescape(Acc),
    decode_text(T, <<TextValue/binary, C>>, State);
decode_text_escape(<<H, T/binary>>, Acc, TextValue, State) ->
    decode_text_escape(T, <<Acc/binary, H>>, TextValue, State).

decode_text_ws(<<>>, Acc, WS, State) -> more(decode_text_ws, {Acc, WS}, State);
decode_text_ws(<<H, T/binary>>, Acc, WS, State) when ?WS(H) ->
    decode_text_ws(T, Acc, <<WS/binary, H>>, State);
decode_text_ws(T = <<$<, _/binary>>, Acc, _, State) ->
    pop(T, State#state{current = Acc});
decode_text_ws(<<$&, T/binary>>, Acc, WS, State) ->
    decode_text_escape(T, <<>>, <<Acc/binary, WS/binary>>, State);
decode_text_ws(<<H/utf8, T/binary>>, Acc, WS, State) ->
    decode_text(T, <<Acc/binary, WS/binary, H/utf8>>, State).

%% Hierarchy

push(State = #state{current = C, stack = Stack}) ->
    State#state{current = undefined, stack = [C | Stack]}.

pop(Binary, #state{stack = [], current = C}) -> stop(C, Binary);
pop(Binary, State) ->
    #state{current = C, stack = [H = #elt{children = Cs} | T]} = State,
    State1 = State#state{current = H#elt{children = [C | Cs]}, stack = T},
    decode_child(Binary, <<>>, State1).

reverse_children(State = #state{current = XML = #elt{children = Cs}}) ->
    State#state{current = XML#elt{children = lists:reverse(Cs)}}.

%% -------------------------------------------------------------------
%% Misc
%% -------------------------------------------------------------------

more(F, State) -> more(F, no_args, State).

more(F, A, State) -> {more, State#state{func = F, args = A}}.

stop(C, Binary) ->{ok, C, Binary}.

unescape(<<"lt">>) -> $<;
unescape(<<"gt">>) -> $>;
unescape(<<"quot">>) -> $"; %% "
unescape(<<"apos">>) -> $'; %% '
unescape(<<"amp">>) -> $&.

call(B, State = #state{func = decode, args = no_args}) -> decode(B, State);
call(B, State = #state{func = decode_start, args = Acc}) ->
    decode_start(B, Acc, State);
call(B, State = #state{func = decode_question, args = {Check, Acc}}) ->
    decode_question(B, Check, Acc, State);
call(B, State = #state{func = decode_decl_attr, args = {Check, Acc}}) ->
    decode_decl_attr(B, Check, Acc, State);
call(B, State = #state{func = decode_decl_version_val, args = {Quote,Check}}) ->
    decode_decl_version_val(B, Quote, Check, State);
call(B, State = #state{func = decode_decl_rest, args = Phase}) ->
    decode_decl_rest(B, Phase, State);
call(B, State = #state{func = decode_decl_end, args = {Check, Alone}}) ->
    decode_decl_end(B, Check, Alone, State);
call(B, State = #state{func = decode_decl_enc_val, args = {Quote, Check}}) ->
    decode_decl_enc_val(B, Quote, Check, State);
call(B, State = #state{func = decode_decl_alone_val, args = {Quote, Check}}) ->
    decode_decl_alone_val(B, Quote, Check, State);
call(B, State = #state{func = decode_bang_start, args = no_args}) ->
    decode_bang_start(B, State);
call(B, State = #state{func = decode_bang, args = {Acc, Fun}}) ->
    decode_bang(B, Acc, Fun, State);
call(B, State = #state{func = decode_cdata, args = Acc}) ->
    decode_cdata(B, Acc, State);
call(B, State = #state{func = decode_cdata_end, args = {Acc, Term}}) ->
    decode_cdata_end(B, Acc, Term, State);
call(B, State = #state{func = decode_comment, args = Acc}) ->
    decode_comment(B, Acc, State);
call(B, State = #state{func = decode_comment_end, args = {Check, Acc0, Acc}}) ->
    decode_comment_end(B, Check, Acc0, Acc, State);
call(B, State = #state{func = decode_pi_target, args = Acc}) ->
    decode_pi_target(B, Acc, State);
call(B, State = #state{func = decode_pi_end, args = {Acc, Target}}) ->
    decode_pi_end(B, Acc, Target, State);
call(B, State = #state{func = decode_pi_parts, args = {Acc, Target}}) ->
    decode_pi_parts(B, Acc, Target, State);
call(B, State = #state{func = decode_doctype_name, args = Acc}) ->
    decode_doctype_name(B, Acc, State);
call(B, State = #state{func = decode_doctype_rest, args = Decl}) ->
    decode_doctype_rest(B, Decl, State);
call(B, State = #state{func=decode_doctype_external,args={Check,Type,Decl}}) ->
    decode_doctype_external(B, Check, Type, Decl, State);
call(B, State = #state{func = decode_doctype_external_system,
                       args={Acc, Quote, Decl}}) ->
    decode_doctype_external_system(B, Acc, Quote, Decl, State);
call(B, State = #state{func = decode_doctype_external_public,
                       args={Acc, Quote, Decl}}) ->
    decode_doctype_external_public(B, Acc, Quote, Decl, State);
call(B, State = #state{func = decode_doctype_sub, args = {Acc, Decl}}) ->
    decode_doctype_sub(B, Acc, Decl, State);
call(B, State = #state{func = decode_doctype_per, args = {Acc, Decl}}) ->
    decode_doctype_per(B, Acc, Decl, State);
call(B, State = #state{func = decode_doctype_end, args = Decl}) ->
    decode_doctype_end(B, Decl, State);
call(B, State = #state{func = decode_empty, args = no_args}) ->
    decode_empty(B, State);
call(B, State = #state{func = decode_child, args = Acc}) ->
    decode_child(B, Acc, State);
call(B, State = #state{func = decode_end_tag, args = Acc}) ->
    decode_end_tag(B, Acc, State);
call(B, State = #state{func = decode_text, args = Acc}) ->
    decode_text(B, Acc, State);
call(B, State = #state{func = decode_text_ws, args = {Acc, WS}}) ->
    decode_text_ws(B, Acc, WS, State);
call(B, State = #state{func = decode_name, args = Acc}) ->
    decode_name(B, Acc, State);
call(B, State = #state{func = decode_value_start, args = Name}) ->
    decode_value_start(B, Name, State);
call(B, State = #state{func = decode_value, args = {Acc, Del, Name}}) ->
    decode_value(B, Acc, Del, Name, State);
call(B, State = #state{func = decode_assign, args = Name}) ->
    decode_assign(B, Name, State);
call(B, State = #state{func = decode_eos, args = Acc}) ->
    decode_eos(B, Acc, State);
call(B, State = #state{func = decode_eos_end, args = no_args}) ->
    decode_eos_end(B, State);
call(B, State = #state{func = decode_text_escape, args = {Acc, TextValue}}) ->
    decode_text_escape(B, Acc, TextValue, State);
call(B, State = #state{func = decode_value_escape, args={Acc,AccV,Del,Name}}) ->
    decode_value_escape(B, Acc, AccV, Del, Name, State).

%% call(B, State = #state{func = F, args = no_args}) -> F(B, State);
%% call(B, State = #state{func = F, args = {A1, A2}}) -> F(B, A1, A2, State);
%% call(B, State = #state{func = F, args = {A1,A2,A3}}) -> F(B, A1, A2, A3, State);
%% call(B, State = #state{func = F, args={A1,A2,A3,A4}}) -> F(B,A1,A2,A3,A4,State);
%% call(B, State = #state{func = F, args = A}) -> F(B, A, State).
