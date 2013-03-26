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
%%%  A JSON to and from erlang terms library.
%%%
%%%  JSON is represented as follows:
%%%
%%%  text  : object | array
%%%  object: {[{string, value}*]}
%%%  array : [value*]
%%%  string: atom | <<octet*>>
%%%  number: integer | float
%%%  true  : 'true'
%%%  false : 'false'
%%%  null  : 'null'
%%%
%%%  Strings can be represented by atoms when generating JSON, but will not
%%%  not be generated when converting JSON to erlang. It can be specified
%%%  what encoding is used for the strings with latin-1 being the default.
%%%  All atoms are assumed to be in latin-1 and can not be specified.
%%%
%%%  The encoding of a JSON text is determined and can be specified when
%%%  convering from Erlang terms with the deafult being UTF-8.
%%%
%%%  When converting Erlang terms to JSON iolists are generated but
%%%  it can generate a binary if so instructed.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(json).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2]).

%% Types
-type plain_format() :: latin1 | encoding().
-type encoding()     :: utf8 | {utf16, little | big} | {utf32, little | big}.
-type opt()          :: {atom_strings, boolean()} | binary | iolist |
                        {plain_string, plain_format()} | {encoding, encoding()}.

-type json()        :: json_text().
-type json_text()   :: json_object() | json_array().
-type json_object() :: {[{json_string(), json_value()}]}.
-type json_array()  :: [json_value()].
-type json_value()  :: false | true | null |
                       number() | json_string() |
                       json_object() | json_array().
-type json_string() :: atom() | string().

%% Records
-record(opts, {encoding = utf8 :: encoding(),
               plain_string = latin1 :: plain_format(),
               atom_strings = true :: boolean(),
               return_type = iolist :: iolist | binary
              }).

%% Defines
-define(NULL, 0).
-define(BEL, 7).
-define(BS, 8).
-define(HT, 9).
-define(LF, 10).
-define(VT, 11).
-define(FF, 12).
-define(CR, 13).
-define(SPC, 32).
-define(IS_WS(WS), WS == ?HT; WS == ?LF; WS == ?CR; WS == ?SPC).
-define(ESCAPE(C), C =< 16#1F; C == 34; C == 47; C == 92).

-define(ENCODINGS,
        [utf8, {utf16, little}, {utf16, big}, {utf32, little}, {utf32, big}]).

-define(PLAINFORMATS,
        [latin1 | ?ENCODINGS].

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Term) -> JSON.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent of encode(Term, []) -> JSON.
%% @end
%%--------------------------------------------------------------------
-spec encode(json()) -> iolist().
%%--------------------------------------------------------------------
encode(Plain) -> encode(Plain, []).

%%--------------------------------------------------------------------
%% Function: encode(Term, Options) -> JSON.
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binaryis returned
%%     iolist -> a iolist is returned
%%     {atom_strings, Bool} -> determines if atoms for strings are allowed
%%     {plain_string, Format} -> what format the strings are encoded in
%%     {encoding, Encoding} -> what encoding is used for the resulting JSON
%% @end
%%--------------------------------------------------------------------
-spec encode(json(), [opt()]) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Plain, Opts) ->
    #opts{return_type = Return} = ParsedOpts = parse_opts(Opts),
    case Return of
        iolist -> encode_text(Plain, ParsedOpts);
        binary -> iolist_to_binary(encode_text(Plain, ParsedOpts))
    end.

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

encode_text({Object}, Opts) when is_list(Object) ->
    encode_object(Object, [], Opts);
encode_text(Array, Opts) when is_list(Array) ->
    [encode_char($[, Opts) | encode_array(Array, [], Opts)].

encode_object([], [], Opts) -> encode_chars(<<"{}">>, Opts);
encode_object([{Name, Value}], Acc, Opts) ->
    [encode_char(${, Opts) |
     lists:reverse([encode_char($}, Opts),
                    encode_value(Value, Opts),
                    encode_char($:, Opts),
                    encode_string(Name, Opts) | Acc])];
encode_object([{Name, Value} | T], Acc, Opts) ->
    Value1 = encode_value(Value, Opts),
    Name1 = encode_string(Name, Opts),
    Acc1 = [encode_char($,, Opts), Value1, encode_char($:, Opts), Name1 | Acc],
    encode_object(T, Acc1, Opts).

encode_array([], Acc, Opts) -> lists:reverse([encode_char($],Opts) | Acc]);
encode_array([H], Acc, Opts) ->
    lists:reverse([encode_char($],Opts), encode_value(H, Opts) | Acc]);
encode_array([H | Array], Acc, Opts) ->
    encode_array(Array,
                 [encode_char($,, Opts), encode_value(H, Opts) | Acc],
                 Opts).

encode_value(true, Opts) -> encode_chars(<<"true">>, Opts);
encode_value(false, Opts) -> encode_chars(<<"false">>, Opts);
encode_value(null, Opts) -> encode_chars(<<"null">>, Opts);
encode_value(String, Opts) when is_atom(String) -> encode_string(String, Opts);
encode_value({Object}, Opts) when is_list(Object) ->
    encode_object(Object, [], Opts);
encode_value(Array, Opts) when is_list(Array) ->
    [encode_char($[, Opts) | encode_array(Array, [], Opts)];
encode_value(BinaryString, Opts) when is_binary(BinaryString) ->
    encode_string(BinaryString, Opts);
encode_value(Integer, Opts) when is_integer(Integer) ->
    encode_chars(integer_to_list(Integer), Opts);
%% TODO: the float to list generates horrible result, look at the Burger and
%%        Dybvig but for now use io_lib:format.
encode_value(Float, Opts) when is_float(Float) ->
    [String] = io_lib:format("~p", [Float]),
    encode_chars(String, Opts).

encode_string(Atom, Opts = #opts{atom_strings = true}) when is_atom(Atom) ->
    encode_string(list_to_binary(atom_to_list(Atom)),
                  Opts#opts{plain_string = latin1});
encode_string(String, Opts) when is_binary(String) ->
    #opts{plain_string = Plain, encoding = Encoding} = Opts,
    [encode_char($", Opts),
     char_code(escape(String, Plain, Opts), Plain, Encoding),
     encode_char($", Opts)].

escape(String, Plain, Opts) ->
    case escapeable(String, Plain) of
        true -> escape(String, <<>>, Plain, Opts);
        false -> String
    end.

escapeable(<<>>, _) -> false;
escapeable(<<H, _/binary>>, latin1) when ?ESCAPE(H) -> true;
escapeable(<<H, _/binary>>, utf8) when ?ESCAPE(H) -> true;
escapeable(<<H, _/binary>>, {utf16, little}) when ?ESCAPE(H) -> true;
escapeable(<<_, H, _/binary>>, {utf16, big}) when ?ESCAPE(H) -> true;
escapeable(<<H, _/binary>>, {utf32, little}) when ?ESCAPE(H) -> true;
escapeable(<<_, _, _, H, _/binary>>, {utf32, big}) when ?ESCAPE(H) -> true;
escapeable(<<_, _, T/binary>>, Plain = {utf16, _}) -> escapeable(T, Plain);
escapeable(<<_, _, _, _, T/binary>>, Plain = {utf32,_}) -> escapeable(T, Plain);
escapeable(<<_, T/binary>>, Plain) -> escapeable(T, Plain).

escape(<<>>, Acc, _, _) -> Acc;
escape(<<H, T/binary>>, Acc, latin1, Opts) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H))/binary>>, latin1, Opts);
escape(<<H, T/binary>>, Acc, utf8, Opts) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H))/binary>>, utf8, Opts);
escape(<<H, _, T/binary>>, Acc, Plain = {utf16, little},Opts) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, Opts))/binary>>, Plain, Opts);
escape(<<_, H, T/binary>>, Acc, Plain = {utf16, big}, Opts) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, Opts))/binary>>, Plain, Opts);
escape(<<H, _, _, _,T/binary>>,Acc,Plain={utf32,little},Opts) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, Opts))/binary>>, Plain, Opts);
escape(<<_, _, _, H, T/binary>>, Acc, Plain={utf32,big},Opts) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, opts))/binary>>, Plain, Opts);
escape(<<H1, H2, T/binary>>, Acc, Plain = {utf16, _}, Opts) ->
    escape(T, <<Acc/binary, H1, H2>>, Plain, Opts);
escape(<<H1, H2, H3, H4, T/binary>>, Acc, Plain = {utf32,_}, Opts) ->
    escape(T, <<Acc/binary, H1, H2, H3, H4>>, Plain, Opts);
escape(<<H, T/binary>>, Acc, Plain, Opts) ->
    escape(T, <<Acc/binary, H>>, Plain, Opts).

escape_char(C, Opts) -> encode_chars(escape_char(C), Opts).

escape_char($") -> <<$\\, $">>;
escape_char($\\) -> <<$\\, $\\>>;
escape_char($/) -> <<$\\, $\/>>;
escape_char(?BS) -> <<$\\, $b>>;
escape_char(?FF) -> <<$\\, $f>>;
escape_char(?LF) -> <<$\\, $n>>;
escape_char(?CR) -> <<$\\, $r>>;
escape_char(?HT) -> <<$\\, $t>>;
escape_char(C) -> solidus_escape(C).

solidus_escape(Code) ->
    case integer_to_list(Code, 16) of
        [D] -> <<$\\, $u, $0, $0, $0, D>>;
        [D1, D2] -> <<$\\, $u, $0, $0, D1, D2>>
    end.

encode_chars(Chars, #opts{encoding = utf8}) -> Chars;
encode_chars(Chars, Opts) when is_list(Chars) ->
    << <<(encode_char(C, Opts))/binary>> || C <- Chars>>;
encode_chars(Chars, Opts) when is_binary(Chars) ->
    << <<(encode_char(C, Opts))/binary>> || <<C>> <= Chars>>.

encode_char(C, #opts{encoding = utf8}) -> <<C>>;
encode_char(C, #opts{encoding = {utf16, little}}) -> <<C, 0>>;
encode_char(C, #opts{encoding = {utf16, big}}) -> <<0, C>>;
encode_char(C, #opts{encoding = {utf32, little}}) -> <<C, 0, 0, 0>>;
encode_char(C, #opts{encoding = {utf32, big}}) -> <<0, 0, 0, C>>.

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts([]) -> #opts{};
parse_opts(Opts) -> lists:foldl(fun parse_opt/2, #opts{}, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt({atom_strings, Bool}, Opts) when is_boolean(Bool)->
    Opts#opts{atom_strings = Bool};
parse_opt(Opt = {plain_string, PlainString}, Opts) ->
    case lists:member(PlainString, ?PLAINFORMATS) of
        true -> Opts#opts{plain_string = PlainString};
        false -> erlang:error(badarg, [Opt])
    end;
parse_opt(Opt = {encoding, Encoding} , Opts) ->
    case lists:member(Encoding, ?ENCODINGS) of
        true -> Opts#opts{encoding = Encoding};
        false -> erlang:error(badarg, [Opt])
    end;
parse_opt(Opt, _) ->
    erlang:error(badarg, [Opt]).

char_code(Text, Coding, Coding) -> Text;
char_code(Text, From, To) -> unicode:characters_to_binary(Text, From, To).
