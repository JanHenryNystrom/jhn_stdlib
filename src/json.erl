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
%%%  A JSON to and from erlang terms library based on rfc4627.
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
%%%
%%%  latin1 is defined in ISO-8859-1.
%%%  UTF formats are defined in Unicode 5.0 (ISBN 0-321-48091-0).
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(json).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Compiler directives
-compile({no_auto_import, [float_to_binary/1]}).

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2
        ]).

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
               return_type = iolist :: iolist | binary,
               orig_call
              }).

%% Defines

%% Char macros
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

%% Decode macros
-define(IS_INT(C), C>=$0, C=<$9).
-define(IS_POS_INT(C), C>=$1, C=<$9).
-define(IS_SIGN(C), C == $-; C == $+).
-define(IS_EXP(C), C==$E; C==$e).
-define(ZERO_OR_POST(Stage), Stage == zero; Stage == post).
-define(EXP_ZERO_OR_POST(C, Stage),
        ((Stage == zero) orelse (Stage == post))
        andalso ((C == $E) orelse (C == $e))).

%% Supported encodings
-define(ENCODINGS,
        [utf8, {utf16, little}, {utf16, big}, {utf32, little}, {utf32, big}]).

%% Supported string formats
-define(PLAINFORMATS,
        [latin1 | ?ENCODINGS]).

%% Defines for float_to_binary/1.
-define(BIG_POW, (1 bsl 52)).
-define(MIN_EXP, (-1074)).

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
encode(Term) -> encode(Term, #opts{orig_call = {encode, [Term], ?LINE}}).

%%--------------------------------------------------------------------
%% Function: encode(Term, Options) -> JSON.
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     iolist -> a iolist is returned
%%     {atom_strings, Bool} -> determines if atoms for strings are allowed
%%     {plain_string, Format} -> what format the strings are encoded in
%%     {encoding, Encoding} -> what encoding is used for the resulting JSON
%% @end
%%--------------------------------------------------------------------
-spec encode(json(), [opt()]) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Term, Opts = #opts{}) ->
    encode_text(Term, Opts);
encode(Term, Opts) -> Line = ?LINE,
    #opts{return_type = Return} = ParsedOpts =
        parse_opts(Opts, #opts{orig_call = {encode, [Term, Opts], Line}}),
    case Return of
        iolist -> encode_text(Term, ParsedOpts);
        binary -> iolist_to_binary(encode_text(Term, ParsedOpts))
    end.

%%--------------------------------------------------------------------
%% Function: decode(JSON) -> Term.
%% @doc
%%   Decodes the binary into a structured Erlang term.
%%   Equivalent of decode(JSON, []) -> Term.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> json().
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, #opts{orig_call = {decode, [Binary], ?LINE}}).

%%--------------------------------------------------------------------
%% Function: decode(JSON, Options) -> Term.
%% @doc
%%   Decodes the binary into a structured Erlang.
%%   Decode will give an exception if the binary is not well formed JSON.
%%   Options are:
%%     {plain_string, Format} -> what format the strings are encoded in
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), [opt()]) -> json().
%%--------------------------------------------------------------------
decode(Binary, Opts = #opts{}) ->
    decode_text(Binary, Opts#opts{encoding = encoding(Binary)});
decode(Binary, Opts) -> Line = ?LINE,
    OptsRec = parse_opts(Opts, #opts{orig_call = {decode, [Binary, Opts], Line},
                                     encoding = encoding(Binary)}),
    decode_text(Binary, OptsRec).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

encode_text({Object}, Opts) when is_list(Object) ->
    encode_object(Object, [], Opts);
encode_text(Array, Opts) when is_list(Array) ->
    [encode_char($[, Opts) | encode_array(Array, [], Opts)];
encode_text(_, Opts) ->
    badarg(Opts).


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
    encode_object(T, Acc1, Opts);
encode_object(_, _, Opts) ->
    badarg(Opts).

encode_array([], Acc, Opts) -> lists:reverse([encode_char($],Opts) | Acc]);
encode_array([H], Acc, Opts) ->
    lists:reverse([encode_char($],Opts), encode_value(H, Opts) | Acc]);
encode_array([H | Array], Acc, Opts) ->
    encode_array(Array,
                 [encode_char($,, Opts), encode_value(H, Opts) | Acc],
                 Opts);
encode_array(_, _, Opts) ->
    badarg(Opts).

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
encode_value(Float, Opts) when is_float(Float) ->
    encode_chars(float_to_binary(Float), Opts).

encode_string(Atom, Opts = #opts{atom_strings = true}) when is_atom(Atom) ->
    encode_string(list_to_binary(atom_to_list(Atom)),
                  Opts#opts{plain_string = latin1});
encode_string(String, Opts) when is_binary(String) ->
    #opts{plain_string = Plain, encoding = Encoding} = Opts,
    [encode_char($", Opts),
     char_code(escape(String, Plain, Opts), Plain, Encoding),
     encode_char($", Opts)];
encode_string(_, Opts) ->
    badarg(Opts).

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
    escape(T, <<Acc/binary, (escape_char(H, Opts))/binary>>, Plain, Opts);
escape(<<H1, H2, T/binary>>, Acc, Plain = {utf16, _}, Opts) ->
    escape(T, <<Acc/binary, H1, H2>>, Plain, Opts);
escape(<<H1, H2, H3, H4, T/binary>>, Acc, Plain = {utf32,_}, Opts) ->
    escape(T, <<Acc/binary, H1, H2, H3, H4>>, Plain, Opts);
escape(<<H, T/binary>>, Acc, Plain, Opts) ->
    escape(T, <<Acc/binary, H>>, Plain, Opts).

escape_char(C, #opts{plain_string = Plain}) ->
    encode_chars(escape_char(C), #opts{encoding = Plain}).

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

%% ===================================================================
%% float_to_binary/1 the implementation based on
%% "Printing Floating-Point Numbers Quickly and Accurately"
%%  by R.,G. Burger and R.,K. Dybvig in Proceedings of the SIGPLAN '96
%%  Conference on Programming Language Design and Implementation.
%% ===================================================================

float_to_binary(0.0) -> "0.0";
float_to_binary(Float) when is_float(Float) ->
    {Sign, Frac, Exp} = mantissa_exponent(Float),
    {Place, Digits} = float_to_digits(Float, Exp, Frac, (Frac band 1) =:= 0),
    insert_decimal(Place, << <<($0 + D)>> || <<D>> <= Digits>>, Sign).

mantissa_exponent(F) ->
    case <<F:64/float>> of
        <<Sign:1, 0:11, M:52>> -> % denormalized
            E = log2floor(M),
            {sign(Sign), M bsl (53 - E), E - 52 - 1075};
        <<Sign:1, BE:11, M:52>> when BE < 2047 ->
            {sign(Sign), M + ?BIG_POW, BE - 1075}
    end.

sign(0) -> <<>>;
sign(1) -> <<$->>.

float_to_digits(Float, Exp, Frac, Ok) when Exp >= 0, Frac =:= ?BIG_POW ->
    BExp = 1 bsl Exp,
    scale(Frac * BExp * 4, 4, BExp * 2, BExp, Ok, Float);
float_to_digits(Float, Exp, Frac, Ok) when Exp >=0 ->
    BExp = 1 bsl Exp,
    scale(Frac * BExp * 2, 2, BExp, BExp, Ok, Float);
float_to_digits(Float, Exp, Frac, Ok) when Exp < ?MIN_EXP ->
    BExp = 1 bsl (?MIN_EXP - Exp),
    scale(Frac * 2, 1 bsl (1 - Exp), BExp, BExp, Ok, Float);
float_to_digits(Float, Exp, Frac,Ok) when Exp > ?MIN_EXP,Frac =:= ?BIG_POW ->
    scale(Frac * 4, 1 bsl (2 - Exp), 2, 1, Ok, Float);
float_to_digits(Float, Exp, Frac, Ok) ->
    scale(Frac * 2, 1 bsl (1 - Exp), 1, 1, Ok, Float).

scale(R, S, MPlus, MMinus, Ok, Float) ->
    case int_ceil(math:log10(abs(Float)) - 1.0e-10) of
        Est when Est >= 0 ->
            fixup(R, S * int_pow(10, Est), MPlus, MMinus, Est, Ok);
        Est ->
            Scale = int_pow(10, -Est),
            fixup(R * Scale, S, MPlus * Scale, MMinus * Scale, Est, Ok)
    end.

fixup(R, S, MPlus, MMinus, K, Ok = true) when R + MPlus >= S ->
    {K + 1, generate(R, S, MPlus, MMinus, Ok, <<>>)};
fixup(R, S, MPlus, MMinus, K, Ok = false) when R + MPlus > S ->
    {K + 1, generate(R, S, MPlus, MMinus, Ok, <<>>)};
fixup(R, S, MPlus, MMinus, K, Ok) ->
    {K, generate(R * 10, S, MPlus * 10, MMinus * 10, Ok, <<>>)}.

generate(R0, S, MPlus, MMinus, true, Acc) ->
    D = R0 div S,
    R = R0 rem S,
    generate(R =< MMinus, R + MPlus >= S, D, R, S, MPlus, MMinus, true, Acc);
generate(R0, S, MPlus, MMinus, false, Acc) ->
    D = R0 div S,
    R = R0 rem S,
    generate(R < MMinus, R + MPlus > S, D, R, S, MPlus, MMinus, false, Acc).

generate(true, false, D, _, _, _, _, _, Acc) -> <<Acc/binary, D>>;
generate(true, true, D, R, S, _, _, _, Acc) when R * 2 < S -> <<Acc/binary, D>>;
generate(true, true, D, _, _, _, _, _, Acc) -> <<Acc/binary, (D + 1)>>;
generate(false, true, D, _, _, _, _, _, Acc) -> <<Acc/binary, (D + 1)>>;
generate(false, false, D, R, S, MPlus, MMinus, Ok, Acc) ->
    generate(R * 10, S, MPlus * 10, MMinus * 10, Ok, <<Acc/binary,D>>).

insert_decimal(0, S, Sign) -> <<Sign/binary, "0.", S/binary>>;
insert_decimal(Place, S = <<_>>, Sign) when Place < 0, Place >= -2 ->
    <<Sign/binary, "0.", (binary:copy(<<$0>>, -Place))/binary, S>>;
insert_decimal(Place, S = <<_>>, Sign) when Place < 0 ->
    insert_exp(S, integer_to_binary(Place - 1), Sign);
insert_decimal(Place, S, Sign) when Place < 0 ->
    ExpL = integer_to_binary(Place - 1),
    case  -Place =< byte_size(ExpL) of
        true -> <<Sign/binary, "0.", (binary:copy(<<$0>>, -Place))/binary, S>>;
        false -> insert_exp(S, ExpL, Sign)
    end;
insert_decimal(Place, S = <<_>>, Sign) ->
    ExpL = integer_to_binary(Place - 1),
    case Place =< byte_size(ExpL) + 2 of
        true ->
            Naughts = binary:copy(<<$0>>, Place - 1),
            <<Sign/binary, S/binary, Naughts/binary, ".0">>;
        false ->
            insert_exp(S, ExpL, Sign)
    end;
insert_decimal(Place, S, Sign) when Place >= byte_size(S) ->
    L = byte_size(S),
    ExpL = integer_to_binary(Place - 1),
    case Place - L =< byte_size(ExpL) of
        true ->
            Naughts = binary:copy(<<$0>>, Place - L),
            <<Sign/binary, S/binary, Naughts/binary, ".0">>;
        false ->
            insert_exp(S, ExpL, Sign)
    end;
insert_decimal(Place, S, Sign) ->
    Int = binary_part(S, {0, Place}),
    Frac = binary_part(S, {0, byte_size(S) - Place}),
    <<Sign/binary, Int/binary, ".", Frac/binary>>.

insert_exp(<<C>>, ExpL, Sign) -> <<Sign/binary, C, ".0e", ExpL/binary>>;
insert_exp(<<C, S/binary>>, ExpL, Sign) ->
    <<Sign/binary, C, ".", S/binary, "e", ExpL/binary>>.

int_ceil(X) when is_float(X) ->
    T = trunc(X),
    case (X - T) of
        Neg when Neg =< 0 -> T;
        Pos when Pos > 0 -> T + 1
    end.

int_pow(X, 0) when is_integer(X) -> 1;
int_pow(X, N) when is_integer(X), is_integer(N), N > 0 -> int_pow(X, N, 1).

int_pow(X, N, R) when N < 2 -> R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).

log2floor(Int) when is_integer(Int), Int > 0 -> log2floor(Int, 0).

log2floor(0, N) -> N;
log2floor(Int, N) -> log2floor(Int bsr 1, 1 + N).

%% ===================================================================
%% Decoding
%% ===================================================================

encoding(<<_, 0, 0, 0, _/binary>>) -> {utf32, little};
encoding(<<0, 0, 0, _/binary>>) ->    {utf32, big};
encoding(<<_, 0, _, 0, _/binary>>) -> {utf16, little};
encoding(<<0, _, 0, _/binary>>) ->    {utf16, big};
encoding(_) -> utf8.

decode_text(Binary, Opts) ->
    case next(Binary, Opts) of
        {WS, T} when ?IS_WS(WS)-> decode_text(T, Opts);
        {${, T} -> {Object, _} = decode_object(T, false, [], Opts), Object;
        {$[, T}-> {Array, _} = decode_array(T, false, [], Opts), Array;
        _ -> badarg(Opts)
    end.

decode_object(Binary, Expect, Acc, Opts) ->
    case {next(Binary, Opts), Expect} of
        {{WS, T}, _} when ?IS_WS(WS) -> decode_object(T, Expect, Acc, Opts);
        {{$}, T}, _} -> {{lists:reverse(Acc)}, T};
        {{$,, T}, true} -> decode_object(T, false, Acc, Opts);
        {{$", T}, false} ->
            {Name, T1} = decode_string(T, Opts),
            {Value, T2} = decode_value(skip(T1, $:, Opts), Opts),
            decode_object(T2, true, [{Name, Value} | Acc], Opts);
        _ ->
            badarg(Opts)
    end.

decode_array(Binary, Expect, Acc, Opts) ->
    case {next(Binary, Opts), Expect} of
        {{WS, T}, _} when ?IS_WS(WS) -> decode_array(T, Expect, Acc, Opts);
        {{$,, T}, true} -> decode_array(T, false, Acc, Opts);
        {{$], T}, _} -> {lists:reverse(Acc), T};
        {_, false} ->
            {Value, T} = decode_value(Binary, Opts),
            decode_array(T, true, [Value | Acc], Opts);
        _ ->
            badarg(Opts)
    end.

decode_value(Binary, Opts) ->
    case next(Binary, Opts) of
        {WS, T} when ?IS_WS(WS) -> decode_value(T, Opts);
        {$t, T} -> decode_base("rue", T, true, Opts);
        {$f, T} -> decode_base("alse", T, false, Opts);
        {$n, T} -> decode_base("ull", T, null, Opts);
        {${, T} -> decode_object(T, false, [], Opts);
        {$[, T} -> decode_array(T, false, [], Opts);
        {$", T} -> decode_string(T, Opts);
        {$-, T} -> decode_number(T, pre, bint, [$-], Opts);
        {H, _} when H >= $0, H =< $9 ->
            decode_number(Binary, pre, int, [], Opts);
        _ ->
            badarg(Opts)
    end.

decode_base("", T, Value, _) -> {Value, T};
decode_base([H | T], Binary, Value, Opts) ->
    case next(Binary, Opts) of
        {H, Binary1} -> decode_base(T, Binary1, Value, Opts);
        _ -> badarg(Opts)
    end.

decode_number(Binary, Stage, Phase, Acc, Opts) ->
    case {next(Binary, Opts), Stage, Phase} of
        {{$0, T}, pre, int} -> decode_number(T, zero, int, [$0 | Acc], Opts);
        {{H, T}, pre, exp}  when ?IS_SIGN(H) ->
            decode_number(T, sign, exp, [H | Acc], Opts);
        {{H, T}, pre, float} when ?IS_INT(H) ->
            decode_number(T, post, float, [H | Acc], Opts);
        {{H, T}, pre, _} when ?IS_POS_INT(H) ->
            decode_number(T, post, Phase, [H | Acc], Opts);
        {{H, T}, sign, _} when ?IS_POS_INT(H) ->
            decode_number(T, post, Phase, [H | Acc], Opts);
        {{H, T}, post, _} when ?IS_INT(H) ->
            decode_number(T, post, Phase, [H | Acc], Opts);
        {{$., T}, _, int} when ?ZERO_OR_POST(Stage) ->
            decode_number(T, pre, float, [$. | Acc], Opts);
        {{E, T}, _, int} when ?EXP_ZERO_OR_POST(E, Stage) ->
            decode_number(T, pre, exp, [E, $0, $. | Acc], Opts);
        {{E, T}, post, float} when ?IS_EXP(E) ->
            decode_number(T, pre, exp, [E | Acc], Opts);
        {_, State, int} when ?ZERO_OR_POST(State) ->
            {list_to_integer(lists:reverse(Acc)), Binary};
        {_, post, _} ->
            {list_to_float(lists:reverse(Acc)), Binary};
        _ ->
            badarg(Opts)
    end.

decode_string(Binary, Opts=#opts{encoding = Encoding, plain_string = Plain}) ->
    {Unescaped, T} = unescape(Binary, [], Opts),
    {char_code(Unescaped, Encoding, Plain), T}.

unescape(Binary, Acc, Opts) ->
    case next(Binary, Opts) of
        {$\\, T} -> unescape_solid(T, Acc, Opts);
        {$", T} -> {lists:reverse(Acc), T};
        {H, T} -> unescape(T, [H | Acc], Opts)
    end.

unescape_solid(Binary, Acc, Opts) ->
    case next(Binary, Opts) of
        {$", T} -> unescape(T, [encode_char($", Opts) | Acc], Opts);
        {$\\, T} -> unescape(T, [encode_char($\\, Opts) | Acc], Opts);
        {$/, T} -> unescape(T, [encode_char($/, Opts) | Acc], Opts);
        {$0, T} -> unescape(T, [encode_char(?NULL, Opts) | Acc], Opts);
        {$a, T} -> unescape(T, [encode_char(?BEL, Opts) | Acc], Opts);
        {$b, T} -> unescape(T, [encode_char(?BS, Opts) | Acc], Opts);
        {$t, T} -> unescape(T, [encode_char(?HT, Opts) | Acc], Opts);
        {$n, T} -> unescape(T, [encode_char(?LF, Opts) | Acc], Opts);
        {$f, T} -> unescape(T, [encode_char(?FF, Opts) | Acc], Opts);
        {$v, T} -> unescape(T, [encode_char(?VT, Opts) | Acc], Opts);
        {$r, T} -> unescape(T, [encode_char(?CR, Opts) | Acc], Opts);
        {$s, T} -> unescape(T, [encode_char(?SPC, Opts) | Acc], Opts);
        {$u, T} -> unescape_hex(T, Acc, Opts);
        {H, T} when is_integer(H) ->
            unescape(T, [encode_char(H, Opts), encode_char($\\,Opts)|Acc],Opts);
        {H, T} when is_binary(H) ->
            unescape(T, [H, encode_char($\\, Opts) | Acc], Opts)
    end.

unescape_hex(<<A, B, C, D, T/binary>>, Acc, Opts = #opts{encoding = utf8}) ->
    unescape(T, [encode_hex([A, B, C, D], Opts) | Acc], Opts);
unescape_hex(Binary, Acc, Opts = #opts{encoding = {utf16, little}}) ->
    <<A, 0, B, 0, C, 0, D, 0, T/binary>> = Binary,
    unescape(T, [encode_hex([A, B, C, D], Opts) | Acc], Opts);
unescape_hex(Binary, Acc, Opts = #opts{encoding = {utf16, big}}) ->
    <<0, A, 0, B, 0, C, 0, D, T/binary>> = Binary,
    unescape(T, [encode_hex([A, B, C, D], Opts) | Acc], Opts);
unescape_hex(Binary, Acc, Opts = #opts{encoding = {utf32, little}}) ->
    <<A, 0, 0, 0, B, 0, 0, 0, C, 0, 0, 0, D, 0, 0, 0, T/binary>> = Binary,
    unescape(T, [encode_hex([A, B, C, D], Opts) | Acc], Opts);
unescape_hex(Binary, Acc, Opts = #opts{encoding = {utf32, big}}) ->
    <<0, 0, 0, A, 0, 0, 0, B, 0, 0, 0, C, 0, 0, 0, D, T/binary>> = Binary,
    unescape(T, [encode_hex([A, B, C, D], Opts) | Acc], Opts);
unescape_hex(_, _, Opts) ->
    badarg(Opts).

encode_hex(List, Opts) -> encode_char(list_to_integer(List, 16), Opts).

skip(Binary, H, Opts) ->
    case next(Binary, Opts) of
        {H, T} -> T;
        {WS, T} when ?IS_WS(WS) -> skip(T, H, Opts);
        _ -> badarg(Opts)
    end.

next(<<H, T/binary>>, #opts{encoding = utf8}) ->
    {H, T};
next(<<H, 0, T/binary>>, #opts{encoding = {utf16, little}}) ->
    {H, T};
next(<<H1, H2, T/binary>>, #opts{encoding = {utf16, little}}) ->
    {<<H1, H2>>, T};
next(<<0, H, T/binary>>, #opts{encoding = {utf16, big}}) ->
    {H, T};
next(<<H1, H2, T/binary>>, #opts{encoding = {utf16, big}}) ->
    {<<H1, H2>>, T};
next(<<H, 0, 0, 0, T/binary>>, #opts{encoding = {utf32, little}}) ->
    {H, T};
next(<<H1, H2, H3, H4, T/binary>>, #opts{encoding = {utf32, little}}) ->
    {<<H1, H2, H3, H4>>, T};
next(<<0, 0, 0, H, T/binary>>, #opts{encoding = {utf32, big}}) ->
    {H, T};
next(<<H1, H2, H3, H4, T/binary>>, #opts{encoding = {utf32, big}}) ->
    {<<H1, H2, H3, H4>>, T};
next(_, Opts) ->
    badarg(Opts).

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

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
parse_opt(_, Rec) ->
    badarg(Rec).

encode_char(C, #opts{encoding = utf8}) -> <<C>>;
encode_char(C, #opts{encoding = {utf16, little}}) -> <<C, 0>>;
encode_char(C, #opts{encoding = {utf16, big}}) -> <<0, C>>;
encode_char(C, #opts{encoding = {utf32, little}}) -> <<C, 0, 0, 0>>;
encode_char(C, #opts{encoding = {utf32, big}}) -> <<0, 0, 0, C>>.

char_code(Text, Coding, Coding) -> Text;
char_code(Text, From, To) -> unicode:characters_to_binary(Text, From, To).

badarg(#opts{orig_call = {Funcion, Args, Line}}) ->
    Trace = [{?MODULE, Funcion, Args, [{file, ?FILE}, {line, Line}]} |
             lists:dropwhile(fun(T) -> element(1, T) == ?MODULE end,
                             erlang:get_stacktrace())],
    exit({badarg, Trace}).
