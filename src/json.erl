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
%%%  A JSON library based on:
%%%    The JavaScript Object Notation (JSON) Data Interchange Format (rfc7159)
%%%    JavaScript Object Notation (JSON) Pointer (rfc6901)
%%%    JavaScript Object Notation (JSON) Patch (rfc6902)
%%%    JSON Reference (draft-pbryan-zyp-json-ref-03)
%%%    JSON Schema: core definitions and terminology (draft-zyp-json-schema-04)
%%%
%%%  JSON is represented as follows:
%%%
%%%  text  : object | array
%%%  object: {[{string, value}*]}
%%%  array : [value*]
%%%  string: atom | `<<octet*>>'
%%%  number: integer | float
%%%  true  : 'true'
%%%  false : 'false'
%%%  null  : 'null'
%%%
%%%  Strings can be represented by atoms when generating JSON, but will not
%%%  not be generated when converting JSON to erlang. It can be specified
%%%  what encoding is used for the strings with UTF-8 being the default.
%%%  All atoms are assumed to be in UTF-8 and can not be specified.
%%%
%%%  The encoding of a JSON text is determined and can be specified when
%%%  convering from Erlang terms with the deafult being UTF-8.
%%%
%%%  When converting Erlang terms to JSON iolists are generated but
%%%  it can generate a binary if so instructed.
%%%
%%%  UTF formats are defined in Unicode 5.0 (ISBN 0-321-48091-0).
%%%
%%% Only supports R17 and later.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2015, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(json).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Compiler directives
-compile({no_auto_import, [float_to_binary/1]}).

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2,
         pointer/1, pointer/2,
         select/2, select/3
        ]).

%% Exported types
-export_type([json/0]).

%% Types
-type encoding()     :: utf8 | {utf16, little | big} | {utf32, little | big}.
-type opt()          :: {atom_strings, boolean()} | {atom_keys, boolean()} |
                        {existing_atom_keys, boolean()} |
                        bom |binary | iolist | decode |
                        {pointer, encoding()} |
                        {plain_string, encoding()} | {encoding, encoding()}.

-type json()        :: json_text().
-type json_text()   :: json_object() | json_array().
-type json_object() :: {[{json_string(), json_value()}]}.
-type json_array()  :: [json_value()].
-type json_value()  :: false | true | null |
                       number() | json_string() |
                       json_object() | json_array().
-type json_string() :: atom() | string().

-type json_pointer() :: [binary() | atom() | '-' | pos_integer()].

%% Records
-record(opts, {encoding = utf8 :: encoding(),
               plain_string = utf8 :: encoding(),
               atom_strings = true :: boolean(),
               atom_keys = false :: boolean(),
               existing_atom_keys = false :: boolean(),
               bom = false :: boolean(),
               return_type = iolist :: iolist | binary,
               decode = true :: boolean(),
               pointer = utf8 :: encoding(),
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
-define(POINTER_ESCAPE(C), C == $~; C == $/).

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
-define(PLAINFORMATS, ?ENCODINGS).

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
%%     bom -> a UTF byte order mark is added at the head of the encoding
%%     {atom_strings, Bool} -> determines if atoms for strings are allowed
%%     {plain_string, Format} -> what format the strings are encoded in
%%     {encoding, Encoding} -> what encoding is used for the resulting JSON
%% @end
%%--------------------------------------------------------------------
-spec encode(json(), [opt()] | #opts{}) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Term, Opts = #opts{}) ->
    encode_text(Term, Opts);
encode(Term, Opts) -> Line = ?LINE,
    #opts{return_type = Return, encoding = Encoding, bom = Bom} = ParsedOpts =
        parse_opts(Opts, #opts{orig_call = {encode, [Term, Opts], Line}}),
    case {Bom, Return} of
        {false, iolist} -> encode_text(Term, ParsedOpts);
        {false, binary} -> iolist_to_binary(encode_text(Term, ParsedOpts));
        {true, iolist} ->
            [unicode:encoding_to_bom(Encoding), encode_text(Term, ParsedOpts)];
        {true, binary} ->
            iolist_to_binary([unicode:encoding_to_bom(Encoding),
                              encode_text(Term, ParsedOpts)])
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
decode(Binary) -> Line = ?LINE,
    decode(Binary, #opts{orig_call = {decode, [Binary], Line},
                         plain_string = utf8}).

%%--------------------------------------------------------------------
%% Function: decode(JSON, Options) -> Term.
%% @doc
%%   Decodes the binary into a structured Erlang.
%%   Decode will give an exception if the binary is not well formed JSON.
%%   Options are:
%%     bom -> the binary to decode has a UTF byte order mark
%%     {plain_string, Format} -> what format the strings are encoded in
%%     {atom_keys, Bool} -> if true all object keys are converted to atoms,
%%                          default is false.
%%     {existing_atom_keys, Bool} -> if true all object keys are converted
%%                          to atoms, decoding fails if the atom does not
%%                          already exist, default is false.
%%     For both atom_keys and existing_atom_keys the string has to Unicode
%%     characters up to 0xFF.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), [opt()] | #opts{}) -> json().
%%--------------------------------------------------------------------
decode(Binary, Opts = #opts{}) ->
    {Binary, Encoding} = encoding(Binary, Opts),
    decode_text(Binary, Opts#opts{encoding = Encoding});
decode(Binary, Opts) -> Line = ?LINE,
    OptsRec = parse_opts(Opts, #opts{orig_call = {decode, [Binary, Opts], Line},
                                     plain_string = utf8}),
    {Binary1, Encoding} = encoding(Binary, OptsRec),
    decode_text(Binary1, OptsRec#opts{encoding = Encoding}).

%%--------------------------------------------------------------------
%% Function: pointer(Term) -> Pointer Pointer.
%% @doc
%%   Encodes a term as a JSON pointer.
%%   Equivalent of pointer(Term, []) -> Pointer.
%% @end
%%--------------------------------------------------------------------
-spec pointer(json_pointer()) -> binary().
%%--------------------------------------------------------------------
pointer(Term) -> Line = ?LINE,
    pointer(Term, #opts{orig_call = {pointer, [Term], Line}}).

%%--------------------------------------------------------------------
%% Function: pointer(Term, Options) -> Json Pointer
%% @doc
%%   Encodes a term as a JSON pointer.
%%   Pointer will give an exception if the Term is malformed.
%%   the pointer not well formed json_string.
%%   Options are:
%%     binary -> a binary is returned
%%     iolist -> a iolist is returned
%%     {pointer, Format} -> The UTF encoding of the pointer, default UTF-8
%%     {plain_string, Format} -> what format the strings are encoded in
%%     {atom_strings, Bool}
%% @end
%%--------------------------------------------------------------------
-spec pointer(json_pointer(), [opt()]) -> binary().
%%--------------------------------------------------------------------
pointer(Term, Opts = #opts{}) -> pointer_gen(Term, Opts, []);
pointer(Term, Opts) -> Line = ?LINE,
    OptsRec = #opts{return_type = Return} =
        parse_opts(Opts,
                   #opts{orig_call = {pointer, [Term, Opts], Line}}),
    case Return of
        iolist -> pointer_gen(Term, OptsRec, []);
        binary -> iolist_to_binary(pointer_gen(Term, OptsRec, []))
    end.

%%--------------------------------------------------------------------
%% Function: select(JSONPointer, JSON) -> Term.
%% @doc
%%   Selects and decodes a Fragment of a JSON document based on the Pointer.
%%   Equivalent of select(JSONPointer, JSON, []) -> Term.
%% @end
%%--------------------------------------------------------------------
-spec select(binary(), binary()) -> json_value() | binary() | {error, _}.
%%--------------------------------------------------------------------
select(Pointer, Binary) -> Line = ?LINE,
    decode(Binary, #opts{orig_call = {select, [Pointer, Binary], Line}}).

%%--------------------------------------------------------------------
%% Function: select(JSONPointer, JSON, Options) -> Term or Fragment or error.
%% @doc
%%   Selects and optionally decodes a Fragment of a JSON document based on
%%%  the Pointer.
%%   Select will give an exception if the binary is not well formed JSON,
%%   the pointer not well formed json_string.
%%   Options are:
%%     {decode, Bool} -> the fragment of the JSON selected(value) is decoded,
%%                       default true.
%%     bom -> the binary to decode has a UTF byte order mark
%%     {pointer, Format} -> The UTF encoding of the pointer
%%   Options passed to decoding if enabled:
%%     {plain_string, Format}
%%     {atom_keys, Bool}
%%     {existing_atom_keys, Bool}
%% @end
%%--------------------------------------------------------------------
-spec select(binary(), binary(), [opt()]) ->
          json_value() | binary() | {error, _}.
%%--------------------------------------------------------------------
select(Pointer, Binary, Opts = #opts{}) ->
    {Binary, Encoding} = encoding(Binary, Opts),
    select_text(Pointer, Binary, Opts#opts{encoding = Encoding});
select(Pointer, Binary, Opts) -> Line = ?LINE,
    OptsRec = parse_opts(Opts, #opts{orig_call = {select, [Binary, Opts], Line},
                                     plain_string = utf8}),
    {Binary1, Encoding} = encoding(Binary, OptsRec),
    select_text(Pointer, Binary1, OptsRec#opts{encoding = Encoding}).


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
    encode_chars(float_to_binary(Float), Opts);
encode_value(_, Opts) ->
    badarg(Opts).

encode_string(Atom, Opts = #opts{atom_strings = true}) when is_atom(Atom) ->
    encode_string(atom_to_binary(Atom, utf8), Opts#opts{plain_string = utf8});
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
escapeable(<<H, _/binary>>, utf8) when ?ESCAPE(H) -> true;
escapeable(<<H, 0, _/binary>>, {utf16, little}) when ?ESCAPE(H) -> true;
escapeable(<<0, H, _/binary>>, {utf16, big}) when ?ESCAPE(H) -> true;
escapeable(<<H, 0:24, _/binary>>, {utf32, little}) when ?ESCAPE(H) -> true;
escapeable(<<0:24, H, _/binary>>, {utf32, big}) when ?ESCAPE(H) -> true;
escapeable(<<_:16, T/binary>>, Plain = {utf16, _}) -> escapeable(T, Plain);
escapeable(<<_:32, T/binary>>, Plain = {utf32,_}) -> escapeable(T, Plain);
escapeable(<<_, T/binary>>, Plain) -> escapeable(T, Plain).

escape(<<>>, Acc, _, _) -> Acc;
escape(<<H, T/binary>>, Acc, utf8, Opts) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H))/binary>>, utf8, Opts);
escape(<<H, 0, T/binary>>, Acc, Plain = {utf16, little},Opts) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, Opts))/binary>>, Plain, Opts);
escape(<<0, H, T/binary>>, Acc, Plain = {utf16, big}, Opts) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, Opts))/binary>>, Plain, Opts);
escape(<<H, 0:24,T/binary>>,Acc,Plain={utf32, little},Opts) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, Opts))/binary>>, Plain, Opts);
escape(<<0:24, H, T/binary>>, Acc, Plain={utf32, big},Opts) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, Opts))/binary>>, Plain, Opts);
escape(<<H:16, T/binary>>, Acc, Plain = {utf16, _}, Opts) ->
    escape(T, <<Acc/binary, H:16>>, Plain, Opts);
escape(<<H:32, T/binary>>, Acc, Plain = {utf32,_}, Opts) ->
    escape(T, <<Acc/binary, H:32>>, Plain, Opts);
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
insert_decimal(Place, <<S>>, Sign) when Place < 0, Place > -4 ->
    <<Sign/binary, "0.", (binary:copy(<<$0>>, -Place))/binary, S>>;
insert_decimal(Place, S = <<_>>, Sign) when Place < 0 ->
    insert_exp(S, integer_to_binary(Place - 1), Sign);
insert_decimal(Place, S, Sign) when Place < 0 ->
    ExpL = integer_to_binary(Place - 1),
    case  -Place =< byte_size(ExpL) of
        true ->
            Naughts = binary:copy(<<$0>>, -Place),
            <<Sign/binary, "0.", Naughts/binary, S/binary>>;
        false ->
            insert_exp(S, ExpL, Sign)
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
    Frac = binary_part(S, {Place, byte_size(S) - Place}),
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

encoding(Binary, #opts{bom = true}) ->
    {Encoding, Size} = unicode:bom_to_encoding(Binary),
    BOM = (8 * Size),
    <<_:BOM, Binary1/binary>> = Binary,
    {Binary1, Encoding};
encoding(B = <<_, 0, 0, 0, _/binary>>, _) -> {B, {utf32, little}};
encoding(B = <<0, 0, 0, _/binary>>, _) -> {B, {utf32, big}};
encoding(B = <<_, 0, _, 0, _/binary>>, _) -> {B, {utf16, little}};
encoding(B = <<0, _, 0, _/binary>>, _) -> {B, {utf16, big}};
encoding(B, _) -> {B, utf8}.

decode_text(Binary, Opts) ->
    case next(Binary, Opts) of
        {WS, T} when ?IS_WS(WS)-> decode_text(T, Opts);
        {${, T} -> {Object, _} = decode_object(T,{false,false},[],Opts), Object;
        {$[, T}-> {Array, _} = decode_array(T, {false, false}, [], Opts), Array;
        _ -> badarg(Opts)
    end.

decode_object(Binary, Expect, Acc, Opts) ->
    case {next(Binary, Opts), Expect} of
        {{WS, T}, _} when ?IS_WS(WS) -> decode_object(T, Expect, Acc, Opts);
        {{$}, T}, {false, _}} -> {{lists:reverse(Acc)}, T};
        {{$,, T}, {false, true}} -> decode_object(T, {true, false}, Acc, Opts);
        {{$", T}, {_, false}} when Opts#opts.atom_keys ->
            {Name, T1} = decode_string(T, Opts#opts{plain_string = utf8}),
            Name1 = binary_to_atom(Name, utf8),
            {Value, T2} = decode_value(skip(T1, $:, Opts), Opts),
            decode_object(T2, {false, true}, [{Name1, Value} | Acc], Opts);
        {{$", T}, {_, false}} when Opts#opts.existing_atom_keys ->
            {Name, T1} = decode_string(T, Opts#opts{plain_string = utf8}),
            Name1 = binary_to_existing_atom(Name, utf8),
            {Value, T2} = decode_value(skip(T1, $:, Opts), Opts),
            decode_object(T2, {false, true}, [{Name1, Value} | Acc], Opts);
        {{$", T}, {_, false}} ->
            {Name, T1} = decode_string(T, Opts),
            {Value, T2} = decode_value(skip(T1, $:, Opts), Opts),
            decode_object(T2, {false, true}, [{Name, Value} | Acc], Opts);
        _ ->
            badarg(Opts)
    end.

decode_array(Binary, Expect, Acc, Opts) ->
    case {next(Binary, Opts), Expect} of
        {{WS, T}, _} when ?IS_WS(WS) -> decode_array(T, Expect, Acc, Opts);
        {{$,, T}, {false, true}} -> decode_array(T, {true, false}, Acc, Opts);
        {{$], T}, {false, _}} -> {lists:reverse(Acc), T};
        {_, {_, false}} ->
            {Value, T} = decode_value(Binary, Opts),
            decode_array(T, {false, true}, [Value | Acc], Opts);
        _ ->
            badarg(Opts)
    end.

decode_value(Binary, Opts) ->
    case next(Binary, Opts) of
        {WS, T} when ?IS_WS(WS) -> decode_value(T, Opts);
        {$t, T} -> decode_base("rue", T, true, Opts);
        {$f, T} -> decode_base("alse", T, false, Opts);
        {$n, T} -> decode_base("ull", T, null, Opts);
        {${, T} -> decode_object(T, {false, false}, [], Opts);
        {$[, T} -> decode_array(T, {false, false}, [], Opts);
        {$", T} -> decode_string(T, Opts);
        {$-, T} -> decode_number(T, pre, int, [$-], Opts);
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
    {char_code(iolist_to_binary(Unescaped), Encoding, Plain), T}.

unescape(Binary, Acc, Opts = #opts{encoding = Encoding}) ->
    case next(Binary, Opts) of
        {$\\, T} -> unescape_solid(T, Acc, Opts);
        {$", T} -> {lists:reverse(Acc), T};
        {H, T} when Encoding == utf8 -> unescape(T, [H | Acc], Opts);
        _ when Encoding == {utf16, little}; Encoding == {utf16, big} ->
            <<H:16, T/binary>> = Binary,
            unescape(T, [<<H:16>> | Acc], Opts);
        _ when Encoding == {utf32, little}; Encoding == {utf32, big} ->
            <<H:32, T/binary>> = Binary,
            unescape(T, [<<H:32>> | Acc], Opts)
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
    <<A, 0:24, B, 0:24, C, 0:24, D, 0:24, T/binary>> = Binary,
    unescape(T, [encode_hex([A, B, C, D], Opts) | Acc], Opts);
unescape_hex(Binary, Acc, Opts = #opts{encoding = {utf32, big}}) ->
    <<0:24, A, 0:24, B, 0:24, C, 0:24, D, T/binary>> = Binary,
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

next(<<H, T/binary>>, #opts{encoding = utf8}) -> {H, T};
next(<<H, 0, T/binary>>, #opts{encoding = {utf16, little}}) -> {H, T};
next(<<0, H, T/binary>>, #opts{encoding = {utf16, big}}) -> {H, T};
next(<<H:16, T/binary>>, #opts{encoding = {utf16, _}}) -> {<<H:16>>, T};
next(<<H, 0:24, T/binary>>, #opts{encoding = {utf32, little}}) -> {H, T};
next(<<0:24, H, T/binary>>, #opts{encoding = {utf32, big}}) -> {H, T};
next(<<H:32, T/binary>>, #opts{encoding = {utf32, _}}) -> {<<H:32>>, T};
next(_, Opts) -> badarg(Opts).

%% ===================================================================
%% Pointer encoding
%% ===================================================================

pointer_gen([], _, Acc) -> lists:reverse(Acc);
pointer_gen([H | T], Opts, Acc) when is_binary(H) ->
    #opts{plain_string = Plain, pointer = Encoding} = Opts,
    H1 = [encode_char($/, Opts#opts{encoding = Encoding}),
          char_code(pointer_escape(H, Plain), Plain, Encoding)],
    pointer_gen(T, Opts, [H1 | Acc]);
pointer_gen(['-' | T], Opts = #opts{pointer = Encoding}, Acc) ->
    H1 = encode_chars([$/, $-], Opts#opts{encoding = Encoding}),
    pointer_gen(T, Opts, [H1 | Acc]);
pointer_gen([H | T], Opts, Acc) when is_atom(H) ->
    #opts{atom_strings = true, plain_string = Plain} = Opts,
    H1 = iolist_to_binary(char_code(atom_to_binary(H, utf8), utf8, Plain)),
    pointer_gen([H1 | T], Opts, Acc);
pointer_gen([H | T], Opts, Acc) when is_integer(H), H >= 0 ->
    H1 = encode_chars([$/ | integer_to_list(H)],
                      Opts#opts{encoding = Opts#opts.pointer}),
    pointer_gen(T, Opts, [H1 | Acc]);
pointer_gen(_, Opts, _) ->
    badarg(Opts).

pointer_escape(String, Plain) ->
    case pointer_escapeable(String, Plain) of
        true -> pointer_escape(String, <<>>, Plain);
        false -> String
    end.

pointer_escapeable(<<>>, _) -> false;
pointer_escapeable(<<H, _/binary>>, utf8) when ?POINTER_ESCAPE(H) -> true;
pointer_escapeable(<<H, 0, _/binary>>, {utf16, little})
  when ?POINTER_ESCAPE(H) -> true;
pointer_escapeable(<<0, H, _/binary>>, {utf16, big})
  when ?POINTER_ESCAPE(H) -> true;
pointer_escapeable(<<H, 0:24, _/binary>>, {utf32, little})
  when ?POINTER_ESCAPE(H) -> true;
pointer_escapeable(<<0:24, H, _/binary>>, {utf32, big})
  when ?POINTER_ESCAPE(H) -> true;
pointer_escapeable(<<_:16, T/binary>>, Plain = {utf16, _}) ->
    pointer_escapeable(T, Plain);
pointer_escapeable(<<_:32, T/binary>>, Plain = {utf32, _}) ->
    pointer_escapeable(T, Plain);
pointer_escapeable(<<_, T/binary>>, Plain) ->
    pointer_escapeable(T, Plain).

pointer_escape(<<>>, Acc, _) -> Acc;
pointer_escape(<<H, T/binary>>, Acc, utf8)
  when ?POINTER_ESCAPE(H) ->
    pointer_escape(T, <<Acc/binary, (pointer_escape_char(H))/binary>>, utf8);
pointer_escape(<<H, 0, T/binary>>, Acc, Plain = {utf16, little})
  when ?POINTER_ESCAPE(H) ->
    Acc1 = <<Acc/binary, (pointer_escape_char(H, Plain))/binary>>,
    pointer_escape(T, Acc1, Plain);
pointer_escape(<<0, H, T/binary>>, Acc, Plain = {utf16, big})
  when ?POINTER_ESCAPE(H) ->
    Acc1 = <<Acc/binary, (pointer_escape_char(H, Plain))/binary>>,
    pointer_escape(T, Acc1, Plain);
pointer_escape(<<H, 0:24,T/binary>>,Acc,Plain = {utf32, little})
  when ?POINTER_ESCAPE(H) ->
    Acc1 = <<Acc/binary, (pointer_escape_char(H, Plain))/binary>>,
    pointer_escape(T, Acc1, Plain);
pointer_escape(<<0:24, H, T/binary>>, Acc, Plain = {utf32, big})
  when ?POINTER_ESCAPE(H) ->
    Acc1 = <<Acc/binary, (pointer_escape_char(H, Plain))/binary>>,
    pointer_escape(T, Acc1, Plain);
pointer_escape(<<H:16, T/binary>>, Acc, Plain = {utf16, _}) ->
    pointer_escape(T, <<Acc/binary, H:16>>, Plain);
pointer_escape(<<H:32, T/binary>>, Acc, Plain = {utf32, _}) ->
    pointer_escape(T, <<Acc/binary, H:32>>, Plain);
pointer_escape(<<H, T/binary>>, Acc, Plain) ->
    pointer_escape(T, <<Acc/binary, H>>, Plain).

pointer_escape_char(C, Plain) ->
    encode_chars(pointer_escape_char(C), #opts{encoding = Plain}).

pointer_escape_char($~) -> <<$~, $0>>;
pointer_escape_char($/) -> <<$~, $1>>.

%% ===================================================================
%% Selection
%% ===================================================================

select_text(<<"">>, Bin, Opts) when is_binary(Bin) -> decode(Bin, Opts);
select_text(_, _, _) -> nyi.

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(bom, Opts) -> Opts#opts{bom = true};
parse_opt(decode, Opts) -> Opts#opts{decode = false};
parse_opt({atom_strings, Bool}, Opts) when is_boolean(Bool)->
    Opts#opts{atom_strings = Bool};
parse_opt({atom_keys, Bool}, Opts) when is_boolean(Bool)->
    Opts#opts{atom_keys = Bool};
parse_opt({existing_atom_keys, Bool}, Opts) when is_boolean(Bool)->
    Opts#opts{existing_atom_keys = Bool};
parse_opt({plain_string, PlainString}, Opts) ->
    case lists:member(PlainString, ?PLAINFORMATS) of
        true -> Opts#opts{plain_string = PlainString};
        false -> badarg(Opts)
    end;
parse_opt({encoding, Encoding} , Opts) ->
    case lists:member(Encoding, ?ENCODINGS) of
        true -> Opts#opts{encoding = Encoding};
        false -> badarg(Opts)
    end;
parse_opt({pointer, Encoding} , Opts) ->
    case lists:member(Encoding, ?PLAINFORMATS) of
        true -> Opts#opts{pointer = Encoding};
        false -> badarg(Opts)
    end;
parse_opt(_, Rec) ->
    badarg(Rec).

encode_chars(Chars, #opts{encoding = utf8}) -> Chars;
encode_chars(Chars, Opts) when is_list(Chars) ->
    << <<(encode_char(C, Opts))/binary>> || C <- Chars>>;
encode_chars(Chars, Opts) when is_binary(Chars) ->
    << <<(encode_char(C, Opts))/binary>> || <<C>> <= Chars>>.

encode_char(C, #opts{encoding = utf8}) -> <<C>>;
encode_char(C, #opts{encoding = {utf16, little}}) -> <<C, 0>>;
encode_char(C, #opts{encoding = {utf16, big}}) -> <<0, C>>;
encode_char(C, #opts{encoding = {utf32, little}}) -> <<C, 0:24>>;
encode_char(C, #opts{encoding = {utf32, big}}) -> <<0:24, C>>.

char_code(Text, Coding, Coding) -> Text;
char_code(Text, utf8, {utf16, big}) ->
    << <<C/utf16-big>> || <<C/utf8>> <= Text >>;
char_code(Text, utf8, {utf16, little}) ->
    << <<C/utf16-little>> || <<C/utf8>> <= Text >>;
char_code(Text, utf8, {utf32, big}) ->
    << <<C/utf32-big>> || <<C/utf8>> <= Text >>;
char_code(Text, utf8, {utf32, little}) ->
    << <<C/utf32-little>> || <<C/utf8>> <= Text >>;
char_code(Text, {utf16, big}, utf8) ->
    << <<C/utf8>> || <<C/utf16-big>> <= Text >>;
char_code(Text, {utf16, big}, {utf16, little}) ->
    << <<C/utf16-little>> || <<C/utf16-big>> <= Text >>;
char_code(Text, {utf16, big}, {utf32, big}) ->
    << <<C/utf32-big>> || <<C/utf16-big>> <= Text >>;
char_code(Text, {utf16, big}, {utf32, little}) ->
    << <<C/utf32-little>> || <<C/utf16-big>> <= Text >>;
char_code(Text, {utf16, little}, utf8) ->
    << <<C/utf8>> || <<C/utf16-little>> <= Text >>;
char_code(Text, {utf16, little}, {utf16, big}) ->
    << <<C/utf16-big>> || <<C/utf16-little>> <= Text >>;
char_code(Text, {utf16, little}, {utf32, big}) ->
    << <<C/utf32-big>> || <<C/utf16-little>> <= Text >>;
char_code(Text, {utf16, little}, {utf32, little}) ->
    << <<C/utf32-little>> || <<C/utf16-little>> <= Text >>;
char_code(Text, {utf32, big}, utf8) ->
    << <<C/utf8>> || <<C/utf32-big>> <= Text >>;
char_code(Text, {utf32, big}, {utf16, big}) ->
    << <<C/utf16-big>> || <<C/utf32-big>> <= Text >>;
char_code(Text, {utf32, big}, {utf16, little}) ->
    << <<C/utf16-little>> || <<C/utf32-big>> <= Text >>;
char_code(Text, {utf32, big}, {utf32, little}) ->
    << <<C/utf32-little>> || <<C/utf32-big>> <= Text >>;
char_code(Text, {utf32, little}, utf8) ->
    << <<C/utf8>> || <<C/utf32-little>> <= Text >>;
char_code(Text, {utf32, little}, {utf16, big}) ->
    << <<C/utf16-big>> || <<C/utf32-little>> <= Text >>;
char_code(Text, {utf32, little}, {utf16, little}) ->
    << <<C/utf16-little>> || <<C/utf32-little>> <= Text >>;
char_code(Text, {utf32, little}, {utf32, big}) ->
    << <<C/utf32-big>> || <<C/utf32-little>> <= Text >>.

%%--------------------------------------------------------------------
-spec badarg(#opts{}) -> no_return().
%%--------------------------------------------------------------------
badarg(#opts{orig_call = {Funcion, Args, Line}}) ->
    Trace = [{?MODULE, Funcion, Args, [{file, ?FILE}, {line, Line}]} |
             lists:dropwhile(fun(T) -> element(1, T) == ?MODULE end,
                             erlang:get_stacktrace())],
    exit({badarg, Trace}).
