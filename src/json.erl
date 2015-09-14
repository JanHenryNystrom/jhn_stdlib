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
%%%    The application/json Media Type for JavaScript Object Notation (JSON)
%%%                                                                    (rfc4627)
%%%    The JavaScript Object Notation (JSON) Data Interchange Format   (rfc7159)
%%%    JavaScript Object Notation (JSON) Pointer                       (rfc6901)
%%%    JavaScript Object Notation (JSON) Patch                         (rfc6902)
%%%    JSON Reference                             (draft-pbryan-zyp-json-ref-03)
%%%    JSON Schema: core definitions and terminology  (draft-zyp-json-schema-04)
%%     JSON Schema: interactive and non interactive validation 
%%                                         (draft-fge-json-schema-validation-00)
%%%
%%%  JSON is represented as follows:
%%%
%%%  text          : value
%%%  rfc4627_text  : object | array (rfc4627 compability mode)
%%%  
%%%  value         : true | false | null | object | array | number | string
%%%
%%%  object        : {[{string, value}*]} |
%%%                  map() (maps option enabled)
%%%  array         : [value*]
%%%  string        : atom() | `<<octet*>>'
%%%  number        : integer() | float()
%%%  true          : atom(true)
%%%  false         : atom(false)
%%%  null          : atom(null)
%%%
%%%  Strings can be represented by atoms when generating JSON, but will not
%%%  not be generated when converting JSON to erlang. It can be specified
%%%  what encoding is used for the strings with UTF-8 being the default.
%%%  All atoms are assumed to be in UTF-8 and can not be specified.
%%%
%%%  The encoding of a JSON text is determined and can be specified when
%%%  converting from Erlang terms with the deafult being UTF-8.
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
         eval/2, eval/3,
         validate/2, validate/3
        ]).

%% Exported types
-export_type([json/0, pointer/0]).

%% Types
-type encoding()     :: utf8 | {utf16, little | big} | {utf32, little | big}.
-type opt()          :: {atom_strings, boolean()} |
                        atom_keys | {atom_keys, boolean()} |
                        existing_atom_keys | {existing_atom_keys, boolean()} |
                        bom | binary | iolist | decode | encode |
                        {plain_string, encoding()} | {encoding, encoding()}.

-type json()                :: json_value() | json_rfc4627_text().
-type json_rfc4627_text()   :: json_object() | json_array().
-type json_object()         :: {[{json_string(), json_value()}]} | map().
-type json_array()          :: [json_value()].
-type json_value()          :: false | true | null |
                               number() | json_string() |
                               json_object() | json_array().
-type json_string()         :: atom() | string().

-type pointer() :: [binary() | atom() | '-' | pos_integer()].

%% Records
-record(state,
        {
          %% Options
          pointer = false :: boolean(),
          maps = false :: boolean() | safe,
          rfc4627 = false :: boolean(),
          encoding = utf8 :: encoding(),
          plain_string = utf8 :: encoding(),
          atom_strings = true :: boolean(),
          atom_keys = false :: boolean(),
          existing_atom_keys = false :: boolean(),
          bom = false :: boolean(),
          return_type = iolist :: iolist | binary,
          decode = false :: boolean(),
          encode = false :: boolean(),
          %% Internal use
          step = 1 :: 1 | 2 | 4,
          pos = 0 :: integer(),
          schema :: json(),
          props_validated = false :: boolean(),
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

-define(UTF16L, {utf16, little}).
-define(UTF16B, {utf16, big}).
-define(UTF32L, {utf32, little}).
-define(UTF32B, {utf32, big}).

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
encode(Term) ->
    encode(Term, #state{orig_call = {encode, [Term], ?LINE}}).

%%--------------------------------------------------------------------
%% Function: encode(Term, Options) -> JSON | JSONPointer.
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     pointer -> the term represents a pointer
%%     rfc4627 -> compability rfc4627 mode
%%     maps -> shorthand for {maps, true}
%%     {maps, Bool} -> if true maps is a valid representation for objects,
%%                     default false.
%%     binary -> a binary is returned
%%     iolist -> an iolist is returned
%%     bom -> a UTF byte order mark is added at the head of the encoding
%%     {atom_strings, Bool} -> determines if atoms for strings are allowed
%%     {plain_string, Format} -> what format the strings are encoded in
%%     {encoding, Encoding} -> what encoding is used for the resulting JSON
%% @end
%%--------------------------------------------------------------------
-spec encode(json() | pointer(), [opt()] | #state{}) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Term, State = #state{}) ->
    encode_value(Term, State);
encode(Term, Opts) ->
    Line = ?LINE,
    State =
        #state{pointer = Pointer,
               rfc4627 = RFC4627,
               return_type = ReturnType,
               encoding = Encoding,
               bom = Bom} =
        parse_opts(Opts, #state{orig_call = {encode, [Term, Opts], Line}}),
    Encoded = case {Pointer, RFC4627} of
                  {true, _} -> encode_pointer(Term, State, []);
                  {false, true} -> encode_rfc4627_text(Term, State);
                  _ -> encode_value(Term, State)
              end,
    case {Bom, ReturnType} of
        {false, iolist} -> Encoded;
        {false, binary} -> iolist_to_binary(Encoded);
        {true, iolist} -> [unicode:encoding_to_bom(Encoding), Encoded];
        {true, binary} ->
            iolist_to_binary([unicode:encoding_to_bom(Encoding), Encoded])
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
decode(Binary) ->
    Line = ?LINE,
    decode(Binary, #state{orig_call = {decode, [Binary], Line}}).

%%--------------------------------------------------------------------
%% Function: decode(JSON | JSONPointer, Options) -> Term.
%% @doc
%%   Decodes the binary into a structured Erlang.
%%   Decode will give an exception if the binary is not well formed JSON.
%%   Options are:
%%     rfc4627 -> compability rfc4627 mode
%%     maps -> shorthand for {maps, true}
%%     {maps, safe} -> maps are used as representation for objects with unique
%%                     items
%%     {maps, Bool} -> if true maps are used as representation for objects,
%%                     since this causes potential compatibility issues it is
%%                     recomended only in combination with schema validation
%%                     where the schema requires unique items, default false.
%%     bom -> the binary to decode has a UTF byte order mark
%%     {plain_string, Format} -> what format the strings are encoded in
%%     atom_keys -> shorthand for {atom_keys, true}
%%     {atom_keys, Bool} -> if true all object keys are converted to atoms,
%%                          default is false.
%%     existing_atom_keys -> shorthand for {existing_atom_keys, true}
%%     {existing_atom_keys, Bool} -> if true all object keys are converted
%%                          to atoms, decoding fails if the atom does not
%%                          already exist, default is false.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), [opt()] | #state{}) -> json().
%%--------------------------------------------------------------------
decode(Binary, State = #state{}) ->
    {Binary, Encoding} = encoding(Binary, State),
    {Value, _} = decode_text(Binary, State#state{encoding = Encoding}),
    Value;
decode(Binary, Opts) ->
    Line = ?LINE,
    State = #state{rfc4627 = RFC4627} =
        parse_opts(Opts, #state{orig_call = {decode, [Binary, Opts], Line}}),
    {Binary1, Encoding} = encoding(Binary, State),
    State1 = State#state{encoding = Encoding},
    case RFC4627 of
        true ->
            {RFC4627Text, _} = decode_rfc4627_text(Binary1, State1),
            RFC4627Text;
        _ ->
            {Value, _} = decode_text(Binary1, State1),
            Value
    end.

%%--------------------------------------------------------------------
%% Function: eval(JSONPointer, JSON) -> Term.
%% @doc
%%   Selects and decodes a Fragment of a JSON document based on the Pointer.
%%   Equivalent of select(JSONPointer, JSON, []) -> Term.
%% @end
%%--------------------------------------------------------------------
-spec eval(binary(), binary()) -> json() | binary() | {error, _}.
%%--------------------------------------------------------------------
eval(Pointer, JSON) ->
    Line = ?LINE,
    eval(Pointer, JSON, #state{orig_call = {eval, [Pointer, JSON], Line}}).

%%--------------------------------------------------------------------
%% Function: eval(JSONPointer, JSON, Options) -> Term or Fragment or error.
%% @doc
%%   Selects and optionally decodes a Fragment of a JSON document based on
%%%  the Pointer.
%%   Select will give an exception if the binary is not well formed JSON,
%%   the pointer not well formed json_string.
%%   Options are:
%%     decode -> the JSON selected(value) is decoded
%%     bom -> the binary to decode has a UTF byte order mark
%%   Options passed to decoding if enabled or the JSON decoded:
%%     maps
%%     {plain_string, Format}
%%     {atom_keys, Bool}
%%     {existing_atom_keys, Bool}
%% @end
%%--------------------------------------------------------------------
-spec eval(pointer() | binary(), json() | binary(), [opt()]) ->
          json() | binary() | {error, _}.
%%--------------------------------------------------------------------
eval(Pointer, JSON, State = #state{}) when is_binary(Pointer) ->
    eval(decode(Pointer, State), JSON, State);
eval(Pointer, Binary, State = #state{decode = true}) when is_binary(Binary) ->
    {Binary1, Enc} = encoding(Binary, State),
    eval_binary(Pointer, Binary1, [], step(State#state{encoding = Enc}));
eval(Pointer, Binary, State = #state{}) when is_binary(Binary) ->
    {Binary1, Enc} = encoding(Binary, State),
    case eval_binary(Pointer, Binary1,[],step(State#state{encoding=Enc})) of
        {pos, Start, Length} -> binary:part(Binary, {Start, Length});
        Error = {error, _} -> Error
    end;
eval(Pointer, Binary, State = #state{}) ->
    {Binary, Encoding} = encoding(Binary, State),
    eval_json(Pointer, Binary, [], State#state{encoding = Encoding});
eval(Pointer, Binary, Opts) ->
    Line = ?LINE,
    State = parse_opts(Opts, #state{orig_call = {eval, [Binary, Opts], Line}}),
    eval(Pointer, Binary, State).

%%--------------------------------------------------------------------
%% Function: validate(JSONSchema, JSON) -> true | {true, Term} | false.
%% @doc
%%   Validates a JSON document based on the Schema.
%%   Equivalent of validate(JSONSchema, JSON, [])
%% @end
%%--------------------------------------------------------------------
-spec validate(binary(), binary()) -> true | {true, json()} | false.
%%--------------------------------------------------------------------
validate(Schema, JSON) -> validate(Schema, JSON, #state{}).

%%--------------------------------------------------------------------
%% Function: validate(JSONSchema, JSON, Options) -> true, {true, Term} | false.
%% @doc
%%   Validates a JSON document, and optionally decodes, based on the Schema
%%   Options are:
%%     decode -> the JSON validated is decoded
%%     bom -> the binary to decode has a UTF byte order mark
%%   Options passed to decoding if enabled or the JSON decoded:
%%     maps
%%     {plain_string, Format}
%%     {atom_keys, Bool}
%%     {existing_atom_keys, Bool}
%% @end
%%--------------------------------------------------------------------
-spec validate(json() | binary(), json() | binary(), [opt()]) ->
          true | {true, json()} | false.
%%--------------------------------------------------------------------
validate(Schema, JSON, State = #state{}) when is_binary(Schema) ->
    validate(decode(Schema, #state{atom_keys = true}), JSON, State);
validate(Schema, Binary, State) when is_binary(Binary) ->
    validate(Schema, decode(Binary, State));
validate(Schema, JSON, State = #state{decode = Decode}) ->
    try {Decode, validate_schema(Schema, JSON, State)} of
        {true, _} -> {true, JSON};
        {false, _} -> true
    catch
        _ : _ -> false
    end;
validate(Schema, JSON, Opts) ->
    validate(Schema, JSON, parse_opts(Opts, #state{})).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

encode_rfc4627_text({Object}, State) -> encode_object(Object, State);
encode_rfc4627_text(Object = #{}, State = #state{maps = true}) ->
    encode_object(Object, State);
encode_rfc4627_text(Array, State) when is_list(Array) ->
    [encode_char($[, State) | encode_array(Array, [], State)];
encode_rfc4627_text(_, State) ->
    badarg(State).

encode_object([], State) -> encode_chars(<<"{}">>, State);
encode_object(Object = #{}, State) -> 
    Comma = encode_char($,, State),
    Colon = encode_char($:, State),
    Encode = fun(Name, Value, Acc) ->
                     [encode_value(Value, State), Colon,
                      encode_string(Name, State), Comma | Acc]
             end,
    case lists:reverse(maps:fold(Encode, [], Object)) of
        [] -> encode_chars(<<"{}">>, State);
        [_ | Members] ->
            [encode_char(${, State), Members, encode_char($}, State)]
    end;
encode_object(Members, State) ->
    Comma = encode_char($,, State),
    Colon = encode_char($:, State),
    encode_object1(Members, [], Comma, Colon, State).

encode_object1([{Name, Value}], Acc, _, Colon, State) ->
    Name1 = encode_string(Name, State),
    Value1 = encode_value(Value, State),
    [encode_char(${, State) |
     lists:reverse([encode_char($}, State), Value1, Colon, Name1 | Acc])];
encode_object1([{Name, Value} | T], Acc, Comma, Colon, State) ->
    Name1 = encode_string(Name, State),
    Value1 = encode_value(Value, State),
    Acc1 = [Comma, Value1, Colon, Name1 | Acc],
    encode_object1(T, Acc1, Comma, Colon, State);
encode_object1(_, _, _, _, State) ->
    badarg(State).

encode_array([], Acc, State) -> lists:reverse([encode_char($],State) | Acc]);
encode_array([H], Acc, State) ->
    lists:reverse([encode_char($],State), encode_value(H, State) | Acc]);
encode_array([H | Array], Acc, State) ->
    encode_array(Array,
                 [encode_char($,, State), encode_value(H, State) | Acc],
                 State);
encode_array(_, _, State) ->
    badarg(State).

encode_value(true, State) -> encode_chars(<<"true">>, State);
encode_value(false, State) -> encode_chars(<<"false">>, State);
encode_value(null, State) -> encode_chars(<<"null">>, State);
encode_value(String, State) when is_atom(String) -> encode_string(String, State);
encode_value({Object}, State)  -> encode_object(Object, State);
encode_value(Object = #{}, State = #state{maps = true}) ->
    encode_object(Object, State);
encode_value(Array, State) when is_list(Array) ->
    [encode_char($[, State) | encode_array(Array, [], State)];
encode_value(BinaryString, State) when is_binary(BinaryString) ->
    encode_string(BinaryString, State);
encode_value(Integer, State) when is_integer(Integer) ->
    encode_chars(integer_to_list(Integer), State);
encode_value(Float, State) when is_float(Float) ->
    encode_chars(float_to_binary(Float), State);
encode_value(_, State) ->
    badarg(State).

encode_string(Atom, State = #state{atom_strings = true}) when is_atom(Atom) ->
    encode_string(atom_to_binary(Atom, utf8), State);
encode_string(String, State) when is_binary(String) ->
    #state{plain_string = Plain, encoding = Encoding} = State,
    [encode_char($", State),
     char_code(escape(String, Plain, State), Plain, Encoding),
     encode_char($", State)];
encode_string(_, State) ->
    badarg(State).

escape(String, Plain, State) ->
    case escapeable(String, Plain) of
        true -> escape(String, <<>>, Plain, State);
        false -> String
    end.

escapeable(<<>>, _) -> false;
escapeable(<<H, _/binary>>, utf8) when ?ESCAPE(H) -> true;
escapeable(<<H, 0, _/binary>>, ?UTF16L) when ?ESCAPE(H) -> true;
escapeable(<<0, H, _/binary>>, ?UTF16B) when ?ESCAPE(H) -> true;
escapeable(<<H, 0:24, _/binary>>, ?UTF32L) when ?ESCAPE(H) -> true;
escapeable(<<0:24, H, _/binary>>, ?UTF32B) when ?ESCAPE(H) -> true;
escapeable(<<_:16, T/binary>>, Plain = {utf16, _}) -> escapeable(T, Plain);
escapeable(<<_:32, T/binary>>, Plain = {utf32,_}) -> escapeable(T, Plain);
escapeable(<<_, T/binary>>, Plain) -> escapeable(T, Plain).

escape(<<>>, Acc, _, _) -> Acc;
escape(<<H, T/binary>>, Acc, utf8, State) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H))/binary>>, utf8, State);
escape(<<H, 0, T/binary>>, Acc, Plain = ?UTF16L, State) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, State))/binary>>, Plain, State);
escape(<<0, H, T/binary>>, Acc, Plain = ?UTF16B, State) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, State))/binary>>, Plain, State);
escape(<<H, 0:24,T/binary>>,Acc,Plain=?UTF32L, State) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, State))/binary>>, Plain, State);
escape(<<0:24, H, T/binary>>, Acc, Plain=?UTF32B, State) when ?ESCAPE(H) ->
    escape(T, <<Acc/binary, (escape_char(H, State))/binary>>, Plain, State);
escape(<<H:16, T/binary>>, Acc, Plain = {utf16, _}, State) ->
    escape(T, <<Acc/binary, H:16>>, Plain, State);
escape(<<H:32, T/binary>>, Acc, Plain = {utf32,_}, State) ->
    escape(T, <<Acc/binary, H:32>>, Plain, State);
escape(<<H, T/binary>>, Acc, Plain, State) ->
    escape(T, <<Acc/binary, H>>, Plain, State).
escape_char(C, #state{plain_string = Plain}) ->
    encode_chars(escape_char(C), #state{encoding = Plain}).

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

encoding(Binary, #state{bom = true}) ->
    {Encoding, Size} = unicode:bom_to_encoding(Binary),
    BOM = (8 * Size),
    <<_:BOM, Binary1/binary>> = Binary,
    {Binary1, Encoding};
encoding(B = <<_, 0, 0, 0, _/binary>>, _) -> {B, ?UTF32L};
encoding(B = <<0, 0, 0, _/binary>>, _) -> {B, ?UTF32B};
encoding(B = <<_, 0, _, 0, _/binary>>, _) -> {B, ?UTF16L};
encoding(B = <<0, _, 0, _/binary>>, _) -> {B, ?UTF16B};
encoding(B = <<_, 0, _/binary>>, _) -> {B, ?UTF16L};
encoding(B = <<0, _, _/binary>>, _) -> {B, ?UTF16B};
encoding(B, _) -> {B, utf8}.

decode_text(Binary, State) ->
    case next(Binary, State) of
        {$/, T} -> decode_pointer(T, [], State);
        _ -> decode_value(Binary, State)
    end.

decode_rfc4627_text(Binary, State) ->
    case next(Binary, State) of
        {WS, T} when ?IS_WS(WS)-> decode_rfc4627_text(T, State);
        {${, T} -> decode_object(T,{false,false},[], State);
        {$[, T}-> decode_array(T, {false, false}, [], State);
        _ -> badarg(State)
    end.

decode_object(Binary, Expect, Acc, State) ->
    case {next(Binary, State), Expect} of
        {{WS, T}, _} when ?IS_WS(WS) -> decode_object(T, Expect, Acc, State);
        {{$}, T}, {false, _}} when State#state.maps -> {maps:from_list(Acc), T};
        {{$}, T}, {false, _}} when State#state.maps == safe ->
            case unique_keys(Acc) of
                true -> {maps:from_list(Acc), T};
                false -> {{lists:reverse(Acc)}, T}
            end;
        {{$}, T}, {false, _}} ->
            {{lists:reverse(Acc)}, T};
        {{$,, T}, {false, true}} -> decode_object(T, {true, false}, Acc, State);
        {{$", T}, {_, false}} when State#state.atom_keys ->
            {Name, T1} = decode_string(T, State#state{plain_string = utf8}),
            Name1 = binary_to_atom(Name, utf8),
            {Value, T2} = decode_value(decode_skip(T1, $:, State), State),
            decode_object(T2, {false, true}, [{Name1, Value} | Acc], State);
        {{$", T}, {_, false}} when State#state.existing_atom_keys ->
            {Name, T1} = decode_string(T, State#state{plain_string = utf8}),
            Name1 = binary_to_existing_atom(Name, utf8),
            {Value, T2} = decode_value(decode_skip(T1, $:, State), State),
            decode_object(T2, {false, true}, [{Name1, Value} | Acc], State);
        {{$", T}, {_, false}} ->
            {Name, T1} = decode_string(T, State),
            {Value, T2} = decode_value(decode_skip(T1, $:, State), State),
            decode_object(T2, {false, true}, [{Name, Value} | Acc], State);
        _ ->
            badarg(State)
    end.

unique_keys(Members) -> length(Members) == length(lists:ukeysort(1, Members)).

decode_array(Binary, Expect, Acc, State) ->
    case {next(Binary, State), Expect} of
        {{WS, T}, _} when ?IS_WS(WS) -> decode_array(T, Expect, Acc, State);
        {{$,, T}, {false, true}} -> decode_array(T, {true, false}, Acc, State);
        {{$], T}, {false, _}} -> {lists:reverse(Acc), T};
        {_, {_, false}} ->
            {Value, T} = decode_value(Binary, State),
            decode_array(T, {false, true}, [Value | Acc], State);
        _ ->
            badarg(State)
    end.

decode_value(Binary, State) ->
    case next(Binary, State) of
        {WS, T} when ?IS_WS(WS) -> decode_value(T, State);
        {$t, T} -> decode_base("rue", T, true, State);
        {$f, T} -> decode_base("alse", T, false, State);
        {$n, T} -> decode_base("ull", T, null, State);
        {${, T} -> decode_object(T, {false, false}, [], State);
        {$[, T} -> decode_array(T, {false, false}, [], State);
        {$", T} -> decode_string(T, State);
        {$-, T} -> decode_number(T, pre, int, [$-], State);
        {H, _} when H >= $0, H =< $9 ->
            decode_number(Binary, pre, int, [], State);
        _ ->
            badarg(State)
    end.

decode_base("", T, Value, _) -> {Value, T};
decode_base([H | T], Binary, Value, State) ->
    case next(Binary, State) of
        {H, Binary1} -> decode_base(T, Binary1, Value, State);
        _ -> badarg(State)
    end.

decode_number(Binary, Stage, Phase, Acc, State) ->
    case {next(Binary, State), Stage, Phase} of
        {{$0, T}, pre, int} -> decode_number(T, zero, int, [$0 | Acc], State);
        {{H, T}, pre, exp}  when ?IS_SIGN(H) ->
            decode_number(T, sign, exp, [H | Acc], State);
        {{H, T}, pre, float} when ?IS_INT(H) ->
            decode_number(T, post, float, [H | Acc], State);
        {{H, T}, pre, _} when ?IS_POS_INT(H) ->
            decode_number(T, post, Phase, [H | Acc], State);
        {{H, T}, sign, _} when ?IS_POS_INT(H) ->
            decode_number(T, post, Phase, [H | Acc], State);
        {{H, T}, post, _} when ?IS_INT(H) ->
            decode_number(T, post, Phase, [H | Acc], State);
        {{$., T}, _, int} when ?ZERO_OR_POST(Stage) ->
            decode_number(T, pre, float, [$. | Acc], State);
        {{E, T}, _, int} when ?EXP_ZERO_OR_POST(E, Stage) ->
            decode_number(T, pre, exp, [E, $0, $. | Acc], State);
        {{E, T}, post, float} when ?IS_EXP(E) ->
            decode_number(T, pre, exp, [E | Acc], State);
        {_, Stage, int} when ?ZERO_OR_POST(Stage) ->
            {list_to_integer(lists:reverse(Acc)), Binary};
        {_, post, _} ->
            {list_to_float(lists:reverse(Acc)), Binary};
        _ ->
            badarg(State)
    end.

decode_string(Binary, State=#state{encoding = Enc, plain_string = Plain}) ->
    {Unescaped, T} = unescape(Binary, [], State),
    {char_code(iolist_to_binary(Unescaped), Enc, Plain), T}.

unescape(Binary, Acc, State = #state{encoding = Encoding}) ->
    case next(Binary, State) of
        {$\\, T} -> unescape_solid(T, Acc, State);
        {$", T} -> {lists:reverse(Acc), T};
        {H, T} when Encoding == utf8 -> unescape(T, [H | Acc], State);
        _ when Encoding == ?UTF16L; Encoding == ?UTF16B ->
            <<H:16, T/binary>> = Binary,
            unescape(T, [<<H:16>> | Acc], State);
        _ when Encoding == ?UTF32L; Encoding == ?UTF32B ->
            <<H:32, T/binary>> = Binary,
            unescape(T, [<<H:32>> | Acc], State)
    end.

unescape_solid(Binary, Acc, State) ->
    case next(Binary, State) of
        {$", T} -> unescape(T, [encode_char($", State) | Acc], State);
        {$\\, T} -> unescape(T, [encode_char($\\, State) | Acc], State);
        {$/, T} -> unescape(T, [encode_char($/, State) | Acc], State);
        {$0, T} -> unescape(T, [encode_char(?NULL, State) | Acc], State);
        {$a, T} -> unescape(T, [encode_char(?BEL, State) | Acc], State);
        {$b, T} -> unescape(T, [encode_char(?BS, State) | Acc], State);
        {$t, T} -> unescape(T, [encode_char(?HT, State) | Acc], State);
        {$n, T} -> unescape(T, [encode_char(?LF, State) | Acc], State);
        {$f, T} -> unescape(T, [encode_char(?FF, State) | Acc], State);
        {$v, T} -> unescape(T, [encode_char(?VT, State) | Acc], State);
        {$r, T} -> unescape(T, [encode_char(?CR, State) | Acc], State);
        {$s, T} -> unescape(T, [encode_char(?SPC, State) | Acc], State);
        {$u, T} -> unescape_hex(T, Acc, State);
        {H, T} when is_integer(H) ->
            unescape(T,[encode_char(H,State),encode_char($\\,State)|Acc],State);
        {H, T} when is_binary(H) ->
            unescape(T, [H, encode_char($\\, State) | Acc], State)
    end.

unescape_hex(<<A, B, C, D, T/binary>>, Acc, State = #state{encoding = utf8}) ->
    unescape(T, [encode_hex([A, B, C, D], State) | Acc], State);
unescape_hex(Binary, Acc, State = #state{encoding = ?UTF16L}) ->
    <<A, 0, B, 0, C, 0, D, 0, T/binary>> = Binary,
    unescape(T, [encode_hex([A, B, C, D], State) | Acc], State);
unescape_hex(Binary, Acc, State = #state{encoding = ?UTF16B}) ->
    <<0, A, 0, B, 0, C, 0, D, T/binary>> = Binary,
    unescape(T, [encode_hex([A, B, C, D], State) | Acc], State);
unescape_hex(Binary, Acc, State = #state{encoding = ?UTF32L}) ->
    <<A, 0:24, B, 0:24, C, 0:24, D, 0:24, T/binary>> = Binary,
    unescape(T, [encode_hex([A, B, C, D], State) | Acc], State);
unescape_hex(Binary, Acc, State = #state{encoding = ?UTF32B}) ->
    <<0:24, A, 0:24, B, 0:24, C, 0:24, D, T/binary>> = Binary,
    unescape(T, [encode_hex([A, B, C, D], State) | Acc], State);
unescape_hex(_, _, State) ->
    badarg(State).

encode_hex(List, State) -> encode_char(list_to_integer(List, 16), State).

decode_skip(Binary, H, State) ->
    case next(Binary, State) of
        {H, T} -> T;
        {WS, T} when ?IS_WS(WS) -> decode_skip(T, H, State);
        _ -> badarg(State)
    end.

%% ===================================================================
%% Pointer encoding
%% ===================================================================

encode_pointer([], _, Acc) -> lists:reverse(Acc);
encode_pointer([H | T], State, Acc) when is_binary(H) ->
    #state{plain_string = Plain, encoding = Encoding} = State,
    H1 = [encode_char($/, State),
          char_code(pointer_escape(H, Plain), Plain, Encoding)],
    encode_pointer(T, State, [H1 | Acc]);
encode_pointer(['-' | T], State, Acc) ->
    encode_pointer(T, State, [encode_chars([$/, $-], State) | Acc]);
encode_pointer([H | T], State, Acc) when is_atom(H) ->
    #state{atom_strings = true, plain_string = Plain} = State,
    H1 = iolist_to_binary(char_code(atom_to_binary(H, utf8), utf8, Plain)),
    encode_pointer([H1 | T], State, Acc);
encode_pointer([H | T], State, Acc) when is_integer(H), H >= 0 ->
    H1 = encode_chars([$/ | integer_to_list(H)],
                      State#state{encoding = State#state.encoding}),
    encode_pointer(T, State, [H1 | Acc]);
encode_pointer(_, State, _) ->
    badarg(State).

pointer_escape(String, Plain) ->
    case pointer_escapeable(String, Plain) of
        true -> pointer_escape(String, <<>>, Plain);
        false -> String
    end.

pointer_escapeable(<<>>, _) -> false;
pointer_escapeable(<<H, _/binary>>, utf8)
  when ?POINTER_ESCAPE(H) -> true;
pointer_escapeable(<<H, 0, _/binary>>, ?UTF16L)
  when ?POINTER_ESCAPE(H) -> true;
pointer_escapeable(<<0, H, _/binary>>, ?UTF16B)
  when ?POINTER_ESCAPE(H) -> true;
pointer_escapeable(<<H, 0:24, _/binary>>, ?UTF32L)
  when ?POINTER_ESCAPE(H) -> true;
pointer_escapeable(<<0:24, H, _/binary>>, ?UTF32B)
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
pointer_escape(<<H, 0, T/binary>>, Acc, Plain = ?UTF16L)
  when ?POINTER_ESCAPE(H) ->
    Acc1 = <<Acc/binary, (pointer_escape_char(H, Plain))/binary>>,
    pointer_escape(T, Acc1, Plain);
pointer_escape(<<0, H, T/binary>>, Acc, Plain = ?UTF16B)
  when ?POINTER_ESCAPE(H) ->
    Acc1 = <<Acc/binary, (pointer_escape_char(H, Plain))/binary>>,
    pointer_escape(T, Acc1, Plain);
pointer_escape(<<H, 0:24,T/binary>>,Acc,Plain = ?UTF32L)
  when ?POINTER_ESCAPE(H) ->
    Acc1 = <<Acc/binary, (pointer_escape_char(H, Plain))/binary>>,
    pointer_escape(T, Acc1, Plain);
pointer_escape(<<0:24, H, T/binary>>, Acc, Plain = ?UTF32B)
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
    encode_chars(pointer_escape_char(C), #state{encoding = Plain}).

pointer_escape_char($~) -> <<$~, $0>>;
pointer_escape_char($/) -> <<$~, $1>>.

%% ===================================================================
%% Pointer decoding
%% ===================================================================

decode_pointer(<<>>, Acc, _) -> {lists:reverse(Acc), <<>>};
decode_pointer(Binary, Acc, State) ->
    case next(Binary, State) of
        {H, T} when ?IS_INT(H) ->
            {Point, T1} = decode_pointer_int(T, [H], State),
            decode_pointer(T1, [Point | Acc], State);
        {$-, T} ->
            case next(T, State) of
                eob -> {lists:reverse(['-'| Acc]), <<>>};
                {$/, T1}  ->
                    decode_pointer(T1, ['-' | Acc], State)
            end;
        {H, T} ->
            {Point, T1} = decode_pointer_member(T, [H], State),
            decode_pointer(T1, [Point | Acc], State)
    end.

decode_pointer_int(Binary, Acc, State) ->
    case next(Binary, State) of
        eob -> {list_to_integer(lists:reverse(Acc)), <<>>};
        {$/, T} -> {list_to_integer(lists:reverse(Acc)), T};
        {H, T} when ?IS_INT(H) -> decode_pointer_int(T, [H | Acc], State)
    end.    

decode_pointer_member(Bin, Acc,State=#state{encoding=Enc,plain_string=Plain}) ->
    #state{encoding = Enc, plain_string = Plain} = State,
    case next(Bin, State) of
        eob ->
            {pointer_key(lists:reverse(Acc), State), <<>>};
        {$/, T} ->
            {pointer_key(lists:reverse(Acc), State), T};
        {$~, T} ->
            case next(T, State) of
                {$0, T1} ->
                    decode_pointer_member(T1,
                                          [encode_char($~, Plain) | Acc],
                                          State);
                {$1, T1} ->
                    decode_pointer_member(T1,
                                          [encode_char($/, Plain) | Acc],
                                          State)
            end;
        {H, T} ->
            decode_pointer_member(T, [H | Acc], State)
    end.

pointer_key(Key, #state{encoding=Enc, atom_keys = true}) ->
    binary_to_atom(char_code(iolist_to_binary(Key), Enc, utf8), utf8);
pointer_key(Key, #state{encoding=Enc, existing_atom_keys = true}) ->
    binary_to_atom(char_code(iolist_to_binary(Key), Enc, utf8), utf8);
pointer_key(Key, #state{encoding = Encoding, plain_string = Plain}) ->
    char_code(iolist_to_binary(Key), Encoding, Plain).

%% ===================================================================
%% Pointer Evaluation
%% ===================================================================

eval_binary([], Binary, _, State = #state{decode = true}) ->
    decode(Binary, State);
eval_binary([], Binary, _, State) ->
    {T, State1  = #state{pos = Pos}} = skip_ws(Binary, State),
    {_, #state{pos = Pos1}} = skip_value(T, State1),
    {pos, Pos, Pos1 - Pos};
eval_binary(P = ['-' | _ ], Binary, Path, State) ->
    case next(Binary, State) of
        {H, BT} when ?IS_WS(H) -> eval_binary(P, BT, Path, State);
        {$[, BT} ->
            case eval_binary_dash(BT, {false, false}, 0, State) of
                Error = {error, _} -> Error;
                Size -> {error, {too_large_index, lists:reverse([Size | Path])}}
            end;
        _ ->
            {error, {incorrect_pointer, lists:reverse(['-' | Path])}}
    end;
eval_binary(P = [N | T], Binary, Path, State) when is_integer(N) ->
    case next(Binary, State) of
        {H, BT} when ?IS_WS(H) -> eval_binary(P, BT, Path, inc(State));
        {$[, BT} ->
            case eval_binary_array(N, BT, {false, false}, Path, inc(State)) of
                {error, too_large_index} ->
                    {error, {too_large_index, lists:reverse([N | Path])}};
                Error = {error, _} -> Error;
                {BT1, State1} -> eval_binary(T, BT1, [N | Path], State1)
            end;
        _ ->
            {error, {incorrect_pointer, lists:reverse([N | Path])}}
    end;
eval_binary(P = [Key | T], Bin,Path,State) when is_binary(Key); is_atom(Key) ->
    case next(Bin, State) of
        {H, BT} when ?IS_WS(H) -> eval_binary(P, BT, Path, inc(State));
        {${, BT} ->
            case eval_binary_object(Key, BT, {false, false}, Path, inc(State)) of
                Error = {error, _} -> Error;
                {BT1, State1} -> eval_binary(T, BT1, [Key | Path], State1)
            end;
        _ ->
            {error, {incorrect_pointer, lists:reverse([Key | Path])}}
    end.

eval_binary_dash(Binary, Expect, Size, State) -> 
    case {next(Binary, State), Expect} of
        {{WS, T}, _} when ?IS_WS(WS) ->
            eval_binary_dash(T, Expect, Size, State);
        {{$,, T}, {false, true}} ->
            eval_binary_dash(T, {true, false}, Size, State);
        {{$], _}, {false, _}} ->
            Size;
        {_, {_, false}} ->
            {T, _} = skip_value(Binary, State),
            eval_binary_dash(T, {false, true}, Size + 1, State);
        _ ->
            badarg(State)
    end.

eval_binary_object(Key, Binary, Expect, Path, State) ->
    case {next(Binary, State), Expect} of
        {{WS, T}, _} when ?IS_WS(WS) ->
            eval_binary_object(Key, T, Expect, Path, inc(State));
        {{$}, _}, {false, _}} ->
            {error, {non_member, lists:reverse([key | Path])}};
        {{$,, T}, {false, true}} ->
            eval_binary_object(Key, T, {true, false}, Path, inc(State));
        {{$", T}, {_, false}} ->
            case eval_binary_string(T, inc(State)) of
                {Key, T1, State1} -> skip_char(T1, $:, State1);
                {_, T1, State1} ->
                    {T2, State2} = skip_char(T1, $:, State1),
                    {T3, State3} = skip_value(T2, State2),
                    eval_binary_object(Key, T3, {false, true}, Path, State3)
            end;
        _ ->
            badarg(State)
    end.

eval_binary_array(0, Binary, _, _, State) -> {Binary, State};
eval_binary_array(N, Binary, Expect, Path, State) -> 
    case {next(Binary, State), Expect} of
        {{WS, T}, _} when ?IS_WS(WS) ->
            eval_binary_array(N, T, Expect, Path, inc(State));
        {{$,, T}, {false, true}} ->
            eval_binary_array(N - 1, T, {true, false}, Path, inc(State));
        {{$], _}, {false, _}} ->
            {error, too_large_index};
        {_, {_, false}} ->
            {T, State1} = skip_value(Binary, State),
            eval_binary_array(N, T, {false, true}, Path, State1);
        _ ->
            badarg(State)
    end.

eval_binary_string(Bin, State=#state{encoding = Enc, atom_keys = true}) ->
    {Unescaped, T, State1} = eval_binary_unescape(Bin, [], State),
    {binary_to_atom(char_code(iolist_to_binary(Unescaped), Enc, utf8),utf8),
     T,
     State1};
eval_binary_string(Bin, State=#state{encoding=Enc, existing_atom_keys=true}) ->
    {Unescaped, T, State1} = eval_binary_unescape(Bin, [], State),
    {binary_to_atom(char_code(iolist_to_binary(Unescaped), Enc, utf8),utf8),
     T,
     State1};
eval_binary_string(Bin, State=#state{encoding = Enc, plain_string = Plain}) ->
    {Unescaped, T, State1} = eval_binary_unescape(Bin, [], State),
    {char_code(iolist_to_binary(Unescaped), Enc, Plain), T, State1}.

eval_binary_unescape(Bin, Acc, State = #state{encoding = Encoding}) ->
    case next(Bin, State) of
        {$\\, T} -> eval_binary_unescape_solid(T, Acc, inc(State));
        {$", T} -> {lists:reverse(Acc), T, inc(State)};
        {H, T} when Encoding == utf8 ->
            eval_binary_unescape(T, [H | Acc], inc(State));
        _ when Encoding == ?UTF16L; Encoding == ?UTF16B ->
            <<H:16, T/binary>> = Bin,
            eval_binary_unescape(T, [<<H:16>> | Acc], inc(State));
        _ when Encoding == ?UTF32L; Encoding == ?UTF32B ->
            <<H:32, T/binary>> = Bin,
            eval_binary_unescape(T, [<<H:32>> | Acc], inc(State))
    end.

eval_binary_unescape_solid(Binary, Acc, State) ->
    case next(Binary, State) of
        {$", T} ->
            eval_binary_unescape(T, [encode_char($", State) | Acc],inc(State));
        {$\\, T} ->
            eval_binary_unescape(T, [encode_char($\\, State) | Acc],inc(State));
        {$/, T} ->
            eval_binary_unescape(T, [encode_char($/, State) | Acc], inc(State));
        {$0, T} ->
            eval_binary_unescape(T, [encode_char(?NULL, State)|Acc],inc(State));
        {$a, T} ->
            eval_binary_unescape(T, [encode_char(?BEL, State) |Acc],inc(State));
        {$b, T} ->
            eval_binary_unescape(T, [encode_char(?BS, State) | Acc],inc(State));
        {$t, T} ->
            eval_binary_unescape(T, [encode_char(?HT, State) | Acc],inc(State));
        {$n, T} ->
            eval_binary_unescape(T, [encode_char(?LF, State) | Acc],inc(State));
        {$f, T} ->
            eval_binary_unescape(T, [encode_char(?FF, State) | Acc],inc(State));
        {$v, T} ->
            eval_binary_unescape(T, [encode_char(?VT, State) | Acc],inc(State));
        {$r, T} ->
            eval_binary_unescape(T, [encode_char(?CR, State) | Acc],inc(State));
        {$s, T} ->
            eval_binary_unescape(T, [encode_char(?SPC, State) |Acc],inc(State));
        {$u, T} -> eval_binary_unescape_hex(T, Acc, State);
        {H, T} when is_integer(H) ->
            eval_binary_unescape(T,
                                 [encode_char(H, State),
                                  encode_char($\\,State) | Acc],
                                 inc(State));
        {H, T} when is_binary(H) ->
            eval_binary_unescape(T, [H, encode_char($\\, State)|Acc],inc(State))
    end.

eval_binary_unescape_hex(Binary, Acc, State = #state{encoding = utf8}) ->
    <<A, B, C, D, T/binary>> = Binary,
    eval_binary_unescape(T, [encode_hex([A, B, C, D], State)|Acc],inc(State,4));
eval_binary_unescape_hex(Binary, Acc, State = #state{encoding = ?UTF16L}) ->
    <<A, 0, B, 0, C, 0, D, 0, T/binary>> = Binary,
    eval_binary_unescape(T, [encode_hex([A, B, C, D], State)|Acc],inc(State,4));
eval_binary_unescape_hex(Binary, Acc, State = #state{encoding = ?UTF16B}) ->
    <<0, A, 0, B, 0, C, 0, D, T/binary>> = Binary,
    eval_binary_unescape(T, [encode_hex([A, B, C, D], State)|Acc],inc(State,4));
eval_binary_unescape_hex(Binary, Acc, State = #state{encoding = ?UTF32L}) ->
    <<A, 0:24, B, 0:24, C, 0:24, D, 0:24, T/binary>> = Binary,
    eval_binary_unescape(T, [encode_hex([A, B, C, D], State)|Acc],inc(State,4));
eval_binary_unescape_hex(Binary, Acc, State = #state{encoding = ?UTF32B}) ->
    <<0:24, A, 0:24, B, 0:24, C, 0:24, D, T/binary>> = Binary,
    eval_binary_unescape(T, [encode_hex([A, B, C, D], State)|Acc],inc(State,4));
eval_binary_unescape_hex(_, _, State) ->
    badarg(State).


skip_value(Binary, State) ->
    case next(Binary, State) of
        {WS, T} when ?IS_WS(WS) -> skip_value(T, inc(State));
        {$t, T} -> skip_base("rue", T, inc(State));
        {$f, T} -> skip_base("alse", T, inc(State));
        {$n, T} -> skip_base("ull", T, inc(State));
        {${, T} -> skip_object(T, {false, false}, inc(State));
        {$[, T} -> skip_array(T, {false, false}, inc(State));
        {$", T} -> skip_string(T, inc(State));
        {$-, T} -> skip_number(T, pre, int, inc(State));
        {H, _} when H >= $0, H =< $9 ->
            skip_number(Binary, pre, int, State);
        _ -> badarg(State)
    end.

skip_base("", T, State) -> {T, State};
skip_base([H | T], Binary, State) ->
    case next(Binary, State) of
        {H, Binary1} -> skip_base(T, Binary1, inc(State));
        _ -> badarg(State)
    end.

skip_object(Binary, Expect, State) ->
    case {next(Binary, State), Expect} of
        {{WS, T}, _} when ?IS_WS(WS) -> skip_object(T, Expect, inc(State));
        {{$}, T}, {false, _}} -> {T, inc(State)};
        {{$,, T}, {false, true}} -> skip_object(T, {true, false}, inc(State));
        {{$", T}, {_, false}} ->
            {T1, State1} = skip_string(T, inc(State)),
            {T2, State2} = skip_char(T1, $:, State1),
            {T3, State3} = skip_value(T2, State2),
            skip_object(T3, {false, true}, State3);
        _ ->
            badarg(State)
    end.

skip_char(Binary, H, State) ->
    case next(Binary, State) of
        {H, T} -> {T, inc(State)};
        {WS, T} when ?IS_WS(WS) -> skip_char(T, H, inc(State));
        _ -> badarg(State)
    end.

skip_ws(Binary, State) ->
    case next(Binary, State) of
        {WS, T} when ?IS_WS(WS) -> skip_ws(T, inc(State));
        _ -> {Binary, State}
    end.

skip_array(Binary, Expect, State) ->
    case {next(Binary, State), Expect} of
        {{WS, T}, _} when ?IS_WS(WS) -> skip_array(T, Expect, inc(State));
        {{$,, T}, {false, true}} -> skip_array(T, {true, false}, inc(State));
        {{$], T}, {false, _}} -> {T, inc(State)};
        {_, {_, false}} ->
            {T, State1} = skip_value(Binary, State),
            skip_array(T, {false, true}, State1);
        _ ->
            badarg(State)
    end.

skip_string(Binary, State) ->
    case next(Binary, State) of
        {$\\, T} -> skip_string_solid(T, inc(State));
        {$", T} -> {T, inc(State)};
        {_, T}  -> skip_string(T, inc(State))
    end.

skip_string_solid(Binary, State) ->
    case next(Binary, State) of
        {$u, T} -> skip_string_hex(T, inc(State));
        {_, T} -> skip_string(T, inc(State))
    end.

skip_string_hex(<<_:32, T/binary>>, State = #state{encoding = utf8}) ->
    skip_string(T, inc(State, 4));
skip_string_hex(<<_:64, T/binary>>, State = #state{encoding = {utf16, _}}) ->
    skip_string(T, inc(State, 4));
skip_string_hex(<<_:128, T/binary>>, State = #state{encoding = {utf32, _}}) ->
    skip_string(T, inc(State, 4));
skip_string_hex(_, State) ->
    badarg(State).

skip_number(Binary, Stage, Phase, State) ->
    case {next(Binary, State), Stage, Phase} of
        {{$0, T}, pre, int} -> skip_number(T, zero, int, inc(State));
        {{H, T}, pre, exp} when ?IS_SIGN(H) ->
            skip_number(T, sign, exp, inc(State));
        {{H, T}, pre, float} when ?IS_INT(H) ->
            skip_number(T, post, float, inc(State));
        {{H, T}, pre, _} when ?IS_POS_INT(H) ->
            skip_number(T, post, Phase, inc(State));
        {{H, T}, sign, _} when ?IS_POS_INT(H) ->
            skip_number(T, post, Phase, inc(State));
        {{H, T}, post, _} when ?IS_INT(H) ->
            skip_number(T, post, Phase, inc(State));
        {{$., T}, _, int} when ?ZERO_OR_POST(Stage) ->
            skip_number(T, pre, float, inc(State));
        {{E, T}, _, int} when ?EXP_ZERO_OR_POST(E, Stage) ->
            skip_number(T, pre, exp, inc(State));
        {{E, T}, post, float} when ?IS_EXP(E) ->
            skip_number(T, pre, exp, inc(State));
        {_, State, int} when ?ZERO_OR_POST(State) ->
            {Binary, State};
        {_, post, _} ->
            {Binary, State};
        _ ->
            badarg(State)
    end.

step(State = #state{encoding = utf8}) -> State#state{step = 1};
step(State = #state{encoding = {utf16, _}}) -> State#state{step = 2};
step(State = #state{encoding = {utf32, _}}) -> State#state{step = 4}.

inc(State = #state{step = Step, pos = Pos}) ->
    State#state{pos = Pos + Step}.
inc(State = #state{step = Step, pos = Pos}, N) ->
    State#state{pos = Pos + (Step * N)}.

eval_json([], JSON, _, State = #state{encode = true}) -> encode(JSON, State);
eval_json([], JSON, _, _) ->  JSON;
eval_json(['-' | _], JSON, Path, _) when is_list(JSON) ->
    {error, {too_large_index, lists:reverse([length(JSON) | Path])}};
eval_json(['-' | _], _, Path, _) ->
    {error, {incorrect_pointer, lists:reverse(['-' | Path])}};
eval_json([N | T], JSON, Path, State) when is_integer(N), length(JSON) > N ->
    eval_json(T, lists:nth(N + 1, JSON), [N | Path], State);
eval_json([N | _], _, Path, _) when is_integer(N) ->
    {error, {too_large_index, lists:reverse([N | Path])}};
eval_json([Key | T], JSON = #{}, Path, State =#state{maps=M}) when M /= false ->
    case maps:find(Key, JSON) of
        {ok, Value} -> eval_json(T, Value, [Key | Path], State);
        _ -> {error, {non_member, lists:reverse([key | Path])}}
    end;
eval_json([Key | T], {Members}, Path, State) ->
    case plist:find(Key, Members) of
        undefined -> {error, {non_member, lists:reverse([Key | Path])}};
        Value -> eval_json(T, Value, [Key | Path], State)
    end;
eval_json(X, _, Path, _) ->
    {error, {incorrect_pointer, lists:reverse([X | Path])}}.

%% ===================================================================
%% Validate schema
%% ===================================================================

validate_schema({Schema}, JSON, State) ->
    validate_json(Schema, JSON, State#state{schema = Schema}).

%% Numeric
validate_json([{multipleOf, _} | T], JSON, State) when not is_number(JSON) ->
    validate_json(T, JSON, State);
validate_json([{multipleOf, N} | T], JSON, State) when is_number(N), N > 0 ->
    0.0 = (JSON / N - trunc(JSON / N)) * N,
    validate_json(T, JSON, State);;
validate_json([{maximum, N} | T], JSON, State) when not is_number(JSON) ->
    validate_json(T, JSON, State);
validate_json([{maximum, N} | T], JSON, State) when is_number(N) ->
    case plist:find(exclusiveMaximum, State#state.schema, false) of
        false -> true = JSON =< N;
        true -> true = JSON < N
    end,
    validate_json(T, JSON, State);
validate_json([{exclusiveMaximum, _} | T], State) ->
    validate_json(T, JSON, State);
validate_json([{minimum, N} | T], JSON, State) when not is_number(JSON) ->
    validate_json(T, JSON, State);
validate_json([{minimum, N} | T], JSON, State) when is_number(N) ->
    case plist:find(exclusiveMinimum, Schema#state.schema, false) of
        false -> true = JSON >= N;
        true -> true = JSON > N
    end,
    validate_json(T, JSON, State);
validate_json([{exclusiveMinimum, _} | T], State) ->
    validate_json(T, JSON, State);
%%  Strings
validate_json([{maxLength, _} | T], JSON, State) when not is_binary(JSON) ->
    validate_json(T, JSON, State);
validate_json([{maxLength, N} | T], JSON, State) when is_integer(N), N >= 0 ->
    true = string_length(JSON, State) =< N,
    validate_json(T, JSON, State);
validate_json([{minLength, _} | T], JSON, State) when not is_binary(JSON) ->
    validate_json(T, JSON, State);
validate_json([{minLength, N} | T], JSON, State) when is_integer(N), N >= 0 ->
    true = string_length(JSON, State) >= N,
    validate_json(T, JSON, State);
validate_json([{pattern, _} | T], JSON, State) when not is_binary(JSON) ->
    validate_json(T, JSON, State);
validate_json([{pattern, Pattern} | T], JSON, State) when is_binary(Pattern) ->
    #state{plain_string = Plain} = State,
    {match, _} = re:run(char_code(JSON, Plain, utf8), Pattern, [unicode]),
    validate_json(T, JSON, State);
%%  Arrays
validate_json([{items, _} | T], JSON, State) when not is_list(JSON) ->
    validate_json(T, JSON, State);
validate_json([{items, Items = {_}} | T], JSON, State) ->
    State1 = State#state{props_validated = false},
    [validate_schema(Items, J, State1) || J <- JSON],
    validate_json(T, JSON, State1);
validate_json([{items, Items} | T], JSON, State) when is_list(Items) ->
    AdditionalItems = plist:find(additionalItems, State#state.schema, true),
    validate_array(JSON, Items, AdditionalItems, State),
    validate_json(T, JSON, State);
validate_json([{additionalItems, _} | T], JSON, State) ->
    validate_json(T, JSON, State);
validate_json([{maxItems, _} | T], JSON, State) when not is_list(JSON) ->
    validate_json(T, JSON, State);
validate_json([{maxItems, N} | T], JSON, State) when is_integer(N), N >= 0 ->
    true = length(JSON) =< N,
    validate_json(T, JSON, State);
validate_json([{minItems, _} | T], JSON, State) when not is_list(JSON) ->
    validate_json(T, JSON, State);
validate_json([{minItems, N} | T], JSON, State) when is_integer(N), N >= 0 ->
    true = length(JSON) >= N,
    validate_json(T, JSON, State);
validate_json([{uniqueItems, _} | T], JSON, State) when not is_list(JSON) ->
    validate_json(T, JSON, State);
validate_json([{uniqueItems, false} | T], JSON, State) ->
    validate_json(T, JSON, State);
validate_json([{uniqueItems, true} | T], JSON, State) ->
    length(JSON) = length(lists:usort(JSON)),
    validate_json(T, JSON, State);
%%  Objects
validate_json([{maxProperties, _} | T], JSON, State) when not is_tuple(JSON) ->
    validate_json(T, JSON, State);
validate_json([{maxProperties, N}|T],JSON={M},State) when is_integer(N), N > 0 ->
    true = length(M) =< N,
    validate_json(T, JSON, State);
validate_json([{minProperties, _} | T], JSON, State) when not is_tuple(JSON) ->
    validate_json(T, JSON, State);
validate_json([{minProperties, N}|T],JSON={M},State) when is_integer(N), N > 0 ->
    true = length(M) >= N,
    validate_json(T, JSON, State);
validate_json([{required, _} | T], JSON, State) when not is_tuple(JSON) ->
    validate_json(T, JSON, State);
validate_json([{required, Reqs = [_ | _]} | T], JSON = {M}, State) ->
    Keys = plist:keys(M),
    [true = lists:member(Req, Keys) || Req <- Reqs],
    validate_json(T, JSON, State);
validate_json([{properties, _} | T], JSON, State=#state{props_validated=true}) ->
    validate_json(T, JSON, State);
validate_json([{patternProperties,_}|T],J,State=#state{props_validated=true}) ->
    validate_json(T, J, State);
validate_json([{additionalProperties, _}|T],J,S=#state{props_validated=true}) ->
    validate_json(T, J, S);
validate_json([{properties, Props} | T], JSON = {_}, State) ->
    Patterns = plist:find(patternProperties, T),
    Additional = plist:find(additionalProperties, T),
    validate_object(JSON, Props, Patterns, Additional, State),
    validate_json(T, JSON, State#state{props_validated = true});
validate_json([{patternProperties, Patterns} | T], JSON = {_}, State) ->
    Props = plist:find(properties, T),
    Additional = plist:find(additionalProperties, T),
    validate_object(JSON, Props, Patterns, Additional, State),
    validate_json(T, JSON, State#state{props_validated = true});
validate_json([{additionalProperties, Additional} | T], JSON = {_}, State) ->
    Props = plist:find(properties, T),
    Patterns = plist:find(patternProperties, T),
    validate_object(JSON, Props, Patterns, Additional, State),
    validate_json(T, JSON, State#state{props_validated = true});
validate_json([{properties, _} | T], JSON, State) ->
    validate_json(T, JSON, State);
validate_json([{patternProperties, _} | T], JSON, State) ->
    validate_json(T, JSON, State);
validate_json([{additionalProperties, _} | T], JSON, State) ->
    validate_json(T, JSON, State);
validate_json([{dependencies, {Deps}} | T], JSON = {_}, State) ->
    validate_dependencies(Deps, JSON, State#state{props_validated = false}),
    validate_json(T, JSON, State);
validate_json([{dependencies, _} | T], JSON, State) ->
    validate_json(T, JSON, State);
%%  Any type
validate_json([{enum, Enums = [_|_]} | T], JSON, State) ->
    validate_enum(Enums, JSON, State),
    validate_json(T, JSON, State);
validate_json([{type, <<"null">>} | T], null, State) ->
    validate_json(T, JSON, State);
validate_json([{type, <<"boolean">>} | T], true, State) ->
    validate_json(T, JSON, State);
validate_json([{type, <<"boolean">>} | T], false, State) ->
    validate_json(T, JSON, State);
validate_json([{type, <<"integer">>} | T], I, State) when is_integer(I) ->
    validate_json(T, JSON, State);
validate_json([{type, <<"number">>}|T],I,State) when is_integer(I);is_float(I) ->
    validate_json(T, JSON, State);
validate_json([{type, <<"string">>} | T], S, State) when is_binary(S) ->
    validate_json(T, JSON, State);
validate_json([{type, <<"array">>} | T], A, State) when is_list(A) ->
    validate_json(T, JSON, State);
validate_json([{type, <<"object">>} | T], {_}, State) ->
    validate_json(T, JSON, State);
validate_json([{type, Types} | T], JSON, State) when is_list(Types) ->
    validate_type(Types, JSON, State),
    validate_json(T, JSON, State);
validate_json([{allOf, Schemas = [_ | _]} | T], JSON, State) ->
    State1 = State#state{props_validated = false},
    [validate_schema(Schema, JSON, State1) || Schema <- Schemas],
    validate_json(T, JSON, State);
validate_json([{anyOf, Schemas = [_ | _]} | T], JSON, State) ->
    validate_anyof(Schemas, JSON, State#state{props_validated = false},),
    validate_json(T, JSON, State);
validate_json([{oneOf, Schemas = [_ | _]} | T], JSON, State) ->
    validate_oneof(Schemas, JSON, false, State#state{props_validated = false}),
    validate_json(T, JSON, State);
validate_json([{not, Schema = {_}} | T], JSON, State) ->
    try validate_schema(Schema, JSON, State#state{props_validated = false}) of
        _ -> erlang:throw(not_validated)
    catch _:_ -> validate_json(T, JSON, State)
    end;
validate_json([{definitions, {_}} | T], JSON, State) ->
    validate_json(T, JSON, State);
%% Metadata
validate_json([{title, Title} | T], JSON, State) when is_binary(Title) ->
    validate_json(T, JSON, State);
validate_json([{description, Desc} | T], JSON, State) when is_binary(Desc) ->
    validate_json(T, JSON, State);
validate_json([{default, _} | T], JSON, State) ->
    validate_json(T, JSON, State);
%% Format
validate_json([{format, Format} | T], JSON, State) when is_binary(Format) ->
    validate_json(T, JSON, State);
validate_json([{id, Id} | T], JSON, State) when is_binary(Id)->
    validate_json(T, JSON, State);
validate_json([{'$schema', Schema} | T], JSON, State) when is_binary(Schema)->
    validate_json(T, JSON, State);


validate_array([], _, _, _) -> true;
validate_array(_, [], true, _) -> true;
validate_array(JSON, [], Schema = {_}, State) ->
    State1 = State#state{props_validated = false},
    [validate_schema(Schema, J, State1) || J <- JSON];
validate_array([JSON | T], [Schema | T1], AdditionalItems, State) ->
    State1 = State#state{props_validated = false},
    validate_schema(Schema, JSON, State1),
    validate_array(T, T1, AdditionalItems, State).

validate_object([], Props, Patterns, Additional, State) -> true;
validate_object([{Key, Prop} | T], Props, Patterns, Additional, State) ->
    PropertySchema = select_property(Key, Props, State),
    PatternSchemas = select_patterns(Key, Patterns, State),
    Schemas = case {PropertySchema, PatternSchemas} of
                  {undefined, []} -> select_additional(Additional);
                  {undefined, _} -> PatternSchemas;
                  _ -> [PropertySchema | PatternSchemas]
              end,
    [validate_schema(S, Prop, State) || S <- Schemas],
    true.

select_property(Key, undefined, _) -> undefined;
select_property(Key, {Props}, #state{atom_keys = true}) ->
    plist:find(Key, Props);
select_property(Key, {Props}, #state{existing_atom_keys = true}) ->
    plist:find(Key, Props);
select_property(Key, {Props}, #state{plain_string = Plain}) ->
    Key1 = char_code(atom_to_binary(Key, utf8), utf8, Plain),
    plist:find(Key1, Props).

select_patterns(Key, undefined, _) -> [];
select_patterns(Key, {Patterns}, State = #state{atom_keys = true}) ->
    select_patterns1(Patterns, atom_to_binary(Key, utf8), []);
select_patterns(Key, {Patterns}, State = #state{existing_atom_keys = true}) ->
    select_patterns1(Patterns, atom_to_binary(Key, utf8), []);
select_patterns(Key, {Patterns}, State = #state{plain_string = Plain}) ->
    select_patterns1(Patterns, char_code(Key, Plain, utf8), []).

select_patterns([], _, Acc) -> Acc;
select_patterns([{Pattern, Schema} | T], Key, Acc) ->
    case re:run(Key, atom_to_binary(Pattern, utf8), [unicode]) of
        nomatch -> select_patterns(T, Key, Acc);
        _ -> select_patterns(T, Key, [Schema | Acc])
    end.

select_additional(undefined) -> [];
select_additional(Schema = {_}) -> [Schema];
select_additional(true) -> [].

validate_dependencies([], _, _) -> true;
validate_dependencies([{Key, Keys} | T], JSON = {M}, State) when is_list(Keys) ->
    Key1 = case State of
               #state{atom_keys = true} -> Key;
               #state{existing_atom_keys = true} -> Key;
               #state{plain_string = Plain} ->
                   char_code(atom_to_binary(Key, utf8), utf8, Plain)
           end,
    case plist:member(Key, M) of
        true -> [validate_dependency(K, M, State) || K <- Keys];
        _ -> true
    end,
    validate_dependencies(T, JSON, State);
validate_dependencies([{Key, Schema} | T], JSON = {M}, State) ->
    Key1 = case State of
               #state{atom_keys = true} -> Key;
               #state{existing_atom_keys = true} -> Key;
               #state{plain_string = Plain} ->
                   char_code(atom_to_binary(Key, utf8), utf8, Plain)
           end,
    case plist:member(Key, M) of
        true -> validate_schema(Schema, JSON, State);
        _ -> true
    end,
    validate_dependencies(T, JSON, State).

validate_dependency(K, M, #state{atom_keys = true}) ->
    true = plist:member(binary_to_atom(K, utf8), M);
validate_dependency(K, M, #state{existing_atom_keys = true}) ->
    true = plist:member(binary_to_atom(K, utf8), M);
validate_dependency(K, M, #state{plain_string = Plain})
    true = plist:member(char_code(K, utf8, Plain), M).

validate_enum([null | _], null, _) -> true;
validate_enum([true | _], true, _) -> true;
validate_enum([false | _], false, _) -> true;
validate_enum([S | T], JSON, State) when is_binary(S), is_binary(JSON) ->
    #state{plain_string = Plain} = State,
    case char_code(S, utf8, Plain) of
        JSON -> true;
        _ -> validate_enum(T, JSON, State)
    end;
validate_enum([N | _], JSON, _) -> is_number(N), is_number(JSON), N =:= JSON;
validate_enum([Array | T], JSON, State) when is_list(Array), is_list(JSON) ->
    try [validate_enum([A], J, State) || {A, J} <- lists:zip(Array, JSON)]
    catch _:_ ->
            validate_enum(T, JSON, State)
    end;
validate_enum([{SProps} | T], {JProps}, State) ->
    try [validate_enum_prop(SProp, JProp, State)  ||
            {SProp, JProp} <- lists:zip(lists:sort(SProps), lists:sort(JProps))]
    catch _:_ -> validate_enum(T, JSON, State)
    end.
                     
validate_enum_prop({Key, SVal}, {Key, JVal}, State = #state{atom_keys = true}) ->
    validate_enum([SVal], JVal, State);
validate_enum_prop({K, SVal}, {K, JVal},State=#state{existing_atom_keys=true}) ->
    validate_enum([SVal], JVal, State);
validate_enum_prop({SKey, SVal}, {JKey, JVal}, #state{plain_string = Plain}) ->
    JKey = char_code(atom_to_binary(SKey, utf8), utf8, Plain),
    validate_enum([SVal], JVal, State).

validate_type([Type | Types], JSON, State) ->
    try validate_json([{type, Type}], JSON State)
    catch _:_ -> validate_type(Types, JSON, State)
    end.

validate_anyof([Schema | Schemas], JSON, State) ->
    try validate_schema(Schema, JSON, State)
    catch _: _ -> validate_anyof(Schemas, JSON, State)
    end.

validate_oneof([], _, true, _) -> true;
validate_oneof([Schema | Schemas], false, JSON, State) ->
    try validate_schema(S, J, State) of
        _ -> validate_schema(Schemas, true, JSON, State)
    catch _: _ -> validate_schema(Schemas, false, JSON, State)
    end;
validate_oneof([Schema | Schemas], true, JSON, State) ->
    try validate_schema(S, J, State) of
        _ -> erlang:throw(more_than_oneof)
    catch _: _ -> validate_schema(Schemas, true, JSON, State)
    end.

string_length(String, #state{plain_string = {utf16,_}}) -> byte_size(String) * 2;
string_length(String, #state{plain_string = {utf32,_}}) -> byte_size(String) * 4;
string_length(String, _) -> utf8_length(String, 0).

utf8_length(<<>>, Acc) -> Acc;
utf8_length(<<_/utf8, T/binary>>, Acc) -> utf8_length(T, Acc + 1).

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts([], State) -> State;
parse_opts(Opts, State) -> lists:foldl(fun parse_opt/2, State, Opts).

parse_opt(pointer, State) -> State#state{pointer = true};
parse_opt(rfc4627, State) -> State#state{rfc4627 = true};
parse_opt(maps, State) -> State#state{maps = true};
parse_opt({maps, Bool}, State) when is_boolean(Bool) -> State#state{maps=Bool};
parse_opt({maps, safe}, State) -> State#state{maps = safe};
parse_opt(binary, State) -> State#state{return_type = binary};
parse_opt(iolist, State) -> State#state{return_type = iolist};
parse_opt(bom, State) -> State#state{bom = true};
parse_opt(decode, State) -> State#state{decode = true};
parse_opt({atom_strings, Bool}, State) when is_boolean(Bool)->
    State#state{atom_strings = Bool};
parse_opt(atom_keys, State) ->
    State#state{atom_keys = true};
parse_opt({atom_keys, Bool}, State) when is_boolean(Bool)->
    State#state{atom_keys = Bool};
parse_opt(existing_atom_keys, State)->
    State#state{existing_atom_keys = true};
parse_opt({existing_atom_keys, Bool}, State) when is_boolean(Bool)->
    State#state{existing_atom_keys = Bool};
parse_opt({plain_string, PlainString}, State) ->
    case lists:member(PlainString, ?PLAINFORMATS) of
        true -> State#state{plain_string = PlainString};
        false -> badarg(State)
    end;
parse_opt({encoding, Encoding} , State) ->
    case lists:member(Encoding, ?ENCODINGS) of
        true -> State#state{encoding = Encoding};
        false -> badarg(State)
    end;
parse_opt(_, State) ->
    badarg(State).

next(<<H, T/binary>>, #state{encoding = utf8}) -> {H, T};
next(<<H, 0, T/binary>>, #state{encoding = ?UTF16L}) -> {H, T};
next(<<0, H, T/binary>>, #state{encoding = ?UTF16B}) -> {H, T};
next(<<H:16, T/binary>>, #state{encoding = {utf16, _}}) -> {<<H:16>>, T};
next(<<H, 0:24, T/binary>>, #state{encoding = ?UTF32L}) -> {H, T};
next(<<0:24, H, T/binary>>, #state{encoding = ?UTF32B}) -> {H, T};
next(<<H:32, T/binary>>, #state{encoding = {utf32, _}}) -> {<<H:32>>, T};
next(<<>>, _) -> eob;
next(_, State) -> badarg(State).

encode_chars(Chars, #state{encoding = utf8}) -> Chars;
encode_chars(Chars, State) when is_list(Chars) ->
    << <<(encode_char(C, State))/binary>> || C <- Chars>>;
encode_chars(Chars, State) when is_binary(Chars) ->
    << <<(encode_char(C, State))/binary>> || <<C>> <= Chars>>.

encode_char(C, #state{encoding = utf8}) -> <<C>>;
encode_char(C, #state{encoding = ?UTF16L}) -> <<C, 0>>;
encode_char(C, #state{encoding = ?UTF16B}) -> <<0, C>>;
encode_char(C, #state{encoding = ?UTF32L}) -> <<C, 0:24>>;
encode_char(C, #state{encoding = ?UTF32B}) -> <<0:24, C>>.

char_code(Text, Coding, Coding) -> Text;
char_code(Text, utf8, ?UTF16B) ->
    << <<C/utf16-big>> || <<C/utf8>> <= Text >>;
char_code(Text, utf8, ?UTF16L) ->
    << <<C/utf16-little>> || <<C/utf8>> <= Text >>;
char_code(Text, utf8, ?UTF32B) ->
    << <<C/utf32-big>> || <<C/utf8>> <= Text >>;
char_code(Text, utf8, ?UTF32L) ->
    << <<C/utf32-little>> || <<C/utf8>> <= Text >>;
char_code(Text, ?UTF16B, utf8) ->
    << <<C/utf8>> || <<C/utf16-big>> <= Text >>;
char_code(Text, ?UTF16B, ?UTF16L) ->
    << <<C/utf16-little>> || <<C/utf16-big>> <= Text >>;
char_code(Text, ?UTF16B, ?UTF32B) ->
    << <<C/utf32-big>> || <<C/utf16-big>> <= Text >>;
char_code(Text, ?UTF16B, ?UTF32L) ->
    << <<C/utf32-little>> || <<C/utf16-big>> <= Text >>;
char_code(Text, ?UTF16L, utf8) ->
    << <<C/utf8>> || <<C/utf16-little>> <= Text >>;
char_code(Text, ?UTF16L, ?UTF16B) ->
    << <<C/utf16-big>> || <<C/utf16-little>> <= Text >>;
char_code(Text, ?UTF16L, ?UTF32B) ->
    << <<C/utf32-big>> || <<C/utf16-little>> <= Text >>;
char_code(Text, ?UTF16L, ?UTF32L) ->
    << <<C/utf32-little>> || <<C/utf16-little>> <= Text >>;
char_code(Text, ?UTF32B, utf8) ->
    << <<C/utf8>> || <<C/utf32-big>> <= Text >>;
char_code(Text, ?UTF32B, ?UTF16B) ->
    << <<C/utf16-big>> || <<C/utf32-big>> <= Text >>;
char_code(Text, ?UTF32B, ?UTF16L) ->
    << <<C/utf16-little>> || <<C/utf32-big>> <= Text >>;
char_code(Text, ?UTF32B, ?UTF32L) ->
    << <<C/utf32-little>> || <<C/utf32-big>> <= Text >>;
char_code(Text, ?UTF32L, utf8) ->
    << <<C/utf8>> || <<C/utf32-little>> <= Text >>;
char_code(Text, ?UTF32L, ?UTF16B) ->
    << <<C/utf16-big>> || <<C/utf32-little>> <= Text >>;
char_code(Text, ?UTF32L, ?UTF16L) ->
    << <<C/utf16-little>> || <<C/utf32-little>> <= Text >>;
char_code(Text, ?UTF32L, ?UTF32B) ->
    << <<C/utf32-big>> || <<C/utf32-little>> <= Text >>.

%%--------------------------------------------------------------------
-spec badarg(#state{}) -> no_return().
%%--------------------------------------------------------------------
badarg(#state{orig_call = {Funcion, Args, Line}}) ->
    Trace = [{?MODULE, Funcion, Args, [{file, ?FILE}, {line, Line - 1}]} |
             lists:dropwhile(fun(T) -> element(1, T) == ?MODULE end,
                             erlang:get_stacktrace())],
    exit({badarg, Trace}).
