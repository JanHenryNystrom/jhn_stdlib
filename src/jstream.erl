%%==============================================================================
%% Copyright 2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A JSON stream library based on:
%%%    The JavaScript Object Notation (JSON) Data Interchange Format   (rfc7159)
%%%
%%%  JSON is represented as follows:
%%%
%%%  value         : true | false | null | object | array | number | string
%%%
%%%  object        : map
%%%  array         : [value*]
%%%  string        : UTF-8 binary
%%%  number        : integer() | float()
%%%  true          : atom(true)
%%%  false         : atom(false)
%%%  null          : atom(null)
%%%
%%%  Strings can be represented by atoms when generating JSON, but will not
%%%  not be generated when converting JSON to erlang. Strings are restricted
%%%  to UTF-8
%%%
%%%  When converting Erlang terms to JSON iolists are generated but
%%%  it can generate a binary if so instructed.
%%%
%%%  Objects as maps, multiple occurrences of the members is not supported.
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jstream).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2]).

%% Exported types
-export_type([json/0]).

%% Types
-type json()     :: true | false | null |
                    number() | jstring() |
                    object() | array().

-type jstring()  :: binary().
-type object()   :: map().
-type array()    :: [json()].

-type cont()     :: {decode, stack()} |
                    {object, {complete(), expect()}, acc(), stack()} |
                    {array, {first(), complete()}, array(), stack()} |
                    {string, binary(), stack()} |
                    {unescape, binary(), stack()} |
                    {number, stage(), phase(), list(), stack()}.

-type complete() :: boolean().
-type expect()   :: name | comma | colon.
-type first()    :: boolean().
-type stage()    :: sign | zero | pre | post.
-type phase()    :: int | float | exp.

-type acc()      :: [{jstring(), json()}].

-type stack()    :: [{array, array()} |
                     {name, acc()} |
                     {value, {name(), acc()}}
                    ].
-type name()     :: jstring().

%% Defines

%% Defines for encode_float/1.
-define(BIG_POW, (1 bsl 52)).
-define(MIN_EXP, (-1074)).

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

%% Decode macros
-define(IS_INT(C), C>=$0, C=<$9).
-define(IS_POS_INT(C), C>=$1, C=<$9).
-define(IS_SIGN(C), C == $-; C == $+).
-define(IS_EXP(C), C==$E; C==$e).
-define(ZERO_OR_POST(Stage), Stage == zero; Stage == post).
-define(EXP_ZERO_OR_POST(C, Stage),
        ((Stage == zero) orelse (Stage == post))
        andalso ((C == $E) orelse (C == $e))).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Term) -> JSON.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent of encode(Term, iolist) -> JSON.
%% @end
%%--------------------------------------------------------------------
-spec encode(json()) -> iodata().
%%--------------------------------------------------------------------
encode(true) -> <<"true">>;
encode(false) -> <<"false">>;
encode(null) -> <<"null">>;
encode(Object = #{}) -> encode_object(Object);
encode([]) -> <<"[]">>;
encode(List = [_ | _]) -> encode_array(List);
encode(I) when is_integer(I) -> integer_to_binary(I);
encode(F) when is_float(F) -> encode_float(F);
encode(String) when is_binary(String) -> encode_string(String);
encode(String) when is_atom(String) ->
    encode_string(atom_to_binary(String, utf8)).

%%--------------------------------------------------------------------
%% Function: encode(Term, Options) -> JSON.
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     iolist -> an iolist is returned (default)
%% @end
%%--------------------------------------------------------------------
-spec encode(json(), binary) -> binary().
%%--------------------------------------------------------------------
encode(V, iolist) -> encode(V);
encode(V, binary) -> iolist_to_binary(encode(V)).


%%--------------------------------------------------------------------
%% Function: decode(JSON) -> {Term, Binary} | {more, Continuation}.
%% @doc
%%   Decodes the binary into a tuple of structured Erlang term and the
%%   remaining binary or a continuation if the binary did not contain
%%   a complete JSON value. The continuation can be used by decode/2 with
%%   a binary containing the rest of the JSON value to decode.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> {json(), binary()} | {more, cont()}. 
%%--------------------------------------------------------------------
decode(B) -> do_decode(B, []).

%%--------------------------------------------------------------------
%% Function: decode(JSON, Options) -> Term.
%% @doc
%%   Decodes a binary and a continuation into a tuple of structured
%%   Erlang term and the remaining binary or a continuation if the binary
%%   did not contain a complete JSON value. The continuation can be used
%%   with a binary containing the rest of the JSON value to decode.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), cont()) -> {json(), binary()} | {more, cont()}. 
%%--------------------------------------------------------------------
decode(B, {decode, S}) -> do_decode(B, S);
decode(B, {object, State, Acc, S}) -> object(B, State, Acc, S);
decode(B, {array, State, Acc, S}) -> array(B, State, Acc, S);
decode(B, {string, Acc, S}) -> string(B, Acc, S);
decode(B, {unescape, Acc, S}) -> unescape(B, Acc, S);
decode(B, {number, State, Phase, Acc, S}) -> number(B, State, Phase, Acc, S).

%% ===================================================================
%% Encoding
%% ===================================================================

encode_object(Object) ->
    case maps:fold(fun element/3, [], Object) of
        [] -> <<"{}">>;
        [_ | Members] -> [<<"{">>, Members, <<"}">>]
    end.

element(N, V, Acc) -> [<<",">>, encode(N), <<":">>, encode(V) | Acc].

encode_array(A) ->
    [_ | Es] = lists:foldr(fun(E, Acc) -> [<<",">>, encode(E) |Acc] end, [], A),
    [<<"[">>, Es, <<"]">>].

encode_string(String) ->
    case escapeable(String) of
        true -> [<<"\"">>, escape(String, <<>>), <<"\"">>];
        false -> [<<"\"">>, String, <<"\"">>]
    end.

escapeable(<<>>) -> false;
escapeable(<<0, _/binary>>) -> true;
escapeable(<<1, _/binary>>) -> true;
escapeable(<<2, _/binary>>) -> true;
escapeable(<<3, _/binary>>) -> true;
escapeable(<<4, _/binary>>) -> true;
escapeable(<<5, _/binary>>) -> true;
escapeable(<<6, _/binary>>) -> true;
escapeable(<<7, _/binary>>) -> true;
escapeable(<<8, _/binary>>) -> true;
escapeable(<<9, _/binary>>) -> true;
escapeable(<<10, _/binary>>) -> true;
escapeable(<<11, _/binary>>) -> true;
escapeable(<<12, _/binary>>) -> true;
escapeable(<<13, _/binary>>) -> true;
escapeable(<<14, _/binary>>) -> true;
escapeable(<<15, _/binary>>) -> true;
escapeable(<<16, _/binary>>) -> true;
escapeable(<<17, _/binary>>) -> true;
escapeable(<<18, _/binary>>) -> true;
escapeable(<<19, _/binary>>) -> true;
escapeable(<<20, _/binary>>) -> true;
escapeable(<<21, _/binary>>) -> true;
escapeable(<<22, _/binary>>) -> true;
escapeable(<<23, _/binary>>) -> true;
escapeable(<<24, _/binary>>) -> true;
escapeable(<<25, _/binary>>) -> true;
escapeable(<<26, _/binary>>) -> true;
escapeable(<<27, _/binary>>) -> true;
escapeable(<<28, _/binary>>) -> true;
escapeable(<<29, _/binary>>) -> true;
escapeable(<<30, _/binary>>) -> true;
escapeable(<<31, _/binary>>) -> true;
escapeable(<<34, _/binary>>) -> true;
escapeable(<<47, _/binary>>) -> true;
escapeable(<<92, _/binary>>) -> true;
escapeable(<<_/utf8, T/binary>>) -> escapeable(T).

escape(<<>>, Acc) -> Acc;
escape(<<?NULL, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0000">>);
escape(<<1, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0001">>);
escape(<<2, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0002">>);
escape(<<3, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0003">>);
escape(<<4, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0004">>);
escape(<<5, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0005">>);
escape(<<6, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0006">>);
escape(<<?BEL, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0007">>);
escape(<<?BS, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\b">>);
escape(<<?HT, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\t">>);
escape(<<?LF, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\n">>);
escape(<<?VT, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u000B">>);
escape(<<?FF, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\f">>);
escape(<<?CR, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\r">>);
escape(<<14, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u000E">>);
escape(<<15, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u000F">>);
escape(<<16, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0010">>);
escape(<<17, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0011">>);
escape(<<18, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0012">>);
escape(<<19, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0013">>);
escape(<<20, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0014">>);
escape(<<21, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0015">>);
escape(<<22, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0016">>);
escape(<<23, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0017">>);
escape(<<24, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0018">>);
escape(<<25, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u0019">>);
escape(<<26, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u001A">>);
escape(<<27, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u001B">>);
escape(<<28, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u001C">>);
escape(<<29, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u001D">>);
escape(<<$", T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\\"">>);
escape(<<$\/, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\/">>);
escape(<<$\\, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\\\">>);
escape(<<H/utf8, T/binary>>, Acc) -> escape(T, <<Acc/binary, H>>).

%% ===================================================================
%% encode_float/1 the implementation based on
%% "Printing Floating-Point Numbers Quickly and Accurately"
%%  by R.,G. Burger and R.,K. Dybvig in Proceedings of the SIGPLAN '96
%%  Conference on Programming Language Design and Implementation.
%% ===================================================================

encode_float(0.0) -> "0.0";
encode_float(Float) when is_float(Float) ->
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

do_decode(<<>>, S) -> {more, {decode, args = S}};
do_decode(<<?HT, T/binary>>, S) -> do_decode(T, S);
do_decode(<<?LF, T/binary>>, S) -> do_decode(T, S);
do_decode(<<?CR, T/binary>>, S) -> do_decode(T, S);
do_decode(<<?SPC, T/binary>>, S) -> do_decode(T, S);
do_decode(<<"true", T/binary>>, S) -> pop(true, T, S);
do_decode(<<"false", T/binary>>, S) -> pop(false, T, S);
do_decode(<<"null", T/binary>>, S) -> pop(null, T, S);
do_decode(<<${, T/binary>>, S) -> object(T, {true, name}, [], S);
do_decode(<<$[, T/binary>>, S) -> array(T, {false, false}, [],S);
do_decode(<<$", T/binary>>, S) -> string(T, <<>>, S);
do_decode(<<$-, T/binary>>, S) -> number(T, pre, int, [$-], S);
do_decode(B = <<$0, _/binary>>, S) -> number(B, pre, int, [], S);
do_decode(B = <<$1, _/binary>>, S) -> number(B, pre, int, [], S);
do_decode(B = <<$2, _/binary>>, S) -> number(B, pre, int, [], S);
do_decode(B = <<$3, _/binary>>, S) -> number(B, pre, int, [], S);
do_decode(B = <<$4, _/binary>>, S) -> number(B, pre, int, [], S);
do_decode(B = <<$5, _/binary>>, S) -> number(B, pre, int, [], S);
do_decode(B = <<$6, _/binary>>, S) -> number(B, pre, int, [], S);
do_decode(B = <<$7, _/binary>>, S) -> number(B, pre, int, [], S);
do_decode(B = <<$8, _/binary>>, S) -> number(B, pre, int, [], S);
do_decode(B = <<$9, _/binary>>, S) -> number(B, pre, int, [], S).

object(<<>>, State, Acc, S) -> {more, {object, State, Acc, S}};
object(<<?HT, T/binary>>, State, Acc, S) -> object(T, State, Acc, S);
object(<<?LF, T/binary>>, State, Acc, S) -> object(T, State, Acc, S);
object(<<?CR, T/binary>>, State, Acc, S) -> object(T, State, Acc, S);
object(<<?SPC, T/binary>>, State, Acc,S) -> object(T, State, Acc, S);
object(<<$}, T/binary>>, {true,_}, Acc, S) -> pop(maps:from_list(Acc), T, S);
object(<<$,, T/binary>>, {true, comma}, Acc, S) -> object(T,{false,name},Acc,S);
object(<<$", T/binary>>, {_, name}, Acc, S) -> string(T, <<>>, [{name, Acc}|S]);
object(<<$:, T/binary>>, {false, colon},Acc,S) -> do_decode(T,[{value,Acc}|S]).

array(<<>>, State, Acc, S) -> {more, {array, State, Acc, S}};
array(<<?HT, T/binary>>, State, Acc, S) -> array(T, State, Acc, S);
array(<<?LF, T/binary>>, State, Acc, S) -> array(T, State, Acc, S);
array(<<?CR, T/binary>>, State, Acc, S) -> array(T, State, Acc, S);
array(<<?SPC, T/binary>>, State, Acc, S) -> array(T, State, Acc, S);
array(<<$,, T/binary>>, {false, true}, Acc, S) -> array(T, {true, false},Acc,S);
array(<<$], T/binary>>, {false, _}, Acc, S) -> pop(lists:reverse(Acc), T, S);
array(T, {_, false}, Acc, S) -> do_decode(T, [{array, Acc} | S]).

string(<<>>, Acc, S) -> {more, {string, Acc, S}};
string(<<$\\, T/binary>>, Acc, S) -> unescape(T, Acc, S);
string(<<$", T/binary>>, Acc, S) -> pop(Acc, T, S);
string(<<H/utf8, T/binary>>, Acc, S) -> string(T, <<Acc/binary, H/utf8>>, S).

unescape(<<>>, Acc, S) -> {more, {unescape, Acc, S}};
unescape(<<$", T/binary>>, Acc, S) -> string(T, <<Acc/binary, $">>, S);
unescape(<<$\\, T/binary>>, Acc, S) -> string(T, <<Acc/binary,$\\ >>, S);
unescape(<<$/, T/binary>>, Acc, S) -> string(T, <<Acc/binary, $/>>, S);
unescape(<<$0, T/binary>>, Acc, S) -> string(T, <<Acc/binary, ?NULL>>,S);
unescape(<<$a, T/binary>>, Acc, S) -> string(T, <<Acc/binary, ?BEL>>, S);
unescape(<<$b, T/binary>>, Acc, S) -> string(T, <<Acc/binary, ?BS>>, S);
unescape(<<$t, T/binary>>, Acc, S) -> string(T, <<Acc/binary, ?HT>>, S);
unescape(<<$n, T/binary>>, Acc, S) -> string(T, <<Acc/binary, ?LF>>, S);
unescape(<<$f, T/binary>>, Acc, S) -> string(T, <<Acc/binary, ?FF>>, S);
unescape(<<$v, T/binary>>, Acc, S) -> string(T, <<Acc/binary, ?VT>>, S);
unescape(<<$r, T/binary>>, Acc, S) -> string(T, <<Acc/binary, ?CR>>, S);
unescape(<<$s, T/binary>>, Acc, S) -> string(T, <<Acc/binary, ?SPC>>, S);
unescape(<<$u, A, B, C, D, T/binary>>, Acc, S) ->
    string(T, <<Acc/binary,(list_to_integer([A, B, C, D]))/utf8>>,S);
unescape(<<H, T/binary>>, Acc, S) ->
    string(T, <<Acc/binary,$\\, H>>, S).

number(<<$0, T/binary>>, pre, int, Acc, S) -> number(T, zero, int, [$0|Acc],S);
number(<<H, T/binary>>, pre, exp, Acc, S) when ?IS_SIGN(H) ->
    number(T, sign, exp, [H | Acc], S);
number(<<H, T/binary>>, pre, exp, Acc, S) when ?IS_INT(H) ->
    number(T, post, exp, [H | Acc], S);
number(<<H, T/binary>>, pre, float, Acc, S) when ?IS_INT(H) ->
    number(T, post, float, [H | Acc], S);
number(<<H, T/binary>>, pre, Phase, Acc, S) when ?IS_POS_INT(H) ->
    number(T, post, Phase, [H | Acc], S);
number(<<H, T/binary>>, sign, Phase, Acc, S) when ?IS_INT(H) ->
    number(T, post, Phase, [H | Acc], S);
number(<<H, T/binary>>, post, Phase, Acc, S) when ?IS_INT(H) ->
    number(T, post, Phase, [H | Acc], S);
number(<<$., T/binary>>,Stage,int,Acc,S) when ?ZERO_OR_POST(Stage) ->
    number(T, pre, float, [$. | Acc], S);
number(<<E,T/binary>>,Stage,int,Acc,S) when ?EXP_ZERO_OR_POST(E, Stage) ->
    number(T, pre, exp, [E, $0, $. | Acc], S);
number(<<E, T/binary>>, post, float, Acc, S)  when ?IS_EXP(E) ->
    number(T, pre, exp, [E | Acc], S);
number(B, Stage, int, Acc, S) when ?ZERO_OR_POST(Stage) ->
    pop(list_to_integer(lists:reverse(Acc)), B, S);
number(B, post, _, Acc, S) ->
    pop(list_to_float(lists:reverse(Acc)), B, S);
number(<<>>, State, Phase, Acc, S) ->
    {more, {number, State, Phase, Acc, S}}.

pop(V, B, []) -> {V, B};
pop(V, B, [{array, Acc} | S]) -> array(B, {false, true}, [V | Acc], S);
pop(N, B, [{name, Acc} | S]) -> object(B, {false, colon}, {N, Acc}, S);
pop(V, B, [{value, {N, Acc}} | S]) -> object(B, {true,comma},[{N, V} | Acc], S).
