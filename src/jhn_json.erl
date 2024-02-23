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
%%%  A JSON stream library based on:
%%%    The JavaScript Object Notation (JSON) Data Interchange Format   (rfc8259)
%%%    JavaScript Object Notation (JSON) Pointer                       (rfc6901)
%%%    JavaScript Object Notation (JSON) Patch                         (rfc6902)
%%%    JSON Merge Patch                                                (rfc7396)
%%%
%%%  JSON is represented as follows:
%%%
%%%  value         : true | false | null | object | array | number | string
%%%  pointer       : top | [integer | string | '-']
%%%  patch         : [object]
%%%  merge         : object
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
%%%  not be generated when converting JSON to erlang or any of the functions
%%%  returning JSON.
%%%
%%%  When converting Erlang terms to JSON iolists are generated but
%%%  it can generate a binary if so instructed.
%%%
%%%  Objects as maps, multiple occurrences of the members is not supported.
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2021-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_json).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2,
         next/1, next/2,
         eval/2, eval/3, patch/2, merge/2
        ]).


%% FIXME
-export([chunk/1]).

%% Exported types
-export_type([json/0, pointer/0, patch/0, merge/0]).

%% Types
-type json()      :: true | false | null |
                     number() | jstring() |
                     object() | array().

-type jstring()   :: binary().
-type object()    :: #{jstring() => json()}.
-type array()     :: [json()].

-type cont()      :: {decode, stack()} |
                     {object, {complete(), expect()}, acc(), stack()} |
                     {array, {first(), complete()}, array(), stack()} |
                     {string, binary(), stack()} |
                     {unescape, binary(), stack()} |
                     {number, stage(), phase(), list(), stack()}.

-type eval_cont() :: {decode, path(), cont()}.

-type complete()  :: boolean().
-type expect()    :: name | comma | colon.
-type first()     :: boolean().
-type stage()     :: sign | zero | pre | post.
-type phase()     :: int | float | exp.

-type acc()       :: [{jstring(), json()}].

-type stack()     :: [{array, array()} |
                         {name, acc()} |
                         {value, {name(), acc()}}].

-type path()      :: [integer() | binary()].

-type name()      :: jstring().


%% FIXME
-type merge() :: object().
-type patch() :: [object()].
-type opt() :: _.
-type next_cont() :: _.
-type opts() :: _.
-type pointer() :: top | ['-' | integer() | jstring() | atom()].

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

%% Eval macros
-define(WS(WS), WS == ?HT; WS == ?LF; WS == ?CR; WS == ?SPC).

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
%% Function: encode(Term, [Option]) -> JSON.
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     pointer -> the term represents a pointer
%%     binary -> a binary is returned
%%     iolist -> an iolist is returned (default)
%% @end
%%--------------------------------------------------------------------
-spec encode(json() | pointer(), [opt()]) -> binary().
%%--------------------------------------------------------------------
encode(T, [binary, _]) -> iolist_to_binary(encode_pointer(T, []));
encode(T, [_, binary]) -> iolist_to_binary(encode_pointer(T, []));
encode(T, [_, _]) ->  encode_pointer(T, []);
encode(T, [pointer]) -> encode_pointer(T, []);
encode(T, [binary]) ->  iolist_to_binary(encode(T));
encode(T, _) -> encode(T).

%%--------------------------------------------------------------------
%% Function: decode(JSON) -> {Term, Binary} | {more, Continuation}.
%% @doc
%%   Decodes the binary into a tuple of structured Erlang term and the
%%   remaining binary or a continuation if the binary did not contain
%%   a complete JSON value. The continuation can be used by decode/2 with
%%   a binary containing the rest of the JSON value to decode.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> {json(), binary()} | pointer() | {more, cont()}.
%%--------------------------------------------------------------------
decode(B) -> decode(B, []).

%%--------------------------------------------------------------------
%% Function: decode(JSON, Continuation) -> {Term, Binary} | {more,Continuation}.
%% @doc
%%   Decodes a binary and a continuation into a tuple of structured
%%   Erlang term and the remaining binary or a continuation if the binary
%%   did not contain a complete JSON value. The continuation can be used
%%   with a binary containing the rest of the JSON value to decode.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), cont() | [opt()]) -> pointer() | {json(), binary()} |
                                            {more, cont()}.
%%--------------------------------------------------------------------
decode(B, {decode, S}) -> do_decode(B, true, S);
decode(B, {base, Check, V, S}) -> base(Check, B, V, true, S);
decode(B, {object, State, Acc, S}) -> object(B, State, Acc, true,S);
decode(B, {array, State, Acc, S}) -> array(B, State, Acc, true, S);
decode(B, {string, Acc, S}) -> string(B, Acc, true, S);
decode(B, {unescape, Acc, S}) -> unescape(B, Acc, true, S);
decode(B, {unescape_hex, Hex, Acc, S}) -> unescape_hex(B, Hex, Acc, true, S);
decode(B, {number, State, Phase, Acc, S}) ->
    number(B, State, Phase, Acc, true, S);
decode(<<>>, []) -> top;
decode(B, []) ->
    case do_decode(B, false, []) of
        {more, _} -> erlang:error(badarg);
        {JSON, _} -> JSON;
        Pointer -> Pointer
    end;
decode(B, [stream]) ->
    do_decode(B, true, []).

%%--------------------------------------------------------------------
%% Function: next(Binary) -> {Binary, Binary} | {more, Continuation}.
%% @doc
%%   Picks the first json on a stream and returns that and the rest.
%% @end
%%--------------------------------------------------------------------
-spec next(binary()) -> {binary(), binary()} | {more, next_cont()}.
%%--------------------------------------------------------------------
next(B) -> do_next(B).

%%--------------------------------------------------------------------
%% Function: next(Binary, Cont) -> {Binary, Binary} | {more, Cont}.
%% @doc
%%   Picks the first json on a stream given a continuation and returns that
%%   and the rest.
%% @end
%%--------------------------------------------------------------------
-spec next(binary(), next_cont()) -> {binary(), binary()} | {more, next_cont()}.
%%--------------------------------------------------------------------
next(T, {next_value, Acc, C}) -> next_value(T, Acc, C);
next(T, {next_base, Base, Acc, C}) -> next_base(Base, T, Acc, C);
next(T, {next_array, Expect, Acc, C}) -> next_array(T, Expect, Acc, C);
next(T, {next_object, Expect, Acc, C}) -> next_object(T, Expect, Acc,C);
next(T, {next_name, Acc, C}) -> next_name(T, Acc, C);
next(T, {next_colon, Flag, Acc, C}) -> next_colon(T, Flag, Acc, C);
next(T, {next_string, Acc, C}) -> next_string(T, Acc, C);
next(T, {next_unescape, Acc, C}) -> next_unescape(T, Acc, C);
next(T, {next_unescape_hex, Hex, Acc, C}) -> next_unescape_hex(T, Hex, Acc, C);
next(T, {next_number, Acc, C}) -> next_number(T, Acc, C).

%%--------------------------------------------------------------------
%% Function: eval(JSONPointer | Continuation, Binary | JSON) -> JSON.
%% @doc
%%   Selects and decodes a Fragment of a JSON document based on the Pointer.
%% @end
%%--------------------------------------------------------------------
-spec eval(binary() | pointer(), json()) -> json() | {error, _};
          (binary() | pointer(), binary()) ->
                  {json(), binary()} | {more, cont()} | {error, _};
          (eval_cont(), binary())  ->
                  {json(), binary()} | {more, cont()} | {error, _}.
%%--------------------------------------------------------------------
eval({eval_binary, P, S, Path}, T) -> eval_binary(P, T, S, Path);
eval({eval_dash, Expect, Size, S, P}, T) -> eval_dash(T, Expect, Size, S, P);
eval({eval_array, N, Expect,S,P,Rest},T) -> eval_array(N, T, Expect,S,P,Rest);
eval({eval_object,Key,Expect,S,P,Rest},T) -> eval_object(Key,T,Expect,S,P,Rest);
eval({eval_key, Key, S, Path, Rest}, T) -> eval_key(T, Key, S, Path, Rest);
eval({skip_value, C}, T) -> skip_value(T, C);
eval({skip_base, Base, C}, T) -> skip_base(Base, T, C);
eval({skip_object, Expect, C}, T) -> skip_object(T, Expect, C);
eval({skip_string, C}, T) -> skip_string(T, C);
eval({skip_colon, Flag, C}, T) -> skip_colon(T, Flag, C);
eval({skip_number, C}, T) -> skip_number(T, C);
eval({unwind, P, JSON}, T) -> unwind(P, T, JSON);
eval({unwind_array, C}, T) -> unwind_array(T, C);
eval({unwind_object, C}, T) ->  unwind_object(T, C);
eval({unwind_name, C}, T) -> unwind_name(T, C);
eval({unwind_colon, C}, T) -> unwind_colon(T, C);
eval({unwind_string, C}, T) ->  unwind_string(T, C);
eval({unwind_value, C}, T) ->  unwind_value(T, C);
eval({unwind_number, C}, T) -> unwind_number(T, C);
eval({unwind_base, Expect, C}, T) ->  unwind_base(Expect, T, C);
eval({decode, Path, Cont}, T) ->
    case decode(T, Cont) of
        {more, Cont} -> {more, {decode, Path, Cont}};
        {JSON, T} -> unwind(Path, T, JSON)
    end;
eval(Pointer, J) ->
    eval(Pointer, J, []).

%%--------------------------------------------------------------------
-spec eval(binary() | pointer(), json(), []) -> json() | {error, _};
          (binary() | pointer(), binary(), opts()) ->
                  {json(), binary()} | {more, cont()} | {error, _}.
%%--------------------------------------------------------------------
eval(top, B, []) when is_binary(B) -> decode(B);
eval(top, J, []) -> J;
eval(Pointer, JSON, Opts) when is_binary(Pointer) ->
    eval(decode(Pointer), JSON, Opts);
eval(top, B, [stream]) when is_binary(B) ->
    eval_binary([], B, true, []);
eval(Pointer, B, []) when is_binary(B) ->
    eval_binary(Pointer, B, false, []);
eval(Pointer, B, [stream]) when is_binary(B) ->
    eval_binary(Pointer, B, true, []);
eval(Pointer, J, []) ->
    eval_json(Pointer, J, []).

%%--------------------------------------------------------------------
%% Function: patch(Patch, JSON) -> JSON.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec patch(patch(), json()) -> json() | error.
%%--------------------------------------------------------------------
patch([], JSON) -> JSON;
patch([Op = #{<<"op">> := <<"test">>} | Ops], JSON) ->
    case apply_op(Op, JSON) of
        true -> patch(Ops, JSON);
        false -> error
    end;
patch([Op | Ops], JSON) ->
    patch(Ops, apply_op(Op, JSON)).

%%--------------------------------------------------------------------
%% Function: merge(Patch, JSON) -> JSON.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec merge(merge(), json()) -> json().
%%--------------------------------------------------------------------
merge(Patch = #{}, JSON = #{}) -> maps:fold(fun merge_object/3, JSON, Patch);
merge(Patch = #{}, _) -> merge(Patch, #{});
merge(Patch, _) -> Patch.

%% ===================================================================
%% Encoding
%% ===================================================================

encode_pointer(top, _) -> <<>>;
encode_pointer([], []) -> <<$/>>;
encode_pointer([], Acc) -> lists:reverse(Acc);
encode_pointer([H | T], Acc) when is_binary(H) ->
    encode_pointer(T, [[$/, pointer_escape(H)] | Acc]);
encode_pointer(['-' | T], Acc) ->
    encode_pointer(T, [[$/, $-] | Acc]);
encode_pointer([H | T], Acc) when is_atom(H) ->
    encode_pointer([atom_to_binary(H, utf8) | T], Acc);
encode_pointer([H | T], Acc) when is_integer(H), H >= 0 ->
    encode_pointer(T, [integer_to_binary(H), $/ | Acc]);
encode_pointer(_, _) ->
    erlang:error(badarg).

pointer_escape(String) ->
    case pointer_escapeable(String) of
        true -> pointer_escape(String, <<>>);
        false -> String
    end.

pointer_escapeable(<<>>) -> false;
pointer_escapeable(<<$~, _/binary>>) -> true;
pointer_escapeable(<<$/, _/binary>>) -> true;
pointer_escapeable(<<_, T/binary>>) -> pointer_escapeable(T).

pointer_escape(<<>>, Acc) -> Acc;
pointer_escape(<<$~, T/binary>>, Acc) ->
    pointer_escape(T, <<Acc/binary, $~, $0>>);
pointer_escape(<<$/, T/binary>>, Acc) ->
    pointer_escape(T, <<Acc/binary, $~, $1>>);
pointer_escape(<<H, T/binary>>, Acc) ->
    pointer_escape(T, <<Acc/binary, H>>).

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
escape(<<30, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u001E">>);
escape(<<31, T/binary>>, Acc) -> escape(T, <<Acc/binary, "\\u001F">>);
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

do_decode(<<>>, true, S) -> {more, {decode, S}};
do_decode(<<?HT, T/binary>>, F, S) -> do_decode(T, F, S);
do_decode(<<?LF, T/binary>>, F, S) -> do_decode(T, F, S);
do_decode(<<?CR, T/binary>>, F, S) -> do_decode(T, F, S);
do_decode(<<?SPC, T/binary>>, F, S) -> do_decode(T, F, S);
do_decode(<<$t, T/binary>>, F, S) -> base("rue", T, true, F, S);
do_decode(<<$f, T/binary>>, F, S) -> base("alse", T, false, F, S);
do_decode(<<$n, T/binary>>, F, S) -> base("ull", T, null, F, S);
do_decode(<<${, T/binary>>, F, S) -> object(T, {true, name}, [], F, S);
do_decode(<<$[, T/binary>>, F, S) -> array(T, {false, false}, [], F, S);
do_decode(<<$", T/binary>>, F, S) -> string(T, <<>>, F, S);
do_decode(<<$-, T/binary>>, F, S) -> number(T, pre, int, [$-], F, S);
do_decode(<<$/>>, false, _) -> [<<>>];
do_decode(<<$/, T/binary>>, false, _) -> pointer(T, []);
do_decode(B = <<$0, _/binary>>, F, S) -> number(B, pre, int, [], F, S);
do_decode(B = <<$1, _/binary>>, F, S) -> number(B, pre, int, [], F, S);
do_decode(B = <<$2, _/binary>>, F, S) -> number(B, pre, int, [], F, S);
do_decode(B = <<$3, _/binary>>, F, S) -> number(B, pre, int, [], F, S);
do_decode(B = <<$4, _/binary>>, F, S) -> number(B, pre, int, [], F, S);
do_decode(B = <<$5, _/binary>>, F, S) -> number(B, pre, int, [], F, S);
do_decode(B = <<$6, _/binary>>, F, S) -> number(B, pre, int, [], F, S);
do_decode(B = <<$7, _/binary>>, F, S) -> number(B, pre, int, [], F, S);
do_decode(B = <<$8, _/binary>>, F, S) -> number(B, pre, int, [], F, S);
do_decode(B = <<$9, _/binary>>, F, S) -> number(B, pre, int, [], F, S).

base("", T, V, F, S) -> pop(V, T, F, S);
base(Check, <<>>, V, true, S) -> {more, {base, Check, V, S}};
base([H | Check], <<H, T/binary>>, V, F, S) -> base(Check, T, V, F, S).

object(<<>>, State, Acc, true, S) -> {more, {object, State, Acc, S}};
object(<<?HT, T/binary>>, State, Acc, F, S) -> object(T, State, Acc, F, S);
object(<<?LF, T/binary>>, State, Acc, F, S) -> object(T, State, Acc, F, S);
object(<<?CR, T/binary>>, State, Acc, F, S) -> object(T, State, Acc, F, S);
object(<<?SPC, T/binary>>, State, Acc,F, S) -> object(T, State, Acc, F, S);
object(<<$}, T/binary>>, {true,_}, Acc, F, S) -> pop(maps:from_list(Acc),T,F,S);
object(<<$,, T/binary>>,{true,comma},Acc,F,S) -> object(T,{false,name},Acc,F,S);
object(<<$", T/binary>>, {_, name},Acc,F,S) -> string(T,<<>>,F,[{name, Acc}|S]);
object(<<$:, T/binary>>, {false, colon}, Acc, F, S) ->
    do_decode(T, F, [{value,Acc}|S]).

array(<<>>, State, Acc, true, S) -> {more, {array, State, Acc, S}};
array(<<?HT, T/binary>>, State, Acc, F, S) -> array(T, State, Acc, F, S);
array(<<?LF, T/binary>>, State, Acc, F, S) -> array(T, State, Acc, F, S);
array(<<?CR, T/binary>>, State, Acc, F, S) -> array(T, State, Acc, F, S);
array(<<?SPC, T/binary>>, State, Acc, F, S) -> array(T, State, Acc, F, S);
array(<<$,, T/binary>>, {false, true},Acc,F,S) -> array(T,{true,false},Acc,F,S);
array(<<$], T/binary>>, {false, _}, Acc, F,S) -> pop(lists:reverse(Acc), T,F,S);
array(T, {_, false}, Acc, F, S) -> do_decode(T, F, [{array, Acc} | S]).

string(<<>>, Acc, true, S) -> {more, {string, Acc, S}};
string(<<$\\, T/binary>>, Acc, F, S) -> unescape(T, Acc, F, S);
string(<<$", T/binary>>, Acc, F, S) -> pop(Acc, T, F, S);
string(<<H/utf8, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary,H/utf8>>,F,S).

unescape(<<>>, Acc, true, S) -> {more, {unescape, Acc, S}};
unescape(<<$", T/binary>>, Acc, F, S) -> string(T, <<Acc/binary, $">>, F, S);
unescape(<<$\\, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary,$\\ >>, F, S);
unescape(<<$/, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary, $/>>, F, S);
unescape(<<$0, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary, ?NULL>>, F, S);
unescape(<<$a, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary, ?BEL>>, F, S);
unescape(<<$b, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary, ?BS>>, F, S);
unescape(<<$t, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary, ?HT>>, F, S);
unescape(<<$n, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary, ?LF>>, F, S);
unescape(<<$f, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary, ?FF>>, F, S);
unescape(<<$v, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary, ?VT>>, F, S);
unescape(<<$r, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary, ?CR>>, F, S);
unescape(<<$s, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary, ?SPC>>, F, S);
unescape(<<$u, T/binary>>, Acc, F, S) -> unescape_hex(T, [], Acc, F, S);
unescape(<<H, T/binary>>, Acc, F, S) -> string(T, <<Acc/binary,$\\, H>>, F, S).

unescape_hex(<<>>, Hex, Acc, true, S) -> {more, {unescape_hex, Hex, Acc, S}};
unescape_hex(<<D, T/binary>>, [C, B, A], Acc, F, S) ->
    string(T, <<Acc/binary,(list_to_integer([A, B, C, D], 16))/utf8>>, F, S);
unescape_hex(<<X, T/binary>>, Hex, Acc, F, S) ->
    unescape_hex(T, [X | Hex], Acc, F, S).

number(<<$0, T/binary>>, pre, int, Acc, F, S) ->
    number(T, zero, int, [$0|Acc], F, S);
number(<<H, T/binary>>, pre, exp, Acc, F, S) when ?IS_SIGN(H) ->
    number(T, sign, exp, [H | Acc], F, S);
number(<<H, T/binary>>, pre, exp, Acc, F, S) when ?IS_INT(H) ->
    number(T, post, exp, [H | Acc], F, S);
number(<<H, T/binary>>, pre, float, Acc, F, S) when ?IS_INT(H) ->
    number(T, post, float, [H | Acc], F, S);
number(<<H, T/binary>>, pre, Phase, Acc, F, S) when ?IS_POS_INT(H) ->
    number(T, post, Phase, [H | Acc], F, S);
number(<<H, T/binary>>, sign, Phase, Acc, F, S) when ?IS_INT(H) ->
    number(T, post, Phase, [H | Acc], F, S);
number(<<H, T/binary>>, post, Phase, Acc, F, S) when ?IS_INT(H) ->
    number(T, post, Phase, [H | Acc], F, S);
number(<<$., T/binary>>, Stage, int, Acc,F, S) when ?ZERO_OR_POST(Stage) ->
    number(T, pre, float, [$. | Acc], F, S);
number(<<E,T/binary>>,Stage, int, Acc, F, S) when ?EXP_ZERO_OR_POST(E, Stage) ->
    number(T, pre, exp, [E, $0, $. | Acc], F, S);
number(<<E, T/binary>>, post, float, Acc, F, S)  when ?IS_EXP(E) ->
    number(T, pre, exp, [E | Acc], F, S);
number(<<>>, State, Phase, Acc, true, S) ->
    {more, {number, State, Phase, Acc, S}};
number(B, Stage, int, Acc, F, S) when ?ZERO_OR_POST(Stage) ->
    pop(list_to_integer(lists:reverse(Acc)), B, F, S);
number(B, post, _, Acc, F, S) ->
    pop(list_to_float(lists:reverse(Acc)), B, F, S).

pointer(<<>>, Acc) -> lists:reverse(Acc);
pointer(<<$->>, Acc) -> lists:reverse(['-'| Acc]);
pointer(<<$-, $/, T/binary>>, Acc) -> pointer(T, ['-' | Acc]);
pointer(<<$0, T/binary>>, Acc) -> pointer_int(T, Acc, [$0]);
pointer(<<$1, T/binary>>, Acc) -> pointer_int(T, Acc, [$1]);
pointer(<<$2, T/binary>>, Acc) -> pointer_int(T, Acc, [$2]);
pointer(<<$3, T/binary>>, Acc) -> pointer_int(T, Acc, [$3]);
pointer(<<$4, T/binary>>, Acc) -> pointer_int(T, Acc, [$4]);
pointer(<<$5, T/binary>>, Acc) -> pointer_int(T, Acc, [$5]);
pointer(<<$6, T/binary>>, Acc) -> pointer_int(T, Acc, [$6]);
pointer(<<$7, T/binary>>, Acc) -> pointer_int(T, Acc, [$7]);
pointer(<<$8, T/binary>>, Acc) -> pointer_int(T, Acc, [$8]);
pointer(<<$9, T/binary>>, Acc) -> pointer_int(T, Acc, [$9]);
pointer(<<H/utf8, T/binary>>, Acc) -> pointer_member(T, Acc, <<H/utf8>>).

pointer_int(<<>>, Pointer, Acc) ->
    lists:reverse([list_to_integer(lists:reverse(Acc)) | Pointer]);
pointer_int(<<$/, T/binary>>, Pointer, Acc) ->
    pointer(T, [list_to_integer(Acc) | Pointer]);
pointer_int(<<H, T/binary>>, Pointer, Acc) ->
    pointer_int(T, Pointer, [H | Acc]).

pointer_member(<<>>, Pointer, Acc) -> lists:reverse([Acc | Pointer]);
pointer_member(<<$/, T/binary>>, Pointer, Acc) -> pointer(T, [Acc| Pointer]);
pointer_member(<<$~, $0, T/binary>>, Pointer, Acc) ->
    pointer_member(T, Pointer, <<Acc/binary, $~>>);
pointer_member(<<$~, $1, T/binary>>, Pointer, Acc) ->
    pointer_member(T, Pointer, <<Acc/binary, $/>>);
pointer_member(<<H/utf8, T/binary>>, Pointer, Acc) ->
    pointer_member(T, Pointer, <<Acc/binary, H/utf8>>).

pop(V, B, _, []) -> {V, B};
pop(V, B, F, [{array, Acc} | S]) -> array(B, {false, true}, [V | Acc], F, S);
pop(N, B, F, [{name, Acc} | S]) -> object(B, {false, colon}, {N, Acc}, F, S);
pop(V, B, F, [{value, {N, Acc}} | S]) ->
    object(B, {true,comma},[{N, V} | Acc], F, S).

%% ===================================================================
%% Pointer Evaluation
%% ===================================================================

eval_binary([], B, false, _) -> decode(B);
eval_binary([], B, true, Path) ->
    case decode(B, [stream]) of
        {more, Cont} -> {more, {decode, Path, Cont}};
        {JSON, T} -> unwind(Path, T, JSON)
    end;
eval_binary(P, <<>>, Stream, Path) ->
    {more, {eval_binary, P, Stream, Path}};
eval_binary(P, <<W, T/binary>>, Stream, Path) when ?WS(W) ->
    eval_binary(P, T, Stream, Path);
eval_binary(['-' | _], <<$[, T/binary>>, Stream, Path) ->
    eval_dash(T, {false, false}, 0, Stream, Path);
eval_binary(['-' | _], _, _, Path) ->
    {error, incorrect(['-' | Path])};
eval_binary([N | R], <<$[, T/binary>>, Stream, Path) when is_integer(N) ->
    eval_array(N, T, {false, false}, Stream, [N | Path], R);
eval_binary([Key | P], B = <<${, _/binary>>, Stream, Path) when is_atom(Key) ->
    eval_binary([atom_to_binary(Key, utf8) | P], B, Stream, Path);
eval_binary([Key | P], <<${, T/binary>>, Stream,Path) when is_binary(Key) ->
    eval_object(Key, T, {false, false}, Stream, [Key | Path], P);
eval_binary([E | _], _, _, Path) ->
    {error, incorrect([E | Path])}.

eval_dash(<<>>, Expect, Size, Stream, Path) ->
    {more, {eval_dash, Expect, Size, Stream, Path}};
eval_dash(<<W, T/binary>>, Expect, Size, Stream, Path) when ?WS(W) ->
    eval_dash(T, Expect, Size, Stream, Path);
eval_dash(<<$,, T/binary>>, {false, true}, Size, Stream, Path) ->
    eval_dash(T, {true, false}, Size, Stream, Path);
eval_dash(<<$], _/binary>>, {false, _}, Size, _, Path) ->
    {error, too_large([Size | Path])};
eval_dash(T, {_, false}, Size, Stream, Path) ->
    skip_value(T, {eval_dash, {false, true}, Size + 1, Stream, Path}).

eval_array(0, T, _, Stream, Path, Rest) -> eval_binary(Rest, T, Stream, Path);
eval_array(N, <<>>, Expect, Stream, Path, Rest) ->
    {more, {eval_array, N, Expect, Stream, Path, Rest}};
eval_array(N, <<W, T/binary>>, Expect, Stream, Path, Rest) when ?WS(W) ->
    eval_array(N, T, Expect, Stream, Path, Rest);
eval_array(N, <<$,, T/binary>>, {false, true}, Stream, Path, Rest) ->
    eval_array(N - 1, T, {true, false}, Stream, Path, Rest);
eval_array(_, <<$], _/binary>>, {false, _}, _, Path, _) ->
    {error, too_large(Path)};
eval_array(N, T, {_, false}, Stream, Path, Rest) ->
    skip_value(T, {eval_array, N, {false, true}, Stream, Path, Rest}).

eval_object(Key, <<>>, Expect, Stream, Path, Rest) ->
    {more, {eval_object, Key, Expect, Stream, Path, Rest}};
eval_object(Key, <<W, T/binary>>, Expect, Stream, Path, Rest) when ?WS(W)->
    eval_object(Key, T, Expect, Stream, Path, Rest);
eval_object(_, <<$}, _/binary>>, {false, _}, _, Path, _) ->
    {error, non_member(Path)};
eval_object(Key, <<$,, T/binary>>, {false, true}, Stream, Path, Rest) ->
    eval_object(Key, T, {true, false}, Stream, Path, Rest);
eval_object(Key, <<$", T/binary>>, {_, false}, Stream, Path, Rest) ->
    eval_key(T, Key, Stream, Path, Rest).

eval_key(<<>>, Key, Stream,Path,Rest) -> {more,{eval_key,Key,Stream,Path,Rest}};
eval_key(<<$", T/binary>>, <<>>, Stream, Path, Rest) ->
    skip_colon(T, false, {eval_binary, Rest, Stream, Path});
eval_key(<<H/utf8, T/binary>>, <<H/utf8, Key/binary>>, Stream, Path, Rest) ->
    eval_key(T, Key, Stream, Path, Rest);
eval_key(T, _, Stream, Path = [Key | _], Rest) ->
    skip_colon(T,
               false,
               {skip_value, {eval_object, Key, {false,true},Stream,Path,Rest}}).

skip_value(<<>>, C) -> {more, {skip_value, C}};
skip_value(<<W, T/binary>>, C) when ?WS(W) -> skip_value(T, C);
skip_value(<<$t, T/binary>>, C) -> skip_base("rue", T, C);
skip_value(<<$f, T/binary>>, C) -> skip_base("alse", T, C);
skip_value(<<$n, T/binary>>, C) -> skip_base("ull", T, C);
skip_value(<<${, T/binary>>, C) -> skip_object(T, {false, false}, C);
skip_value(<<$[, T/binary>>, C) -> skip_array(T, {false, false}, C);
skip_value(<<$", T/binary>>, C) -> skip_string(T, C);
skip_value(<<$-, T/binary>>, C) -> skip_number(T, C);
skip_value(<<H, T/binary>>, C) when ?IS_INT(H) -> skip_number(T, C).

skip_base(Base, <<>>, C) -> {more, {skip_base, Base, C}};
skip_base("", T, C) -> skip_cont(T, C);
skip_base([H | T], <<H, T1/binary>>, C) -> skip_base(T, T1, C).

skip_array(<<>>, Expect, C) -> {more, {skip_array, Expect, C}};
skip_array(<<W, T/binary>>, Expect, C) when ?WS(W) -> skip_array(T,Expect,C);
skip_array(<<$,, T/binary>>, {false, true}, C) -> skip_array(T, {true,false},C);
skip_array(<<$], T/binary>>, {false, _}, C) ->
    skip_cont(T, C);
skip_array(T, {_, false}, C) ->
    skip_value(T, {skip_array, {false, true}, C}).

skip_object(<<>>, Expect, C) -> {more, {skip_object, Expect, C}};
skip_object(<<W, T/binary>>, Expect, C) when ?WS(W) -> skip_object(T, Expect,C);
skip_object(<<$}, T/binary>>, {false, _}, C) -> skip_cont(T, C);
skip_object(<<$,, T/binary>>, {false, true},C) -> skip_object(T,{true,false},C);
skip_object(<<$", T/binary>>, {_, false}, C) ->
    skip_name(T, {skip_object, {false, true}, C}).

skip_name(<<>>, C) -> {more, {skip_name, C}};
skip_name(<<$", T/binary>>, C) -> skip_colon(T, false, {skip_value, C});
skip_name(<<_/utf8, T/binary>>, C) -> skip_name(T, C).

skip_colon(<<>>, Flag, C) -> {more, {skip_colon, Flag, C}};
skip_colon(<<W, T/binary>>, Flag, C) when ?WS(W) -> skip_colon(T, Flag, C);
skip_colon(T, true, {eval_binary, Rest, S, Path}) -> eval_binary(Rest,T,S,Path);
skip_colon(T, true, {skip_value, C}) -> skip_value(T, C);
skip_colon(<<$:, T/binary>>, false, C) -> skip_colon(T, true, C);
skip_colon(<<_/utf8, T/binary>>, false, C) -> skip_colon(T, false, C).

skip_string(<<>>, C) -> {more, {skip_string, C}};
skip_string(<<$", T/binary>>, C) -> skip_cont(T, C);
skip_string(<<_/utf8, T/binary>>, C) ->  skip_string(T, C).

skip_number(<<>>, C) -> {more, {skip_number, C}};
skip_number(<<$0, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$1, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$2, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$3, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$4, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$5, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$6, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$7, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$8, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$9, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$e, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$E, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$-, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$+, T/binary>>, C) -> skip_number(T, C);
skip_number(<<$., T/binary>>, C) -> skip_number(T, C);
skip_number(T = <<_/utf8, _/binary>>, C) -> skip_cont(T, C).

skip_cont(T, {eval_binary, Rest, Stream, Path}) ->
    eval_binary(Rest, T, Stream, Path);
skip_cont(T, {eval_dash, Expect, Size, Stream, Path}) ->
    eval_dash(T, Expect,Size, Stream, Path);
skip_cont(T, {eval_array, N, Expect, Stream, Path, Rest}) ->
    eval_array(N, T, Expect, Stream, Path, Rest);
skip_cont(T, {eval_object, Key, Expect, Stream, Path, Rest}) ->
    eval_object(Key, T, Expect, Stream, Path, Rest);
skip_cont(T, {skip_object, Expect, C}) ->
    skip_object(T, Expect, C);
skip_cont(T, {skip_array, Expect, C}) ->
    skip_array(T, Expect, C);
skip_cont(T, {skip_value, C}) ->
    skip_value(T, C).

unwind([], T, JSON) -> {JSON, T};
unwind(P, <<>>, JSON) -> {more, {unwind, P, JSON}};
unwind([N | P], T, JSON) when is_integer(N) ->
    unwind_array(T, {unwind, P, JSON});
unwind([_ | P], T, JSON) ->
    unwind_object(T, {unwind, P, JSON}).

unwind_array(<<>>, C) -> {more, {unwind_array, C}};
unwind_array(<<W, T/binary>>, C) when ?WS(W) -> unwind_array(T, C);
unwind_array(<<$,, T/binary>>, C) -> unwind_value(T, {unwind_array, C});
unwind_array(<<$], T/binary>>, C) -> unwind_cont(T, C);
unwind_array(T, C) -> unwind_value(T, {unwind_array, C}).

unwind_object(<<>>, C) -> {more, {unwind_object, C}};
unwind_object(<<W, T/binary>>, C) when ?WS(W) -> unwind_object(T, C);
unwind_object(<<$,, T/binary>>, C) -> unwind_object(T, C);
unwind_object(<<$", T/binary>>, C) -> unwind_name(T, C);
unwind_object(<<$}, T/binary>>, C) -> unwind_cont(T, C).

unwind_name(<<>>, C) -> {more, {unwind_name, C}};
unwind_name(<<W, T/binary>>, C) when ?WS(W) -> unwind_name(T, C);
unwind_name(<<$", T/binary>>, C) -> unwind_colon(T, C);
unwind_name(<<_/utf8, T/binary>>, C) -> unwind_name(T, C).

unwind_colon(<<>>, C) -> {more, {unwind_colon, C}};
unwind_colon(<<W, T/binary>>, C) when ?WS(W) -> unwind_colon(T, C);
unwind_colon(<<$:, T/binary>>,C) -> unwind_value(T, {unwind_object, C}).

unwind_string(<<>>, C) -> {more, {unwind_string, C}};
unwind_string(<<W, T/binary>>, C) when ?WS(W) -> unwind_string(T, C);
unwind_string(<<$", T/binary>>,C) -> unwind_cont(T, C);
unwind_string(<<_/utf8, T/binary>>, C) -> unwind_string(T, C).

unwind_value(<<>>, C) -> {more, {unwind_value, C}};
unwind_value(<<W, T/binary>>, C) when ?WS(W) -> unwind_value(T, C);
unwind_value(<<$t, T/binary>>, C) -> unwind_base("rue", T, C);
unwind_value(<<$f, T/binary>>, C) -> unwind_base("alse", T, C);
unwind_value(<<$n, T/binary>>, C) -> unwind_base("ull", T, C);
unwind_value(<<${, T/binary>>, C) -> unwind_object(T, C);
unwind_value(<<$[, T/binary>>, C) -> unwind_array(T, C);
unwind_value(<<$", T/binary>>, C) -> unwind_string(T, C);
unwind_value(<<$-, T/binary>>, C) -> unwind_number(T, C);
unwind_value(<<H, T/binary>>, C) when ?IS_INT(H) -> unwind_number(T, C).

unwind_number(<<>>, C) -> {more, {unwind_number, C}};
unwind_number(<<$0, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$1, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$2, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$3, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$4, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$5, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$6, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$7, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$8, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$9, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$e, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$E, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$-, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$+, T/binary>>, C) -> unwind_number(T, C);
unwind_number(<<$., T/binary>>, C) -> unwind_number(T, C);
unwind_number(T = <<_/utf8, _/binary>>, C) -> unwind_cont(T, C).

unwind_base(Expect, <<>>, C) -> {more, {unwind_base, Expect, C}};
unwind_base([], T, C) -> unwind_cont(T, C);
unwind_base([H | B], <<H, T/binary>>, C) -> unwind_base(B, T, C).

unwind_cont(T, {unwind, Path, JSON}) -> unwind(Path, T, JSON);
unwind_cont(T, {unwind_array, C}) -> unwind_array(T, C);
unwind_cont(T, {unwind_object, C}) -> unwind_object(T, C).

eval_json([], JSON, _) ->  JSON;
eval_json(['-' | _], JSON, Path) when is_list(JSON) ->
    {error, too_large([length(JSON) | Path])};
eval_json(['-' | _], _, Path) ->
    {error, incorrect(['-' | Path])};
eval_json([N | T], JSON, Path) when is_integer(N), length(JSON) > N ->
    eval_json(T, lists:nth(N + 1, JSON), [N | Path]);
eval_json([N | _], _, Path) when is_integer(N) ->
    {error, too_large([N | Path])};
eval_json([Key | T], JSON, Path) ->
    case maps:find(Key, JSON) of
        {ok, Value} -> eval_json(T, Value, [Key | Path]);
        _ -> {error, non_member([Key | Path])}
    end;
eval_json(X, _, Path) ->
    {error, incorrect([X | Path])}.

incorrect(Path) ->
    {incorrect_pointer, encode(lists:reverse(Path), [pointer, binary])}.

too_large(Path) ->
    {too_large_index, encode(lists:reverse(Path), [pointer, binary])}.

non_member(Path) ->
    {non_member, encode(lists:reverse(Path), [pointer, binary])}.

%% ===================================================================
%% Next
%% ===================================================================

do_next(T) ->  next_value(T, <<>>, {next_end}).

next_value(<<>>, Acc, C) -> {more, {next_value, Acc, C}};
next_value(<<W, T/binary>>, Acc, C) when ?WS(W) -> next_value(T, Acc, C);
next_value(<<$t, T/binary>>, Acc, C) ->
    next_base("rue", T, <<Acc/binary, "true">>, C);
next_value(<<$f, T/binary>>, Acc, C) ->
    next_base("alse", T, <<Acc/binary, "false">>, C);
next_value(<<$n, T/binary>>, Acc, C) ->
    next_base("ull", T, <<Acc/binary, "null">>, C);
next_value(<<${, T/binary>>, Acc, C) ->
    next_object(T, {false, false}, <<Acc/binary, ${>>, C);
next_value(<<$[, T/binary>>, Acc, C) ->
    next_array(T, {false, false}, <<Acc/binary, $[>>, C);
next_value(<<$", T/binary>>, Acc, C) ->
    next_string(T, <<Acc/binary, $">>, C);
next_value(<<$-, T/binary>>, Acc, C) ->
    next_number(T, <<Acc/binary, $->>, C);
next_value(<<H, T/binary>>, Acc, C) when ?IS_INT(H) ->
    next_number(T, <<Acc/binary, H>>, C).

next_base("", T, Acc, C) -> next_cont(T, Acc, C);
next_base(Base, <<>>, Acc, C) -> {more, {next_base, Base, Acc, C}};
next_base([H | T], <<H, T1/binary>>, Acc, C) -> next_base(T, T1, Acc, C).

next_array(<<>>, Expect, Acc, C) -> {more, {next_array, Expect, Acc, C}};
next_array(<<W, T/binary>>, Expect, Acc, C) when ?WS(W) ->
    next_array(T, Expect, Acc, C);
next_array(<<$,, T/binary>>, {false, true}, Acc, C) ->
    next_array(T, {true, false}, <<Acc/binary, $,>>, C);
next_array(<<$], T/binary>>, {false, _}, Acc, C) ->
    next_cont(T, <<Acc/binary, $]>>, C);
next_array(T, {_, false}, Acc, C) ->
    next_value(T, Acc, {next_array, {false, true}, C}).

next_object(<<>>, Expect, Acc, C) -> {more, {next_object, Expect, Acc, C}};
next_object(<<W, T/binary>>, Expect, Acc, C) when ?WS(W) ->
    next_object(T, Expect, Acc, C);
next_object(<<$}, T/binary>>, {false, _}, Acc, C) ->
    next_cont(T, <<Acc/binary, $}>>, C);
next_object(<<$,, T/binary>>, {false, true}, Acc, C) ->
    next_object(T, {true,false}, <<Acc/binary, $,>>, C);
next_object(<<$", T/binary>>, {_, false}, Acc, C) ->
    next_name(T, <<Acc/binary, $">>, {next_object, {false, true}, C}).

next_name(<<>>, Acc, C) -> {more, {next_name, Acc, C}};
next_name(<<$", T/binary>>, Acc, C) ->
    next_colon(T, false, <<Acc/binary, $">>, C);
next_name(<<H/utf8, T/binary>>, Acc, C) ->
    next_name(T, <<Acc/binary, H/utf8>>, C).

next_colon(<<>>, Flag, Acc, C) -> {more, {next_colon, Flag, Acc, C}};
next_colon(<<W, T/binary>>, Flag, Acc, C) when ?WS(W) ->
    next_colon(T, Flag, Acc, C);
next_colon(<<$:, T/binary>>, false, Acc, C) ->
    next_colon(T, true, <<Acc/binary, $:>>, C);
next_colon(T = <<_/utf8, _/binary>>, true, Acc, C) ->
    next_value(T, Acc, C).

next_string(<<>>, Acc, C) -> {more, {next_string, Acc, C}};
next_string(<<$", T/binary>>, Acc, C) -> next_cont(T, <<Acc/binary, $">>, C);
next_string(<<$\\, T/binary>>, Acc, C) ->
    next_unescape(T, <<Acc/binary, $\\>>, C);
next_string(<<H/utf8, T/binary>>, Acc, C) ->
    next_string(T, <<Acc/binary, H/utf8>>, C).

next_unescape(<<>>, Acc, C) -> {more, {next_unescape, Acc, C}};
next_unescape(<<$", T/binary>>, Acc, C) -> next_string(T, <<Acc/binary, $">>,C);
next_unescape(<<$\\, T/binary>>, Acc, C) -> next_string(T,<<Acc/binary,$\\>>,C);
next_unescape(<<$/, T/binary>>, Acc, C) -> next_string(T, <<Acc/binary, $/>>,C);
next_unescape(<<$0, T/binary>>, Acc,C) -> next_string(T,<<Acc/binary, $0>>, C);
next_unescape(<<$a, T/binary>>, Acc, C) -> next_string(T,<<Acc/binary, $a>>, C);
next_unescape(<<$b, T/binary>>, Acc, C) -> next_string(T, <<Acc/binary, $b>>,C);
next_unescape(<<$t, T/binary>>, Acc, C) -> next_string(T, <<Acc/binary, $t>>,C);
next_unescape(<<$n, T/binary>>, Acc, C) -> next_string(T, <<Acc/binary, $n>>,C);
next_unescape(<<$f, T/binary>>, Acc, C) -> next_string(T, <<Acc/binary, $f>>,C);
next_unescape(<<$v, T/binary>>, Acc, C) -> next_string(T, <<Acc/binary, $v>>,C);
next_unescape(<<$r, T/binary>>, Acc, C) -> next_string(T, <<Acc/binary, $r>>,C);
next_unescape(<<$s, T/binary>>, Acc, C) -> next_string(T,<<Acc/binary, $s>>, C);
next_unescape(<<$u, T/binary>>, Acc, C) -> next_unescape_hex(T, [], Acc, C);
next_unescape(<<H, T/binary>>, Acc, C) -> next_string(T,<<Acc/binary,$\\,H>>,C).

next_unescape_hex(<<>>, Hex, Acc, C) -> {more,{next_unescape_hex, Hex, Acc, C}};
next_unescape_hex(<<D, T/binary>>, [C, B, A], Acc, Co) ->
    next_string(T, <<Acc/binary, $u, A, B, C, D>>, Co);
next_unescape_hex(<<X, T/binary>>, Hex, Acc, C) ->
    next_unescape_hex(T, [X | Hex], Acc, C).

next_number(<<>>, Acc, C) -> {more, {next_number, Acc, C}};
next_number(<<$0, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $0>>, C);
next_number(<<$1, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $1>>, C);
next_number(<<$2, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $2>>, C);
next_number(<<$3, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $3>>, C);
next_number(<<$4, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $4>>, C);
next_number(<<$5, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $5>>, C);
next_number(<<$6, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $6>>, C);
next_number(<<$7, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $7>>, C);
next_number(<<$8, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $8>>, C);
next_number(<<$9, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $9>>, C);
next_number(<<$e, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $e>> ,C);
next_number(<<$E, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $E>>, C);
next_number(<<$-, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $->>, C);
next_number(<<$+, T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $+>>, C);
next_number(<<$., T/binary>>, Acc, C) -> next_number(T, <<Acc/binary, $.>>, C);
next_number(T = <<_/utf8, _/binary>>, Acc, C) -> next_cont(T, Acc, C).

next_cont(T, Acc, {next_end}) -> {Acc, T};
next_cont(T, Acc, {next_array, Expect, C}) ->
    next_array(T, Expect, Acc, C);
next_cont(T, Acc, {next_object, Expect, C}) ->
    next_object(T, Expect, Acc, C);
next_cont(T, Acc, {next_value, C}) ->
    next_value(T, Acc, C).

%% ===================================================================
%% Patch
%% ===================================================================

apply_op(#{<<"op">> := <<"add">>, <<"path">> := P, <<"value">> := V}, JSON) ->
    add(decode(P), JSON, V);
apply_op(#{<<"op">> := <<"remove">>, <<"path">> := P}, JSON) ->
    remove(decode(P), JSON);
apply_op(#{<<"op">> := <<"replace">>, <<"path">> := P,<<"value">> := V},JSON) ->
    replace(decode(P), JSON, V);
apply_op(#{<<"op">> := <<"move">>, <<"path">> := P, <<"from">> := F}, JSON) ->
    move(decode(P), JSON, decode(F));
apply_op(#{<<"op">> := <<"copy">>, <<"path">> := P, <<"from">> := F}, JSON) ->
    copy(decode(P), JSON, decode(F));
apply_op(#{<<"op">> := <<"test">>, <<"path">> := P, <<"value">> := V}, JSON) ->
    test(decode(P), JSON, V).

add(['-'], [], V) -> [V];
add(['-'], Array = [_ | _], V) -> lists:reverse([V | lists:reverse(Array)]);
add([Key], Object = #{}, V) -> Object#{Key => V};
add([N], Array, V) when is_integer(N) -> add_array(N, Array, V);
add([Key | P], Object = #{}, V) ->
    Value = maps:get(Key, Object),
    Object#{Key => add(P, Value, V)};
add([N | P], Array, V) ->
    add_array(N, Array, V, P).

add_array(0, Array = [_ | _], V) -> [V | Array];
add_array(N, [H | T], V) -> [H | add_array(N - 1, T, V)].

add_array(0, [H | T], V, P) -> [add(P, H, V) | T];
add_array(N, [H | T], V, P) -> [H | add_array(N - 1, T, V, P)].

remove([Key], Object = #{}) ->
    _ = maps:get(Key, Object),
    maps:remove(Key, Object);
remove([N], Array) when is_integer(N) ->
    remove_array(N, Array);
remove([Key | P], Object = #{}) ->
    Value = maps:get(Key, Object),
    Object#{Key => remove(P, Value)};
remove([N | P], Array) when is_integer(N) ->
    remove_array(N, Array, P).

remove_array(0, [_ | T]) -> T;
remove_array(N, [H  | T]) -> [H | remove_array(N - 1, T)].

remove_array(0, [H | T], P) -> [remove(P, H) | T];
remove_array(N, [H | T], P) -> [H | remove_array(N - 1, T, P)].

replace([Key], Object = #{}, V) ->
    _ = maps:get(Key, Object),
    Object#{Key => V};
replace([N], Array, V) when is_integer(N) ->
    replace_array(N, Array, V);
replace([Key | P], O = #{}, V) ->
    Value = maps:get(Key, O),
    O#{Key => replace(P, Value, V)};
replace([N | P], Array, V) ->
    replace_array(N, Array, V, P).

replace_array(0, [_ | T], V) -> [V | T];
replace_array(N, [H | T], V) -> [H | replace_array(N - 1, T, V)].

replace_array(0, [H | T], V, P) -> [replace(P, H, V) | T];
replace_array(N, [H | T], V, P) -> [H | replace_array(N - 1, T, V, P)].

move(P, JSON, From) ->
    {Value, JSON1} = fetch(From, JSON),
    add(P, JSON1, Value).

fetch([Key], Object = #{}) ->
    V = maps:get(Key, Object),
    {V, maps:remove(Key, Object)};
fetch([N], Array) when is_integer(N) ->
    fetch_array(N, Array);
fetch([Key | P], Object = #{}) ->
    Value = maps:get(Key, Object),
    Object#{Key => fetch(P, Value)};
fetch([N | P], Array) when is_integer(N) ->
    fetch_array(N, Array, P).

fetch_array(0, [H | T]) -> [H | T];
fetch_array(N, [H  | T]) -> [H | fetch_array(N - 1, T)].

fetch_array(0, [H | T], P) -> [fetch(P, H) | T];
fetch_array(N, [H | T], P) -> [H | fetch_array(N - 1, T, P)].

copy(P, JSON, From) -> add(P, JSON, eval_json(From, JSON, [])).

test([Key], Object = #{}, Value) ->
    case maps:get(Key, Object, undefined) of
        Value -> true;
        _ -> false
    end;
test([N], A, V) when is_integer(N), length(A) > N ->
    V == lists:nth(N, A);
test([_], _, _) ->
    false;
test([Key | P], Object = #{}, Value) ->
    case maps:get(Key, Object) of
        undefined -> false;
        T -> test(P, T, Value)
    end;
test([N | P], A, V) when is_integer(N), length(A) > N ->
    test(P,lists:nth(N, A),V);
test(_, _, _) ->
    false.

%% ===================================================================
%% Merge
%% ===================================================================

merge_object(Key, null, Object) -> maps:remove(Key, Object);
merge_object(K, PV, O = #{}) -> O#{K => merge(PV, maps:get(K, O))}.

%% FIXME
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
