%%==============================================================================
%% Copyright 2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   Misc math functionality.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_math).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Compiler directives
-compile({no_auto_import, [float_to_binary/1]}).

%% Library functions.
-export([rotl32/2, rotr32/2, float_to_binary/1]).

%% Defines.

%% rotx32
-define(MAX32, 16#FFFFFFFF)

%% float_to_binary/1.
-define(BIG_POW, (1 bsl 52)).
-define(MIN_EXP, (-1074)).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: rotl32(Integer, Steps) -> RotatedInteger
%% @doc
%%   Performs a Steps many left rotation on a 32 bit integer.
%% @end
%%--------------------------------------------------------------------
-spec rotl32(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
%%--------------------------------------------------------------------
rotl32(I, Steps) when I >= 0, I =< ?MAX32 -> do_rotl32(I, Steps rem 32).

%%--------------------------------------------------------------------
%% Function: rotr32(Integer, Steps) -> RotatedInteger
%% @doc
%%   Performs a Steps many right rotation on a 32 bit integer.
%% @end
%%--------------------------------------------------------------------
-spec rotr32(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
%%--------------------------------------------------------------------
rotr32(I, Steps) when I >= 0, I =< ?MAX32 -> do_rotl32(I, 32 - (Steps rem 32)).

%%--------------------------------------------------------------------
%% Function: float_to_binary(Float) -> String
%% @doc
%%   Provides a minimal binary string representation of a float.
%% @end
%%--------------------------------------------------------------------
-spec float_to_binary(float()) -> binary().
%%--------------------------------------------------------------------
float_to_binary(Float) when Float == 0.0 -> <<"0.0">>;
float_to_binary(Float) when is_float(Float) ->
    {Sign, Frac, Exp} = mantissa_exponent(Float),
    {Place, Digits} = float_to_digits(Float, Exp, Frac, (Frac band 1) =:= 0),
    insert_decimal(Place, << <<($0 + D)>> || <<D>> <= Digits>>, Sign).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% --------------------------------------------------------------------
%% rotl3232
%% --------------------------------------------------------------------
do_rotl32(I, 0) -> I;
do_rotl32(I, 1) -> ((I band 16#7FFFFFFF) bsl 1) bor (I bsr 31);
do_rotl32(I, 2) -> ((I band 16#3FFFFFFF) bsl 2) bor (I bsr 30);
do_rotl32(I, 3) -> ((I band 16#1FFFFFFF) bsl 3) bor (I bsr 29);
do_rotl32(I, 4) -> ((I band 16#FFFFFFF) bsl 4) bor (I bsr 28);
do_rotl32(I, 5) -> ((I band 16#7FFFFFF) bsl 5) bor (I bsr 27);
do_rotl32(I, 6) -> ((I band 16#3FFFFFF) bsl 6) bor (I bsr 26);
do_rotl32(I, 7) -> ((I band 16#1FFFFFF) bsl 7) bor (I bsr 25);
do_rotl32(I, 8) -> ((I band 16#FFFFFF) bsl 8) bor (I bsr 24);
do_rotl32(I, 9) -> ((I band 16#7FFFFF) bsl 9) bor (I bsr 23);
do_rotl32(I, 10) -> ((I band 16#3FFFFF) bsl 10) bor (I bsr 22);
do_rotl32(I, 11) -> ((I band 16#1FFFFF) bsl 11) bor (I bsr 21);
do_rotl32(I, 12) -> ((I band 16#FFFFF) bsl 12) bor (I bsr 20);
do_rotl32(I, 13) -> ((I band 16#7FFFF) bsl 13) bor (I bsr 19);
do_rotl32(I, 14) -> ((I band 16#3FFFF) bsl 14) bor (I bsr 18);
do_rotl32(I, 15) -> ((I band 16#1FFFF) bsl 15) bor (I bsr 17);
do_rotl32(I, 16) -> ((I band 16#FFFF) bsl 16) bor (I bsr 16);
do_rotl32(I, 17) -> ((I band 16#7FFF) bsl 17) bor (I bsr 15);
do_rotl32(I, 18) -> ((I band 16#3FFF) bsl 18) bor (I bsr 14);
do_rotl32(I, 19) -> ((I band 16#1FFF) bsl 19) bor (I bsr 13);
do_rotl32(I, 20) -> ((I band 16#FFF) bsl 20) bor (I bsr 12);
do_rotl32(I, 21) -> ((I band 16#7FF) bsl 21) bor (I bsr 11);
do_rotl32(I, 22) -> ((I band 16#3FF) bsl 22) bor (I bsr 10);
do_rotl32(I, 23) -> ((I band 16#1FF) bsl 23) bor (I bsr 9);
do_rotl32(I, 24) -> ((I band 16#FF) bsl 24) bor (I bsr 8);
do_rotl32(I, 25) -> ((I band 16#7F) bsl 25) bor (I bsr 7);
do_rotl32(I, 26) -> ((I band 16#3F) bsl 26) bor (I bsr 6);
do_rotl32(I, 27) -> ((I band 16#1F) bsl 27) bor (I bsr 5);
do_rotl32(I, 28) -> ((I band 16#F) bsl 28) bor (I bsr 4);
do_rotl32(I, 29) -> ((I band 16#7) bsl 29) bor (I bsr 3);
do_rotl32(I, 30) -> ((I band 16#3) bsl 30) bor (I bsr 2);
do_rotl32(I, 31) -> ((I band 16#1) bsl 31) bor (I bsr 1).

%% --------------------------------------------------------------------
%% float_to_binary
%% --------------------------------------------------------------------

%% ===================================================================
%% float_to_binary/1 the implementation based on
%% "Printing Floating-Point Numbers Quickly and Accurately"
%%  by R.,G. Burger and R.,K. Dybvig in Proceedings of the SIGPLAN '96
%%  Conference on Programming Language Design and Implementation.
%% ===================================================================

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
