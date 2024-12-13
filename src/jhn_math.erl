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

%% Compiler options
-compile({inline, [{min, 3}]}).

%% Library functions.
-export([rotl32/2, rotr32/2,
         levenshtein/2,
         luhn/2, verhoeff/2, damm/2
        ]).

%% Defines.

%% rotx32
-define(MAX32, 16#FFFFFFFF).

%% verhoeff
-define(VERHOEFF_D,
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
         1, 2, 3, 4, 0, 6, 7, 8, 9, 5,
         2, 3, 4, 0, 1, 7, 8, 9, 5, 6,
         3, 4, 0, 1, 2, 8, 9, 5, 6, 7,
         4, 0, 1, 2, 3, 9, 5, 6, 7, 8,
         5, 9, 8, 7, 6, 0, 4, 3, 2, 1,
         6, 5, 9, 8, 7, 1, 0, 4, 3, 2,
         7, 6, 5, 9, 8, 2, 1, 0, 4, 3,
         8, 7, 6, 5, 9, 3, 2, 1, 0, 4,
         9, 8, 7, 6, 5, 4, 3, 2, 1, 0}).

-define(VERHOEFF_P,
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
         1, 5, 7, 6, 2, 8, 3, 0, 9, 4,
         5, 8, 0, 3, 7, 9, 6, 1, 4, 2,
         8, 9, 1, 6, 0, 4, 3, 5, 2, 7,
         9, 4, 5, 3, 1, 2, 6, 8, 7, 0,
         4, 2, 8, 6, 5, 7, 3, 9, 0, 1,
         2, 7, 9, 3, 8, 0, 6, 4, 1, 5,
         7, 0, 4, 6, 9, 1, 3, 2, 5, 8}).

-define(VERHOEFF_INV, {0, 4, 3, 2, 1, 5, 6, 7, 8, 9}).

%% damm
-define(DAMM,
        {0, 3, 1, 7, 5, 9, 8, 6, 4, 2,
         7, 0, 9, 2, 1, 5, 4, 8, 6, 3,
         4, 2, 0, 6, 8, 7, 1, 3, 5, 9,
         1, 7, 5, 0, 9, 8, 3, 4, 2, 6,
         6, 1, 2, 3, 0, 4, 5, 9, 7, 8,
         3, 6, 7, 4, 2, 0, 9, 5, 8, 1,
         5, 8, 6, 9, 7, 2, 0, 1, 3, 4,
         8, 9, 4, 5, 3, 6, 2, 0, 1, 7,
         9, 4, 3, 8, 6, 1, 7, 2, 0, 5,
         2, 5, 8, 1, 4, 3, 6, 7, 9, 0
        }).

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
%% Function: levenshtein(String1 String2) -> Distance.
%% @doc
%%   Computes the Levenshtein distance between String1 and String2.
%% @end
%%--------------------------------------------------------------------
-spec levenshtein(string(), string()) -> integer().
%%--------------------------------------------------------------------
levenshtein([], S) -> length(S);
levenshtein(S, []) -> length(S);
levenshtein(S, T) ->
    case equal(S, T, 0, true) of
        true -> 0;
        TLen ->
            V0 = lists:seq(0, TLen),
            i(V0, 0, S, T)
    end.

%%--------------------------------------------------------------------
%% Function: luhn(check | gen, String | Binary ) -> Digit | boolean().
%% @doc
%%   Either generates a Luhn check digit or checks a number with one.
%%   The luhn check digit is assumed to last.
%% @end
%%--------------------------------------------------------------------
-spec luhn(gen | check, string() | binary() | integer()) -> integer()|boolean().
%%--------------------------------------------------------------------
luhn(gen, Number) -> luhn_sum(luhn_to_digits(Number), even, 0);
luhn(check, Number) ->
    [Check | T] = luhn_to_digits(Number),
    case luhn_sum(T, even, 0) of
        Check -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% Function: verhoeff(check | gen, String | Binary ) -> Digit | boolean().
%% @doc
%%   Either generates a Verhoeff check digit or checks a number with one.
%%   The verhoeff check digit is assumed to last.
%% @end
%%--------------------------------------------------------------------
-spec verhoeff(gen | check, string() | binary() | integer()) -> integer().
%%--------------------------------------------------------------------
verhoeff(gen, Number) -> verhoeff_sum(0, [0 | verhoeff_to_digits(Number)], 0);
verhoeff(check, Number) ->
    case verhoeff_sum(0, verhoeff_to_digits(Number), 0) of
        0 -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% Function: damm(check | gen, String | Binary ) -> Digit | boolean().
%% @doc
%%   Either generates a Damm check digit or checks a number with one.
%%   The damm check digit is assumed to last.
%% @end
%%--------------------------------------------------------------------
-spec damm(gen | check, string() | binary() | integer()) -> integer().
%%--------------------------------------------------------------------
damm(gen, Number) -> damm_traverse(damm_to_digits(Number), 0);
damm(check, Number) ->
    case damm_traverse(damm_to_digits(Number), 0) of
        0 -> true;
        _ -> false
    end.

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
%% levenshtein
%% --------------------------------------------------------------------

equal([], [], _, Equal) -> Equal;
equal([H | S], [H | T], Len, true) -> equal(S, T, Len + 1, true);
equal(_, T, Len, _) -> Len + length(T).

i(V0, _, [], _) -> lists:last(V0);
i(V0, I, [Si | S], T) ->
    V1N = [I + 1 | j(Si, T, V0, I + 1)],
    i(V1N, I + 1, S, T).

j(_, [], _, _) -> [];
j(Si, [Si | T], [V0j | V0 =  [V0j1 | _]], V1j) ->
    V1j1 = min(V1j + 1, V0j1 + 1, V0j),
    [V1j1 | j(Si, T, V0, V1j1)];
j(Si, [_ | T], [V0j | V0 =  [V0j1 | _]], V1j) ->
    V1j1 = 1 + min(V1j, V0j1, V0j),
    [V1j1 | j(Si, T, V0, V1j1)].

min(A, B, C) when A < B, A < C -> A;
min(A, B, C) when B < A, B < C -> B;
min(_, _, C) -> C.

%% --------------------------------------------------------------------
%% luhn
%% --------------------------------------------------------------------

luhn_to_digits(String) when is_list(String) ->
    [C - $0 || C <- lists:reverse(String)];
luhn_to_digits(Binary) when is_binary(Binary) ->
    [C - $0 || <<C>> <= jhn_blist:reverse(Binary)];
luhn_to_digits(Integer) ->
    luhn_to_digits(integer_to_list(Integer)).

luhn_sum([], _, Acc) -> 9 - ((Acc + 9) rem 10);
luhn_sum([H | T], even, Acc) when H >= 5 -> luhn_sum(T, odd, (H * 2) - 9 + Acc);
luhn_sum([H | T], even, Acc) -> luhn_sum(T, odd, (H * 2) + Acc);
luhn_sum([H | T], odd, Acc) -> luhn_sum(T, even, H + Acc).

%% --------------------------------------------------------------------
%% verhoeff
%% --------------------------------------------------------------------

verhoeff_to_digits(String) when is_list(String) ->
    [C - $0 || C <- lists:reverse(String)];
verhoeff_to_digits(Binary) when is_binary(Binary) ->
    [C - $0 || <<C>> <= jhn_blist:reverse(Binary)];
verhoeff_to_digits(Integer) ->
    verhoeff_to_digits(integer_to_list(Integer)).

verhoeff_sum(_, [], C) -> element(C + 1, ?VERHOEFF_INV);
verhoeff_sum(I, [N | T], C) ->
    P = element((I rem 8) * 10 + N + 1, ?VERHOEFF_P),
    D = element(C * 10 + P + 1, ?VERHOEFF_D),
    verhoeff_sum(I + 1, T, D).

%% --------------------------------------------------------------------
%% damm
%% --------------------------------------------------------------------

damm_to_digits(String) when is_list(String) ->
    [C - $0 || C <- String];
damm_to_digits(Binary) when is_binary(Binary) ->
    [C - $0 || <<C>> <= Binary];
damm_to_digits(Integer) ->
    damm_to_digits(integer_to_list(Integer)).

damm_traverse([], Interim) -> Interim;
damm_traverse([H | T], Interim) ->
    damm_traverse(T, element((Interim * 10) + H + 1, ?DAMM)).

