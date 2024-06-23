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
         levenshtein/2
        ]).

%% Defines.

%% rotx32
-define(MAX32, 16#FFFFFFFF).

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
