%%==============================================================================
%% Copyright 2016-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%   Implements Levenshtein distance between two strings
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(levenshtein).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Compiler options
-compile({inline, [{min, 3}]}).

%% Library functions
-export([distance/2]).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: distance(String1 String2) -> Distance.
%% @doc
%%   Computes the Levenshtein distance between String1 and String2.
%% @end
%%--------------------------------------------------------------------
-spec distance(string(), string()) -> integer().
%%--------------------------------------------------------------------
distance([], S) -> length(S);
distance(S, []) -> length(S);
distance(S, T) ->
    case equal(S, T, 0, true) of
        true -> 0;
        TLen ->
            V0 = lists:seq(0, TLen),
            i(V0, 0, S, T)
    end.

%% ===================================================================
%% Internal functions.
%% ===================================================================

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



