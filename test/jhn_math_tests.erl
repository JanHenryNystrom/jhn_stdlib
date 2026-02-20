%%==============================================================================
%% Copyright 2024-2026 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the jhn_math library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024-2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_math_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% rotl32/2
%%--------------------------------------------------------------------
rotl32_2_test_() ->
    [?_test(?assertEqual(jhn_math:rotl32(X, S), rotl(X, S))) ||
        X <- lists:seq(0,  16#FFFFFFFF, 57885161),
        S <- lists:seq(1, 31)
    ].

%%--------------------------------------------------------------------
%% levenshtein
%%--------------------------------------------------------------------

levenshtein_test_() ->
    [{"equal",
      ?_test(?assertEqual(0, jhn_math:levenshtein("Rose", "Rose")))},
     {"Empty one",
      ?_test(?assertEqual(4, jhn_math:levenshtein("", "Rose")))},
     {"Empty two",
      ?_test(?assertEqual(4, jhn_math:levenshtein("Rose", "")))},
     {"kitte/sitting",
      ?_test(?assertEqual(3, jhn_math:levenshtein("kitten", "sitting")))}
    ].

%%--------------------------------------------------------------------
%% luhn
%%--------------------------------------------------------------------

luhn_test_() ->
    [?_test(?assertEqual(true, jhn_math:luhn(check, Check))) ||
        N <- [17893729974,
              49927398716, 1234567812345670],
        Check <- [N, integer_to_list(N), integer_to_binary(N)]
    ]
        ++
        [?_test(?assertEqual(false, jhn_math:luhn(check, Check))) ||
            N <- [49927398717, 1234567812345678],
            Check <- [N, integer_to_list(N), integer_to_binary(N)]
        ]
        ++
        [?_test(?assertEqual(D, jhn_math:luhn(gen, Gen))) ||
            {N, D} <- [{1789372997, 4}, {4992739871, 6}, {123456781234567, 0}],
            Gen <- [N, integer_to_list(N), integer_to_binary(N)]
        ].

%%--------------------------------------------------------------------
%% Verhoeff
%%--------------------------------------------------------------------

verhoeff_test_() ->
    [?_test(?assertEqual(true, jhn_math:verhoeff(check, Check))) ||
        N <- [2363, 123451],
        Check <- [N, integer_to_list(N), integer_to_binary(N)]
    ]
        ++
        [?_test(?assertEqual(false, jhn_math:verhoeff(check, Check))) ||
            N <- [12345],
            Check <- [N, integer_to_list(N), integer_to_binary(N)]
        ]
        ++
        [?_test(?assertEqual(D, jhn_math:verhoeff(gen, Gen))) ||
            {N, D} <- [{236, 3}, {12345, 1}],
            Gen <- [N, integer_to_list(N), integer_to_binary(N)]
        ].

%%--------------------------------------------------------------------
%% Damm
%%--------------------------------------------------------------------

damm_test_() ->
    [?_test(?assertEqual(true, jhn_math:damm(check, Check))) ||
        N <- [5724, 112946],
        Check <- [N, integer_to_list(N), integer_to_binary(N)]
    ]
        ++
        [?_test(?assertEqual(false, jhn_math:damm(check, Check))) ||
            N <- [5727, 112949],
            Check <- [N, integer_to_list(N), integer_to_binary(N)]
        ]
        ++
        [?_test(?assertEqual(D, jhn_math:damm(gen, Gen))) ||
            {N, D} <- [{572, 4}, {11294, 6}],
            Gen <- [N, integer_to_list(N), integer_to_binary(N)]
        ].

%% ===================================================================
%% Internal functions.
%% ===================================================================

rotl(X, N) ->
   R = (32 - N),
    <<H:N, T:R>> = <<X:32>>,
    <<Y:32>> = <<T:R, H:N>>,
    Y.
