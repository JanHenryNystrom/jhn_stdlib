%%==============================================================================
%% Copyright 2016-2020 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the levenshtein library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016-2020, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(levenshtein_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% distance/2
%%--------------------------------------------------------------------
bloom_fixed_test_() ->
    [{"equal",
      ?_test(?assertEqual(0, levenshtein:distance("Rose", "Rose")))},
     {"Empty one",
      ?_test(?assertEqual(4, levenshtein:distance("", "Rose")))},
     {"Empty two",
      ?_test(?assertEqual(4, levenshtein:distance("Rose", "")))},
     {"kitte/sitting",
      ?_test(?assertEqual(3, levenshtein:distance("kitten", "sitting")))}
    ].

%% ===================================================================
%% Internal functions.
%% ===================================================================
