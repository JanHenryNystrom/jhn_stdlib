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
%%%   eunit unit tests for the jhn_crc32c library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_hash_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% crc32c/1
%%--------------------------------------------------------------------
crc32c_1_test_() ->
    [?_test(?assertEqual(jhn_hash:crc32c(B), Sum)) ||
        {B, Sum} <-
            [{<<0:256>>, 16#8A9136AA},
             {lists:duplicate(32, 255), 16#62A8AB43},
             {[N || N <- lists:seq(0, 31)], 16#46DD794E},
             {[N || N <- lists:seq(31, 0, -1)], 16#113FDB5C},
             {<<16#1C00000:32, 0:96, 16#14000000:32, 16#400:32, 16#14:32,
                16#18:32, 16#28000000:32, 0:32, 16#2000000:32, 0:32>>,
              16#D9963A56}
            ]
    ].

%%--------------------------------------------------------------------
%% xxh32/1
%%--------------------------------------------------------------------
xxh32_1_test_() ->
    [?_test(?assertEqual(jhn_hash:xxh32(B), Sum)) ||
        {B, Sum} <-
            [{"test", 1042293711}]
    ].

%%--------------------------------------------------------------------
%% xxh32/2
%%--------------------------------------------------------------------
xxh32_2_test_() ->
    [?_test(?assertEqual(jhn_hash:xxh32(B, S), Sum)) ||
        {B, S, Sum} <-
            [{"test", 12345, 3834992036}]
    ].


%% ===================================================================
%% Internal functions.
%% ===================================================================
