%%==============================================================================
%% Copyright 2026 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the jhn_jwt library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_jwt_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").


%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

encode_2_test_() ->
    [?_test(?assertEqual(iolist_to_binary(jhn_base:encode(E, B)), Result)) ||
        {E, Result, B} <-
            [
            ]
    ].


encode_3_test_() ->
    [?_test(?assertEqual(jhn_base:encode(E, B, O), Result)) ||
        {E, O, Result, B} <-
            [
            ]
    ].

%% ===================================================================
%% Decoding
%% ===================================================================

decode_2_test_() ->
    [?_test(?assertEqual(iolist_to_binary(jhn_base:decode(E, B)), Result)) ||
        {E, Result, B} <-
            [
            ]
    ].

decode_3_test_() ->
    [?_test(?assertEqual(jhn_base:decode(E, B, O), Result)) ||
        {E, O, Result, B} <-
            [
            ]
    ].

%% ===================================================================
%% Encoding/Decoding
%% ===================================================================

encode_3_decode_3_test_() ->
    [?_test(?assertEqual(jhn_base:decode(E, jhn_base:encode(E, B, O), O), B)) ||
        {E, O, B} <-
            []
    ].
