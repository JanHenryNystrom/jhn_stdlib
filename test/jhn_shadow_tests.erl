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
%%%   eunit unit tests for the jhn_shadow library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_shadow_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines

%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Hide/Reveal
%% ===================================================================

%% ===================================================================
%% Load/Unload module
%% ===================================================================

load_module_1_test_() ->
    {setup,
     fun() ->
             M = "-module(loaded).\n"
                 "-export([ok/0, atom/1]).\n"
                 "ok() -> <<\"OK.\">>.\n"
                 "atom(S) -> list_to_atom(S).\n",
             ok = jhn_shadow:load_module(M)
     end,
     fun(_) ->
             ok = jhn_shadow:unload_module(loaded)
     end,
    [?_test(?assertMatch(<<"OK.">>, loaded:ok())),
     ?_test(?assertMatch(foo, loaded:atom("foo")))
    ]}.

unload_module_1_test_() ->
    {setup,
     fun() ->
             M = "-module(loaded)."
                 "-export([ok/0, atom/1])."
                 "ok() -> <<\"OK.\">>."
                 "atom(S) -> list_to_atom(S).",
             ok = jhn_shadow:load_module(M)
     end,
     fun(_) -> ok end,
    [?_test(?assertMatch(ok, jhn_shadow:unload_module(loaded))),
     ?_test(?assertError(undef, loaded:ok()))
    ]}.
