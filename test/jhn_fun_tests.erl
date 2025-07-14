%%==============================================================================
%% Copyright 2025 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2025, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_fun_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines

%% logger callbacks
-export([log/2]).

%% ===================================================================
%% logger callbacks
%% ===================================================================

log(E = #{msg := {report, #{label := {application_controller, exit}}}}, _) ->
    #{msg := {_, #{report  := [{application, A}, {exited, How} | _]}}} = E,
    inform({application_exit, A, How});
log(#{msg := {report, #{label := {jhn_shadow,unexpected}, message := M}}}, _) ->
    inform({unexpected, M});
log(#{msg := {report, #{label := {jhn_server,unexpected}, message := M}}}, _) ->
    inform({unexpected, M});
log(Event, _) ->
    ?debugFmt("~nLog: ~p~n", [Event]).

%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Hide/Reveal
%% ===================================================================
hide_3_module_test_() ->
    {setup,
     fun() ->
             ok = jhn_shadow:load_mod(dark, ["hide(M, F, A) -> {M, F, A}."]),
             setup_logger()
     end,
     fun(Config) ->
             ok = jhn_shadow:unload_mod(dark),
             cleanup_logger(Config)
     end,
    [?_test(?assertMatch(ok, jhn_shadow:hide(jhn_blist, dark, [{all, 2}])))
    ]}.


%% ===================================================================
%% Internal functions.
%% ===================================================================

inform(X) -> tester ! X.

wait() -> wait(10000).

wait(Time) -> receive Y -> Y after Time -> timeout end.

setup_logger() ->
    {ok, DefaultConfig} = logger:get_handler_config(default),
    logger:remove_handler(default),
    Config = #{id => ?MODULE,
               config => none,
               level => all,
               module => ?MODULE,
               filter_default => log,
               filters => [],
               formatter => {?MODULE, #{}}
              },
    logger:add_handler(default, ?MODULE, Config),
    DefaultConfig.

cleanup_logger(DefaultConfig = #{module := Mod}) ->
    logger:remove_handler(default),
    logger:add_handler(default, Mod, DefaultConfig).

