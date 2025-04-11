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
%%%   eunit unit tests for the jhn_shadow library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2025, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_shadow_tests).
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
    [?_test(?assertMatch(ok, jhn_shadow:hide(jhn_blist, dark, [{all, 2}]))),
     ?_test(?assertMatch(ok, jhn_shadow:hide(jhn_bloom, dark, [{filter, 0},
                                                               {filter, 1}]))),
     ?_test(?assertMatch({error, already_hidden},
                         jhn_shadow:hide(jhn_bloom, dark, [{filter, 0}]))),
     ?_test(?assertMatch({jhn_blist, all, [a, b]}, jhn_blist:all(a, b))),
     ?_test(?assertMatch({jhn_bloom, filter, []}, jhn_bloom:filter())),
     ?_test(?assertMatch({jhn_bloom, filter, [a]}, jhn_bloom:filter(a))),
     ?_test(?assertMatch(ok, jhn_shadow:reveal(jhn_blist))),
     ?_test(?assertMatch(ok, jhn_shadow:reveal(jhn_bloom))),
     ?_test(?assertMatch({error, not_hidden}, jhn_shadow:reveal(jhn_bloom))),
     ?_test(?assertMatch({error, not_hidden}, jhn_shadow:reveal(foo))),
     ?_test(?assertMatch(true, jhn_blist:all(fun(_) -> true end, <<>>))),
     ?_test(?assertMatch({error, _, _},
                         jhn_shadow:hide(jhn_blist, dark, [{4, 2}])))
    ]}.

hide_3_module_all_test_() ->
    {setup,
     fun() ->
             ok = jhn_shadow:load_mod(dark, ["hide(M, F, A) -> {M, F, A}."]),
             setup_logger()
     end,
     fun(Config) ->
             ok = jhn_shadow:unload_mod(dark),
             cleanup_logger(Config)
     end,
    [?_test(?assertMatch(ok, jhn_shadow:hide(jhn_blist, dark, all))),
     ?_test(?assertMatch({jhn_blist, all, [a, b]}, jhn_blist:all(a, b))),
     ?_test(?assertMatch({jhn_blist, any, [a, b]}, jhn_blist:any(a, b))),
     ?_test(?assertMatch(ok, jhn_shadow:reveal(jhn_blist)))
    ]}.

hide_3_process_test_() ->
    {setup,
     fun() ->
             ok = jhn_shadow:load_mod(
                    dark,
                    ["init(State) -> {ok, State}.",
                     "hide(M, F, A, S) -> {ok, true, [{M, F, A} | S]}.",
                     "peek(get, State) -> {ok, State, []}."]),
             {ok, Pid} = jhn_shadow:create(dark, [{arg, []}]),
             persistent_term:put(the_shadow, Pid),
             {Pid, setup_logger()}
     end,
     fun({Pid, Config}) ->
             true = persistent_term:erase(the_shadow),
             ok = jhn_shadow:destroy(Pid),
             ok = jhn_shadow:unload_mod(dark),
             cleanup_logger(Config)
     end,
    [?_test(?assertMatch(true, register(tester, self()))),
     ?_test(?assertMatch({links, [_]},
                         process_info(persistent_term:get(the_shadow), links))),
     ?_test(?assertMatch({status, _, _, _},
                         sys:get_status(persistent_term:get(the_shadow)))),
     ?_test(?assertMatch(foo, persistent_term:get(the_shadow) ! foo)),
     ?_test(?assertMatch({unexpected, foo}, wait())),
     ?_test(?assertMatch(ok,
                         jhn_shadow:hide(jhn_blist,
                                         persistent_term:get(the_shadow),
                                         [{all, 2}]))),
     ?_test(?assertMatch(true, jhn_blist:all(a, b))),
     ?_test(?assertMatch([{jhn_blist, all, [a, b]}],
                         jhn_shadow:peek(persistent_term:get(the_shadow),
                                         get))),
     ?_test(?assertMatch([],
                         jhn_shadow:peek(persistent_term:get(the_shadow),
                                         get))),
     ?_test(?assertMatch(ok, jhn_shadow:reveal(jhn_blist))),
     ?_test(?assertError(function_clause, jhn_blist:all(a, b)))
    ]}.

hide_3_named_process_test_() ->
    {setup,
     fun() ->
             ok = jhn_shadow:load_mod(
                    dark,
                    ["hide(M, F, A, _) -> {ok, true, {M, F, A}}.",
                     "peek(take, State) -> {ok, State, undefined}."]),
             jhn_shadow:create(dark, [{name, shadow}, {link, false}]),
             setup_logger()
     end,
     fun(Config) ->
             ok = jhn_shadow:destroy(shadow),
             ok = jhn_shadow:unload_mod(dark),
             cleanup_logger(Config)
     end,
    [?_test(?assertMatch(true, register(tester, self()))),
     ?_test(?assertMatch({links, []}, process_info(whereis(shadow), links))),
     ?_test(?assertMatch(ok, jhn_server:cast(shadow, foo))),
     ?_test(?assertMatch({unexpected, foo}, wait())),
     ?_test(?assertMatch({status, _, _, _}, sys:get_status(shadow))),
     ?_test(?assertMatch(ok,
                         jhn_shadow:hide(jhn_blist,
                                         {name, shadow},
                                         [{all, 2}]))),
     ?_test(?assertMatch(true, jhn_blist:all(a, b))),
     ?_test(?assertMatch({jhn_blist, all, [a, b]},
                         jhn_shadow:peek(shadow, take))),
     ?_test(?assertMatch(undefined, jhn_shadow:peek(shadow, take))),
     ?_test(?assertMatch(ok, jhn_shadow:reveal(jhn_blist))),
     ?_test(?assertError(function_clause, jhn_blist:all(a, b)))
    ]}.

%% ===================================================================
%% Load/Unload module
%% ===================================================================

load_mod_1_test_() ->
    {setup,
     fun() ->
             M = "-module(loaded).\n"
                 "-export([ok/0, atom/1]).\n"
                 "ok() -> <<\"OK.\">>.\n"
                 "atom(S) -> list_to_atom(S).\n",
             ok = jhn_shadow:load_mod(M)
     end,
     fun(_) -> ok = jhn_shadow:unload_mod(loaded) end,
    [?_test(?assertMatch(<<"OK.">>, loaded:ok())),
     ?_test(?assertMatch(foo, loaded:atom("foo"))),
     ?_test(?assertMatch({error, _}, jhn_shadow:load_mod(".0."))),
     ?_test(?assertMatch({error, _, _}, jhn_shadow:load_mod("ok() -> ok.")))
    ]}.

load_mod_2_test_() ->
    {setup,
     fun() ->
             Fs = ["ok() -> <<\"OK.\">>.", <<"atom(S) -> list_to_atom(S).">>],
             ok = jhn_shadow:load_mod(loaded, Fs)
     end,
     fun(_) -> ok = jhn_shadow:unload_mod(loaded) end,
    [?_test(?assertMatch(<<"OK.">>, loaded:ok())),
     ?_test(?assertMatch(foo, loaded:atom("foo"))),
     ?_test(?assertMatch({error, _}, jhn_shadow:load_mod(bar, [".0."]))),
     ?_test(?assertMatch({error, _, _},
                         jhn_shadow:load_mod(bar, ["-export([bar/1])."])))
    ]}.

unload_mod_1_test_() ->
    {setup,
     fun() ->
             M = <<"-module(loaded)."
                   "-export([ok/0, atom/1])."
                   "ok() -> <<\"OK.\">>."
                   "atom(S) -> list_to_atom(S).">>,
             ok = jhn_shadow:load_mod(M)
     end,
     fun(_) -> ok end,
    [?_test(?assertMatch({file, _}, code:is_loaded(loaded))),
     ?_test(?assertMatch(<<"OK.">>, loaded:ok())),
     ?_test(?assertMatch(ok, jhn_shadow:unload_mod(loaded))),
     ?_test(?assertError(undef, loaded:ok())),
     ?_test(?assertMatch({error, not_loaded}, jhn_shadow:unload_mod(kernel))),
     ?_test(?assertMatch({error, not_loaded}, jhn_shadow:unload_mod(bla)))
    ]}.

%% ===================================================================
%% Load/Start/Unload application
%% ===================================================================

load_app_2_test_() ->
    BaadSpec1 = #{env => [{1, one}],
                  applications => 5,
                  mod => {1, one},
                  sup => {1, 2, 3},
                  foo => bar},
    BaadSpec2 = #{sup => {app, 2, 3}, mod => fnu},
    BaadSpec3 = #{sup => {app, mod, 3}},
    BaadSpec4 = #{sup => {1, 2, 3, 4}},
    BaadSpec5 = #{sup => {app, 2, 3, 4}},
    BaadSpec6 = #{sup => {app, mod, 3, 4}},
    BaadSpec7 = #{sup => {app, mod, func, 4}},
    BaadSpec8 = #{sup => fnu},
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
    [?_test(?assertMatch(ok, jhn_shadow:load_app(foo, [{one, 1}]))),
     ?_test(?assertMatch(true,
                         lists:keymember(foo,
                                         1,
                                         application:loaded_applications()))),
     ?_test(?assertMatch({ok, 1}, application:get_env(foo, one))),
     ?_test(?assertMatch(ok, jhn_shadow:unload_app(foo))),
     ?_test(?assertMatch([], application:get_all_env(foo))),
     ?_test(?assertMatch({error, _}, jhn_shadow:load_app(foo, [{1, one}]))),
     ?_test(?assertMatch({error, _}, jhn_shadow:load_app(foo, BaadSpec1))),
     ?_test(?assertMatch({error, _}, jhn_shadow:load_app(foo, BaadSpec2))),
     ?_test(?assertMatch({error, _}, jhn_shadow:load_app(foo, BaadSpec3))),
     ?_test(?assertMatch({error, _}, jhn_shadow:load_app(foo, BaadSpec4))),
     ?_test(?assertMatch({error, _}, jhn_shadow:load_app(foo, BaadSpec5))),
     ?_test(?assertMatch({error, _}, jhn_shadow:load_app(foo, BaadSpec6))),
     ?_test(?assertMatch({error, _}, jhn_shadow:load_app(foo, BaadSpec7))),
     ?_test(?assertMatch({error, _}, jhn_shadow:load_app(foo, BaadSpec8)))
    ]}.

start_app_2_test_() ->
    {setup,
     fun() -> setup_logger() end,
     fun(Config) -> cleanup_logger(Config) end,
    [?_test(?assertMatch(true, register(tester, self()))),
     ?_test(?assertMatch(ok, jhn_shadow:start_app(foo, [{one, 1}]))),
     ?_test(?assertMatch(true,
                         lists:keymember(foo,
                                         1,
                                         application:which_applications()))),
     ?_test(?assertMatch({ok, 1}, application:get_env(foo, one))),
     ?_test(?assertMatch(ok, jhn_shadow:stop_app(foo))),
     ?_test(?assertMatch({application_exit, foo, stopped}, wait())),
     ?_test(?assertMatch([], application:get_all_env(foo))),
     ?_test(?assertMatch({error, not_running}, jhn_shadow:stop_app(foo))),
     ?_test(?assertMatch({error, not_shadow_application},
                          jhn_shadow:unload_app(kernel))),
     ?_test(?assertMatch({error, not_loaded},
                          jhn_shadow:unload_app(foo))),
     ?_test(?assertMatch({error, _}, jhn_shadow:start_app(foo, #{bar => 1})))
    ]}.

start_all_app_2_test_() ->
    {setup,
     fun() -> setup_logger() end,
     fun(Config) -> cleanup_logger(Config) end,
    [?_test(?assertMatch(true, register(tester, self()))),
     ?_test(?assertMatch(ok, jhn_shadow:start_all_app(foo, [{one, 1}]))),
     ?_test(?assertMatch(true,
                         lists:keymember(foo,
                                         1,
                                         application:which_applications()))),
     ?_test(?assertMatch({ok, 1}, application:get_env(foo, one))),
     ?_test(?assertMatch(ok, jhn_shadow:stop_app(foo))),
     ?_test(?assertMatch({application_exit, foo, stopped}, wait())),
     ?_test(?assertMatch([], application:get_all_env(foo))),
     ?_test(?assertMatch({error, not_running}, jhn_shadow:stop_app(foo))),
     ?_test(?assertMatch({error, not_shadow_application},
                          jhn_shadow:unload_app(kernel))),
     ?_test(?assertMatch({error, not_loaded},
                          jhn_shadow:unload_app(foo))),
     ?_test(?assertMatch({error, _},
                         jhn_shadow:start_all_app(foo, #{bar => 1}))),
     ?_test(?assertMatch({error, _},
                         jhn_shadow:start_all_app(foo,
                                                  #{applications => [huga]})))
    ]}.

start_app_2_mod_test_() ->
    {setup,
     fun() -> jhn_shadow:load_mod(foo_app,
                                  ["start(_, X) -> tester ! X, {ok, self()}."]),
              setup_logger()
     end,
     fun(Config) -> cleanup_logger(Config) end,
    [?_test(?assertMatch(true, register(tester, self()))),
     ?_test(?assertMatch(ok,
                         jhn_shadow:start_app(foo,
                                              #{mod => {foo_app, x}}))),
     ?_test(?assertMatch(true,
                         lists:keymember(foo,
                                         1,
                                         application:which_applications()))),
     ?_test(?assertMatch(x, wait())),
     ?_test(?assertMatch(ok, jhn_shadow:stop_app(foo))),
     ?_test(?assertMatch({application_exit, foo, stopped}, wait())),
     ?_test(?assertMatch([], application:get_all_env(foo)))
    ]}.

start_app_2_sup_test_() ->
    {setup,
     fun() ->
             jhn_shadow:load_mod(foo_sup,
                                 ["start_link(X) -> tester ! X, {ok, self()}."]
                                ),
             setup_logger()
     end,
     fun(Config) -> cleanup_logger(Config) end,
    [?_test(?assertMatch(true, register(tester, self()))),
     ?_test(?assertMatch(ok,
                         jhn_shadow:start_app(foo,
                                              #{sup => {foo, foo_sup, [x]}}))),
     ?_test(?assertMatch(true,
                         lists:keymember(foo,
                                         1,
                                         application:which_applications()))),
     ?_test(?assertMatch(x, wait())),
     ?_test(?assertMatch(ok, jhn_shadow:stop_app(foo))),
     ?_test(?assertMatch({application_exit, foo, stopped}, wait())),
     ?_test(?assertMatch([], application:get_all_env(foo)))
    ]}.

start_app_2_sup_func_test_() ->
    {setup,
     fun() ->
             jhn_shadow:load_mod(foo_sup,
                                 ["create(X) -> tester ! X, {ok, self()}."]
                                ),
             setup_logger()
     end,
     fun(Config) -> cleanup_logger(Config) end,
    [?_test(?assertMatch(true, register(tester, self()))),
     ?_test(?assertMatch(ok,
                         jhn_shadow:start_app(foo,
                                              #{sup => {foo,
                                                        foo_sup,
                                                        create,
                                                        [x]}}))),
     ?_test(?assertMatch(true,
                         lists:keymember(foo,
                                         1,
                                         application:which_applications()))),
     ?_test(?assertMatch(x, wait())),
     ?_test(?assertMatch(ok, jhn_shadow:stop_app(foo))),
     ?_test(?assertMatch({application_exit, foo, stopped}, wait())),
     ?_test(?assertMatch([], application:get_all_env(foo)))
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

