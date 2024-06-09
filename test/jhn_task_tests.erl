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
%%%   eunit unit tests for the jhn_task behaviour.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_task_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% logger callbacks
-export([log/2]).

%% ===================================================================
%% logger callbacks
%% ===================================================================


log(#{meta := #{error_logger := #{type := crash_report}}}, _) ->
    inform(crash_report),
    ok;
log(#{msg := {report, #{label := {jhn_task, terminating}}}}, _) ->
    inform(terminating_report),
    ok;
log(Event, _) ->
    inform(report),
    ?debugFmt("~nLog: ~p~n", [Event]).


%% ===================================================================
%% Tests.
%% ===================================================================

%%%-------------------------------------------------------------------
% Simple
%%%-------------------------------------------------------------------
simple_test_() ->
    {setup,
     fun setup_simple/0,
     fun cleanup_simple/1,
     {inorder, [{timeout, 120, {"Create", ?_test(run_simple(created))}},
                {timeout, 120, {"Stop", ?_test(run_simple(stop))}}
               ]}
    }.

setup_simple() ->
    logger:remove_handler(default),
    Config = #{id => ?MODULE,
               config => none,
               level => all,
               module => ?MODULE,
               filter_default => log,
               filters => [],
               formatter => {?MODULE, #{}}
              },
    logger:add_handler(default, ?MODULE, Config).

cleanup_simple(_) -> ok.

run_simple(created) ->
    register(tester, self()),
    Result = a_jhn_task:start_link(simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    {ok, Pid} = Result,
    put(pid, Pid),
    ?assertEqual(init, wait()),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links));
run_simple(stop) ->
    Pid = erase(pid),
    ?assertEqual(ok, a_jhn_task:cast(Pid, {stop, normal})),
    ?assertEqual({terminate, normal}, wait()).

%%%-------------------------------------------------------------------
% Createstop
%%%-------------------------------------------------------------------
createstop_test_() ->
    {setup,
     fun setup_createstop/0,
     fun cleanup_createstop/1,
     {inorder, [{timeout, 120, {"Unlinked", ?_test(run_createstop(unlinked))}},
                {timeout, 120, {"Ignore", ?_test(run_createstop(ignore))}},
                {timeout, 120, {"Stop(Init)", ?_test(run_createstop(stop))}},
                {timeout, 120, {"Bad Opt(Init)",
                                ?_test(run_createstop(badopt))}},
                {timeout, 120, {"Bad return(Init)",
                                ?_test(run_createstop(badreturninit))}},
                {timeout, 120, {"Exit(Init)",
                                ?_test(run_createstop(exitinit))}},
                {timeout, 120, {"Die(Init)",
                                ?_test(run_createstop(dieinit))}},
                {timeout, 120, {"No name", ?_test(run_createstop(noname))}},
                {timeout, 120, {"No arg", ?_test(run_createstop(noarg))}},
                {timeout, 120, {"Timer infinity",
                                ?_test(run_createstop(timerinf))}},
                {timeout, 120, {"Stop(Normal)", ?_test(run_simple(stop))}},
                {timeout, 120, {"Timer integer",
                                ?_test(run_createstop(timerint))}},
                {timeout, 120, {"Stop(Normal)", ?_test(run_simple(stop))}},
                {timeout, 120, {"Timer timeout",
                                ?_test(run_createstop(timeout))}},
                {timeout, 120, {"Stop(shutdown)",
                                ?_test(run_createstop(shutdown))}},
                {timeout, 120, {"Stop(Error)",
                                ?_test(run_createstop(stoperror))}}
               ]}
    }.

setup_createstop() -> ok.

cleanup_createstop(_) -> ok.

run_createstop(unlinked) ->
    register(tester, self()),
    Result = a_jhn_task:start(createstop, tester, node(), []),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    {ok, Pid} = Result,
    {links, Links} = process_info(self(), links),
    ?assertEqual(false, lists:member(Pid, Links));
run_createstop(ignore) ->
    Result = a_jhn_task:start_link(ignore, tester, node()),
    ?assertMatch(ignore, Result),
    {ignore, Pid} = wait(),
    ?assertEqual(false, lists:member(testTask, registered())),
    {links, Links} = process_info(self(), links),
    ?assertEqual(false, lists:member(Pid, Links));
run_createstop(stop) ->
    Result = a_jhn_task:start_link(stop, tester, node()),
    ?assertMatch({error, stopped}, Result),
    {stop, Pid} = wait(),
    crash_report = wait(),
    ?assertEqual(false, lists:member(testTask, registered())),
    {links, Links} = process_info(self(), links),
    ?assertEqual(false, lists:member(Pid, Links));
run_createstop(badopt) ->
    Result = a_jhn_task:start_link(simple, tester, node(), [{link, yes}]),
    ?assertEqual({error, [{link, yes}]}, Result),
    ?assertEqual(false, lists:member(testTask, registered()));
run_createstop(badreturninit) ->
    Result = a_jhn_task:start_link(badreturn, tester, node()),
    ?assertEqual({error, {bad_return_value, {uk, state}}}, Result),
    ?assertEqual(false, lists:member(testTask, registered())),
    crash_report = wait();
run_createstop(exitinit) ->
    Result = a_jhn_task:start_link(exitinit, tester, node()),
    ?assertMatch({exit, exitinit, _}, Result),
    ?assertEqual(false, lists:member(testTask, registered())),
    crash_report = wait();
run_createstop(dieinit) ->
    Result = a_jhn_task:start(dieinit, tester, node(), []),
    ?assertMatch({error, fail}, Result),
    ?assertEqual(false, lists:member(testTask, registered())),
    Flag = process_flag(trap_exit, true),
    Result = a_jhn_task:start_link(dieinit, tester, node()),
    ?assertMatch({error, fail}, Result),
    ?assertEqual(false, lists:member(testTask, registered())),
    process_flag(trap_exit, Flag);
run_createstop(noname) ->
    Result = a_jhn_task:start_link(simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(false, lists:member(testTask, registered())),
    {ok, Pid} = Result,
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    ?assertEqual(ok, a_jhn_task:cast(Pid, {stop, normal})),
    ?assertEqual({terminate, normal}, wait());
run_createstop(noarg) ->
    Result = a_jhn_task:start_link(),
    ?assertMatch(ignore, Result),
    ?assertEqual(false, lists:member(testTask, registered()));
run_createstop(timerinf) ->
    Result = a_jhn_task:start_link(simple, tester, node(),
                                    [{timeout, infinity}]),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    {ok, Pid} = Result,
    put(pid, Pid),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links));
run_createstop(timerint) ->
    Result = a_jhn_task:start_link(simple, tester, node(),
                                    [{timeout, 5000}]),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    {ok, Pid} = Result,
    put(pid, Pid),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links));
run_createstop(timeout) ->
    Result = a_jhn_task:start_link(timeout, tester, node(),
                                    [{timeout, 100}]),
    ?assertMatch({error, timeout}, Result),
    ?assertEqual(timeout, wait()),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(testTask));
run_createstop(shutdown) ->
    Result = a_jhn_task:start(shutdown, tester, node(), []),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    {ok, Pid} = Result,
    exit(Pid, shutdown),
    ?assertEqual({terminate, shutdown}, wait()),
    ?assertEqual(terminating_report, wait()),
    ?assertEqual(crash_report, wait()),
    ?assertEqual(false, is_process_alive(Pid)),
    Result1 = a_jhn_task:start(shutdown, tester, node(), []),
    ?assertMatch({ok, _}, Result1),
    ?assertEqual(init, wait()),
    {ok, Pid1} = Result1,
    exit(Pid1, {shutdown, 5000}),
    ?assertEqual({terminate, {shutdown, 5000}}, wait()),
    ?assertEqual(terminating_report, wait()),
    ?assertEqual(crash_report, wait()),
    timer:sleep(100),
    ?assertEqual(false, is_process_alive(Pid1));
run_createstop(stoperror) ->
    Result = a_jhn_task:start(simple, tester, node(), []),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    {ok, Pid} = Result,
    ?assertEqual(ok, a_jhn_task:cast(Pid, {stop, anError})),
    ?assertEqual({terminate, anError}, wait()),
    timer:sleep(10),
    ?assertEqual(false, is_process_alive(Pid)),
    terminating_report = wait(),
    crash_report = wait().

%%%-------------------------------------------------------------------
% Error
%%%-------------------------------------------------------------------
error_test_() ->
    {setup,
     fun setup_error/0,
     fun cleanup_error/1,
     {inorder, [{timeout, 120, {"Terminate(exit)",
                                ?_test(run_error(terminateexit))}},
                {timeout, 120, {"Terminate(throw)",
                                ?_test(run_error(terminatethrow))}}
               ]}
    }.

setup_error() ->
    os:cmd("erl -sname test -run erlang halt"),
    Node = list_to_atom("task_test@" ++ host()),
    {ok, _} = net_kernel:start([Node, shortnames]),
    ok.

cleanup_error(_) ->
    net_kernel:stop(),
    ok.

run_error(terminateexit) ->
    register(tester, self()),
    Result = a_jhn_task:start_link(simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    {ok, Pid} = Result,
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    Flag = process_flag(trap_exit, true),
    a_jhn_task:cast(Pid, exit),
    terminating_report = wait(),
    crash_report = wait(),
    ?assertEqual({'EXIT', Pid, {error, {terminated, exit}}}, wait()),
    process_flag(trap_exit, Flag);
run_error(terminatethrow) ->
    Result = a_jhn_task:start_link(simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    {ok, Pid} = Result,
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    Flag = process_flag(trap_exit, true),
    a_jhn_task:cast(Pid, throw),
    terminating_report = wait(),
    crash_report = wait(),
    ?assertEqual({'EXIT', Pid, {error, {terminated, throw}}}, wait()),
    process_flag(trap_exit, Flag).

%%%-------------------------------------------------------------------
% Coverage
%%%-------------------------------------------------------------------
coverage_test_() ->
    {setup,
     fun setup_coverage/0,
     fun cleanup_coverage/1,
     {inorder, [{timeout, 120, {"Behaviour Info",
                                ?_test(run_coverage(behaviour_info))}}
               ]}
    }.

setup_coverage() -> ok.

cleanup_coverage(_) -> ok.

run_coverage(behaviour_info) ->
    jhn_task:behaviour_info(callbacks).



%% ===================================================================
%% Internal functions.
%% ===================================================================

wait() -> wait(10000).

wait(Time) -> receive Y -> Y after Time -> timeout end.

host() ->
    [Host | _] = string:tokens(net_adm:localhost(), "."),
    Host.

inform(X) -> tester ! X.
