%%==============================================================================
%% Copyright 2013-2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the jhn_fsm behaviour.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_fsm_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-record(state, {name, mod, state_name, data, event, message, terminate,
                code_change, format_status, deferred, handling_deferred
               }).

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
     {inorder, [{timeout, 120, {"Start", ?_test(run_simple(started))}},
                {timeout, 120, {"Call", ?_test(run_simple(call))}},
                {timeout, 120, {"Event", ?_test(run_simple(event))}},
                {timeout, 120, {"Info", ?_test(run_simple(info))}},
                {timeout, 120, {"Reply", ?_test(run_simple(reply))}},
                {timeout, 120, {"From", ?_test(run_simple(from))}},
                {timeout, 120, {"SYS", ?_test(run_simple(sys))}},
                {timeout, 120, {"Hibernate", ?_test(run_simple(hibernate))}},
                {timeout, 120, {"Stop", ?_test(run_simple(stop))}}
               ]}
    }.

setup_simple() ->
    ok.

cleanup_simple(_) ->
    ok.

run_simple(started) ->
    register(tester, self()),
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links));
run_simple(call) ->
    Pid = whereis(testFsm),
    ?assertEqual({reply, aa}, a_jhn_fsm:call(testFsm, {reply, aa})),
    ?assertEqual({reply, bb}, a_jhn_fsm:call(Pid, {reply, bb})),
    ?assertEqual({reply, cc},
                 a_jhn_fsm:call({testFsm, node()}, {reply, cc})),
    ?assertError(badarg, a_jhn_fsm:call(1, {reply, cc})),
    ?assertError(badarg, a_jhn_fsm:call({testFsm, 1}, {reply, cc})),
    ?assertError(badarg, a_jhn_fsm:call({1, node()}, {reply, cc})),
    ?assertError(badarg, a_jhn_fsm:call(testFsm, {reply, aa}, error)),
    ?assertError(badarg, a_jhn_fsm:call(testFsm, {reply, aa}, -1)),
    ?assertError(badarg, a_jhn_fsm:call(testFsm, {reply, aa}, 0));
run_simple(event) ->
    Pid = whereis(testFsm),
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {reply, self(), aa})),
    ?assertEqual({reply, aa}, wait()),
    ?assertEqual(ok, a_jhn_fsm:event(Pid, {reply, self(), bb})),
    ?assertEqual({reply, bb}, wait()),
    ?assertEqual(ok, a_jhn_fsm:event({testFsm, node()}, {reply, self(), cc})),
    ?assertEqual({reply, cc}, wait()),
    ?assertError(badarg, a_jhn_fsm:event(1, {reply, cc})),
    ?assertError(badarg, a_jhn_fsm:event({1, node()}, {reply, cc})),
    ?assertError(badarg, a_jhn_fsm:event({testFsm, 1}, {reply, cc}));
run_simple(info) ->
    testFsm ! {reply, dd},
    ?assertEqual({reply, dd}, wait());
run_simple(reply) ->
    ?assertEqual({error, not_a_call}, jhn_fsm:reply(broken));
run_simple(from) ->
    ?assertEqual({error, not_a_call}, jhn_fsm:from()),
    ?assertEqual({reply, ff}, a_jhn_fsm:call(testFsm, {ereply, ff}));
run_simple(sys) ->
    sys:suspend(testFsm),
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {reply, self(), aa})),
    ?assertEqual(timeout, wait(100)),
    sys:resume(testFsm),
    ?assertEqual({reply, aa}, wait(100)),
    ?assertMatch({status, _, _, _}, sys:get_status(testFsm)),
    sys:suspend(testFsm),
    sys:change_code(testFsm, a_jhn_fsm, old, none),
    ?assertEqual(code_change, wait(100)),
    sys:resume(testFsm);
run_simple(hibernate) ->
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {hibernate, self()})),
    ?assertEqual(hibernate, wait()),
    ?assertEqual({reply, ff}, a_jhn_fsm:call(testFsm, {reply, ff})),
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {hibernate, self()})),
    ?assertEqual(hibernate, wait()),
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {reply, self(), ff})),
    ?assertEqual({reply, ff}, wait()),
    ?assertEqual(hibernate, a_jhn_fsm:call(testFsm, hibernate, 100)),
    ?assertEqual({reply, ff}, a_jhn_fsm:call(testFsm, {reply, ff})),
    ?assertEqual(hibernate, a_jhn_fsm:call(testFsm, hibernate, 100)),
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {reply, self(), ff})),
    ?assertEqual({reply, ff}, wait()),
    ?assertEqual(hibernate, a_jhn_fsm:call(testFsm, hibernate, 100)),
    sys:suspend(testFsm),
    sys:resume(testFsm),
    testFsm ! hibernate,
    ?assertEqual(hibernate, wait());
run_simple(stop) ->
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {stop, normal})),
    ?assertEqual({terminate, normal}, wait()).

%%%-------------------------------------------------------------------
% Startstop
%%%-------------------------------------------------------------------
startstop_test_() ->
    {setup,
     fun setup_startstop/0,
     fun cleanup_startstop/1,
     {inorder, [{timeout, 120, {"Hibernate", ?_test(run_startstop(hibernate))}},
                {timeout, 120, {"Call", ?_test(run_simple(call))}},
                {timeout, 120, {"Stop(Normal)", ?_test(run_simple(stop))}},
                {timeout, 120, {"Unlinked", ?_test(run_startstop(unlinked))}},
                {timeout, 120, {"Call", ?_test(run_simple(call))}},
                {timeout, 120, {"Stop(Normal)", ?_test(run_simple(stop))}},
                {timeout, 120, {"Ignore", ?_test(run_startstop(ignore))}},
                {timeout, 120, {"Stop(Init)", ?_test(run_startstop(stop))}},
                {timeout, 120, {"Bad Opt(Init)",
                                ?_test(run_startstop(badopt))}},
                {timeout, 120, {"Bad return(Init)",
                                ?_test(run_startstop(badreturninit))}},
                {timeout, 120, {"Exit(Init)",
                                ?_test(run_startstop(exitinit))}},
                {timeout, 120, {"Die(Init)",
                                ?_test(run_startstop(dieinit))}},
                {timeout, 120, {"Started", ?_test(run_startstop(started))}},
                {timeout, 120, {"No name", ?_test(run_startstop(noname))}},
                {timeout, 120, {"No arg", ?_test(run_startstop(noarg))}},
                {timeout, 120, {"Timer infinity",
                                ?_test(run_startstop(timerinf))}},
                {timeout, 120, {"Stop(Normal)", ?_test(run_simple(stop))}},
                {timeout, 120, {"Timer integer",
                                ?_test(run_startstop(timerint))}},
                {timeout, 120, {"Stop(Normal)", ?_test(run_simple(stop))}},
                {timeout, 120, {"Timer timeout",
                                ?_test(run_startstop(timeout))}},
                {timeout, 120, {"Stop(shutdown)",
                                ?_test(run_startstop(shutdown))}},
                {timeout, 120, {"Stop(Error)",
                                ?_test(run_startstop(stoperror))}},
                {timeout, 120, {"SYS(terminate)",
                                ?_test(run_startstop(systerminate))}}
               ]}
    }.

setup_startstop() ->
    ok.

cleanup_startstop(_) ->
    ok.

run_startstop(hibernate) ->
    register(tester, self()),
    Result = a_jhn_fsm:start_link(testFsm, hibernate, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links));
run_startstop(unlinked) ->
    Result = a_jhn_fsm:start(testFsm, hibernate, tester, node(), []),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(false, lists:member(Pid, Links));
run_startstop(ignore) ->
    Result = a_jhn_fsm:start_link(testFsm, ignore, tester, node()),
    ?assertMatch(ignore, Result),
    {ignore, Pid} = wait(),
    ?assertEqual(false, lists:member(testFsm, registered())),
    {links, Links} = process_info(self(), links),
    ?assertEqual(false, lists:member(Pid, Links));
run_startstop(stop) ->
    Result = a_jhn_fsm:start_link(testFsm, stop, tester, node()),
    ?assertMatch({error, stopped}, Result),
    {stop, Pid} = wait(),
    ?assertEqual(false, lists:member(testFsm, registered())),
    {links, Links} = process_info(self(), links),
    ?assertEqual(false, lists:member(Pid, Links));
run_startstop(badopt) ->
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node(),
                                     [{link, yes}]),
    ?assertEqual({error, [{link, yes}]}, Result),
    ?assertEqual(false, lists:member(testFsm, registered()));
run_startstop(badreturninit) ->
    Result = a_jhn_fsm:start_link(testFsm, badreturn, tester, node()),
    ?assertEqual({error, {bad_return_value, {uk, state}}}, Result),
    ?assertEqual(false, lists:member(testFsm, registered()));
run_startstop(exitinit) ->
    Result = a_jhn_fsm:start_link(testFsm, exitinit, tester, node()),
    ?assertMatch({error, {exit, exitinit, _}}, Result),
    ?assertEqual(false, lists:member(testFsm, registered()));
run_startstop(dieinit) ->
    Result = a_jhn_fsm:start(testFsm, dieinit, tester, node(), []),
    ?assertMatch({error, fail}, Result),
    ?assertEqual(false, lists:member(testFsm, registered())),
    Flag = process_flag(trap_exit, true),
    Result = a_jhn_fsm:start_link(testFsm, dieinit, tester, node()),
    ?assertMatch({error, fail}, Result),
    ?assertEqual(false, lists:member(testFsm, registered())),
    process_flag(trap_exit, Flag);
run_startstop(started) ->
    unregister(tester),
    register(testFsm, self()),
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node()),
    ?assertEqual({error, {already_created, testFsm, self()}}, Result),
    Result1 = a_jhn_fsm:start_link(testFsm, hibernate, tester, node()),
    ?assertEqual({error, {already_created, testFsm, self()}}, Result1),
    unregister(testFsm),
    register(tester, self());
run_startstop(noname) ->
    Result = a_jhn_fsm:start_link(simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(false, lists:member(testFsm, registered())),
    {ok, Pid} = Result,
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    ?assertEqual(ok, a_jhn_fsm:event(Pid, {stop, normal})),
    ?assertEqual({terminate, normal}, wait());
run_startstop(noarg) ->
    Result = a_jhn_fsm:start_link(),
    ?assertMatch(ignore, Result),
    ?assertEqual(false, lists:member(testFsm, registered()));
run_startstop(timerinf) ->
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node(),
                                    [{timeout, infinity}]),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links));
run_startstop(timerint) ->
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node(),
                                    [{timeout, 5000}]),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links));
run_startstop(timeout) ->
    Result = a_jhn_fsm:start_link(testFsm, timeout, tester, node(),
                                    [{timeout, 100}]),
    ?assertMatch({error, timeout}, Result),
    ?assertEqual(timeout, wait()),
    timer:sleep(100),
    ?assertEqual(false, lists:member(testFsm, registered()));
run_startstop(shutdown) ->
    Result = a_jhn_fsm:start(testFsm, shutdown, tester, node(), []),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    exit(Pid, shutdown),
    ?assertEqual({terminate, shutdown}, wait()),
    timer:sleep(10),
    ?assertEqual(false, is_process_alive(Pid)),
    Result1 = a_jhn_fsm:start(testFsm, shutdown, tester, node(), []),
    ?assertMatch({ok, _}, Result1),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid1 = whereis(testFsm),
    exit(Pid1, {shutdown, 5000}),
    ?assertEqual({terminate, {shutdown, 5000}}, wait()),
    timer:sleep(100),
    ?assertEqual(false, is_process_alive(Pid1));
run_startstop(stoperror) ->
    Result = a_jhn_fsm:start(testFsm, simple, tester, node(), []),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    ?assertEqual(ok, a_jhn_fsm:event(Pid, {stop, anError})),
    ?assertEqual({terminate, anError}, wait()),
    timer:sleep(10),
    ?assertEqual(false, is_process_alive(Pid)),
    Result1 = a_jhn_fsm:start(testFsm, simple, tester, node(), []),
    ?assertMatch({ok, _}, Result1),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid1 = whereis(testFsm),
    Pid1 ! {stop, anError},
    ?assertEqual({terminate, anError}, wait()),
    timer:sleep(10),
    ?assertEqual(false, is_process_alive(Pid));
run_startstop(systerminate) ->
    Result = a_jhn_fsm:start(testFsm, shutdown, tester, node(), []),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    sys:suspend(testFsm),
    exit(Pid, shutdown),
    ?assertEqual({terminate, shutdown}, wait()),
    timer:sleep(10),
    ?assertEqual(false, is_process_alive(Pid)).

%%%-------------------------------------------------------------------
% Error
%%%-------------------------------------------------------------------
error_test_() ->
    {setup,
     fun setup_error/0,
     fun cleanup_error/1,
     {inorder, [{timeout, 120, {"Badeturn(call)",
                                ?_test(run_error(badreturncall))}},
                {timeout, 120, {"Badeturn(info)",
                                ?_test(run_error(badreturninfo))}},
                {timeout, 120, {"NodeDown",
                                ?_test(run_error(nodedown))}},
                {timeout, 120, {"Terminate(exit)",
                                ?_test(run_error(terminateexit))}},
                {timeout, 120, {"Terminate(throw)",
                                ?_test(run_error(terminatethrow))}},
                {timeout, 120, {"Code change",
                                ?_test(run_error(codechange))}},
                {timeout, 120, {"Unexpected Call",
                                ?_test(run_error(unexpectedcall))}},
                {timeout, 120, {"Unexpected Event",
                                ?_test(run_error(unexpectedevent))}},
                {timeout, 120, {"Unexpected Message",
                                ?_test(run_error(unexpectedmessage))}},
                {timeout, 120, {"Call non existing fsm",
                                ?_test(run_error(callnofsm))}},
                {timeout, 120, {"Event non existing fsm",
                                ?_test(run_error(eventnofsm))}}
               ]}
    }.

setup_error() ->
    os:cmd("erl -sname test -run erlang halt"),
    Node = list_to_atom("fsm_test@" ++ host()),
    {ok, _} = net_kernel:start([Node, shortnames]),
    ok.

cleanup_error(_) ->
    net_kernel:stop(),
    ok.

run_error(badreturncall) ->
    register(tester, self()),
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    Flag = process_flag(trap_exit, true),
    ?assertExit({bad_return_value, {bad, call}},
                 a_jhn_fsm:call(testFsm, badreturn)),
    ?assertEqual({terminate, {bad_return_value, {bad, call}}}, wait()),
    ?assertEqual({'EXIT', Pid, {bad_return_value, {bad, call}}}, wait()),
    process_flag(trap_exit, Flag);
run_error(badreturninfo) ->
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    Flag = process_flag(trap_exit, true),
    Pid ! badreturn,
    ?assertEqual({terminate, {bad_return_value, {bad, call}}}, wait()),
    ?assertEqual({'EXIT', Pid, {bad_return_value, {bad, call}}}, wait()),
    process_flag(trap_exit, Flag);
run_error(nodedown) ->
    NoNode = list_to_atom("does_not_exist@" ++ host()),
    ?assertExit(noconnection,
                a_jhn_fsm:call({notExist, NoNode}, anything));
run_error(terminateexit) ->
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    Flag = process_flag(trap_exit, true),
    a_jhn_fsm:event(Pid, {terminate, exit}),
    ?assertEqual({'EXIT', Pid, {terminated, exit}}, wait()),
    process_flag(trap_exit, Flag);
run_error(terminatethrow) ->
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    Flag = process_flag(trap_exit, true),
    a_jhn_fsm:event(Pid, {terminate, throw}),
    ?assertEqual({'EXIT', Pid, {terminated, throw}}, wait()),
    process_flag(trap_exit, Flag);
run_error(codechange) ->
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    sys:suspend(Pid),
    ?assertEqual({error, {'EXIT', die}},
                 sys:change_code(Pid, a_jhn_fsm, old, die)),
    sys:resume(Pid),
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {stop, normal})),
    ?assertEqual({terminate, normal}, wait());
run_error(unexpectedcall) ->
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    ?assertExit(timeout, a_jhn_fsm:call(testFsm, unexpectedcall, 100)),
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {stop, normal})),
    ?assertEqual({terminate, normal}, wait());
run_error(unexpectedevent) ->
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, unexpectedevent)),
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {hibernate, self()})),
    ?assertEqual(hibernate, wait()),
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, unexpectedevent)),
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {stop, normal})),
    ?assertEqual({terminate, normal}, wait());
run_error(unexpectedmessage) ->
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    testFsm ! unexpectedMessage,
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {hibernate, self()})),
    ?assertEqual(hibernate, wait()),
    testFsm ! unexpectedMessage,
    ?assertEqual(ok, a_jhn_fsm:event(testFsm, {stop, normal})),
    ?assertEqual({terminate, normal}, wait());
run_error(callnofsm) ->
    ?assertExit(noproc, jhn_fsm:call(does_not_exist, hello));
run_error(eventnofsm) ->
    ?assertEqual(ok, a_jhn_fsm:event(does_not_exist, hello)).

%%%-------------------------------------------------------------------
% Coverage
%%%-------------------------------------------------------------------
coverage_test_() ->
    {setup,
     fun setup_coverage/0,
     fun cleanup_coverage/1,
     {inorder, [{timeout, 120, {"Format Status",
                                ?_test(run_coverage(format_status))}},
                {timeout, 120, {"Behaviour Info",
                                ?_test(run_coverage(behaviour_info))}}
               ]}
    }.

setup_coverage() ->
    ok.

cleanup_coverage(_) ->
    ok.

run_coverage(format_status) ->
    register(tester, self()),
    Result = a_jhn_fsm:start_link(testFsm, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testFsm, registered())),
    Pid = whereis(testFsm),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    PDict = process_info(Pid, dictionary),
    {state, State} = a_jhn_fsm:call(Pid, get_state),
    TheState = #state{name = the_name, mod = a_jhn_fsm, state_name = first,
                      data = State, event = false, message = false,
                      terminate = false, code_change = false,
                      format_status = false, deferred = {[], []},
                      handling_deferred = false},
    jhn_fsm:format_status(none, [PDict, TheState]),
    TheState1 = #state{name = self(), mod = will_not_exist, state_name = first,
                      data = State, event = false, message = false,
                      terminate = false, code_change = false,
                      format_status = false, deferred = {[], []},
                      handling_deferred = false},
    jhn_fsm:format_status(none, [PDict, TheState1]),
    TheState2 = #state{name = self(), mod = a_jhn_fsm, state_name = first,
                      data = State, event = false, message = false,
                      terminate = false, code_change = false,
                      format_status = false, deferred = {[], []},
                      handling_deferred = false},
    jhn_fsm:format_status(fail, [PDict, TheState2]);
run_coverage(behaviour_info) ->
    jhn_fsm:behaviour_info(callbacks).

%%%-------------------------------------------------------------------
% States
%%%-------------------------------------------------------------------
states_test_() ->
    {setup,
     fun setup_states/0,
     fun cleanup_states/1,
     {inorder, [{timeout, 120, {"Ring", ?_test(run_states(ring))}},
                {timeout, 120, {"Cross", ?_test(run_states(cross))}},
                {timeout, 120, {"Hibernate", ?_test(run_states(hibernate))}},
                {timeout, 120, {"Deferred", ?_test(run_states(defer))}}
               ]}
    }.

setup_states() ->
    b_jhn_fsm:start(),
    ok.

cleanup_states(_) ->
    b_jhn_fsm:stop(),
    ok.

run_states(ring) ->
    register(tester, self()),
    ?assertEqual(first, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, second})),
    ?assertEqual(second, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, third})),
    ?assertEqual(third, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, first})),
    ?assertEqual(first, b_jhn_fsm:call(b_jhn_fsm, get_state));
run_states(cross) ->
    ?assertEqual(first, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, third})),
    ?assertEqual(third, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, second})),
    ?assertEqual(second, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, first})),
    ?assertEqual(first, b_jhn_fsm:call(b_jhn_fsm, get_state));
run_states(hibernate) ->
    ?assertEqual(first, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(hibernate, b_jhn_fsm:call(b_jhn_fsm, hibernate)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, third})),
    ?assertEqual(third, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {hibernate, self()})),
    ?assertEqual(hibernate, wait()),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, second})),
    ?assertEqual(second, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    b_jhn_fsm ! hibernate,
    ?assertEqual(hibernate, wait()),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, first})),
    ?assertEqual(first, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {event_hibernate, self()})),
    ?assertEqual(hibernate, wait()),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, second})),
    ?assertEqual(second, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, first}));
run_states(defer) ->
    ?assertEqual(first, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {bounce, first})),
    ?assertEqual(first, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, second})),
    ?assertEqual(first, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    b_jhn_fsm ! {bounce, first},
    b_jhn_fsm ! {bounce, first},
    ?assertEqual(first, b_jhn_fsm:call(b_jhn_fsm, get_state)),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, second})),
    ?assertEqual(ok, b_jhn_fsm:event(b_jhn_fsm, {goto, second})),
    ?assertEqual(first, b_jhn_fsm:call(b_jhn_fsm, get_state)).




%% ===================================================================
%% Internal functions.
%% ===================================================================

wait() -> wait(10000).

wait(Time) -> receive Y -> Y after Time -> timeout end.

host() ->
    [Host | _] = string:tokens(net_adm:localhost(), "."),
    Host.
