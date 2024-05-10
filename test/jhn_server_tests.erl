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
%%%   eunit unit tests for the jhn_server behaviour.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_server_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-record(state, {parent, name, mod, data, hibernated,
                handle_msg, terminate, code_change, format_status}).

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
                {timeout, 120, {"Call", ?_test(run_simple(call))}},
                {timeout, 120, {"Sync", ?_test(run_simple(sync))}},
                {timeout, 120, {"Cast", ?_test(run_simple(cast))}},
                {timeout, 120, {"ABCast", ?_test(run_simple(abcast))}},
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

run_simple(created) ->
    register(tester, self()),
    Result = a_jhn_server:start_link(testServer, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links));
run_simple(call) ->
    Pid = whereis(testServer),
    ?assertEqual({reply, aa}, a_jhn_server:call(testServer, {reply, aa})),
    ?assertEqual({reply, bb}, a_jhn_server:call(Pid, {reply, bb})),
    ?assertEqual({reply, cc},
                 a_jhn_server:call({testServer, node()}, {reply, cc})),
    ?assertError(badarg, a_jhn_server:call(1, {reply, cc})),
    ?assertError(badarg, a_jhn_server:call({testServer, 1}, {reply, cc})),
    ?assertError(badarg, a_jhn_server:call({1, node()}, {reply, cc})),
    ?assertError(badarg, a_jhn_server:call(testServer, {reply, aa}, error)),
    ?assertError(badarg, a_jhn_server:call(testServer, {reply, aa}, -1)),
    ?assertError(badarg, a_jhn_server:call(testServer, {reply, aa}, 0));
run_simple(sync) ->
    Pid = whereis(testServer),
    ?assertEqual(ok, a_jhn_server:sync(testServer)),
    ?assertEqual(ok, a_jhn_server:sync(Pid)),
    ?assertEqual(ok, a_jhn_server:sync({testServer, node()})),
    ?assertError(badarg, a_jhn_server:sync(1)),
    ?assertError(badarg, a_jhn_server:sync({testServer, 1})),
    ?assertError(badarg, a_jhn_server:sync({1, node()})),
    ?assertError(badarg, a_jhn_server:sync(testServer, error)),
    ?assertError(badarg, a_jhn_server:sync(testServer, -1)),
    ?assertError(badarg, a_jhn_server:sync(testServer, 0));
run_simple(cast) ->
    Pid = whereis(testServer),
    ?assertEqual(ok, a_jhn_server:cast(testServer, {reply, self(), aa})),
    ?assertEqual({reply, aa}, wait()),
    ?assertEqual(ok, a_jhn_server:cast(Pid, {reply, self(), bb})),
    ?assertEqual({reply, bb}, wait()),
    ?assertEqual(ok, a_jhn_server:cast({testServer, node()},{reply,self(),cc})),
    ?assertEqual({reply, cc}, wait()),
    ?assertError(badarg, a_jhn_server:cast(1, {reply, cc})),
    ?assertError(badarg, a_jhn_server:cast({1, node()}, {reply, cc})),
    ?assertError(badarg, a_jhn_server:cast({testServer, 1}, {reply, cc}));
run_simple(abcast) ->
    ?assertEqual(ok, a_jhn_server:abcast(testServer, {reply, self(), aa})),
    ?assertEqual({reply, aa}, wait()),
    ?assertEqual(ok, a_jhn_server:abcast([node()],
                                         testServer,
                                         {reply,self(),aa})),
    ?assertEqual({reply, aa}, wait()),
    ?assertEqual(ok,
                 a_jhn_server:abcast([node(), node()],
                                     testServer,
                                     {reply, self(), aa})),
    ?assertEqual({reply, aa}, wait()),
    ?assertEqual({reply, aa}, wait()),
    ?assertError(badarg, a_jhn_server:abcast(1, {reply, aa})),
    ?assertError(badarg, a_jhn_server:abcast([node()], 1, {reply, aa})),
    ?assertError(badarg,
                 a_jhn_server:abcast([node(), 1], testServer, {reply, aa}));
run_simple(info) ->
    testServer ! {reply, dd},
    ?assertEqual({reply, dd}, wait());
run_simple(reply) ->
    ?assertEqual({error, not_a_call}, jhn_server:reply(broken));
run_simple(from) ->
    ?assertEqual({error, not_a_call}, jhn_server:from()),
    ?assertEqual({reply, ff}, a_jhn_server:call(testServer, {ereply, ff}));
run_simple(sys) ->
    sys:suspend(testServer),
    ?assertEqual(ok, a_jhn_server:cast(testServer, {reply, self(), aa})),
    ?assertEqual(timeout, wait(100)),
    sys:resume(testServer),
    ?assertEqual({reply, aa}, wait(100)),
    ?assertMatch({status, _, _, _}, sys:get_status(testServer)),
    sys:suspend(testServer),
    sys:change_code(testServer, a_jhn_server, old, none),
    ?assertEqual(code_change, wait(100)),
    sys:resume(testServer);
run_simple(hibernate) ->
    ?assertEqual(ok, a_jhn_server:cast(testServer, {hibernate, self()})),
    ?assertEqual(hibernate, wait()),
    ?assertEqual({reply, ff}, a_jhn_server:call(testServer, {reply, ff})),
    ?assertEqual(ok, a_jhn_server:cast(testServer, {hibernate, self()})),
    ?assertEqual(hibernate, wait()),
    ?assertEqual(ok, a_jhn_server:cast(testServer, {reply, self(), ff})),
    ?assertEqual({reply, ff}, wait()),
    ?assertEqual(hibernate, a_jhn_server:call(testServer, hibernate, 100)),
    ?assertEqual({reply, ff}, a_jhn_server:call(testServer, {reply, ff})),
    ?assertEqual(hibernate, a_jhn_server:call(testServer, hibernate, 100)),
    ?assertEqual(ok, a_jhn_server:cast(testServer, {reply, self(), ff})),
    ?assertEqual({reply, ff}, wait()),
    ?assertEqual(hibernate, a_jhn_server:call(testServer, hibernate, 100)),
    sys:suspend(testServer),
    sys:resume(testServer),
    testServer ! hibernate,
    ?assertEqual(hibernate, wait());
run_simple(stop) ->
    ?assertEqual(ok, a_jhn_server:cast(testServer, {stop, normal})),
    ?assertEqual({terminate, normal}, wait()).

%%%-------------------------------------------------------------------
% Createstop
%%%-------------------------------------------------------------------
createstop_test_() ->
    {setup,
     fun setup_createstop/0,
     fun cleanup_createstop/1,
     {inorder, [{timeout, 120, {"Hibernate",?_test(run_createstop(hibernate))}},
                {timeout, 120, {"Call", ?_test(run_simple(call))}},
                {timeout, 120, {"Sync", ?_test(run_simple(sync))}},
                {timeout, 120, {"Stop(Normal)", ?_test(run_simple(stop))}},
                {timeout, 120, {"Unlinked", ?_test(run_createstop(unlinked))}},
                {timeout, 120, {"Call", ?_test(run_simple(call))}},
                {timeout, 120, {"Sync", ?_test(run_simple(sync))}},
                {timeout, 120, {"Stop(Normal)", ?_test(run_simple(stop))}},
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
                {timeout, 120, {"Created", ?_test(run_createstop(created))}},
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
                                ?_test(run_createstop(stoperror))}},
                {timeout, 120, {"SYS(terminate)",
                                ?_test(run_createstop(systerminate))}}
               ]}
    }.

setup_createstop() ->
    ok.

cleanup_createstop(_) ->
    ok.

run_createstop(hibernate) ->
    register(tester, self()),
    Result = a_jhn_server:start_link(testServer, hibernate, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links));
run_createstop(unlinked) ->
    Result = a_jhn_server:start(testServer, hibernate, tester, node(), []),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(false, lists:member(Pid, Links));
run_createstop(ignore) ->
    Result = a_jhn_server:start_link(testServer, ignore, tester, node()),
    ?assertMatch(ignore, Result),
    {ignore, Pid} = wait(),
    ?assertEqual(false, lists:member(testServer, registered())),
    {links, Links} = process_info(self(), links),
    ?assertEqual(false, lists:member(Pid, Links));
run_createstop(stop) ->
    Result = a_jhn_server:start_link(testServer, stop, tester, node()),
    ?assertMatch({error, stopped}, Result),
    {stop, Pid} = wait(),
    ?assertEqual(false, lists:member(testServer, registered())),
    {links, Links} = process_info(self(), links),
    ?assertEqual(false, lists:member(Pid, Links));
run_createstop(badopt) ->
    Result = a_jhn_server:start_link(testServer, simple, tester, node(),
                                     [{link, yes}]),
    ?assertEqual({error, [{link, yes}]}, Result),
    ?assertEqual(false, lists:member(testServer, registered()));
run_createstop(badreturninit) ->
    Result = a_jhn_server:start_link(testServer, badreturn, tester, node()),
    ?assertEqual({error, {bad_return_value, {uk, state}}}, Result),
    ?assertEqual(false, lists:member(testServer, registered()));
run_createstop(exitinit) ->
    Result = a_jhn_server:start_link(testServer, exitinit, tester, node()),
    ?assertMatch({error, {exit, exitinit, _}}, Result),
    ?assertEqual(false, lists:member(testServer, registered()));
run_createstop(dieinit) ->
    Result = a_jhn_server:start(testServer, dieinit, tester, node(), []),
    ?assertMatch({error, fail}, Result),
    ?assertEqual(false, lists:member(testServer, registered())),
    Flag = process_flag(trap_exit, true),
    Result = a_jhn_server:start_link(testServer, dieinit, tester, node()),
    ?assertMatch({error, fail}, Result),
    ?assertEqual(false, lists:member(testServer, registered())),
    process_flag(trap_exit, Flag);
run_createstop(created) ->
    unregister(tester),
    register(testServer, self()),
    Result = a_jhn_server:start_link(testServer, simple, tester, node()),
    ?assertEqual({error, {already_created, testServer, self()}}, Result),
    Result1 = a_jhn_server:start_link(testServer, hibernate, tester, node()),
    ?assertEqual({error, {already_created, testServer, self()}}, Result1),
    unregister(testServer),
    register(tester, self());
run_createstop(noname) ->
    Result = a_jhn_server:start_link(simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(false, lists:member(testServer, registered())),
    {ok, Pid} = Result,
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    ?assertEqual(ok, a_jhn_server:cast(Pid, {stop, normal})),
    ?assertEqual({terminate, normal}, wait());
run_createstop(noarg) ->
    Result = a_jhn_server:start_link(),
    ?assertMatch(ignore, Result),
    ?assertEqual(false, lists:member(testServer, registered()));
run_createstop(timerinf) ->
    Result = a_jhn_server:start_link(testServer, simple, tester, node(),
                                    [{timeout, infinity}]),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links));
run_createstop(timerint) ->
    Result = a_jhn_server:start_link(testServer, simple, tester, node(),
                                    [{timeout, 5000}]),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links));
run_createstop(timeout) ->
    Result = a_jhn_server:start_link(testServer, timeout, tester, node(),
                                    [{timeout, 100}]),
    ?assertMatch({error, timeout}, Result),
    ?assertEqual(timeout, wait()),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(testServer));
run_createstop(shutdown) ->
    Result = a_jhn_server:start(testServer, shutdown, tester, node(), []),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    exit(Pid, shutdown),
    ?assertEqual({terminate, shutdown}, wait()),
    ?assertEqual(false, is_process_alive(Pid)),
    Result1 = a_jhn_server:start(testServer, shutdown, tester, node(), []),
    ?assertMatch({ok, _}, Result1),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid1 = whereis(testServer),
    exit(Pid1, {shutdown, 5000}),
    ?assertEqual({terminate, {shutdown, 5000}}, wait()),
    timer:sleep(100),
    ?assertEqual(false, is_process_alive(Pid1));
run_createstop(stoperror) ->
    Result = a_jhn_server:start(testServer, simple, tester, node(), []),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    ?assertEqual(ok, a_jhn_server:cast(Pid, {stop, anError})),
    ?assertEqual({terminate, anError}, wait()),
    timer:sleep(1),
    ?assertEqual(false, is_process_alive(Pid)),
    Result1 = a_jhn_server:start(testServer, simple, tester, node(), []),
    ?assertMatch({ok, _}, Result1),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid1 = whereis(testServer),
    Pid1 ! {stop, anError},
    ?assertEqual({terminate, anError}, wait()),
    ?assertEqual(false, is_process_alive(Pid));
run_createstop(systerminate) ->
    Result = a_jhn_server:start(testServer, shutdown, tester, node(), []),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    sys:suspend(testServer),
    exit(Pid, shutdown),
    ?assertEqual({terminate, shutdown}, wait()),
    ?assertEqual(false, is_process_alive(Pid)).

%%%-------------------------------------------------------------------
% Error
%%%-------------------------------------------------------------------
error_test_() ->
    {setup,
     fun setup_error/0,
     fun cleanup_error/1,
     {inorder, [{timeout, 120, {"Badreturn(call)",
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
                {timeout, 120, {"Unexpected Cast",
                                ?_test(run_error(unexpectedcast))}},
                {timeout, 120, {"Unexpected Message",
                                ?_test(run_error(unexpectedmessage))}},
                {timeout, 120, {"Call non existing server",
                                ?_test(run_error(callnoserver))}},
                {timeout, 120, {"Cast non existing server",
                                ?_test(run_error(castnoserver))}}
               ]}
    }.

setup_error() ->
    os:cmd("erl -sname test -run erlang halt"),
    Node = list_to_atom("server_test@" ++ host()),
    {ok, _} = net_kernel:start([Node, shortnames]),
    ok.

cleanup_error(_) ->
    net_kernel:stop(),
    ok.

run_error(badreturncall) ->
    register(tester, self()),
    Result = a_jhn_server:start_link(testServer, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    Flag = process_flag(trap_exit, true),
    ?assertExit({bad_return_value, {bad, call}},
                 a_jhn_server:call(testServer, badreturn)),
    ?assertEqual({terminate, {bad_return_value, {bad, call}}}, wait()),
    ?assertEqual({'EXIT', Pid, {bad_return_value, {bad, call}}}, wait()),
    process_flag(trap_exit, Flag);
run_error(badreturninfo) ->
    Result = a_jhn_server:start_link(testServer, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
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
                a_jhn_server:call({notExist, NoNode}, anything));
run_error(terminateexit) ->
    Result = a_jhn_server:start_link(testServer, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    Flag = process_flag(trap_exit, true),
    a_jhn_server:cast(Pid, {terminate, exit}),
    ?assertEqual({'EXIT', Pid, {terminated, exit}}, wait()),
    process_flag(trap_exit, Flag);
run_error(terminatethrow) ->
    Result = a_jhn_server:start_link(testServer, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    Flag = process_flag(trap_exit, true),
    a_jhn_server:cast(Pid, {terminate, throw}),
    ?assertEqual({'EXIT', Pid, {terminated, throw}}, wait()),
    process_flag(trap_exit, Flag);
run_error(codechange) ->
    Result = a_jhn_server:start_link(testServer, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    sys:suspend(Pid),
    ?assertEqual({error, {'EXIT', die}},
                 sys:change_code(Pid, a_jhn_server, old, die)),
    sys:resume(Pid),
    ?assertEqual(ok, a_jhn_server:cast(testServer, {stop, normal})),
    ?assertEqual({terminate, normal}, wait());
run_error(unexpectedcall) ->
    Result = a_jhn_server:start_link(testServer, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    ?assertExit(timeout, a_jhn_server:call(testServer, unexpectedcall, 100)),
    ?assertEqual(ok, a_jhn_server:cast(testServer, {stop, normal})),
    ?assertEqual({terminate, normal}, wait());
run_error(unexpectedcast) ->
    Result = a_jhn_server:start_link(testServer, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    ?assertEqual(ok, a_jhn_server:cast(testServer, unexpectedcast)),
    ?assertEqual(ok, a_jhn_server:cast(testServer, {hibernate, self()})),
    ?assertEqual(hibernate, wait()),
    ?assertEqual(ok, a_jhn_server:cast(testServer, unexpectedcast)),
    ?assertEqual(ok, a_jhn_server:cast(testServer, {stop, normal})),
    ?assertEqual({terminate, normal}, wait());
run_error(unexpectedmessage) ->
    Result = a_jhn_server:start_link(testServer, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    testServer ! unexpectedMessage,
    ?assertEqual(ok, a_jhn_server:cast(testServer, {hibernate, self()})),
    ?assertEqual(hibernate, wait()),
    testServer ! unexpectedMessage,
    ?assertEqual(ok, a_jhn_server:cast(testServer, {stop, normal})),
    ?assertEqual({terminate, normal}, wait());
run_error(callnoserver) ->
    ?assertExit(noproc, jhn_server:call(does_not_exist, hello));
run_error(castnoserver) ->
    ?assertEqual(ok, jhn_server:cast(does_not_exist, hello)).

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
    Result = a_jhn_server:start_link(testServer, simple, tester, node()),
    ?assertMatch({ok, _}, Result),
    ?assertEqual(init, wait()),
    ?assertEqual(true, lists:member(testServer, registered())),
    Pid = whereis(testServer),
    {links, Links} = process_info(self(), links),
    ?assertEqual(true, lists:member(Pid, Links)),
    PDict = process_info(Pid, dictionary),
    {state, State} = a_jhn_server:call(Pid, get_state),
    TheState =
        #state{parent = the_parent, name = the_name, mod = a_jhn_server,
               data = State, hibernated = false,
               handle_msg = true,
               terminate = true,
               code_change = true,
               format_status = true},
    jhn_server:format_status(none, [PDict, sysState, parent, debug, TheState]),
    TheState1 =
        #state{parent = the_parent, name = self(), mod = will_not_exist,
               data = State, hibernated = false,
               handle_msg = true,
               terminate = true,
               code_change = true,
               format_status = true},
    jhn_server:format_status(none, [PDict, sysState, parent, debug, TheState1]),
    TheState2 =
        #state{parent = the_parent, name = self(), mod = a_jhn_server,
               data = State, hibernated = false,
               handle_msg = true,
               terminate = true,
               code_change = true,
               format_status = true},
    jhn_server:format_status(fail, [PDict, sysState, parent, debug, TheState2]);
run_coverage(behaviour_info) ->
    jhn_server:behaviour_info(callbacks).



%% ===================================================================
%% Internal functions.
%% ===================================================================

wait() -> wait(10000).

wait(Time) -> receive Y -> Y after Time -> timeout end.

host() ->
    [Host | _] = string:tokens(net_adm:localhost(), "."),
    Host.
