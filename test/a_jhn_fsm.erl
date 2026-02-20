%%==============================================================================
%% Copyright 2013-2026 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   A utility module for the jhn_fsm_tests unit test module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(a_jhn_fsm).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

-behaviour(jhn_fsm).

%% Management API
-export([start_link/0, start_link/3, start_link/4, start_link/5,
         start/5
        ]).

%% API
-export([call/2, call/3,
         event/2
        ]).

%% jhn_fsm callbacks
-export([init/1,
%        event/3,
         message/3,
         terminate/3,
         code_change/4
        ]).

%% jhn_fsm state callbacks
-export([first/2]).

%% Records
-record(state, {controller,
                type
               }).

%%====================================================================
%% Management API
%%====================================================================

start_link() ->
    jhn_fsm:create(?MODULE).

start_link(Type, TestName, TestNode) ->
    jhn_fsm:create(?MODULE, [{arg, {undefined, Type, TestName, TestNode}}]).


start_link(Name, Type, TestName, TestNode) ->
    jhn_fsm:create(?MODULE,
                   [{arg, {Name, Type, TestName, TestNode}}, {name, Name} ]).

start_link(Name, Type, TestName, TestNode, Opts) ->
    jhn_fsm:create(?MODULE,
                   [{arg, {Name, Type, TestName, TestNode}},
                    {name, Name} | Opts
                   ]).

start(Name, Type, TestName, TestNode, Opts) ->
    jhn_fsm:create(?MODULE,
                     [{arg, {Name, Type, TestName, TestNode}},
                      {name, Name},
                      {link, false} | Opts
                     ]).

%%====================================================================
%% API
%%====================================================================

call(Server, Msg) -> jhn_fsm:call(Server, Msg).

call(Server, Msg, Timeout) -> jhn_fsm:call(Server, Msg, Timeout).

event(Server, Msg) -> jhn_fsm:cast(Server, Msg).

%%====================================================================
%% jhn_server callbacks
%%====================================================================

init(no_arg) -> ignore;
init({_Name, badreturn, _TestName, _TestNode}) ->
    {uk, state};
init({_Name, exitinit, _TestName, _TestNode}) ->
    exit(exitinit);
init({_Name, dieinit, _TestName, _TestNode}) ->
    exit(self(), fail),
    {ok, first, #state{}};
init({_Name, shutdown, TestName, TestNode}) ->
    process_flag(trap_exit, true),
    Controller = {TestName, TestNode},
    Controller ! init,
    {ok, first, #state{type = shutdown, controller = Controller}};
init({_Name, timeout, TestName, TestNode}) ->
    Controller = {TestName, TestNode},
    Controller ! timeout,
    timer:sleep(10000),
    {ok, first, #state{}};
init({_Name, stop, TestName, TestNode}) ->
    Controller = {TestName, TestNode},
    Controller ! {stop, self()},
    {stop, stopped};
init({_Name, ignore, TestName, TestNode}) ->
    Controller = {TestName, TestNode},
    Controller ! {ignore, self()},
    ignore;
init({_Name, hibernate, TestName, TestNode}) ->
    Controller = {TestName, TestNode},
    Controller ! init,
    {hibernate, first, #state{type = hibernate, controller = Controller}};
init({_Name, Type, TestName, TestNode}) ->
    Controller = {TestName, TestNode},
    Controller ! init,
    {ok, first, #state{type = Type, controller = Controller}}.

message({reply, X}, Name, State) ->
    inform({reply, X}, State),
    {ok, Name, State};
message({stop, Reason}, _, _) ->
    {stop, Reason};
message(badreturn, _, _) ->
    {bad, call};
message(hibernate, Name, State) ->
    inform(hibernate, State),
    {hibernate, Name, State}.

terminate(exit, _, _) ->
    exit({terminated, exit});
terminate(throw, _, _) ->
    throw({terminated, throw});
terminate(Reason, _, State) ->
    inform({terminate, Reason}, State),
    ok.

code_change(_OldVsn, Name, State, die) ->
    exit(die),
    {ok, Name, State};
code_change(_OldVsn, Name, State, _Extra) ->
    inform(code_change, State),
    {ok, Name, State}.

%%====================================================================
%% jhn_fsm state callbacks
%%====================================================================

first(get_state, State) ->
    jhn_fsm:reply({state, State}),
    {ok, first, State};
first({reply, X}, State) ->
    jhn_fsm:reply({reply, X}),
    {ok, first, State};
first({reply, From, X}, State) ->
    From ! {reply, X},
    {ok, first, State};
first({ereply, X}, State) ->
    jhn_fsm:reply(jhn_fsm:from(), {reply, X}),
    {ok, first, State};
first(hibernate, State) ->
    jhn_fsm:reply(hibernate),
    {hibernate, first, State};
first({hibernate, From}, State) ->
    From! hibernate,
    {hibernate, first, State};
first({stop, Reason}, _) ->
    {stop, Reason};
first(badreturn, _) ->
    {bad, call};
first({terminate, How}, _) ->
    {stop, How}.

%%====================================================================
%% Internal functions
%%====================================================================

inform(What, State) ->
    State#state.controller ! What.
