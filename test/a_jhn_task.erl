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
%%%   A utility module for the jhn_task_tests unit test module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(a_jhn_task).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').
-behaviour(jhn_task).

%% Management API
-export([start_link/0, start_link/3, start_link/4,
         start/4
        ]).

%% API
-export([cast/2]).

%% jhn_task callbacks
-export([init/1, do/1]).

%% Records
-record(state, {controller, type}).


%%====================================================================
%% Management API
%%====================================================================

start_link() ->
    jhn_task:create(?MODULE).

start_link(Type, TestName, TestNode) ->
    jhn_task:create(?MODULE, [{arg, {Type, TestName, TestNode}}]).

start_link(Type, TestName, TestNode, Opts) ->
    jhn_task:create(?MODULE, [{arg, {Type, TestName, TestNode}} | Opts]).


start(Type, TestName, TestNode, Opts) ->
    jhn_task:create(?MODULE,
                     [{arg, {Type, TestName, TestNode}},
                      {link, false} | Opts
                     ]).

%%====================================================================
%% API
%%====================================================================

cast(Task, Msg) -> Task ! Msg, ok.

%%====================================================================
%% jhn_task callbacks
%%====================================================================

init(no_arg) -> ignore;
init({badreturn, _TestName, _TestNode}) ->
    {uk, state};
init({exitinit, _TestName, _TestNode}) ->
    exit(exitinit);
init({dieinit, _TestName, _TestNode}) ->
    exit(self(), fail),
    {ok, #state{}};
init({shutdown, TestName, TestNode}) ->
    process_flag(trap_exit, true),
    Controller = {TestName, TestNode},
    Controller ! init,
    {ok, #state{type = shutdown, controller = Controller}};
init({timeout, TestName, TestNode}) ->
    Controller = {TestName, TestNode},
    Controller ! timeout,
    timer:sleep(10000),
    {ok, #state{}};
init({stop, TestName, TestNode}) ->
    Controller = {TestName, TestNode},
    Controller ! {stop, self()},
    {stop, stopped};
init({ignore, TestName, TestNode}) ->
    Controller = {TestName, TestNode},
    Controller ! {ignore, self()},
    ignore;
init({Type, TestName, TestNode}) ->
    Controller = {TestName, TestNode},
    Controller ! init,
    {ok, #state{type = Type, controller = Controller}}.

do(State) ->
    receive
        {stop, normal} ->
            inform({terminate, normal}, State),
            {ok, State};
        {stop, anError} ->
            inform({terminate, anError}, State),
            {stop, anError};
        exit ->
            {error, {terminated, exit}};
        throw ->
            {error, {terminated, throw}};
        {'EXIT', _, shutdown} ->
            inform({terminate, shutdown}, State),
            {stop, shutdown};
        {'EXIT', _, Shutdown = {shutdown, _}} ->
            inform({terminate, Shutdown}, State),
            {stop, Shutdown}
    after 1000 -> exit(timeout)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

inform(What, State) ->
    State#state.controller ! What.
