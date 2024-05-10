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
%%%   A utility module for the jhn_server_tests unit test module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(a_jhn_server).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').
-behaviour(jhn_server).

%% Management API
-export([start_link/0, start_link/3, start_link/4, start_link/5,
         start/5
        ]).

%% API
-export([call/2, call/3,
         sync/1, sync/2,
         cast/2, abcast/2, abcast/3
        ]).

%% jhn_server callbacks
-export([init/1,
         request/2,
         message/2,
         terminate/2,
         code_change/3,
         format_status/2
        ]).

%% Records
-record(state, {controller,
                type
               }).


%%====================================================================
%% Management API
%%====================================================================

start_link() ->
    jhn_server:create(?MODULE).

start_link(Type, TestName, TestNode) ->
    jhn_server:create(?MODULE, [{arg, {undefined, Type, TestName, TestNode}}]).


start_link(Name, Type, TestName, TestNode) ->
    jhn_server:create(?MODULE,
                     [{arg, {Name, Type, TestName, TestNode}}, {name, Name} ]).

start_link(Name, Type, TestName, TestNode, Opts) ->
    jhn_server:create(?MODULE,
                     [{arg, {Name, Type, TestName, TestNode}},
                      {name, Name} | Opts
                     ]).

start(Name, Type, TestName, TestNode, Opts) ->
    jhn_server:create(?MODULE,
                     [{arg, {Name, Type, TestName, TestNode}},
                      {name, Name},
                      {link, false} | Opts
                     ]).

%%====================================================================
%% API
%%====================================================================

call(Server, Msg) -> jhn_server:call(Server, Msg).

call(Server, Msg, Timeout) -> jhn_server:call(Server, Msg, Timeout).

sync(Server) -> jhn_server:sync(Server).

sync(Server, Timeout) -> jhn_server:sync(Server, Timeout).

cast(Server, Msg) -> jhn_server:cast(Server, Msg).

abcast(Server, Msg) -> jhn_server:abcast(Server, Msg).

abcast(Nodes, Server, Msg) -> jhn_server:abcast(Nodes, Server, Msg).

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
    {ok, #state{}};
init({_Name, shutdown, TestName, TestNode}) ->
    process_flag(trap_exit, true),
    Controller = {TestName, TestNode},
    Controller ! init,
    {ok, #state{type = shutdown, controller = Controller}};
init({_Name, timeout, TestName, TestNode}) ->
    Controller = {TestName, TestNode},
    Controller ! timeout,
    timer:sleep(10000),
    {ok, #state{}};
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
    {hibernate, #state{type = hibernate, controller = Controller}};
init({_Name, Type, TestName, TestNode}) ->
    Controller = {TestName, TestNode},
    Controller ! init,
    {ok, #state{type = Type, controller = Controller}}.

request(get_state, State) ->
    jhn_server:reply({state, State}),
    {ok, State};
request({reply, X}, State) ->
    jhn_server:reply({reply, X}),
    {ok, State};
request({reply, From, X}, State) ->
    From ! {reply, X},
    {ok, State};
request({ereply, X}, State) ->
    jhn_server:reply(jhn_server:from(), {reply, X}),
    {ok, State};
request(hibernate, State) ->
    jhn_server:reply(hibernate),
    {hibernate, State};
request({hibernate, From}, State) ->
    From ! hibernate,
    {hibernate, State};
request({stop, Reason}, _) ->
    {stop, Reason};
request(badreturn, _) ->
    {bad, call};
request({terminate, How}, _) ->
    {stop, How}.

message({reply, X}, State) ->
    inform({reply, X}, State),
    {ok, State};
message({stop, Reason}, _) ->
    {stop, Reason};
message(badreturn, _) ->
    {bad, call};
message(hibernate, State) ->
    inform(hibernate, State),
    {hibernate, State}.

terminate(exit, _) ->
    exit({terminated, exit});
terminate(throw, _) ->
    throw({terminated, throw});
terminate(Reason, State) ->
    inform({terminate, Reason}, State),
    ok.

code_change(_OldVsn, State, die) ->
    exit(die),
    {ok, State};
code_change(_OldVsn, State, _Extra) ->
    inform(code_change, State),
    {ok, State}.

format_status(fail, [_, _]) ->
    exit(fail);
format_status(_Opt, [_PDict, State]) ->
    [{data, [{state, State}]}].

%%====================================================================
%% Internal functions
%%====================================================================

inform(What, State) ->
    State#state.controller ! What.
