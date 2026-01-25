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
%%%  A cacerts utility lib:
%%%
%%% @end
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_cacerts).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([load/0, fetch/0, start_link/0, start_link/1]).

%% jhn_server callbacks
-export([init/1, request/2, message/2]).

%% Records
-record(state, {interval :: pos_integer()}).

%% Defines
-define(CASITE, ~"https://mkcert.org/generate/").
-define(OPTS, #{options => [{cacerts, fetch_local()}], close => true}).

%% ===================================================================
%% Library functions.
%% ===================================================================


%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec load() -> ok | {error, _}.
%%--------------------------------------------------------------------
load() ->
    case do_fetch() of
        Error = {error, _} -> Error;
        CaCerts -> persistent_term:put(?MODULE, CaCerts)
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch() -> [binary()] | {error, _}.
%%--------------------------------------------------------------------
fetch() ->
    case persistent_term:get(?MODULE, undefined) of
        undefined ->
            case do_fetch() of
                Error = {error, _} -> Error;
                CaCerts ->
                    persistent_term:put(?MODULE, CaCerts),
                    CaCerts
            end;
         CaCerts ->
            CaCerts
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
start_link() -> start_link(86_400_000).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(pos_integer()) -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
start_link(Interval) -> jhn_server:create(?MODULE, [{arg, Interval}]).

%%====================================================================
%% jhn_server callbacks
%%====================================================================
init(Interval) ->
    self() ! load,
    {ok, #state{interval = Interval}}.

request(Req, State) ->
    unexpected(request, Req),
    {hibernate, State}.

message(load, State = #state{interval = Interval}) ->
    erlang:send_after(Interval, self(), load),
    load(),
    {hibernate, State};
message(Msg, State) ->
    unexpected(message, Msg),
    {hibernate, State}.

%% ===================================================================
%% Internal functions.
%% ===================================================================

do_fetch() ->
    case jhn_shttpc:get(?CASITE, ?OPTS) of
        #{status := {200, _}, body := Body} ->
            [element(2, Cert) || Cert <- public_key:pem_decode(Body)];
        #{status := {_, Error}} ->
            Report = #{message => ~"Load error",
                       pid => self(),
                       id => jhn_cacerts,
                       ca_site => ?CASITE,
                       error => Error},
            logger:log(warning, Report),
            fetch_local()
    end.

fetch_local() ->
        case persistent_term:get(?MODULE, undefined) of
                undefined -> read();
                CaCerts -> CaCerts
            end.

read() ->
    File = filename:join([code:priv_dir(jhn_stdlib), "cacerts"]),
    {ok, [CaCerts]} = file:consult(File),
    CaCerts.

unexpected(Type, Msg) ->
    Report = #{reason => ~"unexpected",
               pid => self(),
               id => jhn_cacerts,
               type => Type,
               message => Msg},
    logger:log(error, Report).

