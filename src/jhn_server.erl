%==============================================================================
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
%%%   A generic server.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_server).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% API
-export([start/1, start/2,
         call/2, call/3,
         sync/1, sync/2,
         cast/2, delayed_cast/2, cancel/1, abcast/2, abcast/3,
         reply/1, reply/2, from/0
        ]).

%% sys callbacks
-export([system_continue/3,
         system_terminate/4,
         system_code_change/4,
         format_status/2]).

%% Internal exports
-export([init/5, loop/1]).

%% Includes
-include_lib("kernel/include/logger.hrl").

%% Records
-record(opts, {arg     = no_arg   :: no_arg | _,
               link    = true     :: boolean(),
               timeout = infinity :: timeout(),
               name               :: atom(),
               errors  = []       :: [_]
              }).

-record(state, {parent             :: pid(),
                name               :: pid() | atom(),
                mod                :: atom(),
                data,
                hibernated = false :: boolean(),
                %% Optional callbacks
                handle_msg         :: boolean(),
                terminate          :: boolean(),
                code_change        :: boolean(),
                format_status      :: boolean()
               }).


-record('$jhn_server', {from           :: reference(),
                        type    = cast :: cast | call | sync,
                        payload = ok   :: _}).


%% Defines
-define(DEFAULT_TIMEOUT, 5000).

%% Types
-type opt()  :: {atom(), _}.
-type opts() :: [opt()].

-type server_ref() :: atom() | {atom(), node()} | pid().

-type init_return(State) :: ignore | return(State).
-type return(State)      :: {ok, State} | {hibernate, State} | {stop, State}.

-type from() :: reference().

%% Exported Types
-export_type([from/0, init_return/1, return/1]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

-callback init(State) -> init_return(State).

-callback handle_req(_, State) -> return(State).
-callback handle_msg(_, State) -> return(State).
-callback terminate(_, _) -> _.
-callback code_change(_, State, _) ->  return(State).
-callback format_status(_, _) -> _.

-optional_callbacks([handle_msg/2, terminate/2, code_change/3,
                     format_status/2]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start(CallbackModule) -> Result.
%% @doc
%%   Starts a jhn_server.
%% @end
%%--------------------------------------------------------------------
-spec start(atom()) -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
start(Mod) -> start(Mod, []).

%%--------------------------------------------------------------------
%% Function: start(CallbackModule, Options) -> Result.
%% @doc
%%   Starts a jhn_server with options.
%%   Options are:
%%     {link, Boolean} -> if the server is linked to the parent, default true
%%     {timeout, infinity | Integer} -> Time in ms for the server to start and
%%         initialise, after that an exception is generated, default 5000.
%%     {name, Atom} -> name that the server is registered under.
%%     {arg, Term} -> argument provided to the init/1 callback function,
%%         default is 'no_arg'.
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),  opts()) -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
start(Mod, Options) ->
    Ref = make_ref(),
    case opts(Options, #opts{}) of
        #opts{errors = Errors = [_ | _]} -> {error, Errors};
        Opts = #opts{link = true, arg = Arg} ->
            Pid = spawn_link(?MODULE, init, [Mod, Arg, Opts, self(), Ref]),
            wait_ack(Pid, Ref, Opts, undefined);
        Opts = #opts{arg = Arg} ->
            Pid = spawn(?MODULE, init, [Mod, Arg, Opts, self(), Ref]),
            wait_ack(Pid, Ref, Opts, erlang:monitor(process, Pid))
    end.

%%--------------------------------------------------------------------
%% Function: cast(Server, Message) -> ok.
%% @doc
%%   A cast is made to the server, allways returns ok.
%% @end
%%--------------------------------------------------------------------
-spec cast(server_ref(), _) -> ok.
%%--------------------------------------------------------------------
cast(Server, Msg) when is_atom(Server) -> do_cast(Server, Msg);
cast(Server, Msg) when is_pid(Server)-> do_cast(Server, Msg);
cast(Server = {Name, Node}, Msg) when is_atom(Name), is_atom(Node) ->
    do_cast(Server, Msg);
cast(Server, Msg) ->
    erlang:error(badarg, [Server, Msg]).

%%--------------------------------------------------------------------
%% Function: delayed_cast(Delay, Message) -> TimerRef.
%% @doc
%%   A cast is made to the calling Server after Delay ms, always returns
%%   a timer reference that can be canceled using cancel/1.
%% @end
%%--------------------------------------------------------------------
-spec delayed_cast(non_neg_integer(), _) -> reference().
%%--------------------------------------------------------------------
delayed_cast(Time, Msg) when is_integer(Time), Time >= 0 ->
    erlang:send_after(Time, self(), #'$jhn_server'{payload = Msg});
delayed_cast(Time, Msg) ->
    erlang:error(badarg, [Time, Msg]).

%%--------------------------------------------------------------------
%% Function: cancel(TimerRef) -> Time | false.
%% @doc
%%   A timer a timer started using delayed_cast/2, returns time
%%   remaining or false.
%% @end
%%--------------------------------------------------------------------
-spec cancel(reference()) -> non_neg_integer() | false.
%%--------------------------------------------------------------------
cancel(Ref) -> erlang:cancel_timer(Ref).

%%--------------------------------------------------------------------
%% Function: abcast(Server, Message) -> ok.
%% @doc
%%   A cast is made to servers registered as Server on all the connected nodes.
%% @end
%%--------------------------------------------------------------------
-spec abcast(atom(), _) -> ok.
%%--------------------------------------------------------------------
abcast(Name, Msg) when is_atom(Name) ->
    [do_cast({Name, Node}, Msg) || Node <- [node() | nodes()]],
    ok;
abcast(Server, Msg) ->
    erlang:error(badarg, [Server, Msg]).

%%--------------------------------------------------------------------
%% Function: abcast(Nodes, Server, Message) -> ok.
%% @doc
%%   A cast is made to servers registered as Server on all Nodes.
%% @end
%%--------------------------------------------------------------------
-spec abcast([node()], atom(), _) -> ok.
%%--------------------------------------------------------------------
abcast(Nodes, Name, Msg) when is_atom(Name) ->
    case lists:all(fun erlang:is_atom/1, Nodes) of
        true -> [do_cast({Name, Node}, Msg) || Node <- Nodes];
        false -> erlang:error(badarg, [Name, Msg])
    end,
    ok;
abcast(Nodes, Server, Msg) ->
    erlang:error(badarg, [Nodes, Server, Msg]).

%%--------------------------------------------------------------------
%% Function: call(Server, Message) -> Term.
%% @doc
%%   A call is made to Server with default timeout 5000ms.
%% @end
%%--------------------------------------------------------------------
-spec call(server_ref(), _) -> _.
%%--------------------------------------------------------------------
call(Server, Msg) -> call(Server, Msg, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------
%% Function: call(Server, Message, Timeout) -> Term.
%% @doc
%%   A call is made to Server with Timeout. Will generate a timeout
%%   exception if the Server takes more than Timeout time to answer.
%%   Generates an exception if the process is dead, dies, or no process
%%   registered under that name. If the server returns that is returned
%%   from the call.
%% @end
%%--------------------------------------------------------------------
-spec call(server_ref(), _, timeout()) -> _.
%%--------------------------------------------------------------------
call(Server, Msg, Timeout)
  when is_pid(Server), Timeout == infinity;
       is_pid(Server), is_integer(Timeout), Timeout > 0;
       is_atom(Server), Timeout == infinity;
       is_atom(Server), is_integer(Timeout), Timeout > 0 ->
    do_call(Server, Msg, Timeout);
call({Name, Node}, Msg, Timeout)
  when is_atom(Name), Node == node(), Timeout == infinity;
       is_atom(Name), Node == node(), is_integer(Timeout), Timeout > 0 ->
    do_call(Name, Msg, Timeout);
call(Server = {Name, Node}, Msg, Timeout)
  when is_atom(Name), is_atom(Node), Timeout == infinity;
       is_atom(Name), is_atom(Node), is_integer(Timeout), Timeout > 0 ->
    do_call(Server, Msg, Timeout);
call(Server, Msg, Timeout) ->
    erlang:error(badarg, [Server, Msg, Timeout]).

%%--------------------------------------------------------------------
%% Function: sync(Server) -> ok.
%% @doc
%%   A sync is made to Server with default timeout 5000ms.
%% @end
%%--------------------------------------------------------------------
-spec sync(server_ref()) -> ok.
%%--------------------------------------------------------------------
sync(Server) -> sync(Server, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------
%% Function: sync(Server, Timeout) -> ok.
%% @doc
%%   A sync is made to Server with Timeout. Will generate a timeout
%%   exception if the Server takes more than Timeout time to answer.
%%   Generates an exception if the process is dead, dies, or no process
%%   registered under that name. If the server returns, ok is returned.
%% @end
%%--------------------------------------------------------------------
-spec sync(server_ref(), timeout()) -> _.
%%--------------------------------------------------------------------
sync(Server, Timeout)
  when is_pid(Server), Timeout == infinity;
       is_pid(Server), is_integer(Timeout), Timeout > 0;
       is_atom(Server), Timeout == infinity;
       is_atom(Server), is_integer(Timeout), Timeout > 0 ->
    do_sync(Server, Timeout);
sync({Name, Node}, Timeout)
  when is_atom(Name), Node == node(), Timeout == infinity;
       is_atom(Name), Node == node(), is_integer(Timeout), Timeout > 0 ->
    do_sync(Name, Timeout);
sync(Server = {Name, Node}, Timeout)
  when is_atom(Name), is_atom(Node), Timeout == infinity;
       is_atom(Name), is_atom(Node), is_integer(Timeout), Timeout > 0 ->
    do_sync(Server, Timeout);
sync(Server, Timeout) ->
    erlang:error(badarg, [Server, Timeout]).

%%--------------------------------------------------------------------
%% Function: reply(Message) -> ok | {error, not_a_call}.
%% @doc
%%   Called inside callback function to provide reply to a call.
%% @end
%%--------------------------------------------------------------------
-spec reply(_) -> ok.
%%--------------------------------------------------------------------
reply(Msg) ->
    case erlang:get('$jhn_msg_store') of
        undefined -> {error, not_a_call};
        #'$jhn_server'{from = From} -> reply(From, Msg)
    end.


%%--------------------------------------------------------------------
%% Function: reply(From, Message) -> ok | {error, not_a_call}
%% @doc
%%   Called by any process will send a reply to the one that sent the
%%   request to the server, the From argument is the result from a call
%%   to from/1 inside a handle_req/2 callback function in response to
%%   a jhn_server:call/2/3
%% @end
%%--------------------------------------------------------------------
-spec reply(from(), _) -> ok.
%%--------------------------------------------------------------------
reply(From, Msg) when is_reference(From) ->
    try From ! #'$jhn_server'{from = From, payload = Msg} of
        _ ->  ok
    catch
        _:_ -> {error, invalid_from}
    end;
reply(From, Msg) ->
    {error, invalid_from}.


%%--------------------------------------------------------------------
%% Function: from() -> From.
%% @doc
%%   Called inside a handle_req/2 or handle_msg/2 callback function it
%%   will provide an opaque data data enables a reply to the call or
%%   cast outside the scope of the callback function.
%% @end
%%--------------------------------------------------------------------
-spec from() -> from().
%%--------------------------------------------------------------------
from() ->
    case erlang:get('$jhn_msg_store') of
        undefined -> {error, not_a_call};
        #'$jhn_server'{from = From} -> From
    end.

%%====================================================================
%% sys callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: system_continue(Parent, Debug, State) ->
%% @private
%%--------------------------------------------------------------------
-spec system_continue(pid(), _, #state{}) -> none().
%%--------------------------------------------------------------------
system_continue(_, _, State) -> next_loop(State).

%%--------------------------------------------------------------------
%% Function: system_terminate(Reason, Parent, Debug, State) ->
%% @private
%%--------------------------------------------------------------------
-spec system_terminate(_, pid(), _, #state{}) -> none().
%%--------------------------------------------------------------------
system_terminate(Reason, _, _, State) -> terminate(Reason, [], State).

%%--------------------------------------------------------------------
%% Function: system_code_change(State, Module, OldVsn, Extra) ->
%% @private
%%--------------------------------------------------------------------
-spec system_code_change(#state{}, _, _, _) -> none().
%%--------------------------------------------------------------------
system_code_change(State = #state{code_change = false}, _, _, _) -> {ok, State};
system_code_change(State = #state{data = Data, mod = Mod}, _, OldVsn, Extra) ->
    try Mod:code_change(OldVsn, Data, Extra) of
        {ok, NewData} -> {ok, State#state{data = NewData}}
    catch
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% Function: format_status(Opt, StatusData) -> Status.
%% @private
%%--------------------------------------------------------------------
-spec format_status(_, _) -> [{atom(), _}].
%%--------------------------------------------------------------------
format_status(_, [_, SysState, Parent, _, State=#state{format_status=false}]) ->
    NameTag = case State#state.name of
                  Name when is_pid(Name) -> pid_to_list(Name);
                  Name when is_atom(Name) -> Name
              end,
    Header = lists:concat(["Status for jhn server ", NameTag]),
    Specfic = [{data, [{"State", State}]}],
    [{header, Header},
     {data, [{"Status", SysState}, {"Parent", Parent}]} | Specfic];
format_status(Opt, [PDict, SysState, Parent, _, State]) ->
    #state{mod = Mod, data = Data} = State,
    NameTag = case State#state.name of
                  Name when is_pid(Name) -> pid_to_list(Name);
                  Name when is_atom(Name) -> Name
              end,
    Header = lists:concat(["Status for jhn server ", NameTag]),
    Specfic = try Mod:format_status(Opt, [PDict, Data])
              catch _:_ -> [{data, [{"State", Data}]}] end,
    [{header, Header},
     {data, [{"Status", SysState}, {"Parent", Parent}]} | Specfic].

%%====================================================================
%% Internal exports
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Module, Arguments, Opts, Parent, MonitorRef) ->
%% @private
%%--------------------------------------------------------------------
-spec init(atom(), _, #opts{}, pid(), reference()) -> _.
%%--------------------------------------------------------------------
init(Mod, Arg, Opts, Parent, Ref) ->
    State =
        #state{parent = Parent,
               mod = Mod,
               handle_msg = erlang:function_exported(Mod, handle_msg, 2),
               terminate = erlang:function_exported(Mod, terminate, 2),
               code_change = erlang:function_exported(Mod, code_change, 3),
               format_status = erlang:function_exported(Mod, format_status, 2)},
    case name(Opts, State) of
        {ok, State1} ->
            try Mod:init(Arg) of
                {ok, Data} ->
                    Parent ! {ack, Ref},
                    next_loop(State1#state{data = Data});
                {hibernate, Data} ->
                    Parent ! {ack, Ref},
                    next_loop(State1#state{data = Data, hibernated = true});
                ignore ->
                    Parent ! {ignore, Ref};
                {stop, Reason} ->
                    Parent ! {nack, Ref, Reason};
                Other ->
                    Parent ! {nack, Ref, {bad_return_value, Other}}
            catch
                throw:Reason -> Parent ! {nack, Ref, Reason};
                _:Reason:Stack -> Parent ! {nack, Ref,{'EXIT', Reason, Stack}}
            end;
        {error, Error} ->
            Parent ! {nack, Ref, Error}
    end.

%%--------------------------------------------------------------------
%% Function: loop(State) ->
%% @private
%%--------------------------------------------------------------------
-spec loop(#state{}) -> none().
%%--------------------------------------------------------------------
loop(State = #state{parent = Parent, mod = Mod, handle_msg = HM}) ->
    receive
        {system, From, Req} ->
            sys:handle_system_msg(Req, From, Parent, ?MODULE, [], State);
        Msg = {'EXIT', Parent, Reason} ->
            terminate(Reason, Msg, State);
        Sync = #'$jhn_server'{from = From, type = sync} ->
            From ! Sync,
            next_loop(State);
        Msg = #'$jhn_server'{payload = Payload} ->
            erlang:put('$jhn_msg_store', Msg),
            Data = State#state.data,
            Return = try Mod:handle_req(Payload, Data) catch C:E -> {C, E} end,
            erlang:put('$jhn_msg_store', undefined),
            return(Return, handle_req, Mod, Msg, State);
        Msg when not HM ->
            unexpected(handle_msg, State, Msg),
            next_loop(State);
        Msg ->
            Data = State#state.data,
            Return = try Mod:handle_msg(Msg, Data) catch C:E -> {C, E} end,
            return(Return, handle_msg, Mod, Msg, State)
    end.

return({ok, NewData}, _, _, _, State) ->
    next_loop(State#state{data = NewData, hibernated = false});
return({hibernate, NewData}, _, _, _, State) ->
    NewState = State#state{data = NewData, hibernated = true},
    next_loop(NewState);
return({stop, Reason}, _, _, Msg, State) ->
    terminate(Reason, Msg, State);
return({error, function_clause}, Type, _, Msg,State) ->
    unexpected(Type, State, Msg),
    next_loop(State);
return(Other, _, _, Msg, State) ->
    terminate({bad_return_value, Other}, Msg, State).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
wait_ack(Pid, Ref, Opts, MRef) ->
    receive
        {ack, Ref} ->
            unmonitor(MRef),
            {ok, Pid};
        {ignore, Ref} ->
            unmonitor(MRef),
            flush_exit(Pid),
            ignore;
        {nack, Ref, Reason} ->
            unmonitor(MRef),
            {error, Reason};
        {'EXIT', Pid, Reason} ->
            unmonitor(MRef),
            {error, Reason};
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, Reason}
    after Opts#opts.timeout ->
            unmonitor(MRef),
            unlink(Pid),
            exit(Pid, kill),
            flush_exit(Pid),
            {error, timeout}
    end.

unmonitor(undefined) -> ok;
unmonitor(Mref) when is_reference(Mref) -> erlang:demonitor(Mref, [flush]).

flush_exit(Pid) ->
    receive {'EXIT', Pid, _} -> ok
    after 0 -> ok
    end.

%%--------------------------------------------------------------------
opts([], Opts) -> Opts;
opts([{link, Value} | T], Opts) when is_boolean(Value) ->
    opts(T, Opts#opts{link = Value});
opts([{timeout, infinity} | T], Opts) ->
    opts(T, Opts#opts{timeout = infinity});
opts([{timeout, Value} | T], Opts) when is_integer(Value), Value > 0 ->
    opts(T, Opts#opts{timeout = Value});
opts([{name, Value} | T], Opts) when is_atom(Value) ->
    opts(T, Opts#opts{name = Value});
opts([{arg, Value} | T], Opts) ->
    opts(T, Opts#opts{arg = Value});
opts([H | T], Opts = #opts{errors = Errors}) ->
    opts(T, Opts#opts{errors = [H | Errors]}).

%%--------------------------------------------------------------------
name(#opts{name = undefined}, State) -> {ok, State#state{name = self()}};
name(#opts{name = Name}, State) ->
    try register(Name, self()) of true -> {ok, State#state{name = Name}}
    catch _:_ -> {error, {already_started, Name, whereis(Name)}} end.

%%--------------------------------------------------------------------
terminate(Reason, Msg, State = #state{terminate = false}) ->
    case Reason of
        normal -> exit(normal);
        shutdown -> exit(shutdown);
        Shutdown = {shutdown, _} -> exit(Shutdown);
        _ ->
            error_info(Reason, Msg, State),
            exit(Reason)
    end;
terminate(Reason, Msg, State = #state{mod = Mod, data = Data}) ->
    try Mod:terminate(Reason, Data) of
        _ ->
            case Reason of
                normal -> exit(normal);
                shutdown -> exit(shutdown);
                Shutdown = {shutdown, _} -> exit(Shutdown);
                _ ->
                    error_info(Reason, Msg, State),
                    exit(Reason)
            end
    catch
        _:Reason1 ->
            error_info(Reason1, Msg, State),
            exit(Reason1)
    end.

error_info(Reason, [], #state{data = Data, name = Name}) ->
    error_logger:format("** JHN server ~p terminating \n"
                        "** When Server state == ~p~n"
                        "** Reason for termination == ~n** ~p~n",
                        [Name, Data, Reason]);
error_info(Reason, Msg, #state{data = Data, name = Name}) ->
    error_logger:format("** JHN server ~p terminating \n"
                        "** Last message in was ~p~n"
                        "** When Server state == ~p~n"
                        "** Reason for termination == ~n** ~p~n",
                        [Name, Msg, Data, Reason]).

%%--------------------------------------------------------------------
do_cast(Server, Msg) ->
    Cast = #'$jhn_server'{payload = Msg},
    try erlang:send(Server, Cast, [noconnect]) of
        %% Wait for autoconnect in separate process.
        noconnect -> spawn(erlang, send, [Server, Cast]);
        _ -> ok
    catch
        _:_ -> ok
    end.

%%--------------------------------------------------------------------
do_call(Server, Msg, Timeout) ->
    M = #'$jhn_server'{type = call, payload = Msg},
    send_recv(Server, M, Timeout).

do_sync(Server, Timeout) ->
    M = #'$jhn_server'{type = sync},
    send_recv(Server, M, Timeout).

send_recv(Server, Msg, Timeout) ->
    MRef = erlang:monitor(process, Server, [{alias, reply_demonitor}]),
    %% The monitor will do any autoconnect.
    try erlang:send(Server, Msg#'$jhn_server'{from = MRef}, [noconnect]) of
        ok ->
            receive
                #'$jhn_server'{from = MRef, payload = Reply} -> Reply;
                {'DOWN', MRef, _, _, Reason} -> exit(Reason)
            after Timeout ->
                    erlang:demonitor(MRef, [flush]),
                    exit(timeout)
            end;
        noconnect ->
            erlang:demonitor(MRef, [flush]),
            Node = case Server of
                       {_, Node0} when is_atom(Node0) -> Node0;
                       Name when is_atom(Name) -> node();
                       _ when is_pid(Server) -> node(Server)
                   end,
            exit({nodedown, Node})
    catch _:_ ->
            exit(noproc)
    end.

%%--------------------------------------------------------------------
next_loop(State = #state{hibernated = true}) ->
    erlang:hibernate(?MODULE, loop, [State]);
next_loop(State) ->
    loop(State).

%%--------------------------------------------------------------------
unexpected(Type, #state{mod = Mod, name = undefined}, Msg) ->
    logger:error(
      "JHN server ~p received unexpected~n"
      "  no matching clause in ~p:~p/2:~n"
      "  ~p~n",
      [self(), Mod, Type, Msg]);
unexpected(Type, #state{mod = Mod, name = Name}, Msg) ->
    logger:error(
      "JHN server ~p(~p) received unexpected~n"
      "  no matching clause in ~p:~p/2:~n"
      "  ~p~n",
      [Name, self(), Mod, Type, Msg]).

%%--------------------------------------------------------------------
