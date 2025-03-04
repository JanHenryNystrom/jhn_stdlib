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
%%%  This is a generic server in the spirit of OTP's gen_server but subtly
%%%  different. The behaviour module provides the server of a
%%%  client-server relation. A generic server process (jhn_server)
%%%  implemented using this module has a standard set of interface functions.
%%%  It also fits into an OTP supervision tree and supports generic OTP tooling
%%%  for, e.g., dynamic upgrades.
%%%
%%%  A jhn_server process assumes all specific parts to be located in a
%%%  callback module exporting a predefined set of functions, some optional.
%%%  <![CDATA[
%%%  The relationship between the behavior functions and the callback
%%%  functions is as follows: ----->/-----> fuction call/return
%%%   ====> message sent
%%%
%%%   User module: <-------------> jhn_server module: <--/==> Callback module:
%%%
%%%   -----------------------------------------------------------------------
%%%
%%%   jhn_server:create/1/2 -----> proc_lib:spawn ----> init/1
%%%
%%%   jhn_server:create/1/2 <----- proc_lib:spawn <==== init/1
%%%
%%%   -----------------------------------------------------------------------
%%%
%%%   jhn_server:call/2/3   =====>  (jhn_server)  -----> request/2
%%%
%%%   jhn_server:call/2/3   <=====  (jhn_server)  <----- jhn_server:reply/1/2
%%%                                                       (optional)
%%%
%%%   -----------------------------------------------------------------------
%%%
%%%   jhn_server:sync/1/2  =====> (jhn_server)
%%%
%%%   jhn_server:sync/1/2  <===== (jhn_server)
%%%
%%%   -----------------------------------------------------------------------
%%%
%%%   jhn_server:cast/2/3   =====> (jhn_server)   -----> request/2
%%%
%%%   All the following are optional
%%%
%%%   Other process         =====> Callback module
%%%
%%%   -----------------------------------------------------------------------
%%%
%%%   Id ! Message          =====>  message/2
%%%
%%%   sys:terminate/2       =====>  terminate/3
%%%
%%%   sys:code_change/4     =====>  code_change/3
%%%
%%%   sys:get_status/2      =====>  format_status/2
%%%
%%%   If a callback function fails or returns a bad value, the jhn_server
%%%   process terminates. N.B. that a gen_server process does not trap exit
%%%   signals automatically, this must be explicitly initiated in the
%%%   callback module. The process is by default linked to the parent.
%%%
%%%   jhn_server supports hibernation. Any call, cast or plain message sent
%%%   to the process that has not a matching clause in the appropriate
%%%   request/2 or messsage/2 call back function will be discarded and logged
%%%   as unexpected. If the optional callback funtion message/2 is not defined
%%%   there are no matching function clauses at all.
%%%
%%%   The callback module shall export:
%%%
%%%   init(Args)
%%%      ==> {ok, State} |
%%%          {hibernate, State} |
%%%          ignore |
%%%          {error, stop} |
%%%          {stop, Reason}
%%%
%%%   request(Msg, State)
%%%      ==> {ok, State} |
%%%          {hibernate, State} |
%%%          {stop, Reason}
%%%
%%%   The callback module can export:
%%%
%%%   message(Msg, State)
%%%      ==> {ok, State} |
%%%          {hibernate, State} |
%%%          {stop, Reason}
%%%
%%%   terminate(Reason, State)
%%%      ==> Return value ignored
%%%
%%%   Lets the user module clean up always called when server terminates
%%%
%%%   code_change(OldVsn, Data, Extra)
%%%      ==> {ok, State}
%%%
%%%   format_status(Opt, _)
%%%      ==> {ok, State}
%%%
%%%      Uses the old format will be replaced.
%%%  ]]>
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_server).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% API
-export([create/1, create/2,
         cast/2, delayed_cast/2, cancel/1, abcast/2, abcast/3,
         call/2, call/3,
         sync/1, sync/2,
         reply/1, reply/2, from/0, type/0
        ]).

%% sys callbacks
-export([system_continue/3,
         system_terminate/4,
         system_code_change/4,
         format_status/2]).

%% proc_lib callbacks
-export([init/4, loop/1]).

%% logger callback
-export([report_to_format/2]).

%% Defines
-define(DEFAULT_TIMEOUT, 5000).

%% Records
-record(opts, {arg     = no_arg           :: no_arg | _,
               link    = true             :: boolean(),
               timeout = ?DEFAULT_TIMEOUT :: timeout(),
               name                       :: atom(),
               errors  = []               :: [_]
              }).

-record(state, {parent             :: pid(),
                name               :: pid() | atom(),
                mod                :: atom(),
                data,
                hibernated = false :: boolean(),
                %% Optional callbacks
                message            :: boolean(),
                terminate          :: boolean(),
                code_change        :: boolean(),
                format_status      :: boolean()
               }).

-record('$jhn_server', {from           :: reference() | undefined,
                        type    = cast :: cast | call | sync,
                        payload = ok   :: _}).

%% Types
-type opt()  :: {atom(), _}.
-type opts() :: [opt()].

-type server_ref() :: atom() | {atom(), node()} | pid().

-type init_return(State) :: ignore | return(State).
-type return(State)      :: {ok, State} | {hibernate, State} |
                            {error, _} | {stop, _}.

-type from() :: reference().

%% Exported Types
-export_type([from/0, init_return/1, return/1]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

-callback init(State) -> init_return(State).

-callback request(_, State) -> return(State).
-callback message(_, State) -> return(State).
-callback terminate(_, _, _) -> _.
-callback code_change(_, State, _) ->  return(State).
-callback format_status(_, _) -> _.

-optional_callbacks([message/2,
                     terminate/3, code_change/3, format_status/2]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: create(CallbackModule) -> Result.
%% @doc
%%   Creates a jhn_server.
%% @end
%%--------------------------------------------------------------------
-spec create(atom()) -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
create(Mod) -> create(Mod, []).

%%--------------------------------------------------------------------
%% Function: create(CallbackModule, Options) -> Result.
%% @doc
%%   Creates a jhn_server with options.
%%   Options are:
%%     {link, Boolean} -> if the server is linked to the parent, default true
%%     {timeout, infinity | Integer} -> Time in ms for the server to create and
%%         initialise, after that an exception is generated, default 5000.
%%     {name, Atom} -> name that the server is registered under.
%%     {arg, Term} -> argument provided to the init/1 callback function,
%%         default is 'no_arg'.
%% @end
%%--------------------------------------------------------------------
-spec create(atom(), opts()) -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
create(Mod, Options) ->
    case opts(Options, #opts{}) of
        #opts{errors = Errors = [_ | _]} -> {error, Errors};
        Opts = #opts{link = true, arg = A, timeout = T} ->
            proc_lib:start_link(?MODULE, init, [Mod, A, Opts, self()], T);
        Opts = #opts{arg = A, timeout = T} ->
            proc_lib:start(?MODULE, init, [Mod, A, Opts, self()], T)
    end.

%%--------------------------------------------------------------------
%% Function: cast(Server, Message, ) -> ok.
%% @doc
%%   A cast is made to the server, allways returns ok.
%% @end
%%--------------------------------------------------------------------
-spec cast(server_ref(), _) -> ok.
%%--------------------------------------------------------------------
cast(Server, Msg) ->
    case is_server_ref(Server) of
        false -> erlang:error(badarg, [Server, Msg]);
        {true, Server1} -> do_cast(Server1, Msg)
    end.

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
%--------------------------------------------------------------------
call(Server, Msg) -> call(Server, Msg, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------
%% Function: call(Server, Message, Timeout) -> Term.
%% @doc
%%   A call is made to Server with Timeout. Will generate a timeout
%%   exception if the Server takes more than Timeout time to answer.
%%   Generates an exception if the process is dead, dies, or no process
%%   registered under that name. If the server replies that is returned
%%   from the call.
%% @end
%%--------------------------------------------------------------------
-spec call(server_ref(), _, timeout()) -> _.
%%--------------------------------------------------------------------
call(Server, Msg, Timeout) ->
    case is_server_ref(Server) of
        false -> erlang:error(badarg, [Server, Msg, Timeout]);
        {true, Server1} -> 
            case is_timeout(Timeout) of
                true -> do_call(Server1, Msg, Timeout);
                false -> erlang:error(badarg, [Server, Msg, Timeout])
            end
    end.

%%--------------------------------------------------------------------
%% Function: sync(Server) -> ok.
%% @doc
%%   A synchronization is made with Server with default timeout 5000ms.
%% @end
%%--------------------------------------------------------------------
-spec sync(server_ref()) -> ok.
%%--------------------------------------------------------------------
sync(Server) -> sync(Server, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------
%% Function: sync(Server, Timeout) -> ok.
%% @doc
%%   A synchronization is made with Server with Timeout. Will generate a timeout
%%   exception if the Server takes more than Timeout time to answer.
%%   Generates an exception if the process is dead, dies, or no process
%%   registered under that name. If the server returns, ok is returned.
%% @end
%%--------------------------------------------------------------------
-spec sync(server_ref(), timeout()) -> _.
%%--------------------------------------------------------------------
sync(Server, Timeout) ->
    case is_server_ref(Server) of
        false -> erlang:error(badarg, [Server, Timeout]);
        {true, Server1} ->
            case is_timeout(Timeout) of
                true -> do_sync(Server1, Timeout);
                false -> erlang:error(badarg, [Server, Timeout])
            end
    end.

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
%%   to from/1 inside a request/2 callback function in response to
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
reply(_, _) ->
    {error, invalid_from}.


%%--------------------------------------------------------------------
%% Function: from() -> From.
%% @doc
%%   Called inside a request/2 of callback function for a call it
%%   will provide an opaque data data enables a reply to the call
%%   outside the scope of the callback function.
%% @end
%%--------------------------------------------------------------------
-spec from() -> from().
%%--------------------------------------------------------------------
from() ->
    case erlang:get('$jhn_msg_store') of
        undefined -> {error, not_a_call};
        #'$jhn_server'{from = From} -> From
    end.

%%--------------------------------------------------------------------
%% Function: type() -> call or cast.
%% @doc
%%   Called inside a request/2 and it tells if it is a call or a cast.
%% @end
%%--------------------------------------------------------------------
-spec type() -> call | cast.
%%--------------------------------------------------------------------
type() ->
    case erlang:get('$jhn_msg_store') of
        undefined -> cast;
        #'$jhn_server'{type = call} -> call
    end.

%%====================================================================
%% sys callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: system_continue(Parent, Debug, State) ->
%% @private
%%--------------------------------------------------------------------
-spec system_continue(pid(), _, #state{}) -> no_return().
%%--------------------------------------------------------------------
system_continue(_, _, State) -> next_loop(State).

%%--------------------------------------------------------------------
%% Function: system_terminate(Reason, Parent, Debug, State) ->
%% @private
%%--------------------------------------------------------------------
-spec system_terminate(_, pid(), _, #state{}) -> no_return().
%%--------------------------------------------------------------------
system_terminate(Reason, _, _, State) -> terminate(Reason, [], State).

%%--------------------------------------------------------------------
%% Function: system_code_change(State, Module, OldVsn, Extra) ->
%% @private
%%--------------------------------------------------------------------
-spec system_code_change(#state{}, _, _, _) -> no_return().
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
format_status(Opt, [PDict, SysState, Parent, _, State]) ->
    case State#state.format_status of
        false -> format_status(Parent, SysState, State);
        true ->
            try (State#state.mod):format_status(Opt, [PDict, State#state.data])
            catch _:_ -> format_status(Parent, SysState, State) end
    end.

%%====================================================================
%% proc_lib callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Module, Arguments, Opts, Parent) ->
%% @private
%%--------------------------------------------------------------------
-spec init(atom(), _, #opts{}, pid()) -> _.
%%--------------------------------------------------------------------
init(Mod, Arg, Opts, Parent) ->
    State =
        #state{parent = Parent,
               mod = Mod,
               message = erlang:function_exported(Mod, message, 2),
               terminate = erlang:function_exported(Mod, terminate, 3),
               code_change = erlang:function_exported(Mod, code_change, 3),
               format_status = erlang:function_exported(Mod, format_status, 2)},
    case name(Opts, State) of
        {ok, State1} ->
            try Mod:init(Arg) of
                {ok, Data} ->
                    proc_lib:init_ack(Parent, {ok, self()}),
                    next_loop(State1#state{data = Data});
                {hibernate, Data} ->
                    proc_lib:init_ack(Parent, {ok, self()}),
                    next_loop(State1#state{data = Data, hibernated = true});
                ignore ->
                    fail(Parent, State1, ignore, normal);
                {error, _}  = Reason ->
                    fail(Parent, State1, Reason, normal);
                {stop, Reason} ->
                    fail(Parent, State1, {error, Reason}, Reason);
                Other ->
                    Bad = {bad_return_value, Other},
                    fail(Parent, State1, {error, Bad}, Bad)
            catch
                Class:Reason:Stack ->
                    Error = {Class, Reason, Stack},
                    fail(Parent, State1, {error, Error}, Error)
            end;
        {error, _}  = Error ->
            proc_lib:init_fail(Parent, Error, {exit, normal})
    end.

%%--------------------------------------------------------------------
%% Function: loop(State) ->
%% @private
%%--------------------------------------------------------------------
-spec loop(#state{}) -> no_return().
%%--------------------------------------------------------------------
loop(State = #state{parent = Parent, mod = Mod, message = HM}) ->
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
            Return = try Mod:request(Payload, Data)
                     catch error:function_clause:Stack ->
                             {function_clause, Stack};
                           C:E ->
                             {C, E}
                     end,
            erlang:put('$jhn_msg_store', undefined),
            return(Return, request, Msg, State);
        Msg when not HM ->
            unexpected(message, Msg, State),
            next_loop(State);
        Msg ->
            Data = State#state.data,
            Return = try Mod:message(Msg, Data)
                     catch error:function_clause:Stack ->
                             {function_clause, Stack};
                            C:E ->
                             {C, E}
                     end,
            return(Return, message, Msg, State)
    end.

return({ok, NewData}, _, _, State) ->
    next_loop(State#state{data = NewData, hibernated = false});
return({hibernate, NewData}, _, _, State) ->
    NewState = State#state{data = NewData, hibernated = true},
    next_loop(NewState);
return({stop, Reason}, _, Msg, State) ->
    terminate(Reason, Msg, State);
return({function_clause, [{M, F, _, _} | _]}, F, Ms, State = #state{mod = M}) ->
    unexpected(F, Ms, State),
    next_loop(State);
return(Other, _, Msg, State) ->
    terminate({bad_return_value, Other}, Msg, State).

%%====================================================================
%% logger callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: report_to_format(Report, FormatOpts) -> Format.
%% @private
%%--------------------------------------------------------------------
-spec report_to_format(logger:report(), logger:report_cb_config()) ->
          unicode:chardata().
%%--------------------------------------------------------------------
report_to_format(Report = #{label := {?MODULE, unexpected}}, _) ->
    #{id := Id, pid := Pid, type := Type, message := Msg} = Report,
    Format = "**  JHN server ~p(~p) received unexpected ~p:~n**  ~p~n",
    io_lib:format(Format, [Id, Pid, Type, Msg]);
report_to_format(Report = #{label := {?MODULE, terminating}}, _) ->
    #{name := Name, data := Data, message := Msg, reason := Reason} = Report,
    {Format, Args} =
        case Msg of
            [] ->
                {"** JHN server ~p terminating ~n"
                 "** When Server state == ~p~n"
                 "** Reason for termination == ~n** ~p~n",
                 [Name, Data, Reason]};
            _ ->
                {"** JHN server ~p terminating ~n"
                 "** Last message in was ~p~n"
                 "** When Server state == ~p~n"
                 "** Reason for termination == ~n** ~p~n",
                 [Name, Msg, Data, Reason]}
        end,
    io_lib:format(Format, Args).

%%====================================================================
%% Internal functions
%%====================================================================

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
    catch _:_ -> {error, {already_created, Name, whereis(Name)}} end.

unname(Pid) when is_pid(Pid) -> ok;
unname(Atom) -> unregister(Atom).

%%--------------------------------------------------------------------
-spec fail(pid(), #state{}, _, _) -> no_return().
%%--------------------------------------------------------------------
fail(Parent, State, Reason, Exit) ->
    unname(State#state.name),
    proc_lib:init_fail(Parent, Reason, {exit, Exit}).

%%--------------------------------------------------------------------
-spec terminate(_, _, #state{}) -> no_return().
%%--------------------------------------------------------------------
terminate(Reason, Msg, State = #state{terminate = false}) ->
    case Reason of
        normal -> exit(normal);
        shutdown -> exit(shutdown);
        Shutdown = {shutdown, _} -> exit(Shutdown);
        _ ->
            terminating(Reason, Msg, State),
            exit(Reason)
    end;
terminate(Reason, Msg, State = #state{mod = Mod, data = Data}) ->
    try Mod:terminate(Reason, Msg, Data) of
        _ ->
            terminate(Reason, Msg, State#state{terminate = false})
    catch
        _:Reason1 ->
            terminate(Reason1, Msg, State#state{terminate = false})
    end.

%%--------------------------------------------------------------------
is_server_ref(Server) when is_pid(Server) -> {true, Server};
is_server_ref(Server) when is_atom(Server) -> {true, Server};
is_server_ref(Server = {Name, Node}) when is_atom(Name) ->
    case node() of
        Name -> {true, Name};
        _ when is_atom(Node) -> {true, Server};
        _ -> false
    end;
is_server_ref(_) ->
    false.

is_timeout(infinity) -> true;
is_timeout(Timeout) when is_integer(Timeout), Timeout > 0 -> true;
is_timeout(_) -> false.

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
    proc_lib:hibernate(?MODULE, loop, [State]);
next_loop(State) ->
    loop(State).

%%--------------------------------------------------------------------
format_status(Parent, SysState, State = #state{data = Data}) ->
    NameTag = case State#state.name of
                  Name when is_pid(Name) -> pid_to_list(Name);
                  Name when is_atom(Name) -> Name
              end,
    Header = lists:concat(["Status for jhn server ", NameTag]),
    [{header, Header},
     {data, [{"Status", SysState}, {"Parent", Parent}, {"State", Data}]}].

%%--------------------------------------------------------------------
%% Logging
%%--------------------------------------------------------------------
unexpected(Type, Msg, State) ->
    Id = case State#state.name of
             undefined -> State#state.mod;
             Name -> Name
         end,
    Msg1 = case Msg of
               #'$jhn_server'{payload = Payload} -> Payload;
               _ -> Msg
           end,
    Report = #{label => {?MODULE, unexpected},
               pid => self(),
               id => Id,
               type => Type,
               message => Msg1},
    Meta = #{domain => [jhn],
             tag => error,
             report_cb => fun ?MODULE:report_to_format/2},
    logger:log(error, Report, Meta).

terminating(Reason, Msg, #state{data = Data, name = Name}) ->
    Name1 = case Name of
                undefined -> self();
                _ -> Name
            end,
    Msg1 = case Msg of
               #'$jhn_server'{payload = Payload} -> Payload;
               _ -> Msg
           end,
    Report = #{label => {?MODULE, terminating},
               name => Name1,
               data => Data,
               message => Msg1,
               reason => Reason},
    Meta = #{domain => [jhn],
             tag => error,
             report_cb => fun ?MODULE:report_to_format/2},
    logger:log(error, Report, Meta).
