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
%%%   A finite state machine.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_fsm).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

-behaviour(jhn_server).

%% API
-export([create/1, create/2,
         cast/2, delayed_cast/2, cancel/1,
         call/2, call/3,
         sync/1, sync/2,
         reply/1, reply/2, from/0, type/0
        ]).

%% jhn_server callbacks
-export([init/1,
         request/2, message/2,
         terminate/3, code_change/3, format_status/2]).

%% logger callback
-export([report_to_format/2]).

%% Records
-record(state, {name                         :: pid() | atom(),
                mod                          :: atom(),
                state_name                   :: atom(),
                data                         :: _,
                %% Optional callbacks
                event                        :: boolean(),
                message                      :: boolean(),
                terminate                    :: boolean(),
                code_change                  :: boolean(),
                format_status                :: boolean(),
                %% Deferred events
                deferred          = {[], []} :: {[_], [_]},
                handling_deferred = false    :: boolean()
               }).

%% Defines
-define(DEFAULT_TIMEOUT, 5000).

%% Types
-type opt()              :: {atom(), _}.
-type opts()             :: [opt()].
-type fsm_ref()          :: atom() | {atom(), node()} | pid().
-type event()            :: _.
-type from()             :: jhn_server:from().

-type init_return(State) :: ignore | return(State).
-type return(State)      :: {ok, atom(), State} |
                            {hibernate, atom(), State} |
                            {stop, atom()}.

%% Exported Types
-export_type([from/0, init_return/1, return/1]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

-callback init(State) -> init_return(State).

-callback event(_, atom(), State) -> return(State).
-callback message(_, atom(), State) -> return(State).
-callback terminate(_, atom(), _) -> _.
-callback code_change(_, atom(), State, _) ->  return(State).
-callback format_status(_, _) -> _.

-optional_callbacks([event/3, message/3,
                     terminate/3, code_change/4, format_status/2]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: create(CallbackModule) -> Result.
%% @doc
%%   Creates a jhn_fsm.
%% @end
%%--------------------------------------------------------------------
-spec create(atom()) -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
create(Mod) -> create(Mod, []).

%%--------------------------------------------------------------------
%% Function: create(CallbackModule, Options) -> Result.
%% @doc
%%   Creates a jhn_fsm with options.
%%   Options are:
%%     {link, Boolean} -> if the fsm is linked to the parent, default true
%%     {timeout, infinity | Integer} -> Time in ms for the fsm to create and
%%         initialise, after that an exception is generated, default 5000.
%%     {name, Atom} -> name that the fsm is registered under.
%%     {arg, Term} -> argument provided to the init/1 callback function,
%%         default is 'no_arg'.
%% @end
%%--------------------------------------------------------------------
-spec create(atom(),  opts()) -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
create(Mod, Opts) ->
    Opts1 = plist:replace(arg, {Mod, Opts}, Opts),
    jhn_server:create(?MODULE, Opts1).

%%--------------------------------------------------------------------
%% Function: event(FSM, Message) -> ok.
%% @doc
%%   An event is sent to the FSM, always retuns ok.
%% @end
%%--------------------------------------------------------------------
-spec cast(fsm_ref(), _) -> ok.
%%--------------------------------------------------------------------
cast(FSM, Event) -> jhn_server:cast(FSM, Event).

%%--------------------------------------------------------------------
%% Function: delayed_event(Delay, Message) -> TimerRef.
%% @doc
%%   An event is sent to the calling FSM after Delay ms, always returns
%%   a timer reference that can be canceled using cancel/1.
%% @end
%%--------------------------------------------------------------------
-spec delayed_cast(non_neg_integer(), _) -> reference().
%%--------------------------------------------------------------------
delayed_cast(Time, Event) -> jhn_server:delayed_cast(Time, Event).

%%--------------------------------------------------------------------
%% Function: cancel(TimerRef) -> Time | false.
%% @doc
%%   A timer a timer started using delayed_event/2, returns time
%%   remaining or false.
%% @end
%%--------------------------------------------------------------------
-spec cancel(reference()) -> non_neg_integer() | false.
%%--------------------------------------------------------------------
cancel(Ref) -> erlang:cancel_timer(Ref).

%%--------------------------------------------------------------------
%% Function: call(FSM, Message) -> Term.
%% @doc
%%   A call is made to FSM with default timeout 5000ms.
%% @end
%%--------------------------------------------------------------------
-spec call(fsm_ref(), _) -> _.
%%--------------------------------------------------------------------
call(FSM, Msg) -> call(FSM, Msg, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------
%% Function: call(FSM, Message, Timeout) -> Term.
%% @doc
%%   A call is made to FSM with Timeout. Will generate a timeout
%%   exception if the FSM takes more than Timeout time to answer.
%%   Generates an exception if the process is dead, dies, or no process
%%   registered under that name. If the FSM returns that is returned
%%   from the call.
%% @end
%%--------------------------------------------------------------------
-spec call(fsm_ref(), _, timeout()) -> _.
%%--------------------------------------------------------------------
call(FSM, Event, Timeout) -> jhn_server:call(FSM, Event, Timeout).

%% Function: sync(FSM) -> ok.
%% @doc
%%   A sync is made to FSM with default timeout 5000ms.
%% @end
%%--------------------------------------------------------------------
-spec sync(fsm_ref()) -> ok.
%%--------------------------------------------------------------------
sync(FSM) -> sync(FSM, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------
%% Function: sync(FSM, Timeout) -> ok.
%% @doc
%%   A sync is made to FSM with Timeout. Will generate a timeout
%%   exception if the FSM takes more than Timeout time to answer.
%%   Generates an exception if the process is dead, dies, or no process
%%   registered under that name. If the fsm returns, ok is returned.
%% @end
%%--------------------------------------------------------------------
-spec sync(fsm_ref(), timeout()) -> _.
%%--------------------------------------------------------------------
sync(FSM, Timeout) -> jhn_server:sync(FSM, Timeout).

%%--------------------------------------------------------------------
%% Function: reply(Message) -> ok.
%% @doc
%%   Called inside call back function to provide reply to a call or event.
%%   If called twice will cause the fsm to crash.
%% @end
%%--------------------------------------------------------------------
-spec reply(_) -> ok.
%%--------------------------------------------------------------------
reply(Msg) -> jhn_server:reply(Msg).

%%--------------------------------------------------------------------
%% Function: reply(From, Message) -> ok.
%% @doc
%%   Called by any process will send a reply to the one that sent the
%%   event to the fsm, the From argument is the result from a call
%%   to from/1 inside a state function or event/3 callback function.
%% @end
%%--------------------------------------------------------------------
-spec reply(from(), _) -> ok.
%%--------------------------------------------------------------------
reply(From, Msg) -> jhn_server:reply(From, Msg).

%%--------------------------------------------------------------------
%% Function: from() -> From.
%% @doc
%%   Called inside a state function or event/3  callback
%%   function it will provide an opaque data data enables a reply to the
%%   call outside the scope of the callback function.
%% @end
%%--------------------------------------------------------------------
-spec from() -> from().
%%--------------------------------------------------------------------
from() -> jhn_server:from().

%%--------------------------------------------------------------------
%% Function: type() -> call or cast.
%% @doc
%%   Called inside a event/2 and it tells if it is a call or a cast.
%% @end
%%--------------------------------------------------------------------
-spec type() -> call | cast.
%%--------------------------------------------------------------------
type() -> jhn_server:type().

%%====================================================================
%% jhn_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init({Module, Argument}) ->
%% @private
%%--------------------------------------------------------------------
-spec init({atom(), opts}) -> jhn_server:init_return().
%%--------------------------------------------------------------------
init({Mod, Opts}) ->
    State =
        #state{name = plist:find(name, Opts, undefined),
               mod = Mod,
               event = erlang:function_exported(Mod, event, 3),
               message = erlang:function_exported(Mod, message, 3),
               terminate = erlang:function_exported(Mod, terminate, 3),
               code_change = erlang:function_exported(Mod, code_change, 4),
               format_status = erlang:function_exported(Mod, format_status, 2)},
    case Mod:init(plist:find(arg, Opts, no_arg)) of
        {ok, StateName, Data} ->
            {ok, State#state{state_name = StateName, data = Data}};
        {hibernate, StateName, Data} ->
            {hibernate, State#state{state_name = StateName, data = Data}};
        Other ->
            Other
    end.

%%--------------------------------------------------------------------
%% Function: request(Event, State) -> {}
%% @private
%%--------------------------------------------------------------------
-spec request(event(), #state{}) -> jhn_server:return().
%%--------------------------------------------------------------------
request(Event, State) ->
    #state{mod = Mod, state_name = StateName, data = Data} = State,
    case erlang:function_exported(Mod, StateName, 2) of
        true ->
            try Mod:StateName(Event, Data) of
                Return -> return(Return, request, Event, State)
            catch
                error:function_clause:Stack ->
                    case Stack of
                        [{Mod, StateName, _, _} | _]  when State#state.event ->
                            handle_all_states(Event, event, request, State);
                        [{Mod, StateName, _, _} | _] ->
                            unexpected(event, Event, State),
                            {ok, State};
                        _ ->
                            erlang:raise(error, function_clause, Stack)
                    end
            end;
        false ->
            handle_all_states(Event, event, request, State)
    end.

%%--------------------------------------------------------------------
%% Function: message(Event, State) -> {}
%% @private
%%--------------------------------------------------------------------
-spec message(event(), #state{}) -> jhn_server:return().
%%--------------------------------------------------------------------
message(Event, State = #state{message = false}) ->
    unexpected(message, Event, State),
    {ok, State};
message(Event, State) ->
    handle_all_states(Event, message, message, State).

%%--------------------------------------------------------------------
%% Function: terminate(Reason, Message, State) ->
%% @private
%%--------------------------------------------------------------------
-spec terminate(_, _, #state{}) -> none().
%%--------------------------------------------------------------------
terminate(Reason, Msg, State = #state{terminate = false}) ->
    case Reason of
        normal -> normal;
        shutdown -> shutdown;
        Shutdown = {shutdown, _} -> Shutdown;
        _ ->
            terminating(Reason, Msg, State),
            exit(Reason)
    end;
terminate(Reason, Msg, State) ->
    #state{mod = Mod, state_name = StateName, data = Data} = State,
    try Mod:terminate(Reason, StateName, Data) of
        _ ->
            terminate(Reason, Msg, State#state{terminate = false})
    catch
        _:Reason1 ->
            terminate(Reason1, Msg, State#state{terminate = false})
    end.

%%--------------------------------------------------------------------
%% Function: system_code_change(OldVsn, State, Extra) ->
%% @private
%%--------------------------------------------------------------------
-spec code_change(_, #state{}, _) -> none().
%%--------------------------------------------------------------------
code_change(_, State= #state{code_change = false}, _) -> {ok, State};
code_change(OldVsn, State, Extra) ->
    #state{data = Data, mod = Mod, state_name = StateName} = State,
    case  Mod:code_change(OldVsn, StateName, Data, Extra) of
        {ok, NewStateName, NewData} ->
            {ok, State#state{state_name = NewStateName, data = NewData}};
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% Function: format_status(Opt, StatusData) -> Status.
%% @private
%%--------------------------------------------------------------------
-spec format_status(_, _) -> [{atom(), _}].
%%--------------------------------------------------------------------
format_status(Opt, [PDict, State]) ->
    #state{mod = Mod, state_name = StateName, data = Data} = State,
    NameTag =
        case State#state.name of
            Name when is_pid(Name) -> pid_to_list(Name);
            Name when is_atom(Name) -> Name
        end,
    Header = lists:concat(["Status for jhn fsm ", NameTag]),
    Specfic =
        case State#state.format_status of
            true ->
                try Mod:format_status(Opt, [PDict, Data])
                catch
                    _:_ -> [{data, [{"State", State}]}]
                end;
            false ->
                [{data, [{"State", State}]}]
        end,
    [{header, Header},
     {data, [{"StateName", StateName}]} |
     Specfic].

%%====================================================================
%% logger callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: report_to_format(Report, FormatOpts) -> Format.
%% @private
%%--------------------------------------------------------------------
-spec report_to_format(logger:report(), logger:report_cb_config()) ->
          unicode:chardata().
%%--------------------------------------------------------------------
report_to_format(Report = #{label := {?MODULE, unexpected}}, _) ->
    #{id := Id, pid := Pid, state := State, type := Type, message := Msg} =
        Report,
    Format = "**  JHN FSM ~p(~p) in ~p received unexpected ~p:~n**  ~p~n",
    io_lib:format(Format, [Id, Pid, State, Type, Msg]);
report_to_format(Report = #{label := {?MODULE, terminating}}, _) ->
    #{name := Name,
      state := State,
      data := Data,
      message := Msg,
      reason := Reason} = Report,
    {Format, Args} =
        case Msg of
            [] ->
                {"** JHN FSM ~p in ~p terminating ~n"
                 "** When FSM state == ~p~n"
                 "** Reason for termination == ~n** ~p~n",
                 [Name, State, Data, Reason]};
            _ ->
                {"** JHN FSM ~p in ~p terminating ~n"
                 "** Last message in was ~p~n"
                 "** When FSM state == ~p~n"
                 "** Reason for termination == ~n** ~p~n",
                 [Name, State, Msg, Data, Reason]}
        end,
    io_lib:format(Format, Args).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
handle_all_states(Event, CallBack, Type, State) ->
    #state{state_name = StateName, mod = Mod, data = Data} = State,
    try Mod:CallBack(Event, StateName, Data) of
        Return -> return(Return, Type, Event, State)
    catch
        error:function_clause:Stack  ->
            case Stack of
                [{Mod, CallBack, _, _} | _] ->
                    unexpected(CallBack, Event, State),
                    {ok, State};
                _ ->
                    erlang:raise(error, function_clause, Stack)
            end
    end.

%%--------------------------------------------------------------------
return({ok, NewStateName, NewData}, _, _, State) ->
    State1 = State#state{state_name = NewStateName, data = NewData},
    return1(State#state.state_name, ok, State1);
return({hibernate, NewStateName, NewData}, _, _, State) ->
    State1 = State#state{state_name = NewStateName, data = NewData},
    return1(State#state.state_name, hibernate, State1);
return(deferred, Type, Msg, State) ->
    return1(State#state.state_name, ok, insert({Type, Msg}, State));
return(Other, _, _, _) ->
    Other.

return1(N, OK, State = #state{state_name = N, handling_deferred = false}) ->
    {OK, State};
return1(Name, OK, State = #state{state_name = Name}) ->
    case remove(State, continue) of
        State1 = #state{} -> {OK, State1};
        {{request, Event}, State1} -> request(Event, State1);
        {{message, Msg}, State1} -> message(Msg, State1)
    end;
return1(_, OK, State) ->
    case remove(State, restart) of
        State1 = #state{} -> {OK, State1};
        {{request, Event}, State1} -> request(Event, State1);
        {{message, Msg}, State1} -> message(Msg, State1)
    end.

%%--------------------------------------------------------------------
unexpected(Type, Msg, State) ->
    Id = case State#state.name of
             undefined -> State#state.mod;
             Name -> Name
         end,
    Report = #{label => {?MODULE, unexpected},
               pid => self(),
               id => Id,
               state => State#state.state_name,
               type => Type,
               message => Msg},
    Meta = #{domain => [jhn],
             tag => error,
             report_cb => fun ?MODULE:report_to_format/2},
    logger:log(error, Report, Meta).

terminating(Reason, Msg, State = #state{name = Name}) ->
    Name1 = case Name of
                undefined -> self();
                _ -> Name
            end,
    Report = #{label => {?MODULE, terminating},
               name => Name1,
               state => State#state.state_name,
               data => State#state.data,
               message => Msg,
               reason => Reason},
    Meta = #{domain => [jhn],
             tag => error,
             report_cb => fun ?MODULE:report_to_format/2},
    logger:log(error, Report, Meta).

%%--------------------------------------------------------------------
insert(Elt, State  = #state{handling_deferred = true}) ->
    {Deferred, New} = State#state.deferred,
    State#state{deferred = {Deferred, [Elt | New]}};
insert(Elt, State = #state{deferred = {Deferred, []}}) ->
    State#state{deferred = {[Elt | Deferred], []}}.

%%--------------------------------------------------------------------
remove(State = #state{deferred = {[], New}}, restart) ->
    remove(State#state{deferred = {lists:reverse(New), []}}, continue);
remove(State = #state{deferred = {[], New}}, continue) ->
    State#state{deferred = {New, []}, handling_deferred = false};
remove(State = #state{deferred = {Tail, []}, handling_deferred = false}, _) ->
    [Elt | Head] = lists:reverse(Tail),
    {Elt, State#state{deferred = {Head, []}, handling_deferred = true}};
remove(State = #state{deferred = {Head, New}}, restart) ->
    remove(State#state{deferred = {lists:reverse(New) ++ Head, []}}, continue);
remove(State = #state{deferred = {[Elt | Head], New}}, continue) ->
    {Elt, State#state{deferred = {Head, New}}}.
