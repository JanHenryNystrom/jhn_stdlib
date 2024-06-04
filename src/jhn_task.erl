%==============================================================================
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
%%%   A generic task.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_task).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% API
-export([create/1, create/2]).

%% proc_lib callbacks
-export([init/4]).

%% Defines
-define(DEFAULT_TIMEOUT, 5000).

%% Records
-record(opts, {arg     = no_arg           :: no_arg | _,
               link    = true             :: boolean(),
               timeout = ?DEFAULT_TIMEOUT :: timeout(),
               errors  = []               :: [_]
              }).

-record(state, {parent             :: pid(),
                mod                :: atom(),
                data   = undefined :: _,
                %% Optional callbacks
                init               :: boolean()
               }).

%% Types
-type opt()  :: {atom(), _}.
-type opts() :: [opt()].

-type init_return(State) :: ignore | return(State).
-type return(State)      :: {ok, State} | {error, _} | {stop, _}.

%% Exported Types
-export_type([init_return/1, return/1]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

-callback init(State) -> init_return(State).

-callback do(State) -> return(State).

-optional_callbacks([init/1]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: create(CallbackModule) -> Result.
%% @doc
%%   Creates a jhn_task.
%% @end
%%--------------------------------------------------------------------
-spec create(atom()) -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
create(Mod) -> create(Mod, []).

%%--------------------------------------------------------------------
%% Function: create(CallbackModule, Options) -> Result.
%% @doc
%%   Creates a jhn_task with options.
%%   Options are:
%%     {link, Boolean} -> if the task is linked to the parent, default true
%%     {timeout, infinity | Integer} -> Time in ms for the task to create and
%%         initialise, after that an exception is generated, default 5000.
%%     {arg, Term} -> argument provided to the init/1 callback function,
%%         default is 'no_arg'.
%% @end
%%--------------------------------------------------------------------
-spec create(atom(),  opts()) -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
create(Mod, Options) ->
    case opts(Options, #opts{}) of
        #opts{errors = Errors = [_ | _]} -> {error, Errors};
        Opts = #opts{link = true, arg = A, timeout = T} ->
            proc_lib:start_link(?MODULE, init, [Mod, A, Opts, self()], T);
        Opts = #opts{arg = A, timeout = T} ->
            proc_lib:start(?MODULE, init, [Mod, A, Opts, self()], T)
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
    State = #state{parent = Parent, mod = Mod},
    case erlang:function_exported(Mod, init, 1) of
        true ->
            try Mod:init(Arg) of
                {ok, Data} ->
                    proc_lib:init_ack(Parent, {ok, self()}),
                    do(State#state{data = Data});
                ignore ->
                    fail(Parent, State, ignore, normal);
                {error, _}  = Reason ->
                    fail(Parent, State, Reason, normal);
                {stop, Reason} ->
                    fail(Parent, State, {error, Reason}, Reason);
                Other ->
                    Bad = {bad_return_value, Other},
                    fail(Parent, State, {error, Bad}, Bad)
            catch
                Class:Reason:Stack ->
                    Error = {Class, Reason, Stack},
                    fail(Parent, State, {error, Error}, Error)
            end;
        false ->
            do(State)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

do(State = #state{data = Data}) ->
    try Mod:do(Data) of
        {ok, Data} -> terminate(normal, State);
        {error, _}  = Reason -> terminate(Reason, State);
        Other -> terminate({error, {bad_return_value, Other}}, State)
    catch
        Class:Reason:Stack -> terminate({error, {Class, Reason, Stack}}, State)
    end.

%%--------------------------------------------------------------------
opts([], Opts) -> Opts;
opts([{link, Value} | T], Opts) when is_boolean(Value) ->
    opts(T, Opts#opts{link = Value});
opts([{timeout, infinity} | T], Opts) ->
    opts(T, Opts#opts{timeout = infinity});
opts([{timeout, Value} | T], Opts) when is_integer(Value), Value > 0 ->
    opts(T, Opts#opts{timeout = Value});
opts([{arg, Value} | T], Opts) ->
    opts(T, Opts#opts{arg = Value});
opts([H | T], Opts = #opts{errors = Errors}) ->
    opts(T, Opts#opts{errors = [H | Errors]}).

%%--------------------------------------------------------------------
fail(Parent, State, Reason, Exit) ->
    proc_lib:init_fail(Parent, Reason, {exit, Exit}).

%%--------------------------------------------------------------------
terminate(normal, _) -> exit(normal);
terminate(shutdown, _) -> exit(shutdown);
terminate(Shutdown = {shutdown, _}, _) -> exit(Shutdown);
terminate(Reason, state{data = Data}) ->
    Report = #{label => {?MODULE, terminating},
               pid => self(),
               data => Data,
               reason => Reason},
    Meta = #{tag => error, report_cb => fun report_to_format/1},
    logger:log(error, Report, Meta),
    exit(Reason).

%%--------------------------------------------------------------------
report_to_format(Report = #{label := {?MODULE, terminating}}) ->
    #{pid := Pid, data := Data, message := Msg, reason := Reason} = Report,
    {"** JHN task ~p terminating ~n"
     "** When Task state == ~p~n"
     "** Reason for termination == ~n** ~p~n",
     [Pid, Data, Reason]};
    end.
