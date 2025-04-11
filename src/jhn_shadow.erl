%%==============================================================================
%% Copyright 2025 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A simple lightweight mocking library that allows the mocking of modules
%%%  and applications and the simplified loading of modules used to mock.
%%%
%%%  A module can be mocked for some or all of its exported functions, statless
%%%  or with a state that can be inspected and interacted with to provide more
%%%  intricate mocking behaviour. The mocked module must be compiled in the
%%%  load path but no special flags or preparation of the module is needed.
%%%
%%%  When the mocking is no longer required the module can be "revealed" and
%%%  used normally. The original module is loaded.
%%%
%%%  To be noted is that in the mocked module for unmocked (passthrough)
%%%  functions any use of the atom of the module name is replaced with
%%%  hidden_{original module name}.
%%%
%%%  
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2025, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_shadow).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([create/2, destroy/1,
         hide/3, reveal/1, peek/2,
         load_mod/1, load_mod/2, unload_mod/1,
         load_app/2, unload_app/1, start_app/2, start_all_app/2, stop_app/1
        ]).

%% jhn_server callbacks
-export([init/1, request/2, format_status/2]).

%% logger callback
-export([report_to_format/2]).

%% Callbacks

%% hide
-callback hide(module(), name(), [_]) -> _.

%% hidden by shadow
-callback init(_) -> {ok, _}.
-callback hide(module(), name(), [_], State) -> {ok, _, State}.
-callback peek(_, State) -> {ok, _, State}.

-optional_callbacks([hide/3, init/1, hide/4, peek/2]).

%% Defines
-define(ATOM_KEY, "AtU8").

-define(APP_DESC, "Shadow app.").
-define(LOADED_MODULE, "jhn_shadow loaded module").
-define(SHADOW_MODULE, "jhn_shadow shadow module").
-define(HIDDEN_MODULE, "jhn_shadow hidden module").

%% Types
-type name() :: atom().
-type fa()   :: {name(), arity()}.
-type app()  :: atom().
-type env()  :: [{atom(), _}].

-type module_opts() :: jhn_server:opts().

-type application_opt() :: env() | #{env => env(),
                                     applications => [atom()],
                                     mod => {module(), _},
                                     sup => {app(), module(), [_]},
                                     sup => {app(), module(), name(), [_]}}.
-type application_opts() :: [application_opt()].

-type shadow() :: pid().

%% Records
-record(state, {name             :: pid() | atom(),
                mod              :: atom(),
                data = undefined :: _,
                %% Optional callbacks
                peek             :: boolean()
               }).

%% ===================================================================
%% Library functions
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec create(module(),  module_opts()) -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
create(Mod, Opts) ->
    jhn_server:create(?MODULE, jhn_plist:replace(arg, {Mod, Opts}, Opts)).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec destroy(shadow()) -> ok.
%%--------------------------------------------------------------------
destroy(Shadow) -> jhn_server:cast(Shadow, stop).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec hide(module(), module() | pid() | {name, name()}, all | [fa()]) ->
          ok | {error, _} | {error, _, _}.
%%--------------------------------------------------------------------
hide(Mod, CB, all) -> hide(Mod, CB, exported(Mod));
hide(Mod, CB, Funcs) ->
    Hid = hidden_name(Mod),
    case code:is_loaded(Hid) of
        false ->
            Forms = gen(Mod, Hid, CB, exported(Mod), Funcs),
            case compile:forms(Forms, [return_errors, binary]) of
                {ok, _, Bin} ->
                    {module, _} = code:load_binary(Mod, ?SHADOW_MODULE, Bin),
                    {module, _} =
                        code:load_binary(Hid,?HIDDEN_MODULE,rename(Mod, Hid)),
                    ok;
                Error ->
                    Error
            end;
        _ ->
            {error, already_hidden}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec reveal(module()) -> ok | {error, not_hidden}.
%%--------------------------------------------------------------------
reveal(Mod) ->
    case code:is_loaded(Mod) of
        {file, ?SHADOW_MODULE} ->
            Hidden = hidden_name(Mod),
            {Mod, B, File} = code:get_object_code(Mod),
            {module, _} = code:load_binary(Mod, File, B),
            unload_mod(Hidden);
        {file, _} ->
            {error, not_hidden};
        false ->
            {error, not_hidden}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec peek(shadow(), _) -> _.
%%--------------------------------------------------------------------
peek(Shadow, Arg) -> jhn_server:call(Shadow, {peek, Arg}) .

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec load_mod(binary() | string()) -> ok | {error, _} | {error, _, _}.
%%--------------------------------------------------------------------
load_mod(B) when is_binary(B) -> load_mod(binary_to_list(B));
load_mod(S) ->
    case forms(S) of
        {ok, Forms} ->
            case compile:forms(Forms, [return_errors, binary]) of
                {ok, Mod, Bin} ->
                    {module, _} = code:load_binary(Mod, ?LOADED_MODULE, Bin),
                    ok;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec load_mod(module(), [binary() | string()]) ->
          ok | {error, _} | {error, _, _}.
%%--------------------------------------------------------------------
load_mod(Mod, Funs) ->
    case forms_mod_funs(Mod, Funs) of
        {ok, Forms} ->
            case compile:forms(Forms, [return_errors, binary]) of
                {ok, Mod, Bin} ->
                    {module, _} = code:load_binary(Mod, ?LOADED_MODULE, Bin),
                    ok;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec unload_mod(module()) -> ok | {error, not_loaded | not_shadow}.
%%--------------------------------------------------------------------
unload_mod(Mod) ->
    Unload = fun() ->
                     true = is_boolean(code:purge(Mod)),
                     true = code:delete(Mod),
                     ok
             end,
    case code:is_loaded(Mod) of
        {file, ?HIDDEN_MODULE} -> Unload();
        {file, ?LOADED_MODULE} -> Unload();
        {file, _} -> {error, not_loaded};
        false -> {error, not_loaded}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec load_app(app(), application_opts()) -> ok | {error, _}.
%%--------------------------------------------------------------------
load_app(Name, Opts) ->
    case parse_application_opts(Opts) of
        {ok, AppOpts} ->
            App = {application, Name, [{description, ?APP_DESC} | AppOpts]},
            application:load(App);
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec unload_app(app()) -> ok | {error, not_loaded | not_shadow_application}.
%%--------------------------------------------------------------------
unload_app(Name) ->
    case application:get_all_key(Name) of
        {ok, KV} ->
            case {jhn_plist:find(description, KV), jhn_plist:find(mod, KV)} of
                {?APP_DESC, {Mod, _}} ->
                    case code:is_loaded(Mod) of
                        {file, ?LOADED_MODULE} -> unload_mod(Mod);
                        _ -> ok
                    end,
                    application:unload(Name);
                {?APP_DESC, _} ->
                    application:unload(Name);
                _ ->
                    {error, not_shadow_application}
            end;
        _ ->
            {error, not_loaded}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec start_app(app(), application_opts()) -> ok | {error, _}.
%%--------------------------------------------------------------------
start_app(Name, Opts) ->
    case load_app(Name, Opts) of
        ok -> application:start(Name);
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec start_all_app(app(), application_opts()) -> ok | {error, _}.
%%--------------------------------------------------------------------
start_all_app(Name, Opts) ->
    case load_app(Name, Opts) of
        ok ->
            case application:ensure_all_started(Name) of
                {ok, Started} ->
                    application:set_env(Name,
                                        jhn_shadow_started,
                                        lists:delete(Name, Started));
                Error ->
                    unload_app(Name),
                    Error
            end;
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec stop_app(app()) -> ok | {error, _}.
%%--------------------------------------------------------------------
stop_app(Name) ->
    case lists:keymember(Name, 1, application:which_applications()) of
        true ->
            ok = application:stop(Name),
            Apps = application:get_env(Name, jhn_shadow_started, []),
            [application:stop(App) || App <- Apps],
            [application:unload(App) || App <- Apps],
            unload_app(Name);
        false ->
            {error, not_running}
    end.

%%====================================================================
%% jhn_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init({Module, Argument}) ->
%% @private
%%--------------------------------------------------------------------
-spec init({atom(), module_opts()}) -> jhn_server:init_return(#state{}).
%%--------------------------------------------------------------------
init({Mod, Opts}) ->
    State = #state{name = jhn_plist:find(name, Opts, undefined),
                   mod = Mod,
                   peek = erlang:function_exported(Mod, peek, 2)},
    case erlang:function_exported(Mod, init, 1) of
        false -> {ok, State};
        _ ->
            case Mod:init(jhn_plist:find(arg, Opts, no_arg)) of
                {ok, Data} -> {ok, State#state{data = Data}};
                Other -> {stop, Other}
            end
    end.

%%--------------------------------------------------------------------
%% Function: request(Event, State) -> {}
%% @private
%%--------------------------------------------------------------------
-spec request(_, #state{}) -> jhn_server:return(#state{}).
%%--------------------------------------------------------------------
request(stop, _) -> {stop, normal};
request({hide, Module, Func, Args}, State = #state{mod = Mod, data = Data}) ->
    try Mod:hide(Module, Func, Args, Data) of
        {ok, Reply, Data1} ->
            jhn_server:reply(Reply),
            {ok, State#state{data = Data1}};
        Other ->
            {stop, Other}
    catch Class:Error ->
            {stop, {Class, Error}}
    end;
request({peek, Arg}, State = #state{peek = false}) ->
    unexpected(peek, Arg, State),
    {ok, State};
request({peek, Arg}, State = #state{mod = Mod, data = Data}) ->
    try Mod:peek(Arg, Data) of
        {ok, Result, Data1} ->
            jhn_server:reply(Result),
            {ok, State#state{data = Data1}};
        Other -> {stop, Other}
    catch Class:Error ->
            {stop, {Class, Error}}
    end;
request(Request, State) ->
    unexpected(request, Request, State),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: format_status(Opt, StatusData) -> Status.
%% @private
%%--------------------------------------------------------------------
-spec format_status(_, _) -> [{atom(), _}].
%%--------------------------------------------------------------------
format_status(_, [_, State]) ->
    NameTag =
        case State#state.name of
            undefined -> pid_to_list(self());
            Name when is_atom(Name) -> Name
        end,
    Header = lists:concat(["Status for jhn shadow ", NameTag]),
    [{header, Header}, {data, [{"State", State}]}].

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
    Format = "**  JHN shadow ~p(~p) received unexpected ~p:~n**  ~p~n",
    io_lib:format(Format, [Id, Pid, Type, Msg]).

%% ===================================================================
%% Internal functions
%% ===================================================================

%%--------------------------------------------------------------------
%% Hide Modules
%%--------------------------------------------------------------------

hidden_name(Mod) -> binary_to_atom(<<"hidden_", (atom_to_binary(Mod))/binary>>).

exported(Mod) ->
    [{F, A} || {F, A} <- Mod:module_info(exports),
               {F, A} /= {module_info, 0},
               {F, A} /= {module_info, 1},
               not erlang:is_builtin(Mod, F, A)].

gen(Mod, Hidden, CB, Exported, Funcs) ->
    [{attribute, 0, module, Mod},
     {attribute, 0, export, Exported} |
     [hidden(CB, Mod, Name, Arity) || {Name, Arity} <- Funcs] ++
     [passthrough(Hidden, Name, Arity) || {Name, Arity} <- Exported -- Funcs]
    ].

hidden(CB, Mod, Name, Arity) when is_atom(CB) ->
    Args = args(Arity),
    HideArgs = [atom(Mod), atom(Name), list(Args)],
    {function, 0, Name, Arity, [clause(Args, [call(CB, hide, HideArgs)])]};
hidden({name, Shadow}, Mod, Name, Arity) when is_atom(Shadow) ->
    Args = args(Arity),
    Tuple = tuple([atom(hide), atom(Mod), atom(Name), list(Args)]),
    Call = call(jhn_server, call, [atom(Shadow), Tuple]),
    {function, 0, Name, Arity, [clause(Args, [Call])]};
hidden(Shadow, Mod, Name, Arity) when is_pid(Shadow) ->
    Args = args(Arity),
    To = call(list_to_pid, [string(pid_to_list(Shadow))]),
    Tuple = tuple([atom(hide), atom(Mod), atom(Name), list(Args)]),
    Call = call(jhn_server, call, [To, Tuple]),
    {function, 0, Name, Arity, [clause(Args, [Call])]}.

passthrough(Hidden, Name, Arity) ->
    Args = args(Arity),
    {function, 0, Name, Arity, [clause(Args, [call(Hidden, Name, Args)])]}.

clause(Pattern, Body) -> {clause, 0, Pattern, [], Body}.

call(Name, Args) -> {call, 0, atom(Name), Args}.
call(Mod, Name, Args) -> {call, 0, {remote, 0, atom(Mod), atom(Name)}, Args}.

atom(Atom) -> {atom, 0, Atom}.

tuple(Elts) -> {tuple, 0, Elts}.

list([]) -> {nil, 0};
list([H | T]) -> {cons, 0, H, list(T)}.

string(S) -> {string, 0, S}.

args(Arity) -> [{var, 0, arg(N)} || N <- lists:seq(1, Arity)].

arg(N) -> binary_to_atom(<<"A", (integer_to_binary(N))/binary>>).

rename(Mod, Name) when is_atom(Name) -> rename(Mod, atom_to_binary(Name));
rename(Mod, Name) ->
    {Mod, B, _} = code:get_object_code(Mod),
    {ok, Mod, Cs} = beam_lib:all_chunks(B),
    {value, {_, <<Atoms:32, Sz:8, _:Sz/binary, T/binary>>}, Cs1} =
        lists:keytake(?ATOM_KEY, 1, Cs),
    Sz1 = byte_size(Name),
    AtomsChunk = <<Atoms:32, Sz1:8, Name:Sz1/binary, T/binary>>,
    {ok, B1} = beam_lib:build_module([{?ATOM_KEY, AtomsChunk} | Cs1]),
    B1.

%%--------------------------------------------------------------------
%% Load Modules
%%--------------------------------------------------------------------

forms_mod_funs(Mod, Fs) ->
    case compile_functions(Fs, [], []) of
        {ok, Exported, Funcs} ->
            {ok, [{attribute, 0, module, Mod},
                  {attribute, 0, export, Exported} | Funcs]};
        Error -> Error
    end.

compile_functions([], Exported, Forms) ->
    {ok, lists:reverse(Exported), lists:reverse(Forms)};
compile_functions([F | Fs], Exported, Forms) when is_binary(F) ->
    compile_functions([binary_to_list(F) | Fs], Exported, Forms);
compile_functions([F | Fs], Exported, Forms) ->
    case erl_scan:string(F) of
        {ok, Tokens, _} ->
            case erl_parse:parse_form(Tokens) of
                {ok, Fun = {function, _, Name, Arity, _}} ->
                    Exported1 = [{Name, Arity} | Exported],
                    compile_functions(Fs, Exported1, [Fun | Forms]);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

forms(S) -> forms_string(split(S, [], []), []).

forms_string([], Acc) -> {ok, lists:reverse(Acc)};
forms_string([H | T], Acc) ->
    case erl_scan:string(H) of
        {ok, Tokens, _} ->
            case erl_parse:parse_form(Tokens) of
                {ok, Parsed} ->
                    forms_string(T, [Parsed | Acc]);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

split([], [], Forms) -> lists:reverse(Forms);
split([$\n | T], Acc, Forms) -> split(T, Acc, Forms);
split([$. | T], Acc, Forms) -> split(T, [], [lists:reverse([$.|Acc]) | Forms]);
split([$" | T], Acc, Forms) -> split_string(T, [$" | Acc], Forms);
split([H | T], Acc, Forms) -> split(T, [H | Acc], Forms).

split_string([$\\, $" | T], Acc, Forms) ->
    split_string(T, [$", $\\ | Acc], Forms);
split_string([$" | T], Acc, Forms) ->
    split(T, [$" | Acc], Forms);
split_string([H | T], Acc, Forms) ->
    split_string(T, [H | Acc], Forms).

%%--------------------------------------------------------------------
%% Load Application
%%--------------------------------------------------------------------
parse_application_opts(Env) when is_list(Env) ->
    parse_application_opts(#{env => Env});
parse_application_opts(Map = #{}) ->
    Keys = [parse_application_opt(K, V) || K := V <- Map],
    case jhn_plist:member(error, Keys) of
        true ->
            {error, [Error || {error, Error} <- Keys]};
        false ->
            {ok, Keys}
    end.

parse_application_opt(env, Env) ->
    case lists:all(fun({Key, _}) -> is_atom(Key) end, Env) of
        true -> {env, Env};
        false -> {error, {bad_env, Env}}
    end;
parse_application_opt(applications, Apps) ->
    case is_list(Apps) andalso lists:all(fun(App) -> is_atom(App) end, Apps) of
        true -> {applications, Apps};
        false -> {error, {bad_applications, Apps}}
    end;
parse_application_opt(mod, ModArg = {Mod, _}) ->
    case is_atom(Mod) of
        true -> {mod, ModArg};
        false -> {error, {bad_mod, ModArg}}
    end;
parse_application_opt(sup, {App, Mod, Args}) ->
    case is_atom(App) and is_atom(Mod) and is_list(Args) of
        false -> {error, {bad_sup, sup, {App, Mod, Args}}};
        true ->
            CBM = callback_name(App),
            Start = ["start(_, Args) -> apply(", Mod, ", start_link, Args)."],
            load_mod(CBM, [lists:concat(Start), "stop(_) -> ok."]),
            {mod, {CBM, Args}}
    end;
parse_application_opt(sup, {App, Mod, Func, Args}) ->
    case lists:all(fun erlang:is_atom/1, [App, Mod, Func]) and is_list(Args) of
        false -> {error, {bad_sup, {App, Mod, Func, Args}}};
        true ->
            CBM = callback_name(App),
            Start = ["start(_, Args) -> apply(", Mod, ", ", Func, ", Args)."],
            load_mod(CBM, [lists:concat(Start), "stop(_) -> ok."]),
            {mod, {CBM, Args}}
    end;
parse_application_opt(Key, Op) ->
    {error, {bad_opt, {Key, Op}}}.


callback_name(App) -> binary_to_atom(<<(atom_to_binary(App))/binary, "_app">>).

%%--------------------------------------------------------------------
%% Logging
%%--------------------------------------------------------------------
unexpected(Type, Msg, State) ->
    Id = case State#state.name of
             undefined -> State#state.mod;
             Name -> Name
         end,
    Report = #{label => {?MODULE, unexpected},
               pid => self(),
               id => Id,
               type => Type,
               message => Msg},
    Meta = #{domain => [jhn],
             tag => error,
             report_cb => fun ?MODULE:report_to_format/2},
    logger:log(error, Report, Meta).

%%--------------------------------------------------------------------
%% Common
%%--------------------------------------------------------------------
