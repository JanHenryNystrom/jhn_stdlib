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
%%%  A simple lightweight mocking library ...
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
         load_app/2, unload_app/1, start_app/2
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
-type app() :: atom().
-type env() :: [{atom(), _}].

-type module_opt() :: jhn_server:opt().
-type module_opts() :: [module_opt()].

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
-spec hide(module(), module() | pid() | {name, name()}, [{name(), arity()}]) ->
          ok | {error, already_hidden}.
%%--------------------------------------------------------------------
hide(Mod, CB, Funcs) ->
    Hid = hidden_name(Mod),
    case code:is_loaded(Hid) of
        false ->
            Forms = gen(Mod, Hid, CB, exported(Mod), Funcs),
            {ok, _, Bin} = compile:forms(Forms, [return_errors, binary]),
            {module, _} = code:load_binary(Mod, ?SHADOW_MODULE, Bin),
            {module, _} = code:load_binary(Hid,?HIDDEN_MODULE,rename(Mod, Hid)),
            ok;
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
-spec load_mod(binary() | string()) -> ok.
%%--------------------------------------------------------------------
load_mod(B) when is_binary(B) -> load_mod(binary_to_list(B));
load_mod(S) ->
    {ok, Mod, Bin} = compile:forms(forms(S), [return_errors, binary]),
    {module, _} = code:load_binary(Mod, ?LOADED_MODULE, Bin),
    ok.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec load_mod(module(), [binary() | string()]) -> ok.
%%--------------------------------------------------------------------
load_mod(Mod, Funs) ->
    Forms = forms_mod_funs(Mod, Funs),
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors, binary]),
    {module, _} = code:load_binary(Mod, ?LOADED_MODULE, Bin),
    ok.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec unload_mod(module()) -> ok.
%%--------------------------------------------------------------------
unload_mod(Mod) ->
    true = is_boolean(code:purge(Mod)),
    true = code:delete(Mod),
    ok.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec load_app(app(), application_opts()) -> ok.
%%--------------------------------------------------------------------
load_app(Name, Opts) ->
    ApplicationOpts = parse_application_opts(Opts),
    App = {application, Name, [{description, ?APP_DESC} | ApplicationOpts]},
    application:load(App).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec unload_app(app()) -> ok.
%%--------------------------------------------------------------------
unload_app(Name) ->
    case application:get_all_key(Name) of
        {ok, KV} ->
            case lists:keyfind(description, 1, KV) of
                {value, {_, ?APP_DESC}} ->
                    case lists:keyfind(mod, 1, KV) of
                        {value, {_, {Mod, _}}} ->
                            case code:is_loaded(Mod) of
                                {file, ?LOADED_MODULE} -> unload_mod(Mod);
                                _ -> ok
                            end;
                        _ ->
                            ok
                    end,
                    application:unload(Name);
                _ ->
                    erlang:error(badarg, [Name])
            end;
        _ ->
            erlang:error(badarg, [Name])
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec start_app(app(), application_opts()) -> ok.
%%--------------------------------------------------------------------
start_app(Name, Opts) ->
    load_app(Name, Opts),
    application:start(Name).

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
    {Exported, Funcs} = compile_functions(Fs, [], []),
    [{attribute, 0, module, Mod}, {attribute, 0, export, Exported} | Funcs].

compile_functions([], Exported, Forms) ->
    {lists:reverse(Exported), lists:reverse(Forms)};
compile_functions([F | Fs], Exported, Forms) when is_binary(F) ->
    compile_functions([binary_to_list(F) | Fs], Exported, Forms);
compile_functions([F | Fs], Exported, Forms) ->
    {ok, Tokens, _} = erl_scan:string(F),
    {ok, Fun = {function, _, Name, Arity, _}} = erl_parse:parse_form(Tokens),
    compile_functions(Fs, [{Name, Arity} | Exported], [Fun | Forms]).

forms(S) -> forms_string(split(S, [], []), []).

forms_string([], Acc) -> lists:reverse(Acc);
forms_string([H | T], Acc) ->
    {ok, Tokens, _} = erl_scan:string(H),
    {ok, Parsed} = erl_parse:parse_form(Tokens),
    forms_string(T, [Parsed | Acc]).

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
    case lists:all(fun({Key, _}) -> is_atom(Key) end, Env) of
        true -> [{env, Env}];
        false -> erlang:error(badarg, [Env])
    end;
parse_application_opts(Map = #{}) ->
    [parse_application_opt(K, V) || K := V <- Map].

parse_application_opt(env, Env) ->
    case lists:all(fun({Key, _}) -> is_atom(Key) end, Env) of
        true -> {env, Env};
        false -> erlang:error(badarg, [Env])
    end;
parse_application_opt(applications, Applications) ->
    case lists:all(fun(App) -> is_atom(App) end, Applications) of
        true -> {applications, Applications};
        false -> erlang:error(badarg, [Applications])
    end;
parse_application_opt(mod, ModArg = {Mod, _}) ->
    case is_atom(Mod) of
        true -> {mod, ModArg};
        false -> erlang:error(badarg, [mod, ModArg])
    end;
parse_application_opt(sup, {App, Mod, Args}) ->
    case is_atom(App) andalso is_atom(Mod) of
        false -> erlang:error(badarg, [sup, {App, Mod, Args}]);
        true ->
            CBM = callback_name(App),
            Start = ["start(Args) -> apply(", Mod, ", start_link, Args)."],
            load_mod(CBM, [lists:concat(Start), "stop(_) -> ok."]),
            {mod, {CBM, Args}}
    end;
parse_application_opt(sup, {App, Mod, Func, Args}) ->
    case is_atom(App) andalso is_atom(Mod) of
        false -> erlang:error(badarg, [sup, {App, Mod, Func, Args}]);
        true ->
            CBM = callback_name(App),
            Start = ["start(Args) -> apply(", Mod, ", ", Func, ", Args)."],
            load_mod(CBM, [lists:concat(Start), "stop(_) -> ok."]),
            {mod, {CBM, Args}}
    end.

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
