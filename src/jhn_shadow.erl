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
%%%  A simple lightweight mocking library ...
%%%
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_shadow).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([hide/3, reveal/1,
         load_module/1, unload_module/1,
         load_application/1, load_application/2,
         start_application/1, start_application/2
        ]).

%% Defines
-define(ATOM_KEY, "AtU8").

%% Types
-type name() :: atom().
-type env() :: [{atom(), _}].

%% Callbacks
-callback hide(module(), name(), [_]) -> _.

%% ===================================================================
%% Library functions
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec hide(module(), module(), [{name(), arity()}]) ->
          ok | {error, already_hidden}.
%%--------------------------------------------------------------------
hide(Mod, CB, Funcs) ->
    Hidden = hidden_name(Mod),
    case code:is_loaded(Hidden) of
        false ->
            Forms = gen(Mod, Hidden, CB, exported(Mod), Funcs),
            {ok, _, Bin} = compile:forms(Forms, [return_errors, binary]),
            {module, _} = code:load_binary(Mod, "Shadow", Bin),
            {module, _} =
                code:load_binary(Hidden, "Hidden", rename(Mod, Hidden)),
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
        {file, "Shadow"} ->
            Hidden = hidden_name(Mod),
            {Mod, B, File} = code:get_object_code(Mod),
            {module, _} = code:load_binary(Mod, File, B),
            unload_module(Hidden);
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
-spec load_module(string()) -> ok.
%%--------------------------------------------------------------------
load_module(S) ->
    {ok, Mod, Bin} = compile:forms(forms(S), [return_errors, binary]),
    {module, _} = code:load_binary(Mod, "", Bin),
    ok.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec unload_module(module()) -> ok.
%%--------------------------------------------------------------------
unload_module(Mod) ->
    true = is_boolean(code:purge(Mod)),
    true = code:delete(Mod),
    ok.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec load_application(name()) -> ok.
%%--------------------------------------------------------------------
load_application(Name) -> load_application(Name, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec load_application(name(), env()) -> ok.
%%--------------------------------------------------------------------
load_application(Name, Env) ->
    App = {application, Name, [{description, "Shadow app."}, {env, Env}]},
    application:load(App).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec start_application(name()) -> ok.
%%--------------------------------------------------------------------
start_application(Name) ->
    load_application(Name),
    application:start(Name).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec start_application(name(), env()) -> ok.
%%--------------------------------------------------------------------
start_application(Name, Env) ->
    load_application(Name, Env),
    application:start(Name).

%% ===================================================================
%% Internal functions
%% ===================================================================

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

hidden(CB, Mod, Name, Arity) ->
    Args = args(Arity),
    HideArgs = [atom(Mod), atom(Name), list(Args)],
    {function, 0, Name, Arity, [clause(Args, [call(CB, hide, HideArgs)])]}.

passthrough(Hidden, Name, Arity) ->
    Args = args(Arity),
    {function, 0, Name, Arity, [clause(Args, [call(Hidden, Name, Args)])]}.

clause(Pattern, Body) -> {clause, 0, Pattern, [], Body}.

call(Mod, Name, Args) -> {call, 0, {remote, 0, atom(Mod), atom(Name)}, Args}.

atom(Atom) -> {atom, 0, Atom}.

list([]) -> {nil, 0};
list([H | T]) -> {cons, 0, H, list(T)}.

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


forms(S) -> forms(split(S, [], []), []).

forms([], Acc) -> lists:reverse(Acc);
forms([H | T], Acc) ->
    {ok, Tokens, _} = erl_scan:string(H),
    {ok, Parsed} = erl_parse:parse_form(Tokens),
    forms(T, [Parsed | Acc]).

split([], [], Forms) -> lists:reverse(Forms);
split([], Acc, Forms) -> lists:reverse([Acc | Forms]);
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
