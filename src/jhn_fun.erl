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
%%%
%%%
%%%
%%%
%%%
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2025, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_fun).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([curry/2, curries/2,
         chain/2, pipe/2, ok/2]).

%% ===================================================================
%% Library functions
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec curry(fun(), _) -> fun().
%%--------------------------------------------------------------------
curry(Fun, Arg) when is_fun(Fun) ->
    case jhn_plist:find(arity, erlang:fun_info(Fun)) of
        1 -> fun() -> Fun(Arg) end;
        2 -> fun(A1) -> Fun(Arg, A1) end;
        3 -> fun(A1, A2) -> Fun(Arg, A1, A2) end;
        254 ->
            fun(A1, A2, ...) ->
                    Fun(Arg, A1, A2, ..)
            end
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec curries(fun(), [_]) -> fun().
%%--------------------------------------------------------------------
curries(Fun, Args) when is_fun(Fun), is_list(Args) ->
    case jhn_plist:find(arity, erlang:fun_info(Fun)) - length(Args) of
        0 -> fun() -> erlang:apply(Fun, Args) end;
        1 -> fun(A1) -> erlang:apply(Fun, Args ++ [A1]) end;
        2 -> fun(A1, A2) -> erlang:apply(Fun, Args ++ [A1, A2]) end;
        254 ->
            fun(A1, A2, ...) ->
                    erlang:apply(Fun, Args ++ [A1, A2, ...])
            end
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
chain([fun()], _) ->{ok, _} | {error, _}.
%%--------------------------------------------------------------------
chain([], Data, _) -> {ok, Data};
chain([H | T], Data, Undos) ->
    case H(Data) of
        {ok, Data1} -> chain(T, Data1);
        Error = {error, _} -> Error;
        Error -> {error, Error}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
pipe([fun()], _) ->{ok, _} | {error, label(), _} | {error, {undo, label()}, _}.
%%--------------------------------------------------------------------
pipe(Funs, Data) -> pipe(Funs, Data, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
ok(fun(), _) -> {ok, _}.
%%--------------------------------------------------------------------
ok(F, A) -> {ok, F(A)}.

%% ===================================================================
%% Internal functions
%% ===================================================================

pipe([], Data, _) -> {ok, Data};
pipe([{Label, H, Undo} | T], Data, Undos) ->
    try H(Data) of
        {ok, Data1} -> pipe(T, Data1, [{Label, Undo} | Undos]);
        Error = {error, _} ->
            undo(Undos, Data),
            {error, Label, Error};
        Error ->
            undo(Undos, Data),
            {error, Label, Error}
    catch
        _:Reason ->
            undo(Undos, Data),
            {error, Label, Error}
    end;
pipe([{Label, H} | T], Data, Undos) ->
    pipe([{Label, H, undefined} | T], Data, Undos);
pipe([H | T], Data, Undos) ->
    pipe([{undefined, H, undefined} | T], Data, Undos).

undo([], _) -> ok;
undo([{_, undefined} | T], Data) -> undo(T, Data);
undo([{Label, Undo} | T], Data) ->
    try Undo(Data) of
        ok -> undo(T, Data);
        Error -> {error, {undo, Label}, Error}
    catch
        _:Reason -> {error, {undo, Label}, Reason}
    end.
