%==============================================================================
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
%%%  A routing tree used for HTTP routing
%%%
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_route_tree).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% API
-export([build/3, lookup/4]).

%% Exported types
-export_type([tree/0]).

%% Records
-record(bucket, {prefix, rest = [], values = []}).
-record(node, {prefix, left = nil, next = nil, right = nil, values = []}).
-record(var, {name, first = nil, next = nil, values = []}).

%% Types
-opaque tree() :: #node{}.
-type separator() :: char().
-type match_type() :: star | var.
-type routes() :: [{binary(), _}].


%% ===================================================================
%% API
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%%   
%% @end
%%--------------------------------------------------------------------
-spec build(match_type(), separator(), routes()) -> tree().
%%--------------------------------------------------------------------
build(Type, Sep, PL) ->
    PL1 = [{lift(Type, jhn_bstring:tokens(K, <<Sep>>)), V} || {K, V} <- PL],
    build(bucket_sort(PL1)).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%%   
%% @end
%%--------------------------------------------------------------------
-spec lookup(binary(), match_type(), separator(), tree()) ->
          undefined | {_, [{atom(), _}]} | _.
%%--------------------------------------------------------------------
lookup(Key, Type, Sep, PrefixTree) ->
    Segments = jhn_bstring:tokens(Key, <<Sep>>),
    case {find(Segments, PrefixTree, [], undefined), Type} of
        {{_, undefined}, _} -> undefined;
        {{Bindings, [Value | _]}, var} -> {Value, Bindings};
        {{_, [Value | _]}, star}-> Value
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

lift(star, Key) -> lift_star(Key, []);
lift(var, Key) -> lift_var(Key, []).

lift_star([], Acc) -> lists:reverse(Acc);
lift_star([<<$*>> | T], Acc) -> lift_star(T, [star | Acc]);
lift_star([H | T], Acc) -> lift_star(T, [H | Acc]).

lift_var([], Acc) -> lists:reverse(Acc);
lift_var([<<$:, H/binary>> | T], Acc) ->
    lift_var(T, [binary_to_atom(H, utf8) | Acc]);
lift_var([<<$[, $:, H/binary>>], Acc) ->
    {lists:reverse(Acc), [binary_to_atom(butlast(H, <<>>), utf8)]};
lift_var([<<$[, H/binary>>], Acc) ->
    {lists:reverse(Acc), [butlast(H, <<>>)]};
lift_var([H | T], Acc) ->
    lift_var(T, [H | Acc]).

butlast(<<$]>>, Acc) -> Acc;
butlast(<<H/utf8, T/binary>>, Acc) -> butlast(T, <<Acc/binary, H/utf8>>).

find([P], #node{prefix = P, values = Vs}, B, Prev) -> {B, update(Vs, Prev)};
find([P], #var{name = Name, values = Vs}, B, Prev) ->
    {[{Name, P} | B], update(Vs, Prev)};
find([P | _], #node{prefix = P, next = nil, values = Vs}, B, Prev) ->
    {B, update(Vs, Prev)};
find([P | _], #var{name = Name, next = nil, values = Vs}, B, Prev) ->
    {[{Name, P} | B], update(Vs, Prev)};
find([P | T], #node{prefix = P, next = Next, values = Vs}, B, Prev) ->
    find(T, Next, B, update(Vs, Prev));
find(K = [P | _], #node{prefix = P1, left = L}, B, Prev) when P < P1 ->
    find(K, L, B, Prev);
find(K, #node{right = Right}, B, Prev) ->
    find(K, Right, B, Prev);
find(K = [P | T], #var{name = Name,first=First,next=Next,values=Vs},B,Prev) ->
    case find(K, First, B, undefined) of
        {_, undefined} -> find(T, Next, [{Name, P} | B], update(Vs, Prev));
        Result -> Result
    end;
find(_, nil, B, Prev) ->
    {B, Prev}.

update([], Prev) -> Prev;
update(New, _) -> New.

build([]) -> nil;
build([#bucket{prefix = P, rest = R, values = Vs} | T]) when is_atom(P) ->
    #var{name = P, first = build(T), next = build(R), values = Vs};
build([#bucket{prefix = P, rest = R, values = Vs}]) ->
    #node{prefix = P, next = build(R), values = Vs};
build([#bucket{prefix = P1, rest = R1, values = Vs1},
       #bucket{prefix = P2, rest = R2, values = Vs2}]) ->
    #node{prefix = P1, next = build(R1), values = Vs1,
          right = #node{prefix = P2, next = build(R2), values = Vs2}};
build(Bs) ->
    Size = length(Bs),
    {Left, #bucket{prefix = P, rest = Rest, values = Vs}, Right} =
        split(Size - 1 - ((Size - 1) div 2), Bs, []),
    #node{prefix = P,
          next = build(Rest),
          left = build(Left),
          right = build(Right),
          values = Vs}.

split(0, [H | T], Acc) -> {lists:reverse(Acc), H, T};
split(N, [H | T], Acc) -> split(N - 1, T, [H | Acc]).

bucket_sort(L) -> bucket_sort(lists:sort(L), []).

bucket_sort([], []) -> [];
bucket_sort([], [B = #bucket{rest = R} | Acc]) ->
    lists:reverse([B#bucket{rest = bucket_sort(lists:reverse(R))} | Acc]);
bucket_sort([{[H], V} | T], []) ->
    bucket_sort(T, [#bucket{prefix = H, values = [V]}]);
bucket_sort([{[H | P], V} | T], []) ->
    bucket_sort(T, [#bucket{prefix = H, rest = [{P, V}]}]);
bucket_sort([{[H], V} | T], [B = #bucket{prefix = H, values = Vs} | Acc]) ->
    bucket_sort(T, [B#bucket{values = [V | Vs]} | Acc]);
bucket_sort([{[H | P], V} | T], [B = #bucket{prefix = H, rest = T1} |Acc]) ->
    bucket_sort(T, [B#bucket{rest = [{P, V} | T1]} | Acc]);
bucket_sort([{[H], V} | T], [B = #bucket{rest = R} | Acc]) ->
    bucket_sort(T, [#bucket{prefix = H, values = [V]},
                    B#bucket{rest = bucket_sort(lists:reverse(R))} | Acc]);
bucket_sort([{[H | P], V} | T], [B = #bucket{rest = R} | Acc]) ->
    bucket_sort(T, [#bucket{prefix = H, rest = [{P, V}]},
                    B#bucket{rest = bucket_sort(lists:reverse(R))} | Acc]).

