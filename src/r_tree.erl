%%==============================================================================
%% Copyright 2016 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%   Implements range trees, where the tree is organized with non-overlapping
%%   ranges.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(r_tree).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([new/0,
         is_r_tree/1, is_empty/1,
         add/3, add/4, adds/2, adds/3,
         delete/2, delete/3, deletes/2, deletes/3,
         member/2, find/2, find/3,
         ranges/1, values/1,
         replace/3, replace/4,
         to_list/1, from_list/1
        ]).

%% Records
-record(r_node, {low           :: value(),
                 high          :: value(),
                 left  = r_nil :: r_tree(),
                 right = r_nil :: r_tree(),
                 value         :: value()}).


%% Types
-opaque r_tree()   :: #r_node{} | r_nil.
-type   key()    :: [_].
-type   range()    :: {value(), value()}.
-type   value()    :: _.
-type   default()  :: _.
-type   flag()     :: check | nocheck.

%% Exported Types
-export_type([r_tree/0]).

%% Defines

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: new() -> Tree.
%% @doc
%%   Creates an empty R-tree.
%% @end
%%--------------------------------------------------------------------
-spec new() -> r_tree().
%%--------------------------------------------------------------------
new() -> r_nil.

%%--------------------------------------------------------------------
%% Function: is_r_tree(X) -> Boolean().
%% @doc
%%   Returns true if X is a R-tree, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_r_tree(_) -> boolean().
%%--------------------------------------------------------------------
is_r_tree(r_nil) -> true;
is_r_tree(#r_node{}) -> true;
is_r_tree(_) -> false.

%%--------------------------------------------------------------------
%% Function: is_empty(Tree) -> Boolean.
%% @doc
%%   Returns true if the Tree is empty, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_empty(_) -> boolean().
%%--------------------------------------------------------------------
is_empty(r_nil) -> true;
is_empty(_) -> false.

%%--------------------------------------------------------------------
%% Function: add(Range, Value, Tree) -> Tree.
%% @doc
%%   The Value is saved in Tree under the Range, if that key is present the
%%   value associated with the key is replaced by Value.
%%   add(Range, Value, Tree) is equivalent to add(Range, Value, Tree,nocheck).
%% @end
%%--------------------------------------------------------------------
-spec add(range(), value(), r_tree()) -> r_tree().
%%--------------------------------------------------------------------
add(Range, Value, Tree) -> add(Range, Value, Tree, nocheck).

%%--------------------------------------------------------------------
%% Function: add(Range, Value, Tree, Flag) -> Tree.
%% @doc
%%   The Value is saved in Tree under the Range, if that key is present the
%%   value associated with the key is replaced by Value. If the flag is
%%   check an exception is generated and if nocheck the value is replaced.
%% @end
%%--------------------------------------------------------------------
-spec add(range(), value(), r_tree(), flag()) -> r_tree().
%%--------------------------------------------------------------------
add({L, H}, Value, r_nil, _) -> #r_node{low = L, high = H, value = Value};
add({L, H}, Value, Tree = #r_node{low = L, high = H}, nocheck) ->
    Tree#r_node{value = Value};
add(R = {_,H}, Value, Tree=#r_node{low = L,left=Left},Flag) when H < L ->
    Tree#r_node{left = add(R, Value, Left, Flag)};
add(R = {L,_}, Value, Tree=#r_node{high = H,right=Right},Flag) when L > H ->
    Tree#r_node{right = add(R, Value, Right, Flag)}.

%%--------------------------------------------------------------------
%% Function: adds(Pairs, Tree) -> Tree.
%% @doc<
%%   For each {Range, Value} pair in Pairs the value is stored under the
%%   Range in the Tree. The adds(Pairs, Tree) call is equivalent to
%%   adds(Pairs, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec adds([range()], r_tree()) -> r_tree().
%%--------------------------------------------------------------------
adds(Pairs, Tree) -> adds(Pairs, Tree, nocheck).

%%--------------------------------------------------------------------
%% Function: adds(Pairs, Tree, Flag) -> Tree.
%% @doc
%%   For each {Range, Value} pair in Pairs the value is stored under the
%%   Range in the Tree. If an Range already has a value associated with
%%   in the Tree the flag determines what happens. If the flag is check
%%   an exception is generated if a range has a value associated with it,
%%   if the flag is nocheck the values will be replaced.
%% @end
%%--------------------------------------------------------------------
-spec adds([{range(), value()}], r_tree(), flag()) -> r_tree().
%%--------------------------------------------------------------------
adds(Pairs, Tree, nocheck) ->
    build(lists:keymerge(1, lists:keysort(1, Pairs), to_list(Tree)));
adds(Pairs, Tree, check) ->
    case lists:any(fun({I, _}) -> member(I, Tree) end, Pairs) of
        false -> adds(Pairs, Tree, nocheck);
        true -> erlang:error(badarg)
    end.

%%--------------------------------------------------------------------
%% Function: delete(Range, Tree) -> Tree.
%% @doc
%%   If a value is associated the Range the Tree returned has that
%%   association removed. The call delete(Range, Tree) is equivalent
%%   to delete(Range, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec delete(range(), r_tree()) -> r_tree().
%%--------------------------------------------------------------------
delete(Range, Tree) -> delete(Range, Tree, nocheck).

%%--------------------------------------------------------------------
%% Function: delete(Range, Tree, Flag) -> Tree.
%% @doc
%%   If a value is associated the Range the Tree returned has that
%%   association removed. If there is no value associated with the Range
%%   in the Tree the flag determines what happens. If the flag is check
%%   an exception is generated if no association exists, if the flag
%%   is nocheck the unchanged tree is returned.
%% @end
%%--------------------------------------------------------------------
-spec delete(range(), r_tree(), flag()) -> r_tree().
%%--------------------------------------------------------------------
delete(_, r_nil, check) -> erlang:error(badarg);
delete(_, r_nil, nocheck) -> r_nil;
delete({L,H}, #r_node{low=L, high=H, left = r_nil, right = r_nil}, _) -> r_nil;
delete({L,H}, #r_node{low = L, high=H, left = Left, right = r_nil}, _) -> Left;
delete({L,H}, #r_node{low=L, high=H, left = r_nil, right = Right}, _) -> Right;
delete({L,H}, #r_node{low = L, high = H, left = Left, right = Right}, _) ->
    build(to_list(Left, to_list(Right)));
delete(R = {_, H}, #r_node{low = L, left = Left}, Flag) when H < L ->
    delete(R, Left, Flag);
delete(R = {L, _}, #r_node{high = H, right = Right}, Flag) when L > H ->
    delete(R, Right, Flag).

%%--------------------------------------------------------------------
%% Function: deletes(Ranges, Tree) -> Tree.
%% @doc
%%  A tree that has all the associations for the ranges removed.
%%  The call deletes(Ranges, Tree) is equivalent to
%%  deletes(Ranges, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec deletes([range()], r_tree()) -> r_tree().
%%--------------------------------------------------------------------
deletes(Ranges, Tree) -> deletes(Ranges, Tree, nocheck).

%%--------------------------------------------------------------------
%% Function: delete(Ranges, Tree, Flag) -> Tree.
%% @doc
%%   A tree that has all the associations for the rangess removed.
%%   If there is no value associated with any of the Ranges
%%   in the Tree, the flag determines what happens. If the flag is check
%%   an exception is generated if, if the flag is nocheck a tree
%%   is returned with the other associations removed.
%% @end
%%--------------------------------------------------------------------
-spec deletes([range()], r_tree(), flag()) -> r_tree().
%%--------------------------------------------------------------------
deletes(Ranges, Tree, nocheck) ->
    build(deletes1(lists:sort(Ranges), to_list(Tree)));
deletes(Ranges, Tree, check) ->
    case lists:all(fun(R) -> member(R, Tree) end, Ranges) of
        true -> deletes(Ranges, Tree, nocheck);
        false -> erlang:error(badarg)
    end.

deletes1(_, []) -> [];
deletes1([], L) -> L;
deletes1([H | T1], [{H, _} | T2]) -> deletes1(T1, T2);
deletes1([H1 | T1], L = [{H2, _} | _]) when H1 > H2 -> deletes1(T1, L);
deletes1(I, [_ | T]) -> deletes1(I, T).

%%--------------------------------------------------------------------
%% Function: member(Range, Tree) -> Boolean.
%% @doc
%%   Returns true if there is a value associated with Range in the tree,
%%   otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec member(range(), r_tree()) -> boolean().
%%--------------------------------------------------------------------
member(_, r_nil) -> false;
member({L, H}, #r_node{low = L, high = H}) -> true;
member(R = {_, H}, #r_node{low = L, left = Left}) when H < L -> member(R, Left);
member(R = {L, _}, #r_node{high = H, right = Right}) when L > H ->
    member(R, Right).

%%--------------------------------------------------------------------
%% Function: find(Key, Tree) -> Value.
%% @doc
%%   Returns the value associated with any range that includes the key in the
%%   Tree or undefined if no such association exists.
%%   The call find(Key, Tree) is equivalent to find(Key, Tree, undefined).
%% @end
%%--------------------------------------------------------------------
-spec find(key(), r_tree()) -> value() | undefined.
%%--------------------------------------------------------------------
find(Key, Tree) -> find(Key, Tree, undefined).

%%--------------------------------------------------------------------
%% Function: find(Key, Tree, Default) -> Value.
%% @doc
%%   Returns the value associated with any range that includes the key in the
%%   Tree or Default if no such association exists.
%% @end
%%--------------------------------------------------------------------
-spec find(key(), r_tree(), default()) -> value() | default().
%%--------------------------------------------------------------------
find(_, r_nil, Default) -> Default;
find(I, #r_node{low = L, left = Left}, Default) when I < L ->
    find(I, Left, Default);
find(I, #r_node{high = H, right = Right}, Default) when I > H ->
    find(I, Right, Default);
find(_, #r_node{value = Value}, _) ->
    Value.

%%--------------------------------------------------------------------
%% Function: ranges(Tree) -> Keys.
%% @doc
%%   Returns all the ranges in ascending order.
%% @end
%%--------------------------------------------------------------------
-spec ranges(r_tree()) -> [range()].
%%--------------------------------------------------------------------
ranges(Tree) -> ranges(Tree, []).

ranges(r_nil, Acc) -> Acc;
ranges(#r_node{low = L, high = H, left = Left, right = Right}, Acc) ->
    ranges(Left, [{L, H} | ranges(Right, Acc)]).

%%--------------------------------------------------------------------
%% Function: values(Tree) -> Values.
%% @doc
%%   Returns all the values in ascending order of their ranges.
%% @end
%%--------------------------------------------------------------------
-spec values(r_tree()) -> [value()].
%%--------------------------------------------------------------------
values(Tree) -> values(Tree, []).

values(r_nil, Acc) -> Acc;
values(#r_node{left = Left, right = Right, value = Value}, Acc) ->
    values(Left, [Value | values(Right, Acc)]).

%%--------------------------------------------------------------------
%% Function: replace(Range, Value, Tree) -> Tree.
%% @doc
%%   Replaces any existing value associated with Range in the tree,
%%   otherwise adds a association for the value with the Range.
%%   The call replace(Range, Value, Tree) is equivalent to
%%   replace(Range, Value, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec replace(range(), value(), r_tree()) -> r_tree().
%%--------------------------------------------------------------------
replace(Range, Value, Tree) -> replace(Range, Value, Tree, nocheck).

%%--------------------------------------------------------------------
%% Function: replace(Range, Values, Tree, Flag) -> Tree.
%% @doc
%%   Replaces any existing value associated with Range in the tree,
%%   otherwise the flag determines what happens. If the flag is check
%%   an exception is generated, otherwise the value is added.
%% @end
%%--------------------------------------------------------------------
-spec replace(range(), value(), r_tree(), flag()) -> r_tree().
%%--------------------------------------------------------------------
replace(Range, Value, Tree, nocheck) -> add(Range, Value, Tree, nocheck);
replace(Range, Value, Tree, check) -> replace_check(Range, Value, Tree).

replace_check(_, _, r_nil) -> erlang:error(badarg);
replace_check({L, H}, V, T = #r_node{low = L, high = H}) -> T#r_node{value = V};
replace_check(R = {_, H}, V, T = #r_node{low = L, left = Left}) when H < L ->
    T#r_node{left = replace_check(R, V, Left)};
replace_check(R = {L, _}, V, T = #r_node{high = H, right=Right}) when L > H ->
    T#r_node{right = replace_check(R, V, Right)}.

%%--------------------------------------------------------------------
%% Function: to_list(Tree) -> Pairs.
%% @doc
%%    From a R-tree a list of {Range, Value} pairs.
%% @end
%%--------------------------------------------------------------------
-spec to_list(r_tree()) -> [{range(), value()}].
%%--------------------------------------------------------------------
to_list(Tree) -> to_list(Tree, []).

to_list(r_nil, Acc) -> Acc;
to_list(#r_node{low = L, high = H, left = Left, right = Right, value=V}, Acc) ->
    to_list(Left, [{{L, H}, V} | to_list(Right, Acc)]).

%%--------------------------------------------------------------------
%% Function: from_list(Pairs) -> Tree.
%% @doc
%%   For each {Range, Value} pair in Pairs the value is stored under the
%%   key in the Tree.
%% @end
%%--------------------------------------------------------------------
-spec from_list([{range(), value()}]) -> r_tree().
%%--------------------------------------------------------------------
from_list(L) -> build(lists:sort(L)).

%% ===================================================================
%% Internal functions.
%% ===================================================================

build(L) ->
    {Tree, []} = build(length(L), L),
    Tree.

build(0, L) -> {r_nil, L};
build(1, [{{L, H}, V} | T]) -> {#r_node{low = L, high = H, value = V}, T};
build(Size, List) ->
    RightHalf = (Size - 1) div 2,
    LeftHalf = Size - 1 - RightHalf,
    {Left, [{{L, H}, V} | T1]} = build(LeftHalf, List),
    {Right, T2} = build(RightHalf, T1),
    {#r_node{low = L, high = H, value = V, left = Left, right = Right}, T2}.
