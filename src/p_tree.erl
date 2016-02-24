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
%%   Implements Prefix trees
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(p_tree).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([new/0,
         is_p_tree/1, is_empty/1,
         add/3, add/4, adds/2, adds/3,
         delete/2, delete/3, deletes/2, deletes/3,
         member/2, find/2, find/3,
         indices/1, values/1,
         replace/3, replace/4,
         to_list/1, from_list/1
        ]).

%% Records
-record(bucket, {pivot       :: value(),
                 rest   = [] :: {value(), value()},
                 values = [] :: [value()]}).

-record(p_node, {pivot         :: value(),
                 left  = p_nil :: p_tree(),
                 next  = p_nil :: p_tree(),
                 right = p_nil :: p_tree(),
                 value         :: value()}).


%% Types
-opaque p_tree()   :: #p_node{} | p_nil.
-type   index()    :: [_].
-type   prefix()   :: [_].
-type   value()    :: _.
-type   default()  :: _.
-type   flag()     :: check | nocheck.

%% Exported Types
-export_type([p_tree/0]).

%% Defines

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: new() -> Tree.
%% @doc
%%   Creates an empty P-tree.
%% @end
%%--------------------------------------------------------------------
-spec new() -> p_tree().
%%--------------------------------------------------------------------
new() -> p_nil.

%%--------------------------------------------------------------------
%% Function: is_p_tree(X) -> Boolean().
%% @doc
%%   Returns true if X is a p_tree, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_p_tree(_) -> boolean().
%%--------------------------------------------------------------------
is_p_tree(p_nil) -> true;
is_p_tree(#p_node{}) -> true;
is_p_tree(_) -> false.

%%--------------------------------------------------------------------
%% Function: is_empty(Tree) -> Boolean.
%% @doc
%%   Returns true if the Tree is empty, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_empty(_) -> boolean().
%%--------------------------------------------------------------------
is_empty(p_nil) -> true;
is_empty(_) -> false.

%%--------------------------------------------------------------------
%% Function: add(Index, Value, Tree) -> Tree.
%% @doc
%%   The Value is saved under the longet Prefix of the Index in the Tree,
%%   if that prefix is present the value associated with the prefix is
%%   replaced by Value.
%%   add(Index, Value, Tree) is equivalent to add(Index, Value, Tree,nocheck).
%% @end
%%--------------------------------------------------------------------
-spec add(prefix(), value(), p_tree()) -> p_tree().
%%--------------------------------------------------------------------
add(Index, Value, Tree) -> add(Index, Value, Tree, nocheck).

%%--------------------------------------------------------------------
%% Function: add(Prefix, Value, Tree, Flag) -> Tree.
%% @doc
%%   The Value is saved under the Prefix in the Tree, the Flag determines
%%   what should happen if that prefix already has a value associated with
%%   in the Tree. If the flag is check an exception is generated and
%%   if nocheck the value is replaced.
%% @end
%%--------------------------------------------------------------------
-spec add(prefix(), value(), p_tree(), flag()) -> p_tree().
%%--------------------------------------------------------------------
add([H], Value, p_nil, _) -> #p_node{pivot = H, value = Value};
add([H | T], Value, p_nil, _) ->
    #p_node{pivot = H, next = add(T, Value, p_nil, nocheck)};
add([H], Value, Tree = #p_node{pivot = H}, nocheck) ->
    Tree#p_node{value = Value};
add([H], Value, Tree = #p_node{pivot = H, value = undefined}, _) ->
    Tree#p_node{value = Value};
add([H | T], Value, Tree = #p_node{pivot = H, next = Next}, Flag) ->
    Tree#p_node{next = add(T, Value, Next, Flag)};
add(I = [H | _], Value, Tree = #p_node{pivot=P, left=Left}, Flag) when H < P ->
    Tree#p_node{left = add(I, Value, Left, Flag)};
add(I, Value, Tree = #p_node{right = Right}, Flag) ->
    Tree#p_node{right = add(I, Value, Right, Flag)}.

%%--------------------------------------------------------------------
%% Function: adds(Pairs, Tree) -> Tree.
%% @doc<
%%   For each {Index, Value} pair in Pairs the value is stored under the
%%   index in the Tree. The adds(Pairs, Tree) call is equivalent to
%%   adds(Pairs, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec adds([{index(), value()}], p_tree()) -> p_tree().
%%--------------------------------------------------------------------
adds(Pairs, Tree) -> adds(Pairs, Tree, nocheck).

%%--------------------------------------------------------------------
%% Function: adds(Pairs, Tree, Flag) -> Tree.
%% @doc
%%   For each {Index, Value} pair in Pairs the value is stored under the
%%   index in the Tree. If an index already has a value associated with
%%   in the Tree the flag determines what happens. If the flag is check
%%   an exception is generated if a index has a value associated with it,
%%   if the flag is nocheck the values will be replaced.
%% @end
%%--------------------------------------------------------------------
-spec adds([{index(), value()}], p_tree(), flag()) -> p_tree().
%%--------------------------------------------------------------------
adds(Pairs, Tree, nocheck) ->
    Pairs1 = lists:keymerge(1, lists:keysort(1, Pairs), to_list(Tree)),
    build(bucket_sort(Pairs1, []));
adds(Pairs, Tree, check) ->
    case lists:any(fun({I, _}) -> member(I, Tree) end, Pairs) of
        false -> adds(Pairs, Tree, nocheck);
        true -> erlang:error(badarg)
    end.

%%--------------------------------------------------------------------
%% Function: delete(Index, Tree) -> Tree.
%% @doc
%%   If a value is associated the Index the Tree returned has that
%%   association removed. The call delete(Index, Tree) is equivalent
%%   to delete(Index, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec delete(index(), p_tree()) -> p_tree().
%%--------------------------------------------------------------------
delete(Index, Tree) -> delete(Index, Tree, nocheck).

%%--------------------------------------------------------------------
%% Function: delete(Index, Tree, Flag) -> Tree.
%% @doc
%%   If a value is associated the Index the Tree returned has that
%%   association removed. If there is no value associated with the Index
%%   in the Tree the flag determines what happens. If the flag is check
%%   an exception is generated if no association exists, if the flag
%%   is nocheck the unchanged tree is returned.
%% @end
%%--------------------------------------------------------------------
-spec delete(index(), p_tree(), flag()) -> p_tree().
%%--------------------------------------------------------------------
delete(_, p_nil, check) -> erlang:error(badarg);
delete(_, p_nil, nocheck) -> p_nil;
delete([H], #p_node{pivot = H, value = undefined}, check) ->
    erlang:error(badarg);
delete([H], #p_node{pivot = H, left = p_nil, next = p_nil, right = p_nil}, _) ->
    p_nil;
delete([H], #p_node{pivot = H, left = Left, next = p_nil, right = p_nil}, _) ->
    Left;
delete([H], #p_node{pivot = H, left = p_nil, next = p_nil, right = Right}, _) ->
    Right;
delete([H], Tree = #p_node{pivot = H}, _) ->
    Tree#p_node{value = undefined};
delete([H | T], #p_node{pivot = H, next = Next}, Flag) ->
    delete(T, Next, Flag);
delete(I = [H | _], #p_node{pivot = P, left = Left}, Flag) when H < P ->
    delete(I, Left, Flag);
delete(I, #p_node{right = Right}, Flag) ->
    delete(I, Right, Flag).

%%--------------------------------------------------------------------
%% Function: deletes(Indces, Tree) -> Tree.
%% @doc
%%  A tree that has all the associations for the indices removed.
%%  The call deletes(Indces, Tree) is equivalent to
%%  deletes(Indces, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec deletes([index()], p_tree()) -> p_tree().
%%--------------------------------------------------------------------
deletes(Indices, Tree) -> deletes(Indices, Tree, nocheck).

%%--------------------------------------------------------------------
%% Function: delete(Index, Tree, Flag) -> Tree.
%% @doc
%%   A tree that has all the associations for the indices removed.
%%   If there is no value associated with any of the Indices
%%   in the Tree, the flag determines what happens. If the flag is check
%%   an exception is generated if, if the flag is nocheck a tree
%%   is returned with the other associations removed.
%% @end
%%--------------------------------------------------------------------
-spec deletes([index()], p_tree(), flag()) -> p_tree().
%%--------------------------------------------------------------------
deletes(Indices, Tree, nocheck) ->
    build(bucket_sort(deletes1(lists:sort(Indices), to_list(Tree)), []));
deletes(Indices, Tree, check) ->
    case lists:all(fun(I) -> member(I, Tree) end, Indices) of
        true -> deletes(Indices, Tree, nocheck);
        false -> erlang:error(badarg)
    end.

deletes1(_, []) -> [];
deletes1([], L) -> L;
deletes1([H | T1], [{H, _} | T2]) -> deletes1(T1, T2);
deletes1([H1 | T1], L = [{H2, _} | _]) when H1 > H2 -> deletes1(T1, L);
deletes1(I, [_ | T]) -> deletes1(I, T).

%%--------------------------------------------------------------------
%% Function: member(Index, Tree) -> Boolean.
%% @doc
%%   Returns true if there is a value associated with Index in the tree,
%%   otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec member(index(), p_tree()) -> boolean().
%%--------------------------------------------------------------------
member(_, p_nil) -> false;
member([H], #p_node{pivot = H, value = Value}) -> Value /= undefined;
member([H | T], #p_node{pivot = H, next = Next}) -> member(T, Next);
member(I = [H | _], #p_node{pivot = P, left = L}) when H < P -> member(I, L);
member(I, #p_node{right = R}) -> member(I, R).

%%--------------------------------------------------------------------
%% Function: find(Index, Tree) -> Value.
%% @doc
%%   Returns the value associated with the longest prefix of the Index in the
%%   Tree or Default if no such association exists.
%%   The call find(Index, Tree) is equivalent to find(Index, Tree, undefined).
%% @end
%%--------------------------------------------------------------------
-spec find(index(), p_tree()) -> value() | undefined.
%%--------------------------------------------------------------------
find(Index, Tree) -> find(Index, Tree, undefined).

%%--------------------------------------------------------------------
%% Function: find(Index, Tree, Default) -> Value.
%% @doc
%%   Returns the value associated with the longest prefix of the Index in the
%%   Tree or Default if no such association exists.
%% @end
%%--------------------------------------------------------------------
-spec find(index(), p_tree(), default()) -> value() | default().
%%--------------------------------------------------------------------
find(_, p_nil, Prev) -> Prev;
find([H], #p_node{pivot = H, value = V}, Prev) -> update(V, Prev);
find([H | T], #p_node{pivot = H, next = Next, value = V}, Prev) ->
    find(T, Next, update(V, Prev));
find(I = [H | _], #p_node{pivot = P, left = Left}, Prev) when H < P ->
    find(I, Left, Prev);
find(I, #p_node{right = Right}, Prev) ->
    find(I, Right, Prev).

update(undefined, Prev) -> Prev;
update(New, _) -> New.

%%--------------------------------------------------------------------
%% Function: indices(Tree) -> Indices.
%% @doc
%%   Returns all the indices in ascending order.
%% @end
%%--------------------------------------------------------------------
-spec indices(p_tree()) -> [prefix()].
%%--------------------------------------------------------------------
indices(Tree) -> indices(Tree, [], []).

indices(p_nil, _, Acc) -> Acc;
indices(#p_node{pivot = P, left=L,next=N,right=R,value=undefined},Xiferp,Acc) ->
    Xiferp1 = [P | Xiferp],
    indices(L, Xiferp, indices(N, Xiferp1, indices(R, Xiferp, Acc)));
indices(#p_node{pivot = P, left=L, next=N, right=R}, Xiferp, Acc) ->
    Xiferp1 = [P | Xiferp],
    indices(L,
            Xiferp,
            [lists:reverse(Xiferp1) |
             indices(N, Xiferp1, indices(R, Xiferp, Acc))]).

%%--------------------------------------------------------------------
%% Function: values(Tree) -> Values.
%% @doc
%%   Returns all the values in ascending order of their indeces.
%% @end
%%--------------------------------------------------------------------
-spec values(p_tree()) -> [index()].
%%--------------------------------------------------------------------
values(Tree) -> values(Tree, []).

values(p_nil, Acc) -> Acc;
values(#p_node{left = L, next = N, right = R, value = undefined}, Acc) ->
    values(L, values(N, values(R, Acc)));
values(#p_node{left = L, next = N, right = R, value = V}, Acc) ->
    values(L, [V | values(N, values(R, Acc))]).

%%--------------------------------------------------------------------
%% Function: replace(Index, Value, Tree) -> Tree.
%% @doc
%%   Replaces any existing value associated with Index in the tree,
%%   otherwise adds a association for the value with the Index.
%%   The call replace(Index, Value, Tree) is equivalent to
%%   replace(Index, Value, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec replace(index(), value(), p_tree()) -> p_tree().
%%--------------------------------------------------------------------
replace(Index, Value, Tree) -> replace(Index, Value, Tree, nocheck).

%%--------------------------------------------------------------------
%% Function: replace(Index, Values, Tree, Flag) -> Tree.
%% @doc
%%   Replaces any existing value associated with Index in the tree,
%%   otherwise the flag determines what happens. If the flag is check
%%   an exception is generated, otherwise the value is added.
%% @end
%%--------------------------------------------------------------------
-spec replace(index(), value(), p_tree(), flag()) -> p_tree().
%%--------------------------------------------------------------------
replace(Index, Value, Tree, nocheck) -> add(Index, Value, Tree, nocheck);
replace(Index, Value, Tree, check) -> replace_check(Index, Value, Tree).

replace_check(_, _, p_nil) -> erlang:error(badarg);
replace_check([H], _, #p_node{pivot = H, value = undefined}) ->
    erlang:error(badarg);
replace_check([H], Value, Tree = #p_node{pivot = H}) ->
    Tree#p_node{value = Value};
replace_check([H | T], Value, Tree = #p_node{pivot = H, next = Next}) ->
    Tree#p_node{next = replace_check(T, Value, Next)};
replace_check(I = [H|_], Value, Tree = #p_node{pivot=P,left=Left}) when H < P ->
    Tree#p_node{left = replace_check(I, Value, Left)};
replace_check(I, Value, Tree = #p_node{right = Right}) ->
    Tree#p_node{right = replace_check(I, Value, Right)}.

%%--------------------------------------------------------------------
%% Function: to_list(Tree) -> Pairs.
%% @doc
%%    From a T-tree a list of {Index, Value} pairs.
%% @end
%%--------------------------------------------------------------------
-spec to_list(p_tree()) -> [{index(), value()}].
%%--------------------------------------------------------------------
to_list(Tree) -> to_list(Tree, [], []).

to_list(p_nil, _, Acc) -> Acc;
to_list(#p_node{pivot = P, left=L,next=N,right=R,value=undefined},Xiferp,Acc) ->
    Xiferp1 = [P | Xiferp],
    to_list(L, Xiferp, to_list(N, Xiferp1, to_list(R, Xiferp, Acc)));
to_list(#p_node{pivot = P, left=L, next=N, right=R, value=V}, Xiferp, Acc) ->
    Xiferp1 = [P | Xiferp],
    to_list(L,
            Xiferp,
            [{lists:reverse(Xiferp1), V} |
             to_list(N, Xiferp1, to_list(R, Xiferp, Acc))]).

%%--------------------------------------------------------------------
%% Function: from_list(Pairs) -> Tree.
%% @doc
%%   For each {Index, Value} pair in Pairs the value is stored under the
%%   index in the Tree. Equivalent to from_list(Pairs, []).
%% @end
%%--------------------------------------------------------------------
-spec from_list([{index(), value()}]) -> p_tree().
%%--------------------------------------------------------------------
from_list(Plist) -> build(bucket_sort(Plist)).

%% ===================================================================
%% Internal functions.
%% ===================================================================

bucket_sort(L) -> bucket_sort(lists:sort(L), []).

bucket_sort([], []) -> [];
bucket_sort([], [B = #bucket{rest = R} | Acc]) ->
    lists:reverse([B#bucket{rest = bucket_sort(lists:reverse(R))} | Acc]);
bucket_sort([{[H], V} | T], []) ->
    bucket_sort(T, [#bucket{pivot = H, values = [V]}]);
bucket_sort([{[H | P], V} | T], []) ->
    bucket_sort(T, [#bucket{pivot = H, rest = [{P, V}]}]);
bucket_sort([{[H], V} | T], [B = #bucket{pivot = H, values = Vs} | Acc]) ->
    bucket_sort(T, [B#bucket{values = [V | Vs]} | Acc]);
bucket_sort([{[H | P], V} | T], [B = #bucket{pivot = H, rest = T1} |Acc]) ->
    bucket_sort(T, [B#bucket{rest = [{P, V} | T1]} | Acc]);
bucket_sort([{[H], V} | T], [B = #bucket{rest = R} | Acc]) ->
    bucket_sort(T, [#bucket{pivot = H, values = [V]},
                    B#bucket{rest = bucket_sort(lists:reverse(R))} | Acc]);
bucket_sort([{[H | P], V} | T], [B = #bucket{rest = R} | Acc]) ->
    bucket_sort(T, [#bucket{pivot = H, rest = [{P, V}]},
                    B#bucket{rest = bucket_sort(lists:reverse(R))} | Acc]).

build([]) -> p_nil;
build([#bucket{pivot = P, rest = R, values = [V]}]) ->
    #p_node{pivot = P, next = build(R), value = V};
build([#bucket{pivot = P1, rest = R1, values = [V1]},
       #bucket{pivot = P2, rest = R2, values = [V2]}]) ->
    #p_node{pivot = P1, next = build(R1), value = V1,
            right = #p_node{pivot = P2, next = build(R2), value = V2}};
build(Bs) ->
    Size = length(Bs),
    case split(Size - 1 - ((Size - 1) div 2), Bs, []) of
        {Left, #bucket{pivot = P, rest = Rest, values = []}, Right} ->
            #p_node{pivot = P,
                    next = build(Rest),
                    left = build(Left),
                    right = build(Right)};
        {Left, #bucket{pivot = P, rest = Rest, values = [V]}, Right} ->
            #p_node{pivot = P,
                    next = build(Rest),
                    left = build(Left),
                    right = build(Right),
                    value = V}
    end.

split(0, [H | T], Acc) -> {lists:reverse(Acc), H, T};
split(N, [H | T], Acc) -> split(N - 1, T, [H | Acc]).
