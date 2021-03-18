%%==============================================================================
%% Copyright 2013-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%   Implements T-trees extended with an notion of least upp and greatest
%%   lower bounds. For more information on T-Trees see:
%%   A Study of Index Structures for Main Memory Database Management Systems.
%%   by Tobin J. Lehman and Michael J. Carey, VLDB 1986.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(t_tree).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([new/0, new/1,
         is_t_tree/1, is_empty/1,
         add/3, add/4, adds/2, adds/3,
         delete/2, delete/3, deletes/2, deletes/3,
         member/2, find/2, find/3,
         least_upper_bound/2, least_upper_bound/3,
         greatest_lower_bound/2, greatest_lower_bound/3,
         first/1, first/2,
         last/1, last/2,
         indices/1, values/1,
         replace/3, replace/4,
         to_list/1, from_list/1
        ]).

%% Records
-record(node, {depth     = 1   :: pos_integer(),
               size      = 1   :: pos_integer(),
               glb             :: index(),
               lub             :: index(),
               left      = nil :: t_node(),
               right     = nil :: t_node(),
               occupants = []  :: [{index(), value()}]}).

-record(t_tree, {min        :: pos_integer(),
                 max        :: pos_integer(),
                 root = nil :: t_node()}).

-record(opts, {min = 5 :: pos_integer(),
               max = 8 :: pos_integer()}).

%% Types
-opaque t_tree()   :: #t_tree{}.
-type   t_node()   :: nil | #node{}.
-type   index()    :: _.
-type   value()    :: _.
-type   default()  :: _.
-type   flag()     :: check | nocheck.
-type   opt()      :: {min, pos_integer()} | {max, pos_integer()}.

%% Exported Types
-export_type([t_tree/0]).

%% Defines

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: new() -> Tree.
%% @doc
%%   Creates an empty T-tree. The call new() is equivalent to new([]).
%% @end
%%--------------------------------------------------------------------
-spec new() -> t_tree().
%%--------------------------------------------------------------------
new() -> new([]).

%%--------------------------------------------------------------------
%% Function: new(Options) -> Tree.
%% @doc
%%   Creates an empty T-tree with min and max internal size according
%%   to the options.
%% @end
%%--------------------------------------------------------------------
-spec new([opt()]) -> t_tree().
%%--------------------------------------------------------------------
new(Opts) ->
    #opts{min = Min, max = Max} = parse_opts(Opts, #opts{}),
    #t_tree{min = Min, max = Max}.

%%--------------------------------------------------------------------
%% Function: is_t_tree(X) -> Boolean().
%% @doc
%%   Returns true if X is a t_tree, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_t_tree(_) -> boolean().
%%--------------------------------------------------------------------
is_t_tree(#t_tree{}) -> true;
is_t_tree(_) -> false.

%%--------------------------------------------------------------------
%% Function: is_empty(Tree) -> Boolean.
%% @doc
%%   Returns true if the Tree is empty, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_empty(_) -> boolean().
%%--------------------------------------------------------------------
is_empty(#t_tree{root = nil}) -> true;
is_empty(_) -> false.

%%--------------------------------------------------------------------
%% Function: add(Index, Value, Tree) -> Tree.
%% @doc
%%   The Value is saved under the Index in the Tree, if that index is
%%   present the value associated with the index is replaced by Value.
%%   add(Index, Value, Tree) is equivalent to add(Index, Value, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec add(index(), value(), t_tree()) -> t_tree().
%%--------------------------------------------------------------------
add(Index, Value, Tree) -> add(Index, Value, Tree, nocheck).

%%--------------------------------------------------------------------
%% Function: add(Index, Values, Tree, Flag) -> Tree.
%% @doc
%%   The Value is saved under the Index in the Tree, the Flag determines
%%   what should happen if that index already has a value associated with
%%   in the Tree. If the flag is check an exception is generated and
%%   if nocheck the value is replaced.
%% @end
%%--------------------------------------------------------------------
-spec add(index(), value(), t_tree(), flag()) -> t_tree().
%%--------------------------------------------------------------------
add(Index, Value, Tree = #t_tree{root = Root}, Flag) ->
    balance(Tree#t_tree{root = add(Index, Value, Root, Tree, Flag)}).

add(Index, Value, nil, _, _) ->
    #node{glb = Index, lub = Index, occupants = [{Index, Value}]};
add(Index, Value, Node = #node{glb = GLB}, Tree, Flag) when Index < GLB ->
    #node{size = Size, left = Left, occupants = Os} = Node,
    case {Left, Tree#t_tree.max > Size} of
        {nil, true} ->
            Node#node{size = Size + 1,
                      glb = Index,
                      occupants = inner_add(Index, Value, Os)};
        _ ->
            recalc_depth(Node#node{left = add(Index, Value, Left, Tree, Flag)})
    end;
add(Index, Value, Node = #node{lub = LUB}, Tree, Flag) when Index > LUB ->
    #node{size = Size, right = Right, occupants = Os} = Node,
    case {Right, Tree#t_tree.max > Size} of
        {nil, true} ->
            Node#node{size = Size + 1,
                      lub = Index,
                      occupants = inner_add(Index, Value, Os)};
        _ ->
            recalc_depth(Node#node{right = add(Index, Value, Right, Tree,Flag)})
    end;
add(Index, Value, Node=#node{size = Size, occupants = Os}, Tree, Flag) ->
    case {inner_member(Index, Os), Flag} of
        {true, check} -> erlang:error(badarg, [Index]);
        {true, _} -> Node#node{occupants = inner_replace(Index, Value, Os)};
        {_, _} when Size < Tree#t_tree.max ->
            Node#node{size = Size + 1, occupants = inner_add(Index, Value, Os)};
        {_, _} ->
            [{Index1, Value1}, H2 = {Index2, _} | Os1] = Os,
            #node{left = Left} = Node,
            Left1 = add(Index1, Value1, Left, Tree, nocheck),
            Node1 =
                Node#node{size = Size - 1,
                          glb = Index2,
                          occupants = [H2 | Os1],
                          left = Left1},
            recalc_depth(add(Index, Value, Node1, Tree, nocheck))
    end.

%%--------------------------------------------------------------------
%% Function: adds(Pairs, Tree) -> Tree.
%% @doc
%%   For each {Index, Value} pair in Pairs the value is stored under the
%%   index in the Tree. The adds(Pairs, Tree) call is equivalent to
%%   adds(Pairs, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec adds([{index(), value()}], t_tree()) -> t_tree().
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
-spec adds([{index(), value()}], t_tree(), flag()) -> t_tree().
%%--------------------------------------------------------------------
adds(Pairs, Tree = #t_tree{min = Min, max = Max}, nocheck) ->
    Pairs1 = lists:keymerge(1, lists:keysort(1, Pairs), to_list(Tree)),
    Size = Min + (Max - Min) div 2,
    #t_tree{min = Min,
            max = Max,
            root = element(1, from_list(Pairs1, length(Pairs1), Size))};
adds(Pairs, Tree = #t_tree{min = Min, max = Max}, check) ->
    case lists:any(fun({I, _}) -> member(I, Tree) end, Pairs) of
        true -> erlang:error(badarg);
        false ->
            Pairs1 = lists:keymerge(1, lists:keysort(1, Pairs), to_list(Tree)),
            Size = Min + (Max - Min) div 2,
            #t_tree{min = Min,
                    max = Max,
                    root = element(1, from_list(Pairs1, length(Pairs1), Size))}
    end.

%%--------------------------------------------------------------------
%% Function: delete(Index, Tree) -> Tree.
%% @doc
%%   If a value is associated the Index the Tree returned has that
%%   association removed. The call delete(Index, Tree) is equivalent
%%   to delete(Index, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec delete(index(), t_tree()) -> t_tree().
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
-spec delete(index(), t_tree(), flag()) -> t_tree().
%%--------------------------------------------------------------------
delete(Index, Tree = #t_tree{root = Root}, Flag) ->
    balance(Tree#t_tree{root = delete1(Index, Root, Tree, Flag)}).

delete1(Index, nil, _, check) -> erlang:error(badarg, [Index]);
delete1(_, nil, _, nocheck) -> nil;
delete1(Index, #node{occupants = [{Index, _}], left = nil, right = nil}, _,_) ->
    nil;
delete1(Index, Node = #node{glb=GLB, left=Left}, Tree, Flag) when Index < GLB ->
    recalc_depth(Node#node{left = delete1(Index, Left, Tree, Flag)});
delete1(Index, Node=#node{lub=LUB, right=Right}, Tree,Flag) when Index > LUB ->
    recalc_depth(Node#node{right = delete1(Index, Right, Tree, Flag)});
delete1(Index, Node, Tree = #t_tree{min = Min}, Flag) ->
    #node{size = Size, occupants = Os, left = Left, right = Right} = Node,
    case {inner_member(Index, Os), Flag, Min} of
        {false, check, _} -> erlang:error(badarg, [Index]);
        {false, _, _} -> Node;
        {_, _, Size} when Left /= nil->
            {H = {Index1, _}, Left1} = steal(lub, Left, Tree),
            recalc_boundary(
              Index,
              recalc_depth(
                Node#node{glb = Index1,
                          left = Left1,
                          occupants = [H | inner_delete(Index, Os)]}));
        {_, _, Size} when Right /= nil->
            {{Index1, Value1}, Right1} = steal(glb, Right, Tree),
            recalc_boundary(
              Index,
              recalc_depth(Node#node{lub = Index1,
                                     right = Right1,
                                     occupants =
                                         inner_add(Index1,
                                                   Value1,
                                                   inner_delete(Index, Os))}));
        {_, _, _} ->
            recalc_boundary(Index,
                            Node#node{size = Size - 1,
                                      occupants = inner_delete(Index, Os)})
    end.

%%--------------------------------------------------------------------
%% Function: deletes(Indces, Tree) -> Tree.
%% @doc
%%  A tree that has all the associations for the indices removed.
%%  The call deletes(Indces, Tree) is equivalent to
%%  deletes(Indces, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec deletes([index()], t_tree()) -> t_tree().
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
-spec deletes([index()], t_tree(), flag()) -> t_tree().
%%--------------------------------------------------------------------
deletes(Indices, Tree = #t_tree{min = Min, max = Max}, nocheck) ->
    Size = Min + (Max - Min) div 2,
    Pairs =
        lists:foldr(fun(Index, Acc) -> lists:keydelete(Index, 1, Acc) end,
                    to_list(Tree),
                    Indices),
    #t_tree{min = Min,
            max = Max,
            root = element(1, from_list(Pairs, length(Pairs), Size))};
deletes(Indices, Tree = #t_tree{min = Min, max = Max}, check) ->
    case lists:all(fun({I, _}) -> member(I, Tree) end, Indices) of
        false -> erlang:error(badarg);
        true ->
            Size = Min + (Max - Min) div 2,
            Pairs =
                lists:foldr(fun(Index, Acc) -> lists:keydelete(Index,1,Acc) end,
                            to_list(Tree),
                            Indices),
            #t_tree{min = Min,
                    max = Max,
                    root = element(1, from_list(Pairs, length(Pairs), Size))}
    end.

%%--------------------------------------------------------------------
%% Function: member(Index, Tree) -> Boolean.
%% @doc
%%   Returns true if there is a value associated with Index in the tree,
%%   otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec member(index(), t_tree()) -> boolean().
%%--------------------------------------------------------------------
member(Index, #t_tree{root = Root}) -> member1(Index, Root).

member1(_, nil) -> false;
member1(Index, #node{glb = GLB, left = Left}) when Index < GLB ->
    member1(Index, Left);
member1(Index, #node{lub = LUB, right = Right}) when Index > LUB ->
    member1(Index, Right);
member1(Index, #node{occupants = Occupants}) ->
    inner_member(Index, Occupants).

%%--------------------------------------------------------------------
%% Function: find(Index, Tree) -> Value.
%% @doc
%%   Returns the value associated with Index in the Tree or undefined
%%   if no such association exists. The call find(Index, Tree) is
%%   equivalent to find(Index, Tree, undefined).
%% @end
%%--------------------------------------------------------------------
-spec find(index(), t_tree()) -> value() | undefined.
%%--------------------------------------------------------------------
find(Index, Tree) -> find(Index, Tree, undefined).

%%--------------------------------------------------------------------
%% Function: find(Index, Tree, Default) -> Value.
%% @doc
%%   Returns the value associated with Index in the Tree or Default
%%   if no such association exists.
%% @end
%%--------------------------------------------------------------------
-spec find(index(), t_tree(), default()) -> value() | default().
%%--------------------------------------------------------------------
find(Index, #t_tree{root = Root}, Default) -> find1(Index, Root, Default).

find1(_, nil, Default) -> Default;
find1(Index, #node{glb = GLB, left = Left}, Default) when Index < GLB ->
    find1(Index, Left, Default);
find1(Index, #node{lub = LUB, right = Right}, Default) when Index > LUB ->
    find1(Index, Right, Default);
find1(Index, #node{occupants = Occupants}, Default) ->
    case lists:keyfind(Index, 1, Occupants) of
        false -> Default;
        {_, Value} -> Value
    end.

%%--------------------------------------------------------------------
%% Function: least_upper_bound(Index, Tree) -> Value.
%% @doc
%%   Returns the value of the smallest index that is greater or equal to
%%   to Index that has a value associated with it, or undefined if no
%%   such index exists. The call least_upper_bound(Index, Tree) is
%%   equivalent to least_upper_bound(Index, Tree, undefined).
%% @end
%%--------------------------------------------------------------------
-spec least_upper_bound(index(), t_tree()) -> value() | undefined.
%%--------------------------------------------------------------------
least_upper_bound(Index, Tree) ->
    least_upper_bound(Index, Tree, undefined).

%%--------------------------------------------------------------------
%% Function: least_upper_bound(Index, Tree, Default) -> Value.
%% @doc
%%   Returns the value of the smallest index that is greater or equal to
%%   to Index that has a value associated with it, or Default if no
%%   such index exists.
%% @end
%%--------------------------------------------------------------------
-spec least_upper_bound(index(), t_tree(), default()) -> value() | default().
%%--------------------------------------------------------------------
least_upper_bound(Index, #t_tree{root = Root}, Default) ->
    least_upper_bound1(Index, Root, Default).

least_upper_bound1(_, nil, Default) -> Default;
least_upper_bound1(Index, Node = #node{glb = GLB, left = Left}, _)
  when Index < GLB ->
    case least_upper_bound1(Index, Left, '$not_found') of
        '$not_found' ->
            #node{occupants = [{_, Value} | _]} = Node,
            Value;
        Value ->
            Value
    end;
least_upper_bound1(Index, #node{lub = LUB, right = Right}, Default)
  when Index > LUB ->
    least_upper_bound1(Index, Right, Default);
least_upper_bound1(Index, #node{occupants = Occupants}, _) ->
    inner_least_upper_bound(Index, Occupants).

%%--------------------------------------------------------------------
%% Function: greatest_lower_bound(Index, Tree) -> Value.
%% @doc
%%   Returns the value of the largest index that is less or equal to
%%   to Index that has a value associated with it, or undefined if no
%%   such index exists. The call greatest_lower_bound(Index, Tree) is
%%   equivalent to greatest_lower_bound(Index, Tree, undefined).
%% @end
%%--------------------------------------------------------------------
-spec greatest_lower_bound(index(), t_tree()) -> value() | undefined.
%%--------------------------------------------------------------------
greatest_lower_bound(Index, Tree) ->
    greatest_lower_bound(Index, Tree, undefined).

%%--------------------------------------------------------------------
%% Function: greatest_lower_bound(Index, Tree, Default) -> Value.
%% @doc
%%   Returns the value of the largest index that is less or equal to
%%   to Index that has a value associated with it, or Default if no
%%   such index exists.
%% @end
%%--------------------------------------------------------------------
-spec greatest_lower_bound(index(), t_tree(), default()) -> value() | default().
%%--------------------------------------------------------------------
greatest_lower_bound(Index, #t_tree{root = Root}, Default) ->
    greatest_lower_bound1(Index, Root, Default).

greatest_lower_bound1(_, nil, Default) -> Default;
greatest_lower_bound1(Index, #node{glb = GLB, left = Left}, Default)
  when Index < GLB ->
    greatest_lower_bound1(Index, Left, Default);
greatest_lower_bound1(Index, Node = #node{lub = LUB, right = Right}, _)
  when Index > LUB ->
    case greatest_lower_bound1(Index, Right, '$not_found') of
        '$not_found' ->
            #node{occupants = Os} = Node,
            {_, Value} = lists:last(Os),
            Value;
        Value ->
            Value
    end;
greatest_lower_bound1(Index, #node{occupants = Occupants}, _) ->
    inner_greatest_lower_bound(Index, Occupants).

%%--------------------------------------------------------------------
%% Function: first(Tree) -> Value.
%% @doc
%%   Returns the value of the smallest index in the Tree or undefined
%%   if the tree is empty. The call first(Tree) if equivalent to
%%   first(Tree, undefined).
%% @end
%%--------------------------------------------------------------------
-spec first(t_tree()) -> value() | undefined.
%%--------------------------------------------------------------------
first(Tree) -> first(Tree, undefined).

%%--------------------------------------------------------------------
%% Function: first(Tree, Default) -> Value.
%% @doc
%%   Returns the value of the smallest index in the Tree or Default
%%   if the tree is empty.
%% @end
%%--------------------------------------------------------------------
-spec first(t_tree(), default()) -> value() | default().
%%--------------------------------------------------------------------
first(#t_tree{root = nil}, Default) -> Default;
first(#t_tree{root = Root}, _) -> first1(Root).

first1(#node{left = nil, occupants = [{_, Value} | _]}) -> Value;
first1(#node{left = Left}) -> first1(Left).

%%--------------------------------------------------------------------
%% Function: last(Tree) -> Value.
%% @doc
%%   Returns the value of the largest index in the Tree or undefined
%%   if the tree is empty. The call last(Tree) if equivalent to
%%   last(Tree, undefined).
%% @end
%%--------------------------------------------------------------------
-spec last(t_tree()) -> value() | undefined.
%%--------------------------------------------------------------------
last(Tree) -> last(Tree, undefined).

%%--------------------------------------------------------------------
%% Function: last(Tree, Default) -> Value.
%% @doc
%%   Returns the value of the largest index in the Tree or Default
%%   if the tree is empty.
%% @end
%%--------------------------------------------------------------------
-spec last(t_tree(), default()) -> value() | default().
%%--------------------------------------------------------------------
last(#t_tree{root = nil}, Default) -> Default;
last(#t_tree{root = Root}, _) -> last1(Root).

last1(#node{right = nil, occupants = Os}) ->
    {_, Value} = lists:last(Os),
    Value;
last1(#node{right = Right}) ->
    last1(Right).

%%--------------------------------------------------------------------
%% Function: indices(Tree) -> Indices.
%% @doc
%%   Returns all the indices in ascending order.
%% @end
%%--------------------------------------------------------------------
-spec indices(t_tree()) -> [index()].
%%--------------------------------------------------------------------
indices(#t_tree{root = Root}) -> indices(Root, []).

indices(nil, Acc) -> Acc;
indices(#node{left = Left, right = Right, occupants = Os}, Acc) ->
    indices(Left,
            lists:foldr(fun({Index, _}, AccFun) -> [Index | AccFun] end,
                        indices(Right, Acc),
                        Os)).

%%--------------------------------------------------------------------
%% Function: values(Tree) -> Values.
%% @doc
%%   Returns all the values in ascending order of their indeces.
%% @end
%%--------------------------------------------------------------------
-spec values(t_tree()) -> [index()].
%%--------------------------------------------------------------------
values(#t_tree{root = Root}) -> values(Root, []).

values(nil, Acc) -> Acc;
values(#node{left = Left, right = Right, occupants = Os}, Acc) ->
    values(Left,
           lists:foldr(fun({_, Value}, AccFun) -> [Value | AccFun] end,
                       values(Right, Acc),
                       Os)).

%%--------------------------------------------------------------------
%% Function: replace(Index, Value, Tree) -> Tree.
%% @doc
%%   Replaces any existing value associated with Index in the tree,
%%   otherwise adds a association for the value with the Index.
%%   The call replace(Index, Value, Tree) is equivalent to
%%   replace(Index, Value, Tree, nocheck).
%% @end
%%--------------------------------------------------------------------
-spec replace(index(), value(), t_tree()) -> t_tree().
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
-spec replace(index(), value(), t_tree(), flag()) -> t_tree().
%%--------------------------------------------------------------------
replace(Index, Value, Tree, nocheck) -> add(Index, Value, Tree, nocheck);
replace(Index, Value, Tree = #t_tree{root = Root}, _) ->
    Tree#t_tree{root = replace1(Index, Value, Root)}.

replace1(Index, _, nil) -> erlang:error(badarg, [Index]);
replace1(Index, Value, Node = #node{glb = GLB, left = Left}) when Index < GLB ->
    Node#node{left = replace1(Index, Value, Left)};
replace1(Index, Value, Node = #node{lub = LUB, right=Right}) when Index > LUB ->
    Node#node{right = replace1(Index, Value, Right)};
replace1(Index, Value, Node = #node{occupants = Os}) ->
    case inner_member(Index, Os) of
        false -> erlang:error(badarg, [Index]);
        true -> Node#node{occupants = inner_replace(Index, Value, Os)}
    end.

%%--------------------------------------------------------------------
%% Function: to_list(Tree) -> Pairs.
%% @doc
%%    From a T-tree a list of {Index, Value} pairs.
%% @end
%%--------------------------------------------------------------------
-spec to_list(t_tree()) -> [{index(), value()}].
%%--------------------------------------------------------------------
to_list(#t_tree{root = nil}) -> [];
to_list(#t_tree{root = #node{occupants = Occupants, left=Left, right=Right}}) ->
    to_list(Left, Occupants ++ to_list(Right, [])).

%%--------------------------------------------------------------------
%% Function: from_list(Pairs) -> Tree.
%% @doc
%%   For each {Index, Value} pair in Pairs the value is stored under the
%%   index in the Tree. Equivalent to from_list(Pairs, []).
%% @end
%%--------------------------------------------------------------------
-spec from_list([{index(), value()}]) -> t_tree().
%%--------------------------------------------------------------------
from_list(Pairs) -> from_list(Pairs, []).

%%--------------------------------------------------------------------
%% Function: from_list(Pairs, Opts) -> Tree.
%% @doc
%%   For each {Index, Value} pair in Pairs the value is stored under the
%%   index in the Tree.
%% @end
%%--------------------------------------------------------------------
-spec from_list([{index(), value()}], [opt()]) -> t_tree().
%%--------------------------------------------------------------------
from_list(Pairs, Opts) ->
    #opts{min = Min, max = Max} = parse_opts(Opts, #opts{}),
    Size = Min + (Max - Min) div 2,
    #t_tree{min = Min,
            max = Max,
            root = element(1,
                           from_list(lists:keysort(1, Pairs),
                                     length(Pairs),
                                     Size))}.

%% ===================================================================
%% Internal functions.
%% ===================================================================

parse_opts(Opts, Rec) ->
    case lists:foldl(fun parse_opt/2, Rec, Opts) of
        Rec1 = #opts{min = Min, max = Max} when Min < Max -> Rec1;
        _ -> erlang:error(badarg, [Opts])
    end.

parse_opt({min, Min}, Opts) -> Opts#opts{min = Min};
parse_opt({max, Max}, Opts) -> Opts#opts{max = Max}.

inner_member(Index, Occupants) -> lists:keymember(Index, 1, Occupants).

inner_add(Index, Value, []) -> [{Index, Value}];
inner_add(Index, Value, [H = {Index1, _} | T]) when Index > Index1 ->
    [H | inner_add(Index, Value, T)];
inner_add(Index, Value, T) ->
    [{Index, Value} | T].

inner_replace(Index, Value, Occupants) ->
    lists:keystore(Index, 1, Occupants, {Index, Value}).

inner_delete(Index, Occupants) -> lists:keydelete(Index, 1, Occupants).

inner_least_upper_bound(Index, [{Index1, _} | T]) when Index > Index1 ->
    inner_least_upper_bound(Index, T);
inner_least_upper_bound(_, [{_, Value} | _]) ->
    Value.

inner_greatest_lower_bound(Index, [_, H = {Index1,_}|T]) when Index >= Index1 ->
    inner_greatest_lower_bound(Index, [H | T]);
inner_greatest_lower_bound(_, [{_, Value} | _]) ->
    Value.

steal(lub, Node = #node{lub = Index, right = nil, occupants = Os}, Tree) ->
    {lists:last(Os), delete1(Index, Node, Tree, nocheck)};
steal(lub, Node = #node{right = Right}, Tree) ->
    {Elt, Right1} = steal(lub, Right, Tree),
    {Elt, recalc_depth(Node#node{right = Right1})};
steal(glb, Node = #node{glb = Index, left = nil, occupants = [H | _]}, Tree) ->
    {H, delete1(Index, Node, Tree, nocheck)};
steal(glb, Node = #node{left = Left}, Tree) ->
    {Elt, Left1} = steal(lub, Left, Tree),
    {Elt, recalc_depth(Node#node{left = Left1})}.

recalc_boundary(Index, Node = #node{glb = Index, occupants = [{GLB, _} | _]}) ->
    Node#node{glb = GLB};
recalc_boundary(Index, Node = #node{lub = Index, occupants = Os}) ->
    {LUB, _} = lists:last(Os),
    Node#node{lub = LUB};
recalc_boundary(_, Node) ->
    Node.

recalc_depth(Node = #node{left = nil, right = nil}) -> Node#node{depth = 1};
recalc_depth(Node = #node{left = nil, right = #node{depth = D}}) ->
    Node#node{depth = D + 1};
recalc_depth(Node = #node{left = #node{depth = D}, right = nil}) ->
    Node#node{depth = D + 1};
recalc_depth(Node = #node{left = #node{depth=D1}, right = #node{depth=D2}}) ->
    Node#node{depth = max(D1, D2) + 1}.

balance(Tree = #t_tree{min = Min, max = Max, root = Root}) ->
    case balanced(Root) of
        true -> Tree;
        false ->
            Pairs = to_list(Tree),
            Size = Min + (Max - Min) div 2,
            #t_tree{min = Min,
                    max = Max,
                    root = element(1, from_list(Pairs, length(Pairs), Size))}
    end.

balanced(nil) -> true;
balanced(#node{left = nil, right = nil}) -> true;
balanced(#node{left = #node{depth = 1}, right = nil}) -> true;
balanced(#node{left = nil, right = #node{depth = 1}}) -> true;
balanced(#node{left = L = #node{depth = D}, right = R = #node{depth = D}}) ->
    balanced(L) andalso balanced(R);
balanced(#node{left= L = #node{depth = LD},right = R = #node{depth = RD}})
  when abs(LD - RD) < 2->
    balanced(L) andalso balanced(R);
balanced(_) ->
    false.

to_list(nil, Acc) -> Acc;
to_list(#node{occupants = Occupants, left=Left, right=Right}, Acc) ->
    to_list(Left, Occupants ++ to_list(Right, Acc)).

from_list([], _, _) -> {nil, []};
from_list(T, 0, _) -> {nil, T};
from_list(Pairs, Size, Occupants) when Size =< Occupants ->
    {Pick, T} = pick(Pairs, Size, []),
    {#node{depth = 1,
           size = Size,
           glb = element(1, hd(Pick)),
           lub = element(1, lists:last(Pick)),
           occupants = Pick},
     T};
from_list(Pairs, Size, Occupants) ->
    RightHalf = (Size - Occupants) div 2,
    LeftHalf = Size - Occupants - RightHalf,
    {Left = #node{depth = LD}, Pairs1} = from_list(Pairs, LeftHalf, Occupants),
    {Pick, T1} = pick(Pairs1, Occupants, []),
    case from_list(T1, RightHalf, Occupants) of
        {nil, T1} ->
            {#node{depth = LD + 1,
                   size = Occupants,
                   glb =  element(1, hd(Pick)),
                   lub = element(1, lists:last(Pick)),
                   left = Left,
                   occupants = Pick},
             T1};
        {Right = #node{depth = RD}, T2} ->
            {#node{depth = max(LD, RD) + 1,
                   size = Occupants,
                   glb =  element(1, hd(Pick)),
                   lub = element(1, lists:last(Pick)),
                   left = Left,
                   right = Right,
                   occupants = Pick},
             T2}
    end.

pick([], _, Acc) -> {lists:reverse(Acc), []};
pick(T, 0, Acc) -> {lists:reverse(Acc), T};
pick([H | T], N, Acc) -> pick(T, N - 1, [H | Acc]).
