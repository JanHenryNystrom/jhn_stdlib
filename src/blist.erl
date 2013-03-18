%%==============================================================================
%% Copyright 2013 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   String Processing Functions for binary encoded strings.
%%%
%%%   This is a drop in replacement for the lists module in stdlib
%%%   working on binaries interpreted as strings of octets in Latin1.
%%%
%%%   The module generates ref binaries as much as possible so if
%%%   copies are more suitable apply binary/copy/1 on the result.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(blist).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([all/2, any/2,
         append/1, append/2,
         concat/1, delete/2, dropwhile/2, duplicate/2, filter/2,
         flatlength/1, flatmap/2, flatten/1, flatten/2,
         foldl/3, foldr/3, foreach/2,
         keydelete/4, keyfind/4, keymap/4, keymember/4, keymerge/4,
         keyreplace/5, keysearch/4, keysort/3, keystore/5, keytake/4,
         last/1, map/2, mapfoldl/3, mapfoldr/3, max/1, member/2,
         merge/1, merge/2, merge/3, merge3/3, min/1, nth/2, nthtail/2,
         partition/2, prefix/2, reverse/1, reverse/2, seq/2, seq/3
        ]).

%% Types
-type thing() :: atom() | integer() | float() | string() | binary().

%% Compiler directives
-compile({no_auto_import,[max/2, min/2]}).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: all(Pred, String) -> Boolean.
%% @doc
%%   Returns true if Pred(Elem) returns true for all elements Elem in String,
%%   otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec all(fun((byte()) -> boolean()), binary()) -> boolean().
%%--------------------------------------------------------------------
all(Pred, <<>>) when is_function(Pred, 1) -> true;
all(Pred, <<H, T/binary>>) ->
    case Pred(H) of
        true -> all(Pred, T);
        false -> false
    end.

%%--------------------------------------------------------------------
%% Function: any(Pred, String) -> Boolean.
%% @doc
%%   Returns true if Pred(Elem) returns true for at least one element
%%   Elem in String.
%% @end
%%--------------------------------------------------------------------
-spec any(fun((byte()) -> boolean()), binary()) -> boolean().
%%--------------------------------------------------------------------
any(Pred, <<>>) when is_function(Pred, 1) -> false;
any(Pred, <<H, T/binary>>) ->
    case Pred(H) of
        true -> true;
        false -> any(Pred, T)
    end.

%%--------------------------------------------------------------------
%% Function: append(ListOfStrings) -> String
%% @doc
%%   Returns a String in which all the sub-strings of ListOfStrings have
%%   been appended.
%% @end
%%--------------------------------------------------------------------
-spec append(list(binary())) -> binary().
%%--------------------------------------------------------------------
append(Binaries) when is_list(Binaries) -> iolist_to_binary(Binaries).

%%--------------------------------------------------------------------
%% Function: append(String1, String2) -> String3
%% @doc
%%   Returns a new list String3 which is made from the elements of
%%   String1 followed by the elements of String2.
%% @end
%%--------------------------------------------------------------------
-spec append(binary(), binary()) -> binary().
%%--------------------------------------------------------------------
append(Binary1, Binary2) when is_binary(Binary1), is_binary(Binary2) ->
    iolist_to_binary([Binary1, Binary2]).

%%--------------------------------------------------------------------
%% Function: concat(Things) -> String.
%% @doc
%%   Concatenates the text representation of the elements of Things.
%%   The elements of Things can be atoms, integers, floats, strings,
%%   or binaries.
%% @end
%%--------------------------------------------------------------------
-spec concat([thing()]) -> binary().
%%--------------------------------------------------------------------
concat(List) when is_list(List) -> concat(List, <<>>).

concat([], Acc) -> Acc;
concat([Atom | T], Acc) when is_atom(Atom) ->
    concat(T, <<Acc/binary, (list_to_binary(atom_to_list(Atom)))/binary>>);
concat([Int | T], Acc) when is_integer(Int) ->
    concat(T, <<Acc/binary, (list_to_binary(integer_to_list(Int)))/binary>>);
concat([Float | T], Acc) when is_float(Float) ->
    concat(T, <<Acc/binary, (list_to_binary(float_to_list(Float)))/binary>>);
concat([List | T], Acc) when is_list(List) ->
    concat(T, <<Acc/binary, (concat_maybe_string(List, <<>>))/binary>>);
concat([Binary | T], Acc) when is_binary(Binary) ->
    concat(T, <<Acc/binary, Binary/binary>>).

concat_maybe_string([], Acc) -> Acc;
concat_maybe_string([H | T], Acc) when is_integer(H) ->
    concat_maybe_string(T, <<Acc/binary, H>>).

%%--------------------------------------------------------------------
%% Function: delete(Elem, String1) -> String2.
%% @doc
%%   Returns a copy of String1 where the first element matching Elem is
%%   deleted, if there is such an element.
%% @end
%%--------------------------------------------------------------------
-spec delete(char(), binary()) -> binary().
%%--------------------------------------------------------------------
delete(Elt, Binary) -> delete(Elt, Binary, <<>>).

delete(_, <<>>, Acc) -> Acc;
delete(Elt, <<Elt, T/binary>>, Acc) -> <<Acc/binary, T/binary>>;
delete(Elt, <<H, T/binary>>, Acc) -> delete(Elt, T, <<Acc/binary, H>>).

%%--------------------------------------------------------------------
%% Function: dropwhile(Pred, String1) -> String2.
%% @doc
%%   Drops elements Elem from String1 while Pred(Elem) returns true and
%%   returns the remaining string.
%% @end
%%--------------------------------------------------------------------
-spec dropwhile(fun((byte()) -> boolean()), binary()) -> binary().
%%--------------------------------------------------------------------
dropwhile(Pred, <<>>) when is_function(Pred, 1) -> <<>>;
dropwhile(Pred, Binary = <<H, T/binary>>) ->
    case Pred(H) of
        true -> dropwhile(Pred, T);
        false -> Binary
    end.

%%--------------------------------------------------------------------
%% Function: duplicate(N, Elem) -> String.
%% @doc
%%   Returns a string which contains N copies of the term Elem.
%% @end
%%--------------------------------------------------------------------
-spec duplicate(pos_integer(), byte()) -> binary().
%%--------------------------------------------------------------------
duplicate(N, Char) when is_integer(N), N >= 0 -> binary:copy(<<Char>>, N).

%%--------------------------------------------------------------------
%% Function: filter(Pred, String1) -> String2.
%% @doc
%%   String2 is a string of all elements Elem in String1 for which
%%   Pred(Elem) returns true.
%% @end
%%--------------------------------------------------------------------
-spec filter(fun((byte()) -> boolean()), binary()) -> binary().
%%--------------------------------------------------------------------
filter(Pred, Binary) when is_function (Pred) -> filter(Pred, Binary, <<>>).

filter(_, <<>>, Acc) -> Acc;
filter(Pred, <<H, T/binary>>, Acc) ->
    case Pred(H) of
        true -> filter(Pred, T, <<Acc/binary, H>>);
        false -> filter(Pred, T, Acc)
    end.

%%--------------------------------------------------------------------
%% Function: flatlength(DeepString) -> Length.
%% @doc
%%   Equivalent to length(iolist_to_binary(DeepString)), but more efficient.
%% @end
%%--------------------------------------------------------------------
-spec flatlength(iolist()) -> non_neg_integer().
%%--------------------------------------------------------------------
flatlength(IOList) -> iolist_size(IOList).

%%--------------------------------------------------------------------
%% Function: flatmap(Fun, String1) -> String2.
%% @doc
%%   Takes a function from As to strings of Bs, and a string of As (String1)
%%   and produces a string of Bs by applying the function to every element
%%   in String1 and appending the resulting strings.
%% @end
%%--------------------------------------------------------------------
-spec flatmap(fun((byte()) -> binary()), binary()) -> binary().
%%--------------------------------------------------------------------
flatmap(Fun, Binary) when is_function(Fun, 1) -> flatmap(Fun, Binary, <<>>).

flatmap(_, <<>>, Acc) -> Acc;
flatmap(Fun, <<H, T/binary>>, Acc) ->
    flatmap(Fun, T, <<Acc/binary, (Fun(H))/binary>>).


%%--------------------------------------------------------------------
%% Function: flatten(DeepString) -> String.
%% @doc
%%   Returns a flattened version of DeepString.
%% @end
%%--------------------------------------------------------------------
-spec flatten(iolist()) -> binary().
%%--------------------------------------------------------------------
flatten(IOList) -> iolist_to_binary(IOList).

%%--------------------------------------------------------------------
%% Function: flatten(DeepString, Tail) -> String.
%% @doc
%%   Returns a flattened version of DeepString with the tail Tail appended.
%% @end
%%--------------------------------------------------------------------
-spec flatten(iolist(), binary()) -> binary().
%%--------------------------------------------------------------------
flatten(IOList, Binary) when is_binary(Binary) ->
    iolist_to_binary([IOList, Binary]).

%%--------------------------------------------------------------------
%% Function: foldl(Fun, Acc0, String) -> Acc1.
%% @doc
%%   Calls Fun(Elem, AccIn) on successive elements A of String, starting
%%   with AccIn == Acc0. Fun/2 must return a new accumulator which is passed
%%   to the next call. The function returns the final value of the accumulator.
%%   Acc0 is returned if the string is empty.
%% @end
%%--------------------------------------------------------------------
-spec foldl(fun((byte(), Acc) -> Acc), Acc, binary()) -> Acc.
%%--------------------------------------------------------------------
foldl(Fun, Acc, <<>>) when is_function(Fun, 2) -> Acc;
foldl(Fun, Acc, <<H, T/binary>>) -> foldl(Fun, Fun(H, Acc), T).

%%--------------------------------------------------------------------
%% Function: foldr(Fun, Acc0, String) -> Acc1.
%% @doc
%%   Like foldl/3, but the string is traversed from right to left.
%% @end
%%--------------------------------------------------------------------
-spec foldr(fun((byte(), Acc) -> Acc), Acc, binary()) -> Acc.
%%--------------------------------------------------------------------
foldr(Fun, Acc, Binary) -> foldl(Fun, Acc, reverse(Binary)).

%%--------------------------------------------------------------------
%% Function: foreach(Fun, String) -> ok
%% @doc
%%   Calls Fun(Elem) for each element Elem in String. This function is
%%   used for its side effects and the evaluation order is defined to be
%%   the same as the order of the elements in the string.
%% @end
%%--------------------------------------------------------------------
-spec foreach(fun((byte()) -> _), binary()) -> ok.
%%--------------------------------------------------------------------
foreach(Fun, <<>>) when is_function(Fun, 1) -> ok;
foreach(Fun, <<H, T/binary>>) ->
    Fun(H),
    foreach(Fun, T).

%%--------------------------------------------------------------------
%% Function: keydelete(Key, N, BlobSequence1) -> BlobSequence2.
%% @doc
%%   Returns a copy of BlobSequence1 where the first occurrence of a
%%   blob whose Nth element compares equal to Key is deleted, if there
%%   is such a blob.
%% @end
%%--------------------------------------------------------------------
-spec keydelete(byte(), pos_integer(), pos_integer(), binary()) -> binary().
%%--------------------------------------------------------------------
keydelete(Key, N, Size, Binary) -> keydelete(Key, N, Size, Binary, <<>>).

keydelete(_, _, _, <<>>, Acc) -> Acc;
keydelete(Key, N, Size, Binary, Acc) ->
    Next = next(Size, Binary),
    case key(N, Binary) of
        Key ->
             <<Acc/binary, Next/binary>>;
        _ ->
            This = this(Size, Binary),
            keydelete(Key, N, Size, Next, <<Acc/binary, This/binary>>)
    end.

%%--------------------------------------------------------------------
%% Function: keyfind(Key, N, BlobSequence) -> Blob | false.
%% @doc
%%   Searches the list of tuples BlobSequence for a tuple whose Nth element
%%  compares equal to Key. Returns Blob if such a tuple is found,
%%   otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec keyfind(byte(), pos_integer(), pos_integer(), binary()) ->
          binary() | false.
%%--------------------------------------------------------------------
keyfind(_, _, _, <<>>) -> false;
keyfind(Key, N, Size, Binary) ->
    case key(N, Binary) of
        Key ->
            this(Size, Binary);
        _ ->
            keyfind(Key, N, Size, next(Size, Binary))
    end.

%%--------------------------------------------------------------------
%% Function: keymap(Fun, N, BlobSequence1) -> BlobSequence2
%% @doc
%%   Returns a sequence of blobs where, for each blob in BlobSequence1,
%%   the Nth element Octet of the blob has been replaced with the result
%%   of calling Fun(Octet).
%% @end
%%--------------------------------------------------------------------
-spec keymap(fun((byte()) -> byte()), pos_integer(), pos_integer(), binary()) ->
          binary().
%%--------------------------------------------------------------------
keymap(F, N, Size, Binary)
  when is_function(F, 1), is_integer(N), is_integer(Size), is_binary(Binary) ->
    keymap(F, N, Size, Binary, <<>>).

keymap(_, _, _, <<>>, Acc) -> Acc;
keymap(F, N, Size, Binary, Acc) ->
    H = binary_part(Binary, {0, N - 1}),
    T = binary_part(Binary, {N, Size - N}),
    Elt = <<H/binary, (F(key(N, Binary))), T/binary>>,
    keymap(F, N, Size, next(Size, Binary), <<Acc/binary, Elt/binary>>).

%%--------------------------------------------------------------------
%% Function: keymember(Key, N, BlobSequence) -> Boolean.
%% @doc
%%   Returns true if there is a blob in BlobSequence whose Nth element
%%   compares equal to Key, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec keymember(byte(), pos_integer(), pos_integer(), binary()) -> boolean().
%%--------------------------------------------------------------------
keymember(_, _, _, <<>>) -> false;
keymember(Key, N, Size, Binary) ->
    case key(N, Binary) of
        Key ->
            true;
        _ ->
            keymember(Key, N, Size, next(Size, Binary))
    end.

%%--------------------------------------------------------------------
%% Function: keymerge(N, BlobSequence1, BlobSequence2) -> BlobSequence3.
%% @doc
%%   Returns the sorted binary formed by merging BlobSequence1 and
%%   BlobSequence2. The merge is performed on the Nth element of each blob.
%%   Both BlobSequence1 and BlobSequence2 must be key-sorted prior to
%%   evaluating this function. When two blobs compare equal, the blob
%%  from BlobSequence1 is picked before the tuple from BlobSequence2.
%% @end
%%--------------------------------------------------------------------
-spec keymerge(pos_integer(), pos_integer(), binary(), binary()) -> binary().
%%--------------------------------------------------------------------
keymerge(N, Size, Binary1, Binary2)
  when is_integer(N), is_integer(Size), is_binary(Binary1),is_binary(Binary2) ->
    keymerge(N, Size, Binary1, Binary2, <<>>).

keymerge(_, _, <<>>, Binary, Acc) -> <<Acc/binary, Binary/binary>>;
keymerge(_, _, Binary, <<>>, Acc) -> <<Acc/binary, Binary/binary>>;
keymerge(N, Size, Binary1, Binary2, Acc) ->
    case key(N, Binary1) > key(N, Binary2) of
        true ->
            keymerge(N, Size, Binary1, next(Size, Binary2),
                     <<Acc/binary, (this(Size, Binary2))/binary>>);
        false ->
            keymerge(N, Size, next(Size, Binary1), Binary2,
                     <<Acc/binary, (this(Size, Binary1))/binary>>)
    end.

%%--------------------------------------------------------------------
%% Function: keyreplace(Key, N, BlobSequence1, NewBlob) -> BlobSequence2.
%% @doc
%%   Returns a copy of BlobSequence1 where the first occurrence of a T
%%   blob whose Nth element compares equal to Key is replaced with NewBlob,
%%   if there is such a blob T.
%% @end
%%--------------------------------------------------------------------
-spec keyreplace(byte(), pos_integer(), pos_integer(), binary(), binary()) ->
           binary().
%%--------------------------------------------------------------------
keyreplace(Key, N, Size, Binary, Item)
  when is_integer(N), is_integer(Size), is_binary(Binary), is_binary(Item) ->
    keyreplace(Key, N, Size, Binary, Item, 0, Binary).

keyreplace(_, _, _, <<>>, _, _, Binary) -> Binary;
keyreplace(Key, N, Size, Binary, Item, Count, Orig) ->
    case key(N, Binary) of
        Key ->
            HeadSize = Size * Count,
            TailSize = (Size * (Count + 1)),
            H = binary_part(Orig, {0, HeadSize}),
            T = binary_part(Orig, {TailSize, byte_size(Orig) - TailSize}),
            <<H/binary, Item/binary, T/binary>>;
        _ ->
            keyreplace(Key, N, Size, next(Size, Binary), Item, Count + 1, Orig)
    end.

%%--------------------------------------------------------------------
%% Function: keysearch(Key, N, BlobSequence) -> {value, Blob} | false.
%% @doc
%%   Searches the sequence of blobs BlobSequence for a blob whose Nth
%%   element compares equal to Key. Returns {value, Blob} if such a blob
%%   is found, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec keysearch(byte(), pos_integer(), pos_integer(), binary()) ->
          {value, binary()} | false.
%%--------------------------------------------------------------------
keysearch(Key, N, Size, Binary) ->
    case keyfind(Key, N, Size, Binary) of
        false -> false;
        Binary1 -> {value, Binary1}
    end.

%%--------------------------------------------------------------------
%% Function: keysort(N, BlobSequence1) -> BlobSequence2.
%% @doc
%%   Returns a binary containing the sorted elements of the blob sequence
%%   BlobSequence1. Sorting is performed on the Nth element of the blobs.
%%   The sort is stable.
%% @end
%%--------------------------------------------------------------------
-spec keysort(pos_integer(), pos_integer(), binary()) -> binary().
%%--------------------------------------------------------------------
keysort(N, Size, <<>>) when is_integer(N), is_integer(Size) -> <<>>;
keysort(N, Size, Binary) when is_integer(N), is_integer(Size) ->
    case byte_size(Binary) of
        Size -> Binary;
        Length when Length rem Size == 0 ->
            Pivot = ((Length div Size) div 2) * Size,
            First = binary_part(Binary, {0, Pivot}),
            Second = binary_part(Binary, {Pivot, Length - Pivot}),
            keymerge(N, Size, keysort(N, Size,First), keysort(N, Size,Second));
        _ ->
            erlang:error(badarg, [N, Size, Binary])
    end.

%%--------------------------------------------------------------------
%% Function: keystore(Key, N, BlobSequence1, NewBlob) -> BlobSequence2.
%% @doc
%%   Returns a copy of BlobSequence1 where the first occurrence of a blob
%%   T whose Nth element compares equal to Key is replaced with NewBlob,
%%   if there is such a tuple T. If there is no such blob T a copy of
%%   BlobSequence1 where NewBlob has been appended to the end is returned.
%% @end
%%--------------------------------------------------------------------
-spec keystore(byte(), pos_integer(), pos_integer(), binary(), binary()) ->
           binary().
%%--------------------------------------------------------------------
keystore(Key, N, Size, Binary, Item)
  when is_integer(N), is_integer(Size), is_binary(Binary), is_binary(Item) ->
    keystore(Key, N, Size, Binary, Item, 0, Binary).

keystore(_, _, _, <<>>, Item, _, Binary) -> <<Binary/binary, Item/binary>>;
keystore(Key, N, Size, Binary, Item, Count, Orig) ->
    case key(N, Binary) of
        Key ->
            HeadSize = Size * Count,
            TailSize = (Size * (Count + 1)),
            H = binary_part(Orig, {0, HeadSize}),
            T = binary_part(Orig, {TailSize, byte_size(Orig) - TailSize}),
            <<H/binary, Item/binary, T/binary>>;
        _ ->
            keystore(Key, N, Size, next(Size, Binary), Item, Count + 1, Orig)
    end.

%%--------------------------------------------------------------------
%% Function: keytake(Key, N, BlobSequence1) ->
%%               {value, Blob, BlobSequence2} | false.
%% @doc
%%   Searches the sequence of blobs BlobSequence1 for a blob whose Nth
%%   element compares equal to Key. Returns {value, Blob, BlobSequence2}
%%   if such a tuple is found, otherwise false. BlobSequence2 is a copy
%%   of BlobSequence1 where the first occurrence of Blob has been removed.
%% @end
%%--------------------------------------------------------------------
-spec keytake(byte(), pos_integer(), pos_integer(), binary()) ->
           {value, binary(), binary()} | false.
%%--------------------------------------------------------------------
keytake(Key, N, Size, Binary)
  when is_integer(N), is_integer(Size), is_binary(Binary) ->
    keytake(Key, N, Size, Binary, 0, Binary).

keytake(_, _, _, <<>>, _, _) -> false;
keytake(Key, N, Size, Binary, Count, Orig) ->
    case key(N, Binary) of
        Key ->
            HeadSize = Size * Count,
            TailSize = (Size * (Count + 1)),
            Item = binary_part(Orig, {HeadSize, Size}),
            H = binary_part(Orig, {0, HeadSize}),
            T = binary_part(Orig, {TailSize, byte_size(Orig) - TailSize}),
            {value, Item, <<H/binary, T/binary>>};
        _ ->
            keytake(Key, N, Size, next(Size, Binary), Count + 1, Orig)
    end.

%%--------------------------------------------------------------------
%% Function: last(binary) -> Last.
%% @doc
%%   Returns the last octet in the binary.
%% @end
%%--------------------------------------------------------------------
-spec last(binary()) -> byte().
%%--------------------------------------------------------------------
last(Binary) ->
    <<L>> = binary_part(Binary, {byte_size(Binary), -1}),
    L.

%%--------------------------------------------------------------------
%% Function: map(Fun, Binary1) -> Binary2.
%% @doc
%%   Takes a function from octets to octets, and a binary produces binary
%%   by applying the function to every octet in the binary. This function
%%   is used to obtain the return values. The evaluation order is
%%   implementation dependent.
%% @end
%%--------------------------------------------------------------------
-spec map(fun((byte()) -> byte()), binary()) -> binary().
%%--------------------------------------------------------------------
map(F, Binary) -> map(F, Binary, <<>>).

map(F, <<>>, Acc) when is_function(F, 1) -> Acc;
map(F, <<H, T/binary>>, Acc) -> map(F, T, <<Acc/binary, (F(H))>>).

%%--------------------------------------------------------------------
%% Function: mapfoldl(Fun, Acc0, Binary1) -> {Binary2, Acc1}.
%% @doc
%%   mapfoldl combines the operations of map/2 and foldl/3 into one pass.
%% @end
%%--------------------------------------------------------------------
-spec mapfoldl(fun((byte(), Acc) -> {byte(), Acc}), Acc, binary()) ->
          {binary(), Acc}.
%%--------------------------------------------------------------------
mapfoldl(Fun, Acc, Binary) when is_function(Fun, 2)->
    mapfoldl(Fun, Acc, Binary, <<>>).

mapfoldl(_, Acc, <<>>, Binary) -> {Binary, Acc};
mapfoldl(Fun, Acc, <<H, T/binary>>, Binary) ->
    {H1, Acc1} = Fun(H, Acc),
    mapfoldl(Fun, Acc1, T, <<Binary/binary, H1>>).

%%--------------------------------------------------------------------
%% Function: mapfoldr(Fun, Acc0, Binary1) -> {Binary2, Acc1}.
%% @doc
%%   mapfoldr combines the operations of map/2 and foldr/3 into one pass
%% @end
%%--------------------------------------------------------------------
-spec mapfoldr(fun((byte(), Acc) -> {byte(), Acc}), Acc, binary()) ->
          {binary(), Acc}.
%%--------------------------------------------------------------------
mapfoldr(Fun, Acc, Binary) when is_function(Fun, 2)->
    mapfoldr(Fun, Acc, reverse(Binary), <<>>).

mapfoldr(_, Acc, <<>>, Binary) -> {reverse(Binary), Acc};
mapfoldr(Fun, Acc, <<H, T/binary>>, Binary) ->
    {H1, Acc1} = Fun(H, Acc),
    mapfoldr(Fun, Acc1, T, <<Binary/binary, H1>>).

%%--------------------------------------------------------------------
%% Function: max(Binary) -> Max.
%% @doc
%%   Returns the first octet of the binary that compares greater than
%%   or equal to all other octets in the binary.
%% @end
%%--------------------------------------------------------------------
-spec max(binary()) -> byte().
%%--------------------------------------------------------------------
max(<<H, T/binary>>) -> max(T, H).

max(<<>>, Max) -> Max;
max(<<H, T/binary>>, Max) when H > Max-> max(T, H);
max(<<_, T/binary>>, Max) -> max(T, Max).

%%--------------------------------------------------------------------
%% Function: member(Elem, Binary) -> Boolean.
%% @doc
%%   Returns true if Elem matches some element of Binary, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec member(char(), binary()) -> boolean().
%%--------------------------------------------------------------------
member(_, <<>>) -> false;
member(C, <<C, _/binary>>) -> true;
member(C, <<_, B/binary>>) -> member(C, B).


%%--------------------------------------------------------------------
%% Function: merge(ListOfBinaries) -> Binary
%% @doc
%%   Returns the sorted binary formed by merging all the sub-binaries of
%%   ListOfBinaries. All sub-binaries must be sorted prior to evaluating
%%   this function. When two octets compare equal, the octets from the
%%   sub-binary with the lowest position in ListOfBinaries is picked
%%   before the other octet.
%% @end
%%--------------------------------------------------------------------
-spec merge([binary()]) -> binary().
%%--------------------------------------------------------------------
merge([]) -> <<>>;
merge(Binaries) ->
    merge_list(lists:sort(fun merge_list_comp/2, Binaries), <<>>).

merge_list([Binary], Acc) -> <<Acc/binary, Binary/binary>>;
merge_list([<<>> | T], Acc) -> merge_list(T, Acc);
merge_list([<<H1, T1/binary>>, B = <<H2, _/binary>> | T], Acc) when H1 =< H2 ->
    merge_list([T1, B | T], <<Acc/binary, H1>>);
merge_list(Binaries, Acc) ->
    merge_list(merge_list_bubble(Binaries), Acc).

merge_list_bubble([H]) -> [H];
merge_list_bubble(L = [<<H1, _/binary>>, <<H2, _/binary>> | _]) when H1 =< H2 ->
    L;
merge_list_bubble([H, H1 | T]) ->
    [H1 | merge_list_bubble([H | T])].

merge_list_comp(<<>>, _) -> true;
merge_list_comp(_, <<>>) -> false;
merge_list_comp(<<H1, _/binary>>, <<H2, _/binary>>) -> H1 =< H2.

%%--------------------------------------------------------------------
%% Function: merge(Binary1, Binary2) -> Binary3.
%% @doc
%%   Returns the sorted binary formed by merging Binary1 and Binary2.
%%   Both Binary1 and Binary2 must be sorted prior to evaluating this function.
%%   When two octets compare equal, the octet from Binary1 is picked
%%   before the octet from Binary2.
%% @end
%%--------------------------------------------------------------------
-spec merge(binary(), binary()) -> binary().
%%--------------------------------------------------------------------
merge(B1, B2) when is_binary(B1), is_binary(B2) -> merge1(B1, B2, <<>>).

merge1(<<>>, B, Acc) -> <<Acc/binary, B/binary>>;
merge1(B, <<>>, Acc) -> <<Acc/binary, B/binary>>;
merge1(<<H1, T1/binary>>, B = <<H2, _/binary>>, Acc) when H1 =< H2 ->
    merge1(T1, B, <<Acc/binary, H1>>);
merge1(B, <<H, T/binary>>, Acc) ->
    merge1(B, T, <<Acc/binary, H>>).

%%--------------------------------------------------------------------
%% Function: merge(Fun, Binary1, Binary2) -> Binary3.
%% @doc
%%   Returns the sorted binary formed by merging Binary1 and Binary2.
%%   Both Binary1 and Binary2 must be sorted according to the ordering
%%   function Fun prior to evaluating this function. Fun(A, B) should
%%   return true if A compares less than or equal to B in the ordering,
%%   false otherwise. When two octets compare equal, the octet from Binary1
%%   is picked before the octet from Binary2.
%% @end
%%--------------------------------------------------------------------
-spec merge(fun((byte(), byte()) -> boolean()), binary(), binary()) ->
          binary().
%%--------------------------------------------------------------------
merge(Pred, B1, B2) when is_function(Pred, 2), is_binary(B1), is_binary(B2) ->
    merge1(Pred, B1, B2, <<>>).

merge1(_, <<>>, B, Acc) -> <<Acc/binary, B/binary>>;
merge1(_, B, <<>>, Acc) -> <<Acc/binary, B/binary>>;
merge1(Pred, B1 = <<H1, T1/binary>>, B2 = <<H2, T2/binary>>, Acc) ->
    case Pred(H1, H2) of
        true -> merge1(Pred, T1, B2, <<Acc/binary, H1>>);
        false -> merge1(Pred, B1, T2, <<Acc/binary, H2>>)
    end.

%%--------------------------------------------------------------------
%% Function: merge3(Binary1, Binary2, Binary3) -> Binary4.
%% @doc
%%   Returns the sorted binary formed by merging Binary1, Binary2 and Binary3.
%%  All of Binary1, Binary2 and Binary3 must be sorted prior to evaluating
%%  this function. When two octets compare equal, the octet from Binary1,
%%  if there is such an octet, is picked before the other octet, otherwise
%%  the octet from Binary2 is picked before the octet from Binary3.
%% @end
%%--------------------------------------------------------------------
-spec merge3(binary(), binary(), binary()) -> binary().
%%--------------------------------------------------------------------
merge3(B1, B2, B3) -> merge([B1, B2, B3]).

%%--------------------------------------------------------------------
%% Function: min(Binary) -> Min.
%% @doc
%%   Returns the first octet of Binary that compares less than or equal
%%   to all other octets of Binary.
%% @end
%%--------------------------------------------------------------------
-spec min(binary()) -> byte().
%%--------------------------------------------------------------------
min(<<H, T/binary>>) -> min(T, H).

min(<<>>, Min) -> Min;
min(<<H, T/binary>>, Min) when H =< Min-> min(T, H);
min(<<_, T/binary>>, Min) -> min(T, Min).

%%--------------------------------------------------------------------
%% Function: nth(N, Binary) -> Octet.
%% @doc
%%   Returns the Nth octet of Binary. One based.
%% @end
%%--------------------------------------------------------------------
-spec nth(pos_integer(), binary()) -> byte().
%%--------------------------------------------------------------------
nth(Pos, Binary) when is_integer(Pos), is_binary(Binary) ->
    <<Item>> = binary_part(Binary, {Pos - 1, 1}),
    Item.

%%--------------------------------------------------------------------
%% Function: nthtail(N, Binary) -> Tail.
%% @doc
%%   Returns the Nth tail of Binary, that is, the sub-binary of Binary
%%   starting at N+1 and continuing up to the end of the binary.
%% @end
%%--------------------------------------------------------------------
-spec nthtail(pos_integer(), binary()) -> binary().
%%--------------------------------------------------------------------
nthtail(Pos, Binary) when is_integer(Pos), is_binary(Binary) ->
    binary_part(Binary, {Pos, byte_size(Binary) - Pos}).

%%--------------------------------------------------------------------
%% Function: partition(Pred, Binary) -> {Satisfying, NotSatisfying}.
%% @doc
%%   Partitions Binary into two binaries, where the first binary contains
%%   all octets for which Pred(Octet) returns true, and the second binary
%%   contains all octets for which Pred(Octet) returns false.
%% @end
%%--------------------------------------------------------------------
-spec partition(fun((byte()) -> boolean()), binary()) -> {binary(), binary()}.
%%--------------------------------------------------------------------
partition(Pred, Binary) when is_function(Pred, 1), is_binary(Binary) ->
    partition(Pred, Binary, <<>>, <<>>).

partition(_, <<>>, Sat, NotSat) -> {Sat, NotSat};
partition(Pred, <<H, T/binary>>, Sat, NotSat) ->
    case Pred(H) of
        true -> partition(Pred, T, <<Sat/binary, H>>, NotSat);
        false -> partition(Pred, T, Sat, <<NotSat/binary, H>>)
    end.

%%--------------------------------------------------------------------
%% Function: prefix(Binary1, Binary2) -> boolean().
%% @doc
%%   Returns true if Binary1 is a prefix of Binary2, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec prefix(binary(), binary()) -> boolean().
%%--------------------------------------------------------------------
prefix(Binary1, Binary2) ->
    case {byte_size(Binary1), byte_size(Binary2)} of
        {Size1, Size2} when Size1 > Size2 -> false;
        {Size1, _} -> Binary1 == binary_part(Binary2, {0, Size1})
    end.

%%--------------------------------------------------------------------
%% Function: reverse(String1) -> String2.
%% @doc
%%   Returns a string with the elements in String1 in reverse order.
%% @end
%%--------------------------------------------------------------------
-spec reverse(binary()) -> binary().
%%--------------------------------------------------------------------
reverse(<<>>) -> <<>>;
reverse(<<H, T/binary>>) -> <<(reverse(T))/binary, H>>.

%%--------------------------------------------------------------------
%% Function: reverse(String1, Tail) -> String2
%% @doc
%%   Returns a string with the elements in String1 in reverse order,
%%   with the tail Tail appended.
%% @end
%%--------------------------------------------------------------------
-spec reverse(binary(), binary()) -> binary().
%%--------------------------------------------------------------------
reverse(Binary, Tail) -> <<(reverse(Binary))/binary, Tail/binary>>.

%%--------------------------------------------------------------------
%% Function: seq(From, To) -> Seq.
%% @doc
%%   Returns a sequence of integers which starts with From and contains the
%%   successive results of adding 1 to the previous element, until To has
%%   been reached number encompassed by the sequence. Wraps to zero when
%%   it reaches 255.
%% @end
%%--------------------------------------------------------------------
-spec seq(byte(), byte()) -> binary().
%%--------------------------------------------------------------------
seq(From, To) when is_integer(From), is_integer(To), From - 1 =< To ->
    seq1(From, To, <<>>).

seq1(From, To, Acc) when From > To -> Acc;
seq1(From, To, Acc) -> seq1(From + 1, To, <<Acc/binary, From>>).

%%--------------------------------------------------------------------
%% Function: seq(From, To, Incr) -> Seq.
%% @doc
%%   Returns a sequence of integers which starts with From and contains the
%%   successive results of adding Incr to the previous element, until To has
%%   been reached or passed (in the latter case, To is not an element of
%%   the sequence). Wraps to zero when it reaches 255.
%% @end
%%--------------------------------------------------------------------
-spec seq(byte(), byte(), byte()) -> binary().
%%--------------------------------------------------------------------
seq(From, From, _) -> <<From>>;
seq(From, To, Incr)
  when is_integer(From), is_integer(To), is_integer(Incr),
       Incr > 0, From - Incr =< To;
       is_integer(From), is_integer(To), is_integer(Incr), Incr < 0,
       From >= To + Incr ->
    seq1(From, To, Incr, <<>>).

seq1(From, To, Incr, Acc) when Incr > 0, From > To -> Acc;
seq1(From, To, Incr, Acc) when Incr < 0, From < To -> Acc;
seq1(From, To, Incr, Acc) -> seq1(From + Incr, To, Incr, <<Acc/binary, From>>).

%% ===================================================================
%% Internal functions.
%% ===================================================================
key(N, Binary) -> <<Key>> = binary_part(Binary, {N - 1, 1}), Key.

next(Size, Binary) -> binary_part(Binary, {Size, byte_size(Binary) - Size}).

this(Size, Binary) -> binary_part(Binary, {0, Size}).
