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

         member/2, reverse/1, reverse/2
        ]).

%% Types
-type thing() :: atom() | integer() | float() | string() | binary().

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
dropwhile(Pred, <<>>) when is_function(Pred) -> <<>>;
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

