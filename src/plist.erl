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
%%%   A property list style handling of {Key, Value} tuples.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(plist).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions.
-export([new/0, new/2,
         add/3, add/4,
         delete/2, delete/3,
         find/2, find/3, find/4, find_all/2,
         keys/1, values/1,
         member/2, replace/3, replace/4,
         compact/1
        ]).

%% Types
-type key() :: _.
-type value() :: _.
-type default() :: _.
-type plist() :: [{key(), value()}].
-type flag() :: check | nocheck.
-type placement() :: first | last.

%% Exported Types
-export_type([plist/0]).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: new() -> PList.
%% @doc
%%   Creates an empty plist.
%% @end
%%--------------------------------------------------------------------
-spec new() -> plist().
%%--------------------------------------------------------------------
new() -> new([], []).

%%--------------------------------------------------------------------
%% Function: new(Keys, Values) -> PList.
%% @doc
%%   Creates a plist from the zipping the lists of keys and values.
%% @end
%%--------------------------------------------------------------------
-spec new([key()], [value()]) -> plist().
%%--------------------------------------------------------------------
new(Keys, Values) -> new(Keys, Values, []).

new([], [], Acc) -> Acc;
new([H1 | T1], [H2 | T2], Acc) -> new(T1, T2, [{H1, H2} | Acc]);
new(KeysT, ValuesT, Acc) ->
    {Keys, Values} = lists:unzip(Acc),
    erlang:error(badarg,
                 [lists:reverse(Keys) ++ KeysT,
                  lists:reverse(Values) ++ ValuesT]).

%%--------------------------------------------------------------------
%% Function: add(Key, Values, PList) -> PList.
%% @doc
%%   Extends Plist with the property without checking if it exists.
%% @end
%%--------------------------------------------------------------------
-spec add(key(), value(), plist()) -> plist().
%%--------------------------------------------------------------------
add(Key, Value, PList) -> add(Key, Value, PList, nocheck).

%%--------------------------------------------------------------------
%% Function: add(Key, Values, PList, Flag) -> PList.
%% @doc
%%   Extends Plist with the property checking if it exists if required.
%% @end
%%--------------------------------------------------------------------
-spec add(key(), value(), plist(), flag()) -> plist().
%%--------------------------------------------------------------------
add(Key, Value, PList, nocheck) -> [{Key, Value} | PList];
add(Key, Value, PList, check) ->
    case lists:keymember(Key, 1, PList) of
        true -> erlang:error(badarg, [Key, Value, PList, check]);
        false -> add(Key, Value, PList, nocheck)
    end.

%%--------------------------------------------------------------------
%% Function: delete(Key, PList) -> PList.
%% @doc
%%   Restricts Plist on the key without checking if it exists.
%% @end
%%--------------------------------------------------------------------
-spec delete(key(), plist()) -> plist().
%%--------------------------------------------------------------------
delete(Key, PList) -> delete(Key, PList, nocheck).

%%--------------------------------------------------------------------
%% Function: delete(Key, PList, Flag) -> PList.
%% @doc
%%   Restricts Plist on the key without checking if it exists if required.
%% @end
%%--------------------------------------------------------------------
-spec delete(key(), plist(), flag()) -> plist().
%%--------------------------------------------------------------------
delete(Key, PList, nocheck) -> lists:keydelete(Key, 1, PList);
delete(_, [], check) -> erlang:error(badarg);
delete(Key, [{Key, _} | T], check) -> T;
delete(Key, [_ | T], check) -> delete(Key, T, check).

%%--------------------------------------------------------------------
%% Function: find(Key, PList) -> Value.
%% @doc
%%   Finds the value of the property or undefined if not found.
%% @end
%%--------------------------------------------------------------------
-spec find(key(), plist()) -> value() | undefined.
%%--------------------------------------------------------------------
find(Key, PList) -> find(Key, PList, undefined).

%%--------------------------------------------------------------------
%% Function: find(Key, PList, Default) -> Value.
%% @doc
%%   Finds the value of the property or Default if not found.
%% @end
%%--------------------------------------------------------------------
-spec find(key(), plist(), default()) -> value() | default().
%%--------------------------------------------------------------------
find(Key, PList, Default) -> find(Key, PList, Default, first).

%%--------------------------------------------------------------------
%% Function: find(Key, PList, Default, Placement) -> Value.
%% @doc
%%   Finds the value of the property or Default if not found.
%%   If more than property is found the one returned if determined by
%%   the Placement as being the first or last.
%% @end
%%--------------------------------------------------------------------
-spec find(key(), plist(), default(), placement()) -> value() | default().
%%--------------------------------------------------------------------
find(_, [], Value, _) -> Value;
find(Key, PList, Default, first) ->
    case lists:keyfind(Key, 1, PList) of
        false -> Default;
        {_, Value} -> Value
    end;
find(Key, [{Key, Value} | T], _, last) -> find(Key, T, Value, last);
find(Key, [_ | T], Value, last) -> find(Key, T, Value, last).

%%--------------------------------------------------------------------
%% Function: find_all(Key, PList) -> Values.
%% @doc
%%   Finds the all values associated with the key.
%% @end
%%--------------------------------------------------------------------
-spec find_all(key(), plist()) -> [value()].
%%--------------------------------------------------------------------
find_all(Key, PList) -> find_all(Key, PList, []).

find_all(_, [], Acc) -> lists:reverse(Acc);
find_all(Key, [{Key, Value} | T], Acc) -> find_all(Key, T, [Value | Acc]);
find_all(Key, [_ | T], Acc) -> find_all(Key, T, Acc).

%%--------------------------------------------------------------------
%% Function: keys(PList) -> Keys.
%% @doc
%%   Returns all the keys.
%% @end
%%--------------------------------------------------------------------
-spec keys(plist()) -> [key()].
%%--------------------------------------------------------------------
keys(PList) -> lists:usort([Key || {Key, _} <- PList]).

%%--------------------------------------------------------------------
%% Function: values(PList) -> Values.
%% @doc
%%   Returns all the values.
%% @end
%%--------------------------------------------------------------------
-spec values(plist()) -> [value()].
%%--------------------------------------------------------------------
values(PList) -> [Value || {_, Value} <- PList].

%%--------------------------------------------------------------------
%% Function: member(PList) -> Boolean.
%% @doc
%%   Returns wether the key is to be found in the PList.
%% @end
%%--------------------------------------------------------------------
-spec member(key(), plist()) -> boolean().
%%--------------------------------------------------------------------
member(Key, PList) -> lists:keymember(Key, 1, PList).

%%--------------------------------------------------------------------
%% Function: replace(Key, Value, PList) -> PList.
%% @doc
%%   Replaces the first occurence in the PList, adding if it if not found.
%% @end
%%--------------------------------------------------------------------
-spec replace(key(), value(), plist()) -> plist().
%%--------------------------------------------------------------------
replace(Key, Value, PList) -> replace(Key, Value, PList, nocheck).

%%--------------------------------------------------------------------
%% Function: replace(Key, Value, PList, Flag) -> PList.
%% @doc
%%   Replaces the first occurence in the PList, esuring that i does exist
%%   if required.
%% @end
%%--------------------------------------------------------------------
-spec replace(key(), value(), plist(), flag()) -> plist().
%%--------------------------------------------------------------------
replace(Key, Value, [], nocheck) -> [{Key, Value}];
replace(_, _, [], check) -> erlang:error(badarg);
replace(Key, Value, [{Key, _} | T], _) -> [{Key, Value} | T];
replace(Key, Value, [H | T], Check) -> [H | replace(Key, Value, T, Check)].

%%--------------------------------------------------------------------
%% Function: compact(PList) -> PList.
%% @doc
%%   Ensures one property per key, with the first given precedence.
%% @end
%%--------------------------------------------------------------------
-spec compact(plist()) -> plist().
%%--------------------------------------------------------------------
compact([]) ->[];
compact(List) -> lists:ukeysort(1, List).
