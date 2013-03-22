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
%%%   eunit unit tests for the plist library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(plist_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% new/0
%%--------------------------------------------------------------------
new_0_test() ->
    [?_test(?assertEqual([], plist:new()))
    ].

%%--------------------------------------------------------------------
%% new/2
%%--------------------------------------------------------------------
new_2_test_() ->
    [?_test(?assertEqual(plist:new(), plist:new([], []))),
     ?_test(?assertMatch([_], plist:new([a], [1]))),
     ?_test(?assert(key_val_list_p(plist:new([a], [1])))),
     ?_test(?assert(key_val_list_p(plist:new([a, b], [1, 2])))),
     ?_test(
        ?assert(
           key_val_list_p(
             plist:new(lists:duplicate(10, "a"),
                       lists:seq(1, 10))))),
     ?_test(
        ?assert(
           key_val_list_p(
             plist:new(lists:seq(1, 10),
                       lists:seq(1, 10))))),
     ?_test(
        ?assertError(badarg, plist:new([a], []))),
     ?_test(
        ?assertError(badarg, plist:new([], [a]))),
     ?_test(
        ?assertError(badarg, plist:new(a, [b]))),
     ?_test(
        ?assertError(badarg, plist:new(lists:seq(1, 10), lists:seq(1, 11))))
     ].

%%--------------------------------------------------------------------
%% add/3
%%--------------------------------------------------------------------
add_3_test_() ->
    ListA = plist:new([1, 2], [a, b]),
    ListB = plist:new([1, 2, 3], [a, b, c]),
    ListC = plist:new([1, 2, 3], [a, b, x]),
    [?_test(?assert(key_val_list_p(plist:add(3, c, [])))),
     ?_test(?assert(key_val_list_p(plist:add(3, c, ListA)))),
     ?_test(?assert(key_val_list_p(plist:add(3, c, ListB)))),
     ?_test(?assert(key_val_list_p(plist:add(3, c, ListC)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, [])))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListA)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListB)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListC))))
     ].

%%--------------------------------------------------------------------
%% add/4
%%--------------------------------------------------------------------
add_4_test_() ->
    ListA = plist:new([1, 2], [a, b]),
    ListB = plist:new([1, 2, 3], [a, b, c]),
    ListC = plist:new([1, 2, 3], [a, b, x]),
    [?_test(?assert(key_val_list_p(plist:add(3, c, [], nocheck)))),
     ?_test(?assert(key_val_list_p(plist:add(3, c, ListA, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:add(3, c, ListB, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:add(3, c, ListC, nocheck)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, [], nocheck)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListA, nocheck)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListB, nocheck)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListC, nocheck)))),
     ?_test(?assert(key_val_list(plist:add(3, c, [], check), 1))),
     ?_test(?assert(key_val_list(plist:add(3, c, ListA, check),
                                 length(ListA) + 1))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, [], check)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListA, check)))),
     ?_test(
        ?assertError(badarg, plist:add(3, c, ListB, check))),
     ?_test(
        ?assertError(badarg, plist:add(3, c, ListC, check)))
     ].


%% ===================================================================
%% Internal functions.
%% ===================================================================

key_val_list_p([]) -> true;
key_val_list_p([{_, _} | T]) -> key_val_list_p(T).

%%--------------------------------------------------------------------
key_val_list([], 0) -> 0;
key_val_list(KVL, L) -> key_val_list(KVL, 0, L).

key_val_list([], N, N) -> true;
key_val_list([], _, _) -> false;
key_val_list([{_, _} | T], N, L) -> key_val_list(T, N + 1, L);
key_val_list(_, _, _) -> false.
