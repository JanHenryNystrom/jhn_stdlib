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
     ?_test(?assert(key_val_list_p(plist:add(1, a, ListB)))),
     ?_test(?assert(key_val_list_p(plist:add(1, c, ListC)))),
     ?_test(?assert(key_val_list_p(plist:add(2, b, ListB)))),
     ?_test(?assert(key_val_list_p(plist:add(2, c, ListC)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, [])))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListA)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListB)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListC)))),
     ?_test(?assertEqual(a, plist:find(1, plist:add(1, a, ListB)))),
     ?_test(?assertEqual(c, plist:find(1, plist:add(1, c, ListC)))),
     ?_test(?assertEqual(b, plist:find(2, plist:add(2, b, ListB)))),
     ?_test(?assertEqual(c, plist:find(2, plist:add(2, c, ListC))))
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
     ?_test(?assert(key_val_list_p(plist:add(1, a, ListB, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:add(1, c, ListC, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:add(2, b, ListB, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:add(2, c, ListC, nocheck)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, [], nocheck)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListA, nocheck)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListB, nocheck)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListC, nocheck)))),
     ?_test(?assert(key_val_list(plist:add(3, c, [], check), 1))),
     ?_test(?assert(key_val_list(plist:add(3, c, ListA, check),
                                 length(ListA) + 1))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, [], check)))),
     ?_test(?assertEqual(c, plist:find(3, plist:add(3, c, ListA, check)))),
     ?_test(?assertError(badarg, plist:add(3, c, ListB, check))),
     ?_test(?assertError(badarg, plist:add(3, c, ListC, check))),
     ?_test(?assertError(badarg, plist:add(1, a, ListB, check))),
     ?_test(?assertError(badarg, plist:add(1, c, ListC, check))),
     ?_test(?assertError(badarg, plist:add(2, b, ListB, check))),
     ?_test(?assertError(badarg, plist:add(2, c, ListC, check))),
     ?_test(?assertError(badarg, plist:add(3, c, ListB, check))),
     ?_test(?assertError(badarg, plist:add(3, c, ListC, check)))
     ].

%%--------------------------------------------------------------------
%% delete/2
%%--------------------------------------------------------------------
delete_2_test_() ->
    ListA = plist:new([1, 2], [a, b]),
    ListB = plist:new([1, 2, 3], [a, b, c]),
    ListC = plist:new([1, 2, 3], [a, b, x]),
    [?_test(?assert(key_val_list_p(plist:delete(3, [])))),
     ?_test(?assert(key_val_list_p(plist:delete(3, ListA)))),
     ?_test(?assert(key_val_list_p(plist:delete(3, ListB)))),
     ?_test(?assert(key_val_list_p(plist:delete(3, ListC)))),
     ?_test(?assert(key_val_list_p(plist:delete(1, ListB)))),
     ?_test(?assert(key_val_list_p(plist:delete(2, ListB)))),
     ?_test(?assertEqual(undefined, plist:find(3, plist:delete(3, [])))),
     ?_test(?assertEqual(undefined, plist:find(3, plist:delete(3, ListA)))),
     ?_test(?assertEqual(undefined, plist:find(3, plist:delete(3, ListB)))),
     ?_test(?assertEqual(undefined, plist:find(3, plist:delete(3, ListC)))),
     ?_test(?assertEqual(undefined, plist:find(1, plist:delete(1, ListB)))),
     ?_test(?assertEqual(undefined, plist:find(2, plist:delete(2, ListB))))
     ].

%%--------------------------------------------------------------------
%% delete/3
%%--------------------------------------------------------------------
delete_3_test_() ->
    ListA = plist:new([1, 2], [a, b]),
    ListB = plist:new([1, 2, 3], [a, b, c]),
    ListC = plist:new([1, 2, 3], [a, b, x]),
    [?_test(?assert(key_val_list_p(plist:delete(3, [], nocheck)))),
     ?_test(?assert(key_val_list_p(plist:delete(3, ListA, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:delete(3, ListB, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:delete(3, ListC, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:delete(1, ListB, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:delete(2, ListB, nocheck)))),
     ?_test(
        ?assertEqual(undefined, plist:find(3, plist:delete(3, [], nocheck)))),
     ?_test(
        ?assertEqual(undefined,
                     plist:find(3, plist:delete(3, ListA, nocheck)))),
     ?_test(
        ?assertEqual(undefined,
                     plist:find(3, plist:delete(3, ListB, nocheck)))),
     ?_test(
        ?assertEqual(undefined,
                     plist:find(3, plist:delete(3, ListC, nocheck)))),
     ?_test(
        ?assertEqual(undefined,
                     plist:find(1, plist:delete(1, ListB, nocheck)))),
     ?_test(
        ?assertEqual(undefined,
                     plist:find(2, plist:delete(2, ListB, nocheck)))),
     ?_test(?assertError(badarg, plist:delete(3, [], check))),
     ?_test(?assertError(badarg, plist:delete(3, ListA, check))),
     ?_test(?assert(key_val_list_p(plist:delete(3, ListB, check)))),
     ?_test(?assert(key_val_list_p(plist:delete(3, ListC, check)))),
     ?_test(?assert(key_val_list_p(plist:delete(1, ListB, check)))),
     ?_test(?assert(key_val_list_p(plist:delete(2, ListB, check)))),
     ?_test(
        ?assertEqual(undefined, plist:find(3, plist:delete(3, ListB, check)))),
     ?_test(
        ?assertEqual(undefined, plist:find(3, plist:delete(3, ListC, check)))),
     ?_test(
        ?assertEqual(undefined, plist:find(1, plist:delete(1, ListB, check)))),
     ?_test(
        ?assertEqual(undefined, plist:find(2, plist:delete(2, ListB, check))))
     ].

%%--------------------------------------------------------------------
%% find/2
%%--------------------------------------------------------------------
find_2_test_() ->
    ListA = plist:new([1], [a]),
    ListB = plist:new([1, 2], [a, b]),
    ListC = plist:new([1, 2, 3], [a, b, c]),
    [?_test(?assertEqual(a, plist:find(1, ListA))),
     ?_test(?assertEqual(a, plist:find(1, ListB))),
     ?_test(?assertEqual(a, plist:find(1, ListC))),
     ?_test(?assertEqual(b, plist:find(2, ListB))),
     ?_test(?assertEqual(b, plist:find(2, ListC))),
     ?_test(?assertEqual(c, plist:find(3, ListC))),
     ?_test(?assertEqual(undefined, plist:find(4, []))),
     ?_test(?assertEqual(undefined, plist:find(4, ListA))),
     ?_test(?assertEqual(undefined, plist:find(4, ListB))),
     ?_test(?assertEqual(undefined, plist:find(4, ListC)))
    ].

%%--------------------------------------------------------------------
%% find/3
%%--------------------------------------------------------------------
find_3_test_() ->
    ListA = plist:new([1], [a]),
    ListB = plist:new([1, 2], [a, b]),
    ListC = plist:new([1, 2, 3], [a, b, c]),
    [?_test(?assertEqual(a, plist:find(1, ListA, test))),
     ?_test(?assertEqual(a, plist:find(1, ListB, test))),
     ?_test(?assertEqual(a, plist:find(1, ListC, test))),
     ?_test(?assertEqual(b, plist:find(2, ListB, test))),
     ?_test(?assertEqual(b, plist:find(2, ListC, test))),
     ?_test(?assertEqual(c, plist:find(3, ListC, test))),
     ?_test(?assertEqual(test, plist:find(4, [], test))),
     ?_test(?assertEqual(test, plist:find(4, ListA, test))),
     ?_test(?assertEqual(test, plist:find(4, ListB, test))),
     ?_test(?assertEqual(test, plist:find(4, ListC, test)))
    ].

%%--------------------------------------------------------------------
%% find/4
%%--------------------------------------------------------------------
find_4_test_() ->
    ListA = [{1, a}],
    ListB = [{1, a}, {2, b}],
    ListC = [{1, a}, {2, b}, {3, c}],
    ListD = [{1, a}, {1, b}, {1, c}],
    ListE = [{1, a}, {2, b}, {3, c}, {1, d}],
    ListF = [{1, a}, {2, b}, {1, c}, {3, d}, {1, e}],
    ListG = [{2, a}, {1, b}, {1, c}, {3, d}],
    [?_test(?assertEqual(a, plist:find(1, ListA, test, first))),
     ?_test(?assertEqual(a, plist:find(1, ListB, test, first))),
     ?_test(?assertEqual(a, plist:find(1, ListC, test, first))),
     ?_test(?assertEqual(b, plist:find(2, ListB, test, first))),
     ?_test(?assertEqual(b, plist:find(2, ListC, test, first))),
     ?_test(?assertEqual(c, plist:find(3, ListC, test, first))),
     ?_test(?assertEqual(test, plist:find(4, [], test, first))),
     ?_test(?assertEqual(test, plist:find(4, ListA, test, first))),
     ?_test(?assertEqual(test, plist:find(4, ListB, test, first))),
     ?_test(?assertEqual(test, plist:find(4, ListC, test, first))),
     ?_test(?assertEqual(a, plist:find(1, ListD, test, first))),
     ?_test(?assertEqual(a, plist:find(1, ListE, test, first))),
     ?_test(?assertEqual(a, plist:find(1, ListF, test, first))),
     ?_test(?assertEqual(b, plist:find(1, ListG, test, first))),
     ?_test(?assertEqual(test, plist:find(4, ListD, test, first))),
     ?_test(?assertEqual(test, plist:find(4, ListE, test, first))),
     ?_test(?assertEqual(test, plist:find(4, ListF, test, first))),
     ?_test(?assertEqual(test, plist:find(4, ListG, test, first))),
     ?_test(?assertEqual(a, plist:find(1, ListA, test, last))),
     ?_test(?assertEqual(a, plist:find(1, ListB, test, last))),
     ?_test(?assertEqual(a, plist:find(1, ListC, test, last))),
     ?_test(?assertEqual(b, plist:find(2, ListB, test, last))),
     ?_test(?assertEqual(b, plist:find(2, ListC, test, last))),
     ?_test(?assertEqual(c, plist:find(3, ListC, test, last))),
     ?_test(?assertEqual(test, plist:find(4, [], test, last))),
     ?_test(?assertEqual(test, plist:find(4, ListA, test, last))),
     ?_test(?assertEqual(test, plist:find(4, ListB, test, last))),
     ?_test(?assertEqual(test, plist:find(4, ListC, test, last))),
     ?_test(?assertEqual(c, plist:find(1, ListD, test, last))),
     ?_test(?assertEqual(d, plist:find(1, ListE, test, last))),
     ?_test(?assertEqual(e, plist:find(1, ListF, test, last))),
     ?_test(?assertEqual(c, plist:find(1, ListG, test, last))),
     ?_test(?assertEqual(test, plist:find(4, ListD, test, last))),
     ?_test(?assertEqual(test, plist:find(4, ListE, test, last))),
     ?_test(?assertEqual(test, plist:find(4, ListF, test, last))),
     ?_test(?assertEqual(test, plist:find(4, ListG, test, last)))
    ].

%%--------------------------------------------------------------------
%% find_all/2
%%--------------------------------------------------------------------
find_all_2_test_() ->
    ListA = [{1, a}],
    ListB = [{1, a}, {2, b}],
    ListC = [{1, a}, {2, b}, {3, c}],
    ListD = [{1, a}, {1, b}, {1, c}],
    ListE = [{1, a}, {2, b}, {3, c}, {1, d}],
    ListF = [{1, a}, {2, b}, {1, c}, {3, d}, {1, e}],
    ListG = [{2, a}, {1, b}, {1, c}, {3, d}],
    [?_test(?assertEqual([a], plist:find_all(1, ListA))),
     ?_test(?assertEqual([a], plist:find_all(1, ListB))),
     ?_test(?assertEqual([a], plist:find_all(1, ListC))),
     ?_test(?assertEqual([b], plist:find_all(2, ListB))),
     ?_test(?assertEqual([b], plist:find_all(2, ListC))),
     ?_test(?assertEqual([c], plist:find_all(3, ListC))),
     ?_test(?assertEqual([], plist:find_all(4, []))),
     ?_test(?assertEqual([], plist:find_all(4, ListA))),
     ?_test(?assertEqual([], plist:find_all(4, ListB))),
     ?_test(?assertEqual([], plist:find_all(4, ListC))),
     ?_test(?assertEqual([a, b, c], plist:find_all(1, ListD))),
     ?_test(?assertEqual([a, d], plist:find_all(1, ListE))),
     ?_test(?assertEqual([a, c, e], plist:find_all(1, ListF))),
     ?_test(?assertEqual([b, c], plist:find_all(1, ListG))),
     ?_test(?assertEqual([], plist:find_all(4, ListD))),
     ?_test(?assertEqual([], plist:find_all(4, ListE))),
     ?_test(?assertEqual([], plist:find_all(4, ListF))),
     ?_test(?assertEqual([], plist:find_all(4, ListG)))
     ].

%%--------------------------------------------------------------------
%% keys/1
%%--------------------------------------------------------------------
keys_1_test_() ->
    ListA = [{1, a}],
    ListB = [{1, a}, {2, b}],
    ListC = [{1, a}, {2, b}, {3, c}],
    ListD = [{1, a}, {1, b}, {1, c}],
    ListE = [{1, a}, {2, b}, {3, c}, {1, d}],
    ListF = [{1, a}, {2, b}, {1, c}, {3, d}, {1, e}],
    ListG = [{2, a}, {1, b}, {1, c}, {3, d}],
    [?_test(?assertEqual([], plist:keys([]))),
     ?_test(?assertEqual([1], plist:keys(ListA))),
     ?_test(?assertEqual([1, 2], plist:keys(ListB))),
     ?_test(?assertEqual([1, 2, 3], plist:keys(ListC))),
     ?_test(?assertEqual([1], plist:keys(ListD))),
     ?_test(?assertEqual([1, 2, 3], plist:keys(ListE))),
     ?_test(?assertEqual([1, 2, 3], plist:keys(ListF))),
     ?_test(?assertEqual([1, 2, 3], plist:keys(ListG)))
     ].

%%--------------------------------------------------------------------
%% values/1
%%--------------------------------------------------------------------
values_1_test_() ->
    ListA = [{1, a}],
    ListB = [{1, a}, {2, b}],
    ListC = [{1, a}, {2, b}, {3, c}],
    ListD = [{1, a}, {1, b}, {1, c}],
    ListE = [{1, a}, {2, b}, {3, c}, {1, d}],
    ListF = [{1, a}, {2, b}, {1, c}, {3, d}, {1, e}],
    ListG = [{2, a}, {1, b}, {1, c}, {3, d}],
    [?_test(?assertEqual([], plist:values([]))),
     ?_test(?assertEqual([a], plist:values(ListA))),
     ?_test(?assertEqual([a, b], plist:values(ListB))),
     ?_test(?assertEqual([a, b, c], plist:values(ListC))),
     ?_test(?assertEqual([a, b, c], plist:values(ListD))),
     ?_test(?assertEqual([a, b, c, d], plist:values(ListE))),
     ?_test(?assertEqual([a, b, c, d, e], plist:values(ListF))),
     ?_test(?assertEqual([a, b, c, d], plist:values(ListG)))
     ].

%%--------------------------------------------------------------------
%% member/1
%%--------------------------------------------------------------------
member_2_test_() ->
    ListA = [{1, a}],
    ListB = [{1, a}, {2, b}],
    ListC = [{1, a}, {2, b}, {3, c}],
    ListD = [{1, a}, {1, b}, {1, c}],
    ListE = [{1, a}, {2, b}, {3, c}, {1, d}],
    ListF = [{1, a}, {2, b}, {1, c}, {3, d}, {1, e}],
    ListG = [{2, a}, {1, b}, {1, c}, {3, d}],
    [?_test(
        ?assertEqual(false,
                     lists:any(fun(X) -> plist:member(X, []) end, [1, 2, 3]))),
     ?_test(
        ?assertEqual(true,
                     lists:all(fun(X) -> plist:member(X, ListA) end, [1]))),
     ?_test(
        ?assertEqual(false,
                     lists:any(fun(X) -> plist:member(X, ListA) end, [2, 3]))),
     ?_test(
        ?assertEqual(true,
                     lists:all(fun(X) -> plist:member(X, ListB) end, [1, 2]))),
     ?_test(
        ?assertEqual(false,
                     lists:any(fun(X) -> plist:member(X, ListB) end, [3]))),
     ?_test(
        ?assertEqual(true,
                     lists:all(fun(X) -> plist:member(X, ListC) end,
                               [1, 2, 3]))),
     ?_test(
        ?assertEqual(false,
                     lists:any(fun(X) -> plist:member(X, ListC) end, [4]))),
     ?_test(
        ?assertEqual(true,
                     lists:all(fun(X) -> plist:member(X, ListD) end,
                               [1]))),
     ?_test(
        ?assertEqual(false,
                     lists:any(fun(X) -> plist:member(X, ListD) end, [2, 3]))),

     ?_test(
        ?assertEqual(true,
                     lists:all(fun(X) -> plist:member(X, ListE) end,
                               [1, 2, 3]))),
     ?_test(
        ?assertEqual(false,
                     lists:any(fun(X) -> plist:member(X, ListE) end, [4]))),

     ?_test(
        ?assertEqual(true,
                     lists:all(fun(X) -> plist:member(X, ListF) end,
                               [1, 2, 3]))),
     ?_test(
        ?assertEqual(false,
                     lists:any(fun(X) -> plist:member(X, ListF) end, [4]))),

     ?_test(
        ?assertEqual(true,
                     lists:all(fun(X) -> plist:member(X, ListG) end,
                               [1, 2, 3]))),
     ?_test(
        ?assertEqual(false,
                     lists:any(fun(X) -> plist:member(X, ListG) end, [4])))

     ].

%%--------------------------------------------------------------------
%% replace/3
%%--------------------------------------------------------------------
replace_3_test_() ->
    ListA = plist:new([1, 2], [a, b]),
    ListB = plist:new([1, 2, 3], [a, b, c]),
    ListC = plist:new([1, 2, 3], [a, b, x]),
    [?_test(?assert(key_val_list_p(plist:replace(3, c, [])))),
     ?_test(?assert(key_val_list_p(plist:replace(3, c, ListA)))),
     ?_test(?assert(key_val_list_p(plist:replace(3, c, ListB)))),
     ?_test(?assert(key_val_list_p(plist:replace(3, c, ListC)))),
     ?_test(?assert(key_val_list_p(plist:replace(1, a, ListB)))),
     ?_test(?assert(key_val_list_p(plist:replace(1, c, ListC)))),
     ?_test(?assert(key_val_list_p(plist:replace(2, b, ListB)))),
     ?_test(?assert(key_val_list_p(plist:replace(2, c, ListC)))),
     ?_test(?assertEqual(c, plist:find(3, plist:replace(3, c, [])))),
     ?_test(?assertEqual(c, plist:find(3, plist:replace(3, c, ListA)))),
     ?_test(?assertEqual(c, plist:find(3, plist:replace(3, c, ListB)))),
     ?_test(?assertEqual(c, plist:find(3, plist:replace(3, c, ListC)))),
     ?_test(?assertEqual(a, plist:find(1, plist:replace(1, a, ListB)))),
     ?_test(?assertEqual(c, plist:find(1, plist:replace(1, c, ListC)))),
     ?_test(?assertEqual(b, plist:find(2, plist:replace(2, b, ListB)))),
     ?_test(?assertEqual(c, plist:find(2, plist:replace(2, c, ListC))))
     ].

%%--------------------------------------------------------------------
%% replace/4
%%--------------------------------------------------------------------
replace_4_test_() ->
    ListA = plist:new([1, 2], [a, b]),
    ListB = plist:new([1, 2, 3], [a, b, c]),
    ListC = plist:new([1, 2, 3], [a, b, x]),
    [?_test(?assert(key_val_list_p(plist:replace(3, c, [], nocheck)))),
     ?_test(?assert(key_val_list_p(plist:replace(3, c, ListA, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:replace(3, c, ListB, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:replace(3, c, ListC, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:replace(1, a, ListB, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:replace(1, c, ListC, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:replace(2, b, ListB, nocheck)))),
     ?_test(?assert(key_val_list_p(plist:replace(2, c, ListC, nocheck)))),
     ?_test(?assertEqual(c, plist:find(3, plist:replace(3, c, [], nocheck)))),
     ?_test(
        ?assertEqual(c, plist:find(3, plist:replace(3, c, ListA, nocheck)))),
     ?_test(
        ?assertEqual(c, plist:find(3, plist:replace(3, c, ListB, nocheck)))),
     ?_test(
        ?assertEqual(c, plist:find(3, plist:replace(3, c, ListC, nocheck)))),
     ?_test(?assertError(badarg, plist:replace(3, c, [], check))),
     ?_test(?assertError(badarg, plist:replace(3, c, ListA, check))),
     ?_test(?assertError(badarg, plist:replace(3, c, [], check))),
     ?_test(?assertError(badarg, plist:replace(3, c, ListA, check))),
     ?_test(?assert(key_val_list_p(plist:replace(3, c, ListB, check)))),
     ?_test(?assert(key_val_list_p(plist:replace(3, c, ListC, check)))),
     ?_test(?assert(key_val_list_p(plist:replace(1, a, ListB, check)))),
     ?_test(?assert(key_val_list_p(plist:replace(1, c, ListC, check)))),
     ?_test(?assert(key_val_list_p(plist:replace(2, b, ListB, check)))),
     ?_test(?assert(key_val_list_p(plist:replace(2, c, ListC, check)))),
     ?_test(?assert(key_val_list_p(plist:replace(3, c, ListB, check)))),
     ?_test(?assert(key_val_list_p(plist:replace(3, c, ListC, check)))),
     ?_test(?assertEqual(c, plist:find(3, plist:replace(3, c, ListB, check)))),
     ?_test(?assertEqual(c, plist:find(3, plist:replace(3, c, ListC, check)))),
     ?_test(?assertEqual(a, plist:find(1, plist:replace(1, a, ListB, check)))),
     ?_test(?assertEqual(c, plist:find(1, plist:replace(1, c, ListC, check)))),
     ?_test(?assertEqual(b, plist:find(2, plist:replace(2, b, ListB, check)))),
     ?_test(?assertEqual(c, plist:find(2, plist:replace(2, c, ListC, check)))),
     ?_test(?assertEqual(c, plist:find(3, plist:replace(3, c, ListB, check)))),
     ?_test(?assertEqual(c, plist:find(3, plist:replace(3, c, ListC, check))))
     ].

%%--------------------------------------------------------------------
%% compact/1
%%--------------------------------------------------------------------
compact_1_test_() ->
    ListA = [{1, a}],
    ListB = [{1, a}, {2, b}],
    ListC = [{1, a}, {2, b}, {3, c}],
    ListD = [{1, a}, {1, b}, {1, c}],
    ListE = [{1, a}, {2, b}, {3, c}, {1, d}],
    ListF = [{1, a}, {2, b}, {1, c}, {3, d}, {1, e}],
    ListG = [{2, a}, {1, b}, {1, c}, {3, d}],
    [?_test(?assertEqual([], plist:compact([]))),
     ?_test(?assertEqual(ListA, plist:compact(ListA))),
     ?_test(?assertEqual(ListB, plist:compact(ListB))),
     ?_test(?assertEqual(ListC, plist:compact(ListC))),
     ?_test(?assertEqual(ListA, plist:compact(ListD))),
     ?_test(?assertEqual(ListC, plist:compact(ListE))),
     ?_test(?assertEqual([{1, a}, {2, b}, {3, d}], plist:compact(ListF))),
     ?_test(?assertEqual([{1, b}, {2, a}, {3, d}], plist:compact(ListG)))
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
