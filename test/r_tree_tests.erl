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
%%%   eunit unit tests for the r_tree library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(r_tree_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(SHORT,
        [{{3755474944,3755737087},"JP"},
         {{3755737088,3755868159},"CN"},
         {{3755868160,3755933695},"KR"},
         {{3755933696,3755966463},"JP"},
         {{3755966464,3755974655},"IN"},
         {{3755974656,3755976703},"JP"},
         {{3755976704,3755978751},"KH"},
         {{3755978752,3755986943},"CN"},
         {{3755986944,3755988991},"JP"},
         {{3755988992,3755990015},"HK"},
         {{3755990016,3755991039},"SG"},
         {{3755991040,3755999231},"JP"},
         {{3755999232,3756002815},"IN"},
         {{3756002816,3756003071},"LK"},
         {{3756003072,3757047807},"IN"},
         {{3757047808,3757834239},"CN"},
         {{3757834240,3757867007},"AU"},
         {{3757867008,3757875519},"CN"},
         {{3757875520,3757875583},"HK"},
         {{3757875584,3757880063},"CN"},
         {{3757880064,3757883391},"HK"},
         {{3757883392,3757899775},"CN"},
         {{3757899776,3757965311},"KR"},
         {{3757965312,3758063615},"CN"},
         {{3758063616,3758079999},"HK"},
         {{3758080000,3758088191},"KR"},
         {{3758088192,3758090239},"ID"},
         {{3758090240,3758091263},"AU"},
         {{3758091264,3758092287},"CN"},
         {{3758092288,3758093311},"HK"},
         {{3758093312,3758094335},"IN"},
         {{3758095360,3758095871},"CN"},
         {{3758095872,3758096127},"SG"},
         {{3758096128,3758096383},"AU"}]).

-define(SHORT_RANGES, [Range || {Range, _} <- ?SHORT]).
-define(SHORT_KEYS, [Low || {{Low, _}, _} <- ?SHORT]).
-define(SHORT_VALUES, [Value || {_, Value} <- ?SHORT]).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% new/0
%%--------------------------------------------------------------------
new_0_test_() ->
    [{"Create", [?_test(?assert(r_tree:is_r_tree(r_tree:new())))]}
    ].

%%--------------------------------------------------------------------
%% is_r_tree/1
%%--------------------------------------------------------------------
is_r_tree_1_test_() ->
    [{"Empty", [?_test(?assertEqual(true, r_tree:is_r_tree(r_tree:new())))]},
     {"Not", [?_test(?assertEqual(false, r_tree:is_r_tree({foo, bar})))]}
    ].

%%--------------------------------------------------------------------
%% is_empty/1
%%--------------------------------------------------------------------
is_empty_1_test_() ->
    [{"Empty", [?_test(?assertEqual(true, r_tree:is_empty(r_tree:new())))]},
     {"Not",
      [?_test(
          ?assertEqual(false,
                       r_tree:is_empty(r_tree:add({1, 5},
                                                  "US",
                                                  r_tree:new()))))]}
    ].

%%--------------------------------------------------------------------
%% add/3
%%--------------------------------------------------------------------
add_3_test_() ->
    [{"Add",
      [?_test(
          ?assert(
             r_tree:is_r_tree(
               lists:foldl(fun({R, V}, Acc) -> r_tree:add(R, V, Acc) end,
                           r_tree:new(),
                           ?SHORT))))]},
     {"Add/Delete",
      [?_test(
          ?assert(
             r_tree:is_empty(
               lists:foldl(fun(R, Acc) -> r_tree:delete(R, Acc) end,
                           lists:foldl(
                             fun({R, V}, Acc) -> r_tree:add(R, V, Acc) end,
                             r_tree:new(),
                             ?SHORT),
                           ?SHORT_RANGES))))]},
     {"Add/Delete reverse",
      [?_test(
          ?assert(
             r_tree:is_empty(
               lists:foldl(fun(R, Acc) -> r_tree:delete(R, Acc) end,
                           lists:foldl(
                             fun({R, V}, Acc) -> r_tree:add(R, V, Acc) end,
                             r_tree:new(),
                             ?SHORT),
                           lists:reverse(?SHORT_RANGES)))))]},
     {"Add and check the ranges",
      [?_test(
          ?assertEqual(
             ?SHORT_RANGES,
             r_tree:ranges(
               lists:foldl(fun({R, V}, Acc) -> r_tree:add(R, V, Acc) end,
                           r_tree:new(),
                           ?SHORT))))
      ]},
     {"Add reverse and check the ranges",
      [?_test(
          ?assertEqual(
             ?SHORT_RANGES,
             r_tree:ranges(
               lists:foldl(fun({R, V}, Acc) -> r_tree:add(R, V, Acc) end,
                           r_tree:new(),
                           lists:reverse(?SHORT)))))
      ]},
     {"Add and check the values",
      [?_test(
          ?assertEqual(
             ?SHORT_VALUES,
             r_tree:values(
               lists:foldl(fun({R, V}, Acc) -> r_tree:add(R, V, Acc) end,
                           r_tree:new(),
                           ?SHORT))))]}
    ].

%%--------------------------------------------------------------------
%% add/4
%%--------------------------------------------------------------------
add_4_test_() ->
    [{"Add and read no check fail",
      [?_test(begin
                  Tree = lists:foldl(fun({R, V}, Acc) ->
                                             r_tree:add(R, V, Acc, check)
                                     end,
                                     r_tree:new(),
                                     ?SHORT),
                  [?assert(r_tree:is_r_tree(r_tree:add(R, V, Tree, nocheck))) ||
                      {R, V} <- ?SHORT]
              end)]},
     {"Add check fail",
      [?_test(begin
                  Tree = lists:foldl(fun({R, V}, Acc) ->
                                             r_tree:add(R, V, Acc, check)
                                     end,
                                     r_tree:new(),
                                     ?SHORT),
                  [?assertError(_, r_tree:add(R, V, Tree, check)) ||
                      {R, V} <- ?SHORT]
              end)]}
    ].

%%--------------------------------------------------------------------
%% adds/2
%%--------------------------------------------------------------------
adds_2_test_() ->
    load_plist(),
    Where = get(where),
    Ranges = get(ranges),
    [{"Add",
      ?_test(?assert(r_tree:is_r_tree(r_tree:adds(Where, r_tree:new()))))},
     {"Add/Delete",
      ?_test(
          ?assert(
             r_tree:is_empty(
               r_tree:deletes(lists:reverse(Ranges),
                              r_tree:adds(Where, r_tree:new()),
                              check))))
     },
     {"Add and check the keys",
      ?_test(
          ?assertEqual(Ranges,
                       r_tree:ranges(r_tree:adds(Where, r_tree:new()))))}
    ].

%%--------------------------------------------------------------------
%% adds/3
%%--------------------------------------------------------------------
adds_3_test_() ->
    load_plist(),
    Where = get(where),
    [{"Add with check success",
      ?_test(
         ?assert(r_tree:is_r_tree(r_tree:adds(Where, r_tree:new(), check))))
     },
     {"Add with check fail",
      ?_test(
         begin
             Tree = r_tree:from_list(Where),
             [?assertError(badarg,
                           r_tree:adds([{{a, b}, b}, {R, d}, {{e, f}, f}],
                                       Tree,
                                       check)) ||
                 R <- ?SHORT_RANGES]
         end)}
    ].

%%--------------------------------------------------------------------
%% delete/3
%%--------------------------------------------------------------------
delete_3_test_() ->
    [{"Delete empty nocheck",
      ?_test(
         ?assert(r_tree:is_empty(r_tree:delete({1, 2},r_tree:new(),nocheck))))},
     {"Delete empty nocheck",
      ?_test(
         ?assert(
            r_tree:is_r_tree(
              r_tree:delete({3, 4},
                            r_tree:from_list([{{1, 2}, a}, {{7, 8}, b}]),
                            nocheck))))},
     {"Delete empty check",
      ?_test(?assertError(badarg, r_tree:delete({1, 2}, r_tree:new(), check)))},
     {"Delete empty check",
      ?_test(
         ?assertError(badarg,
                      r_tree:delete({3, 4},
                                    r_tree:from_list([{{1, 2}, a}, {{6,7}, b}]),
                                    check)))}
    ].

%%--------------------------------------------------------------------
%% member/2
%%--------------------------------------------------------------------
member_2_test_() ->
    load_plist(),
    Ranges = get(ranges),
    Tree = r_tree:from_list(get(where)),
    [{"Member with check success",
      ?_test(?assert(lists:all(fun(Y) -> r_tree:member(Y, Tree) end, Ranges)))},
     {"Member without check success",
      ?_test(?assert(not lists:any(fun(Y) -> r_tree:member(Y, Tree) end,
                                   [{a, b}, {0, 1}, {true, false}])))}
    ].

%%--------------------------------------------------------------------
%% find/2
%%--------------------------------------------------------------------
find_2_test_() ->
    load_plist(),
    Tree = r_tree:from_list(get(where)),
    [{"Find success",
      [?_test(?assertEqual(V, r_tree:find(R, Tree))) ||
          {R, V} <- lists:zip(?SHORT_KEYS, ?SHORT_VALUES)]},
     {"Find failure",
       [?_test(?assertEqual(undefined, r_tree:find([$a + X], Tree))) ||
           X <- lists:seq(1, 20)]}
    ].

%%--------------------------------------------------------------------
%% find/3
%%--------------------------------------------------------------------
find_3_test_() ->
    load_plist(),
    Tree = r_tree:from_list(get(where)),
    [{"Find success",
      [?_test(?assertEqual(V, r_tree:find(R, Tree, none))) ||
          {R, V} <- lists:zip(?SHORT_KEYS, ?SHORT_VALUES)]},
     {"Find failure",
      [?_test(?assertEqual(none, r_tree:find([$a + X], Tree, none))) ||
          X <- lists:seq(1, 20)]}
    ].

%%--------------------------------------------------------------------
%% replace/3
%%--------------------------------------------------------------------
replace_3_test_() ->
    load_plist(),
    Tree = r_tree:from_list(get(where)),
    [{"Replace existing",
      [?_test(?assertEqual(I,
                           r_tree:find(I, r_tree:replace(R, I, Tree)))) ||
          {I, R} <- lists:zip(?SHORT_KEYS, ?SHORT_RANGES)]},
     {"Replace not existing",
      [?_test(
          ?assertEqual(I1,
                       r_tree:find({I1 + 1000, I2 + 1000},
                                   r_tree:replace({I1 + 1000, I2 + 1000},
                                                  I1,
                                                  Tree)))) ||
          {I1, I2} <- ?SHORT_KEYS]}
    ].

%%--------------------------------------------------------------------
%% replace/4
%%--------------------------------------------------------------------
replace_4_test_() ->
    load_plist(),
    Tree = r_tree:from_list(get(where)),
    [{"Replace existing",
      [?_test(?assertEqual(I,
                           r_tree:find(I, r_tree:replace(R, I, Tree,check)))) ||
          {I, R} <- lists:zip(?SHORT_KEYS, ?SHORT_RANGES)]},
     {"Replace not existing",
      [?_test(
          ?assertError(badarg,
                       r_tree:replace({I1 + 1000,I2 + 1000},I1,Tree,check))) ||
          {I1, I2} <- ?SHORT_KEYS]}
    ].

%% ===================================================================
%% Internal functions.
%% ===================================================================

dir() -> code:lib_dir(jhn_stdlib, test).

file(File) -> filename:join([dir(), File]).

plist(File) -> {ok, L} = file:consult(file(File)), L.

load_plist() ->
    case get(where) of
        undefined ->
            Where = lists:sort(plist("where.txt")),
            put(where, Where),
            put(ranges, [R || {R, _} <- Where]),
            put(values, [V || {_, V} <- Where]);
        _ ->
            ok
    end.
