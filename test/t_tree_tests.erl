%%==============================================================================
%% Copyright 2013-2016 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the t_tree library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2016, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(t_tree_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Seuqences

-define(STRAIGHT, lists:seq(1, 100)).
-define(ODD, [X || X <- ?STRAIGHT, X rem 2 == 1]).
-define(EVEN, [X || X <- ?STRAIGHT, X rem 2 == 0]).

-define(MIX1, ?ODD ++ ?EVEN).
-define(MIX2, ?ODD ++ lists:reverse(?EVEN)).
-define(MIX3, lists:reverse(?ODD) ++ ?EVEN).
-define(MIX4, lists:reverse(?ODD) ++ lists:reverse(?EVEN)).

-define(SEUQENCES, [?STRAIGHT, ?MIX1, ?MIX2, ?MIX3, ?MIX4]).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% new/0
%%--------------------------------------------------------------------
new_0_test_() ->
    [{"Create", [?_test(?assert(t_tree:is_t_tree(t_tree:new())))]}
    ].

%%--------------------------------------------------------------------
%% new/1
%%--------------------------------------------------------------------
new_1_test_() ->
    [{"Empty", [?_test(?assert(t_tree:is_t_tree(t_tree:new([]))))]},
     {"Min", [?_test(?assert(t_tree:is_t_tree(t_tree:new([{min, 6}]))))]},
     {"Max", [?_test(?assert(t_tree:is_t_tree(t_tree:new([{max, 12}]))))]},
     {"Min Max",
      [?_test(?assert(t_tree:is_t_tree(t_tree:new([{min, 10}, {max, 11}]))))]},
     {"Broken",
      [?_test(?assertError(badarg, t_tree:new([{min, 5}, {max, 5}]))),
       ?_test(?assertError(badarg, t_tree:new([{max, 5}]))),
       ?_test(?assertError(badarg, t_tree:new([{min, 12}])))
      ]}
    ].

%%--------------------------------------------------------------------
%% is_t_tree/1
%%--------------------------------------------------------------------
is_t_tree_1_test_() ->
    [{"Empty", [?_test(?assertEqual(true, t_tree:is_t_tree(t_tree:new([]))))]},
     {"Not", [?_test(?assertEqual(false, t_tree:is_t_tree({foo, bar})))]}
    ].

%%--------------------------------------------------------------------
%% is_empty/1
%%--------------------------------------------------------------------
is_empty_1_test_() ->
    [{"Empty", [?_test(?assertEqual(true, t_tree:is_empty(t_tree:new([]))))]},
     {"Not",
      [?_test(
          ?assertEqual(false,
                       t_tree:is_empty(t_tree:add(1, 1, t_tree:new([])))))]}
    ].

%%--------------------------------------------------------------------
%% add/3
%%--------------------------------------------------------------------
add_3_test_() ->
    [{"Add",
      [?_test(
          ?assert(
             t_tree:is_t_tree(
               lists:foldl(fun(X, Acc) ->
                                   t_tree:add(X, integer_to_list(X), Acc)
                           end,
                           t_tree:new(),
                           Seq)))) || Seq <- ?SEUQENCES]},
     {"Add/Delete",
      [?_test(
          ?assert(
             t_tree:is_empty(
               lists:foldl(fun(X, Acc) ->
                                   t_tree:delete(X, Acc)
                           end,
                           lists:foldl(
                             fun(X, Acc) ->
                                     t_tree:add(X, integer_to_list(X), Acc)
                             end,
                             t_tree:new(),
                             Seq),
                           Seq)))) || Seq <- ?SEUQENCES]},
     {"Add and check the indices",
      [?_test(
          ?assertEqual(
             lists:sort(Seq),
             t_tree:indices(
               lists:foldl(fun(X, Acc) ->
                                   t_tree:add(X, integer_to_list(X), Acc)
                           end,
                           t_tree:new(),
                           Seq)))) || Seq <- ?SEUQENCES]},
     {"Add and check the values",
      [?_test(
          ?assertEqual(
             [integer_to_list(X) || X <- lists:sort(Seq)],
             t_tree:values(
               lists:foldl(fun(X, Acc) ->
                                   t_tree:add(X, integer_to_list(X), Acc)
                           end,
                           t_tree:new(),
                           Seq)))) || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% add/4
%%--------------------------------------------------------------------
add_4_test_() ->
    [{"Add and readd no check fail",
      [?_test(
          begin
              Tree = lists:foldl(fun(X, Acc) ->
                                         t_tree:add(X,
                                                    integer_to_list(X),
                                                    Acc,
                                                    check)
                                 end,
                                 t_tree:new(),
                                 Seq),
              [?assert(t_tree:is_t_tree(t_tree:add(X, 1, Tree, nocheck))) ||
                  X <- Seq]
          end) || Seq <- ?SEUQENCES]},
     {"Add check success",
      [?_test(
          ?assert(
             t_tree:is_t_tree(
               lists:foldl(fun(X, Acc) ->
                                   t_tree:add(X, integer_to_list(X), Acc, check)
                           end,
                           t_tree:new(),
                           Seq)))) || Seq <- ?SEUQENCES]},
     {"Add check fail",
      [?_test(
          begin
              Tree = lists:foldl(fun(X, Acc) ->
                                         t_tree:add(X,
                                                    integer_to_list(X),
                                                    Acc,
                                                    check)
                                 end,
                                 t_tree:new(),
                                 Seq),
              [?assertError(badarg, t_tree:add(X, 1, Tree, check)) || X <- Seq]
          end) || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% adds/2
%%--------------------------------------------------------------------
adds_2_test_() ->
    [{"Add",
      [?_test(
          ?assert(
             t_tree:is_t_tree(
               t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                           t_tree:new()))))
             || Seq <- ?SEUQENCES]},
     {"Add/Delete",
      [?_test(
          ?assert(
             t_tree:is_empty(
               t_tree:deletes([X || X <- Seq],
                              t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                          t_tree:new())))))
             || Seq <- ?SEUQENCES]},
     {"Add and check the indices",
      [?_test(
          ?assertEqual(
             lists:sort(Seq),
             t_tree:indices(
               t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                           t_tree:new()))))
             || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% adds/3
%%--------------------------------------------------------------------
adds_3_test_() ->
    [{"Add with check success",
      [?_test(
          ?assert(
             t_tree:is_t_tree(
               t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                           t_tree:new(),
                           check))))
             || Seq <- ?SEUQENCES]},
     {"Add with check fail",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertError(badarg,
                            t_tree:adds([{a, b}, {X, d}, {e, f}], Tree, check))
               || X <- Seq]
          end
         ) || Seq <- ?SEUQENCES]}
     ].

%%--------------------------------------------------------------------
%% delete/3
%%--------------------------------------------------------------------
delete_3_test_() ->
    [{"Delete empty nocheck",
      ?_test(
         ?assert(t_tree:is_empty(t_tree:delete(1, t_tree:new(), nocheck))))},
     {"Delete empty nocheck",
      ?_test(
         ?assert(
            t_tree:is_t_tree(
              t_tree:delete(3,
                            t_tree:adds([{1, a}, {5, b}],
                                        t_tree:new()),
                            nocheck))))},
     {"Delete empty check",
      ?_test(?assertError(badarg, t_tree:delete(1, t_tree:new(), check)))},
     {"Delete empty check",
      ?_test(
         ?assertError(badarg,
                      t_tree:delete(3,
                                    t_tree:adds([{1, a}, {5, b}],
                                                t_tree:new()),
                                    check)))}
    ].

%%--------------------------------------------------------------------
%% member/2
%%--------------------------------------------------------------------
member_2_test_() ->
    [{"Member with check success",
      [?_test(
          ?assert(
            lists:all(
              fun(Y) ->
                      t_tree:member(
                        Y,
                        t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                    t_tree:new(),
                                    check))
              end,
              Seq)))
       || Seq <- ?SEUQENCES]},
     {"Member without check success",
      [?_test(
          ?assert(
             not lists:any(
                   fun(Y) ->
                           t_tree:member(
                             Y,
                             t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                         t_tree:new(),
                                         check))
                   end,
                   [-1, 50.5, 1000])))
       || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% find/2
%%--------------------------------------------------------------------
find_2_test_() ->
    [{"Find success",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(integer_to_list(X), t_tree:find(X, Tree)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]},
     {"Find failure",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(undefined, t_tree:find(X - 0.5, Tree)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% find/3
%%--------------------------------------------------------------------
find_3_test_() ->
    [{"Find success",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(integer_to_list(X), t_tree:find(X, Tree, none)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]},
     {"Find failure",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(none, t_tree:find(X - 0.5, Tree, none)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% least_upper_bound/2
%%--------------------------------------------------------------------
least_upper_bound_2_test_() ->
    [{"Least_Upper_Bound success",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(integer_to_list(X),
                            t_tree:least_upper_bound(X, Tree)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]},
     {"Least_Upper_Bound success",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(integer_to_list(X),
                            t_tree:least_upper_bound(X - 0.5, Tree)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]},
     {"Least_Upper_Bound failure",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(undefined, t_tree:least_upper_bound(X, Tree)) ||
                  X <- [1000, 100000]]
          end)
       || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% least_upper_bound/3
%%--------------------------------------------------------------------
least_upper_bound_3_test_() ->
    [{"Least_Upper_Bound success",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(integer_to_list(X),
                            t_tree:least_upper_bound(X, Tree, none)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]},
     {"Least_Upper_Bound success",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(integer_to_list(X),
                            t_tree:least_upper_bound(X - 0.5, Tree, none)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]},
     {"Least_Upper_Bound failure",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(none, t_tree:least_upper_bound(X, Tree, none)) ||
                  X <- [1000, 100000]]
          end)
       || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% greatest_lower_bound/2
%%--------------------------------------------------------------------
greatest_lower_bound_2_test_() ->
    [
     {"Greatest_Lower_Bound success",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(integer_to_list(X),
                            t_tree:greatest_lower_bound(X, Tree)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]},
     {"Greatest_Lower_Bound success",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(integer_to_list(X),
                            t_tree:greatest_lower_bound(X + 0.5, Tree)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]},
     {"Greatest_Lower_Bound failure",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(undefined, t_tree:greatest_lower_bound(X, Tree)) ||
                  X <- [-1, -100]]
          end)
       || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% greatest_lower_bound/3
%%--------------------------------------------------------------------
greatest_lower_bound_3_test_() ->
    [
     {"Greatest_Lower_Bound success",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(integer_to_list(X),
                            t_tree:greatest_lower_bound(X, Tree, none)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]},
     {"Greatest_Lower_Bound success",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(integer_to_list(X),
                            t_tree:greatest_lower_bound(X + 0.5, Tree, none)) ||
                  X <- Seq]
          end)
       || Seq <- ?SEUQENCES]},
     {"Greatest_Lower_Bound failure",
      [?_test(
          begin
              Tree = t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                 t_tree:new()),
              [?assertEqual(none, t_tree:greatest_lower_bound(X, Tree, none)) ||
                  X <- [-1, -100]]
          end)
       || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% first/1
%%--------------------------------------------------------------------
first_1_test_() ->
    [{"first sucess",
      [?_test(
          ?assertEqual("1",
                       t_tree:first(
                         t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                     t_tree:new())))) ||
          Seq <- ?SEUQENCES
      ]},
     {"First fail",
      [?_test(?assertEqual(undefined, t_tree:first(t_tree:new())))]}
    ].


%%--------------------------------------------------------------------
%% first/2
%%--------------------------------------------------------------------
first_2_test_() ->
    [{"first sucess",
      [?_test(
          ?assertEqual("1",
                       t_tree:first(
                         t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                     t_tree:new()),
                         none)))||
          Seq <- ?SEUQENCES
      ]},
     {"First fail",
      [?_test(?assertEqual(none, t_tree:first(t_tree:new(), none)))]}
    ].

%%--------------------------------------------------------------------
%% last/1
%%--------------------------------------------------------------------
last_1_test_() ->
    [{"last sucess",
      [?_test(
          ?assertEqual("100",
                       t_tree:last(
                         t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                     t_tree:new())))) ||
          Seq <- ?SEUQENCES
      ]},
     {"Last fail",
      [?_test(?assertEqual(undefined, t_tree:last(t_tree:new())))]}
    ].

%%--------------------------------------------------------------------
%% last/2
%%--------------------------------------------------------------------
last_2_test_() ->
    [{"last sucess",
      [?_test(
          ?assertEqual("100",
                       t_tree:last(
                         t_tree:adds([{X, integer_to_list(X)} || X <- Seq],
                                     t_tree:new()),
                         none)))||
          Seq <- ?SEUQENCES
      ]},
     {"Last fail",
      [?_test(?assertEqual(none, t_tree:last(t_tree:new(), none)))]}
    ].

%%--------------------------------------------------------------------
%% replace/3
%%--------------------------------------------------------------------
replace_3_test_() ->
    [{"Replace existing",
      [?_test([?assertEqual(
                  X + 1,
                  t_tree:find(X,
                              t_tree:replace(
                                X,
                                X + 1,
                                t_tree:adds([{Y, integer_to_list(Y)}
                                             || Y <- Seq],
                                            t_tree:new()))))
               || X <- Seq])
       || Seq <- ?SEUQENCES]},
     {"Replace not existing",
      [?_test([?assertEqual(
                  X,
                  t_tree:find(X + 1000,
                              t_tree:replace(
                                X + 1000,
                                X,
                                t_tree:adds([{Y, integer_to_list(Y)}
                                             || Y <- Seq],
                                            t_tree:new()))))
               || X <- Seq])
       || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% replace/4
%%--------------------------------------------------------------------
replace_4_test_() ->
    [{"Replace existing sucess",
      [?_test([?assertEqual(
                  X + 1,
                  t_tree:find(X,
                              t_tree:replace(
                                X,
                                X + 1,
                                t_tree:adds([{Y, integer_to_list(Y)}
                                             || Y <- Seq],
                                            t_tree:new()),
                                check)))
               || X <- Seq])
       || Seq <- ?SEUQENCES]},
     {"Replace not existing fail",
      [?_test([?assertError(badarg,
                              t_tree:replace(
                                X + 1000,
                                X,
                                t_tree:adds([{Y, integer_to_list(Y)}
                                             || Y <- Seq],
                                            t_tree:new()),
                                check))
               || X <- Seq])
       || Seq <- ?SEUQENCES]},
     {"Replace not existing fail inner",
      [?_test([?assertError(badarg,
                              t_tree:replace(
                                X,
                                X,
                                t_tree:adds([{Y, integer_to_list(Y)}
                                             || Y <- lists:delete(X, Seq)],
                                            t_tree:new()),
                                check))
               || X <- Seq])
       || Seq <- ?SEUQENCES]}
     ].


%% ===================================================================
%% Internal functions.
%% ===================================================================
