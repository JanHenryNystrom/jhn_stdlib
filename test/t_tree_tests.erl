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
%%%   eunit unit tests for the t_tree library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
                           Seq)))) || Seq <- ?SEUQENCES]}
    ].

%%--------------------------------------------------------------------
%% add/4
%%--------------------------------------------------------------------
add_4_test_() ->
    [{"Add Check success",
      [?_test(
          ?assert(
             t_tree:is_t_tree(
               lists:foldl(fun(X, Acc) ->
                                   t_tree:add(X, integer_to_list(X), Acc, check)
                           end,
                           t_tree:new(),
                           Seq)))) || Seq <- ?SEUQENCES]},
     {"Add Check fail",
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

%% ===================================================================
%% Internal functions.
%% ===================================================================
