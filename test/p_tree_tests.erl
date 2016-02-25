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
%%%   eunit unit tests for the p_tree library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(p_tree_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(KEYS, ["1", "1201", "4670", "459340"]).
-define(NUMBERS, ["1", "1201", "46706661875", "4618142404"]).
-define(COUNTRIES,
        ["USA", "USA - New Jersey", "Sweden - Mobile - Telia ", "Sweden"]).


%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% new/0
%%--------------------------------------------------------------------
new_0_test_() ->
    [{"Create", [?_test(?assert(p_tree:is_p_tree(p_tree:new())))]}
    ].

%%--------------------------------------------------------------------
%% is_p_tree/1
%%--------------------------------------------------------------------
is_p_tree_1_test_() ->
    [{"Empty", [?_test(?assertEqual(true, p_tree:is_p_tree(p_tree:new())))]},
     {"Not", [?_test(?assertEqual(false, p_tree:is_p_tree({foo, bar})))]}
    ].

%%--------------------------------------------------------------------
%% is_empty/1
%%--------------------------------------------------------------------
is_empty_1_test_() ->
    [{"Empty", [?_test(?assertEqual(true, p_tree:is_empty(p_tree:new())))]},
     {"Not",
      [?_test(
          ?assertEqual(false,
                       p_tree:is_empty(p_tree:add("1",
                                                  "US",
                                                  p_tree:new()))))]}
    ].

%%--------------------------------------------------------------------
%% add/3
%%--------------------------------------------------------------------
add_3_test_() ->
    load_plist(),
    Countries = get(countries),
    Keys = get(keys),
    Values = get(values),
    [{"Add",
      [?_test(
          ?assert(
             p_tree:is_p_tree(
               lists:foldl(fun({I, V}, Acc) -> p_tree:add(I, V, Acc) end,
                           p_tree:new(),
                           Countries))))]},
     {"Add/Delete",
      [?_test(
          ?assert(
             p_tree:is_empty(
               lists:foldl(fun(I, Acc) -> p_tree:delete(I, Acc) end,
                           lists:foldl(
                             fun({I, V}, Acc) -> p_tree:add(I, V, Acc) end,
                             p_tree:new(),
                             Countries),
                           Keys))))]},
     {"Add/Delete reverse",
      [?_test(
          ?assert(
             p_tree:is_empty(
               lists:foldl(fun(I, Acc) -> p_tree:delete(I, Acc) end,
                           lists:foldl(
                             fun({I, V}, Acc) -> p_tree:add(I, V, Acc) end,
                             p_tree:new(),
                             Countries),
                           lists:reverse(Keys)))))]},
     {"Add and check the keys",
      [?_test(
          ?assertEqual(
             Keys,
             p_tree:keys(
               lists:foldl(fun({I, V}, Acc) -> p_tree:add(I, V, Acc) end,
                           p_tree:new(),
                           Countries))))
      ]},
     {"Add reverse and check the keys",
      [?_test(
          ?assertEqual(
             Keys,
             p_tree:keys(
               lists:foldl(fun({I, V}, Acc) -> p_tree:add(I, V, Acc) end,
                           p_tree:new(),
                           lists:reverse(Countries)))))
      ]},
     {"Add and check the values",
      [?_test(
          ?assertEqual(
             Values,
             p_tree:values(
               lists:foldl(fun({I, V}, Acc) -> p_tree:add(I, V, Acc) end,
                           p_tree:new(),
                           Countries))))]}
    ].

%%--------------------------------------------------------------------
%% add/4
%%--------------------------------------------------------------------
add_4_test_() ->
    load_plist(),
    Countries = get(countries),
    [{"Add and read no check fail",
      [?_test(begin
                  Tree = lists:foldl(fun({I, V}, Acc) ->
                                             p_tree:add(I, V, Acc, check)
                                     end,
                                     p_tree:new(),
                                     Countries),
                  [?assert(p_tree:is_p_tree(p_tree:add(I, V, Tree, nocheck))) ||
                      {I, V} <- Countries]
              end)]},
     {"Add check fail",
      [?_test(begin
                  Tree = lists:foldl(fun({I, V}, Acc) ->
                                             p_tree:add(I, V, Acc, check)
                                     end,
                                     p_tree:new(),
                                     Countries),
                  [?assertError(_, p_tree:add(I, V, Tree, check)) ||
                      {I, V} <- Countries]
              end)]}
    ].

%%--------------------------------------------------------------------
%% adds/2
%%--------------------------------------------------------------------
adds_2_test_() ->
    load_plist(),
    Countries = get(countries),
    Keys = get(keys),
    [{"Add",
      ?_test(?assert(p_tree:is_p_tree(p_tree:adds(Countries, p_tree:new()))))},
     {"Add/Delete",
      ?_test(
          ?assert(
             p_tree:is_empty(
               p_tree:deletes(lists:reverse(Keys),
                              p_tree:adds(Countries, p_tree:new()),
                              check))))
     },
     {"Add and check the keys",
      ?_test(
          ?assertEqual(Keys,
                       p_tree:keys(p_tree:adds(Countries, p_tree:new()))))}
    ].

%%--------------------------------------------------------------------
%% adds/3
%%--------------------------------------------------------------------
adds_3_test_() ->
    load_plist(),
    Countries = get(countries),
    Keys = get(keys),
    [{"Add with check success",
      ?_test(
         ?assert(p_tree:is_p_tree(p_tree:adds(Countries, p_tree:new(), check))))
     },
     {"Add with check fail",
      ?_test(
         begin
             Tree = p_tree:from_list(Countries),
             [?assertError(badarg,
                           p_tree:adds([{"a", b}, {I, d}, {"e", f}],
                                       Tree,
                                       check)) ||
                 I <- Keys]
         end)}
    ].

%%--------------------------------------------------------------------
%% delete/3
%%--------------------------------------------------------------------
delete_3_test_() ->
    [{"Delete empty nocheck",
      ?_test(
         ?assert(p_tree:is_empty(p_tree:delete("1", p_tree:new(), nocheck))))},
     {"Delete empty nocheck",
      ?_test(
         ?assert(
            p_tree:is_p_tree(
              p_tree:delete("3",
                            p_tree:from_list([{"1", a}, {"5", b}]),
                            nocheck))))},
     {"Delete empty check",
      ?_test(?assertError(badarg, p_tree:delete('1', p_tree:new(), check)))},
     {"Delete empty check",
      ?_test(
         ?assertError(badarg,
                      p_tree:delete("3",
                                    p_tree:from_list([{"1", a}, {"5", b}]),
                                    check)))}
    ].

%%--------------------------------------------------------------------
%% member/2
%%--------------------------------------------------------------------
member_2_test_() ->
    load_plist(),
    Countries = get(countries),
    Keys = get(keys),
    Tree = p_tree:from_list(Countries),
    [{"Member with check success",
      ?_test(
         ?assert(
            lists:all(fun(Y) -> p_tree:member(Y, Tree) end, Keys)))},
     {"Member without check success",
      ?_test(
         ?assert(
            not lists:any(fun(Y) -> p_tree:member( Y, Tree) end,
                          ["a", "001", "0"])))}
    ].

%%--------------------------------------------------------------------
%% find/2
%%--------------------------------------------------------------------
find_2_test_() ->
    load_plist(),
    Tree = p_tree:from_list(get(countries)),
    [{"Find success",
      [?_test(?assertEqual(V, p_tree:find(I, Tree))) ||
          {I, V} <- lists:zip(?NUMBERS, ?COUNTRIES)]},
     {"Find failure",
       [?_test(?assertEqual(undefined, p_tree:find([$a + X], Tree))) ||
           X <- lists:seq(1, 20)]}
    ].

%%--------------------------------------------------------------------
%% find/3
%%--------------------------------------------------------------------
find_3_test_() ->
    load_plist(),
    Tree = p_tree:from_list(get(countries)),
    [{"Find success",
      [?_test(?assertEqual(V, p_tree:find(I, Tree, none))) ||
          {I, V} <- lists:zip(?NUMBERS, ?COUNTRIES)]},
     {"Find failure",
      [?_test(?assertEqual(none, p_tree:find([$a + X], Tree, none))) ||
          X <- lists:seq(1, 20)]}
    ].

%%--------------------------------------------------------------------
%% replace/3
%%--------------------------------------------------------------------
replace_3_test_() ->
    load_plist(),
    Tree = p_tree:from_list(get(countries)),
    [{"Replace existing",
      [?_test(?assertEqual(I,
                           p_tree:find(I, p_tree:replace(I, I, Tree)))) ||
          I <- ?KEYS]},
     {"Replace not existing",
      [?_test(?assertEqual(I,
                           p_tree:find(I ++ "a",
                                       p_tree:replace(I ++ "a", I, Tree)))) ||
          I <- ?KEYS]}
    ].

%%--------------------------------------------------------------------
%% replace/4
%%--------------------------------------------------------------------
replace_4_test_() ->
    load_plist(),
    Tree = p_tree:from_list(get(countries)),
    [{"Replace existing success",
      [?_test(?assertEqual(I, p_tree:find(I,p_tree:replace(I,I,Tree,check)))) ||
          I <- ?KEYS]},
     {"Replace not existing",
       [?_test(?assertError(badarg, p_tree:replace(I ++ "a", I, Tree,check))) ||
          I <- ?KEYS]}
    ].

%% ===================================================================
%% Internal functions.
%% ===================================================================

dir() -> code:lib_dir(jhn_stdlib, test).

file(File) -> filename:join([dir(), File]).

plist(File) -> {ok, L} = file:consult(file(File)), L.

load_plist() ->
    case get(countries) of
        undefined ->
            Countries = plist("countries.txt"),
            put(countries, Countries),
            put(keys, [I || {I, _} <- Countries]),
            put(values, [V || {_, V} <- Countries]);
        _ ->
            ok
    end.
