%%==============================================================================
%% Copyright 2020-2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the pb_tree library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2020-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(pb_tree_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(KEYS, [<<"1">>, <<"1201">>, <<"4670">>, <<"459340">>]).
-define(NUMBERS, [<<"1">>, <<"1201">>, <<"46706661875">>, <<"4618142404">>]).
-define(COUNTRIES,
        ["USA", "USA - New Jersey", "Sweden - Mobile - Telia ", "Sweden"]).


%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% new/0
%%--------------------------------------------------------------------
new_0_test_() ->
    [{"Create", [?_test(?assert(pb_tree:is_pb_tree(pb_tree:new())))]}
    ].

%%--------------------------------------------------------------------
%% is_pb_tree/1
%%--------------------------------------------------------------------
is_pb_tree_1_test_() ->
    [{"Empty",
      [?_test(?assertEqual(true, pb_tree:is_pb_tree(pb_tree:new())))]},
     {"Not", [?_test(?assertEqual(false, pb_tree:is_pb_tree({foo, bar})))]}
    ].

%%--------------------------------------------------------------------
%% is_empty/1
%%--------------------------------------------------------------------
is_empty_1_test_() ->
    [{"Empty", [?_test(?assertEqual(true, pb_tree:is_empty(pb_tree:new())))]},
     {"Not",
      [?_test(
          ?assertEqual(false,
                       pb_tree:is_empty(pb_tree:add(<<"1">>,
                                                  "US",
                                                  pb_tree:new()))))]}
    ].

%%--------------------------------------------------------------------
%% add/3
%%--------------------------------------------------------------------
add_3_test_() ->
    load_plist(),
    Countries = get(pb_countries),
    Keys = get(pb_keys),
    Values = get(pb_values),
    [{"Add",
      [?_test(
          ?assert(
             pb_tree:is_pb_tree(
               lists:foldl(fun({I, V}, Acc) -> pb_tree:add(I, V, Acc) end,
                           pb_tree:new(),
                           Countries))))]},
     {"Add/Delete",
      [?_test(
          ?assert(
             pb_tree:is_empty(
               lists:foldl(fun(I, Acc) -> pb_tree:delete(I, Acc) end,
                           lists:foldl(
                             fun({I, V}, Acc) -> pb_tree:add(I, V, Acc) end,
                             pb_tree:new(),
                             Countries),
                           Keys))))]},
     {"Add/Delete reverse",
      [?_test(
          ?assert(
             pb_tree:is_empty(
               lists:foldl(fun(I, Acc) -> pb_tree:delete(I, Acc) end,
                           lists:foldl(
                             fun({I, V}, Acc) -> pb_tree:add(I, V, Acc) end,
                             pb_tree:new(),
                             Countries),
                           lists:reverse(Keys)))))]},
     {"Add and check the keys",
      [?_test(
          ?assertEqual(
             Keys,
             pb_tree:keys(
               lists:foldl(fun({I, V}, Acc) -> pb_tree:add(I, V, Acc) end,
                           pb_tree:new(),
                           Countries))))
      ]},
     {"Add reverse and check the keys",
      [?_test(
          ?assertEqual(
             Keys,
             pb_tree:keys(
               lists:foldl(fun({I, V}, Acc) -> pb_tree:add(I, V, Acc) end,
                           pb_tree:new(),
                           lists:reverse(Countries)))))
      ]},
     {"Add and check the values",
      [?_test(
          ?assertEqual(
             Values,
             pb_tree:values(
               lists:foldl(fun({I, V}, Acc) -> pb_tree:add(I, V, Acc) end,
                           pb_tree:new(),
                           Countries))))]}
    ].

%%--------------------------------------------------------------------
%% add/4
%%--------------------------------------------------------------------
add_4_test_() ->
    load_plist(),
    Countries = get(pb_countries),
    [{"Add and read no check fail",
      [?_test(begin
                  Tree = lists:foldl(fun({I, V}, Acc) ->
                                             pb_tree:add(I, V, Acc, check)
                                     end,
                                     pb_tree:new(),
                                     Countries),
                  [?assert(pb_tree:is_pb_tree(
                             pb_tree:add(I, V, Tree, nocheck))) ||
                      {I, V} <- Countries]
              end)]},
     {"Add check fail",
      [?_test(begin
                  Tree = lists:foldl(fun({I, V}, Acc) ->
                                             pb_tree:add(I, V, Acc, check)
                                     end,
                                     pb_tree:new(),
                                     Countries),
                  [?assertError(_, pb_tree:add(I, V, Tree, check)) ||
                      {I, V} <- Countries]
              end)]}
    ].

%%--------------------------------------------------------------------
%% adds/2
%%--------------------------------------------------------------------
adds_2_test_() ->
    load_plist(),
    Countries = get(pb_countries),
    Keys = get(pb_keys),
    [{"Add",
      ?_test(?assert(pb_tree:is_pb_tree(
                       pb_tree:adds(Countries, pb_tree:new()))))},
     {"Add/Delete",
      ?_test(
          ?assert(
             pb_tree:is_empty(
               pb_tree:deletes(lists:reverse(Keys),
                              pb_tree:adds(Countries, pb_tree:new()),
                              check))))
     },
     {"Add and check the keys",
      ?_test(
          ?assertEqual(Keys,
                       pb_tree:keys
                         (pb_tree:adds(Countries, pb_tree:new()))))}
    ].

%%--------------------------------------------------------------------
%% adds/3
%%--------------------------------------------------------------------
adds_3_test_() ->
    load_plist(),
    Countries = get(pb_countries),
    Keys = get(pb_keys),
    [{"Add with check success",
      ?_test(
         ?assert(pb_tree:is_pb_tree(
                   pb_tree:adds(Countries, pb_tree:new(), check))))
     },
     {"Add with check fail",
      ?_test(
         begin
             Tree = pb_tree:from_list(Countries),
             [?assertError(badarg,
                           pb_tree:adds([{"a", b}, {I, d}, {"e", f}],
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
         ?assert(pb_tree:is_empty(
                   pb_tree:delete("1", pb_tree:new(), nocheck))))},
     {"Delete empty nocheck",
      ?_test(
         ?assert(
            pb_tree:is_pb_tree(
              pb_tree:delete("3",
                            pb_tree:from_list([{<<"1">>, a}, {<<"5">>, b}]),
                            nocheck))))},
     {"Delete empty check",
      ?_test(?assertError(badarg,
                          pb_tree:delete('1', pb_tree:new(), check)))},
     {"Delete empty check",
      ?_test(
         ?assertError(badarg,
                      pb_tree:delete(<<"3">>,
                                    pb_tree:from_list([{<<"1">>, a},
                                                        {<<"5">>, b}]),
                                    check)))}
    ].

%%--------------------------------------------------------------------
%% member/2
%%--------------------------------------------------------------------
member_2_test_() ->
    load_plist(),
    Countries = get(pb_countries),
    Keys = get(pb_keys),
    Tree = pb_tree:from_list(Countries),
    [{"Member with check success",
      ?_test(
         ?assert(
            lists:all(fun(Y) -> pb_tree:member(Y, Tree) end, Keys)))},
     {"Member without check success",
      ?_test(
         ?assert(
            not lists:any(fun(Y) -> pb_tree:member( Y, Tree) end,
                          ["a", "001", "0"])))}
    ].

%%--------------------------------------------------------------------
%% find/2
%%--------------------------------------------------------------------
find_2_test_() ->
    load_plist(),
    Tree = pb_tree:from_list(get(pb_countries)),
    [{"Find success",
      [?_test(?assertEqual(V, pb_tree:find(I, Tree))) ||
          {I, V} <- lists:zip(?NUMBERS, ?COUNTRIES)]},
     {"Find failure",
       [?_test(?assertEqual(undefined, pb_tree:find(<<($a + X)>>, Tree))) ||
           X <- lists:seq(1, 20)]}
    ].

%%--------------------------------------------------------------------
%% find/3
%%--------------------------------------------------------------------
find_3_test_() ->
    load_plist(),
    Tree = pb_tree:from_list(get(pb_countries)),
    [{"Find success",
      [?_test(?assertEqual(V, pb_tree:find(I, Tree, none))) ||
          {I, V} <- lists:zip(?NUMBERS, ?COUNTRIES)]},
     {"Find failure",
      [?_test(?assertEqual(none, pb_tree:find(<<($a + X)>>, Tree, none))) ||
          X <- lists:seq(1, 20)]}
    ].

%%--------------------------------------------------------------------
%% replace/3
%%--------------------------------------------------------------------
replace_3_test_() ->
    load_plist(),
    Tree = pb_tree:from_list(get(pb_countries)),
    [{"Replace existing",
      [?_test(?assertEqual(I,
                           pb_tree:find(I, pb_tree:replace(I, I, Tree)))) ||
          I <- ?KEYS]},
     {"Replace not existing",
      [?_test(?assertEqual(I,
                           pb_tree:find(<<I/binary, "a">>,
                                       pb_tree:replace(<<I/binary, "a">>,
                                                        I,
                                                        Tree)))) ||
          I <- ?KEYS]}
    ].

%%--------------------------------------------------------------------
%% replace/4
%%--------------------------------------------------------------------
replace_4_test_() ->
    load_plist(),
    Tree = pb_tree:from_list(get(pb_countries)),
    [{"Replace existing success",
      [?_test(?assertEqual(I,
                           pb_tree:find(I,
                                         pb_tree:replace(I,I,Tree,check)))) ||
          I <- ?KEYS]},
     {"Replace not existing",
       [?_test(?assertError(badarg,
                            pb_tree:replace(<<I/binary, "a">>,
                                             I,
                                             Tree,
                                             check))) ||
          I <- ?KEYS]}
    ].

%% ===================================================================
%% Internal functions.
%% ===================================================================

dir() -> code:lib_dir(jhn_stdlib, test).

file(File) -> filename:join([dir(), File]).

plist(File) -> {ok, L} = file:consult(file(File)), L.

load_plist() ->
    case get(pb_countries) of
        undefined ->
            Countries = plist("countries.txt"),
            put(pb_countries, [{list_to_binary(I), V} || {I, V} <- Countries]),
            put(pb_keys, [list_to_binary(I) || {I, _} <- Countries]),
            put(pb_values, [V || {_, V} <- Countries]);
        _ ->
            ok
    end.
