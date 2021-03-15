%%==============================================================================
%% Copyright 2016-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the bloom library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(bloom_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% bloom/0
%%--------------------------------------------------------------------
bloom_fixed_test_() ->
    EmptyFilter = bloom:filter(),
    List = shuffle(lists:seq(1, 400000), 4000),
    [{"filter",
      ?_test(?assertEqual(true, bloom:is_filter(bloom:filter())))},
     {"type",
      ?_test(?assertEqual(fixed, bloom:type(bloom:filter([fixed]))))},
     [{"Capacity " ++ integer_to_list(N),
       ?_test(?assertEqual(true,
                           bloom:capacity(bloom:filter([{size, N}])) > N))} ||
         N <- [4000, 100000, 1000000, 1000000, 10000000, 100000000]],
     {"non member",
      ?_test([?assertEqual(false, bloom:member(E, EmptyFilter)) ||
                E <- List])},
     {"Add",
      ?_test(?assertEqual(true,
                          bloom:is_filter(
                            lists:foldl(fun bloom:add/2,
                                        EmptyFilter,
                                        [42, 42 | List]))))},
     begin
         Tree = lists:foldl(fun bloom:add/2, EmptyFilter, List),
         {"member",
          ?_test([?assertEqual(true, bloom:member(E, Tree)) || E <- List])}
     end
    ].

bloom_scalable_test_() ->
    EmptyFilter = bloom:filter([scalable]),
    List = shuffle(lists:seq(1, 400000), 40000),
    [{"filter",
      ?_test(?assertEqual(true,
                          bloom:is_filter(bloom:filter([scalable]))))},
     {"type",
      ?_test(
         ?assertEqual(scalable, bloom:type(bloom:filter([scalable]))))},
     [{"Capacity " ++ integer_to_list(N),
       ?_test(?assertEqual(infinity,
                           bloom:capacity(
                             bloom:filter([scalable, {size, N}]))))} ||
         N <- [1000000, 1000000, 10000000, 100000000]],
     {"non member",
      ?_test([?assertEqual(false, bloom:member(E, EmptyFilter)) ||
                E <- List])},
     {"Add",
      ?_test(?assertEqual(true,
                          bloom:is_filter(
                            lists:foldl(fun bloom:add/2, EmptyFilter, List))))},
     {"Add many",
      ?_test(?assertEqual(true,
                          bloom:is_filter(
                            lists:foldl(fun bloom:add/2,
                                        EmptyFilter,
                                       lists:seq(1, 100000)))))},
     begin
         Tree = lists:foldl(fun bloom:add/2, EmptyFilter, List),
         {"member",
          ?_test([?assertEqual(true, bloom:member(E, Tree)) || E <- List])}
     end
    ].

%% ===================================================================
%% Internal functions.
%% ===================================================================

shuffle(L, N) -> lists:sublist(shuffle(L), 1, N).
shuffle(L) ->
    N = 1000 * length(L),
    L2 = [{rand:uniform(N), E} || E <- L],
    {_, L3} = lists:unzip(lists:keysort(1, L2)),
    L3.
