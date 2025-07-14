%%==============================================================================
%% Copyright 2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the RESP3 library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_resp_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines

-define(SIMPLE,
        [null, true, false, nan, inf, neg_inf, 1, 1.0, -17,
         16#8000000000000000, -16#8000000000000001,
         {string, <<"foo">>}, {error, <<"ERR">>, <<"bar">>},
         {blob, <<"foo">>}, {blob_error, <<"ERR">>, <<"foo">>},
         {verbatim, <<"txt">>, <<"bat">>}
        ]).
-define(ARRAYS, [[], ?SIMPLE]).
-define(MAPS, [#{}, maps:from_list(lists:zip(?SIMPLE, ?SIMPLE))]).
-define(SETS,
        [{set, sets:new([{version, 2}])},
         {set, sets:from_list(?SIMPLE, [{version, 2}])}]).
-define(ATTRIBUTES, [{attribute, A, V} || A <- ?MAPS, V <- ?SIMPLE]).
-define(PUSHES, [{push, {string, <<"FOO">>}, []} |
                 [{push, {string, <<"BAR">>}, A} || A <- ?ARRAYS]]).

%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Encode/Decode
%% ===================================================================

%%--------------------------------------------------------------------
%% encode/2/decode/1
%%--------------------------------------------------------------------
encode_2_decode_1_test_() ->
    {inparallel,
     [?_test(?assertMatch({R, <<>>},
                          jhn_resp:decode(
                            iolist_to_binary(jhn_resp:encode(R)))))
      || R <- ?SIMPLE ++ ?ARRAYS ++ ?MAPS ++ ?SETS ++ ?ATTRIBUTES ++ ?PUSHES
     ]}.

%% ===================================================================
%% Bad options
%% ===================================================================
bad_option_test_() ->
    [?_test(?assertError(badarg, jhn_resp:encode(1, [oops])))].

%% ===================================================================
%% Internal functions.
%% ===================================================================

