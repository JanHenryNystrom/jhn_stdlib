%%==============================================================================
%% Copyright 2018-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the bencoding library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2018-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(bencoding_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines

-define(ENCODED,
        [<<"4:spam">>, <<"0:">>,
         <<"i3e">>, <<"i-3e">>, <<"i0e">>,
         <<"l4:spam4:eggse">>, <<"le">>,
         <<"d3:cow3:moo4:spam4:eggse">>, <<"d4:spaml1:a1:bee">>,
         <<"d9:publisher3:bob17:publisher-webpage15:www.example.com18:"
           "publisher.location4:homee">>, <<"de">>]).

-define(DECODED,
        [<<"spam">>, <<>>,
         3, -3, 0,
         [<<"spam">>, <<"eggs">>], [],
         #{<<"cow">> => <<"moo">>, <<"spam">> => <<"eggs">>},
         #{<<"spam">> => [<<"a">>, <<"b">>]},
         #{<<"publisher">> => <<"bob">>,
           <<"publisher-webpage">> => <<"www.example.com">>,
           <<"publisher.location">> => <<"home">>},
         #{}]).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% Encode
%%--------------------------------------------------------------------
encode_1_test_() ->
    [{format(D),
      ?_test(?assertMatch(_, bencoding:encode(D)))} ||
        D <- ?DECODED].

encode_2_test_() ->
    [{format(D),
      ?_test(?assertMatch(<<_/binary>>, bencoding:encode(D, [binary])))} ||
        D <- ?DECODED].

%%--------------------------------------------------------------------
%% Decode
%%--------------------------------------------------------------------
decode_1_test_() ->
    [{D, ?_test(?assertMatch(_, bencoding:decode(D)))} ||
        D <- ?ENCODED].

decode_2_test_() ->
    [[{D, ?_test(?assertMatch({_, <<>>}, bencoding:decode(D, [continue])))} ||
        D <- ?ENCODED],
     [{<<D/binary, "cr">>,
       ?_test(?assertMatch({_, <<"cr">>},
                           bencoding:decode(<<D/binary, "cr">>,
                                            [continue])))} ||
         D <- ?ENCODED]
    ].

%%--------------------------------------------------------------------
%% Encode/Decode
%%--------------------------------------------------------------------
encode_2_decode_1_test_() ->
    [{format(D),
      ?_test(?assertMatch(D,
                          bencoding:decode(bencoding:encode(D, [binary]))))} ||
        D <- ?DECODED].

%% ===================================================================
%% Internal functions.
%% ===================================================================

format(B = <<_/binary>>) -> <<$", B/binary, $">>;
format(I) when is_integer(I) -> integer_to_binary(I);
format([]) -> <<"[]">>;
format(L = [_ | _]) ->
    <<$[, (bstring:join([format(E) || E <- L], <<", ">>))/binary, $]>>;
format(M = #{}) ->
    <<${,
      (bstring:join([<<$", K/binary, "\" => ", (format(V))/binary>> ||
                        {K, V} <- maps:to_list(M)], <<", ">>))/binary,
      $}>>.
