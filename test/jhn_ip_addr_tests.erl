%%==============================================================================
%% Copyright 2016-2026 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the ip_add library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016-2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_ip_addr_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(IPv4, ["127.0.0.1", "8.8.8.8"]).
-define(IPv6, ["1:2:3:4:5:6:7:8", "8:8:8:8:8:8:8:8",
               "a:b:c:d:e:f:9:0"
              ]).
-define(IPv4_RANGES, lists:seq(1, 31)).
-define(IPv6_RANGES, lists:seq(1, 127)).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% Encode
%%--------------------------------------------------------------------
encode_1_test_() ->
    [{IP,
      ?_test(
         ?assertEqual(
            IP,
            jhn_ip_addr:encode(jhn_ip_addr:decode(
                                 jhn_ip_addr:encode(
                                   jhn_ip_addr:decode(IP))),
                               [list])))}
     || IP <- ?IPv4 ++ ?IPv6
    ].

encode_1_range_test_() ->
    [{range(IP, Range),
      ?_test(
         ?assertEqual(
            range(IP, Range),
            jhn_ip_addr:encode(jhn_ip_addr:decode(
                                 jhn_ip_addr:encode(
                                   jhn_ip_addr:decode(range(IP, Range),
                                                      [range])),
                                 [range]),
                               [list])))}
     || IP <- ?IPv4 ++ ?IPv6,
        Range <- ?IPv4_RANGES
    ].

range(IP, Range) -> IP ++ "/" ++ integer_to_list(Range).

%%--------------------------------------------------------------------
%% Bounds
%%--------------------------------------------------------------------

bounds_1_test_() ->
    [{range(IP, Range),
      ?_test(?assertMatch({_, _}, jhn_ip_addr:bounds(range(IP, Range))))}
     || IP <- ?IPv4 ++ ?IPv6,
        Range <- ?IPv4_RANGES
    ].

%%--------------------------------------------------------------------
%% Decode/Encode/Decode/Encode
%%--------------------------------------------------------------------
encode_decode_encode_1_decode_1_test_() ->
    [{IP,
      ?_test(
         ?assertEqual(IP,
                      jhn_ip_addr:encode(
                        jhn_ip_addr:decode(
                          jhn_ip_addr:encode(jhn_ip_addr:decode(IP))),
                        [list])))} || IP <- ?IPv4 ++ ?IPv6
    ].

encode_decode_encode_2_decode_2_test_() ->
    [{IP,
      ?_test(
         ?assertEqual(IP,
                      jhn_ip_addr:encode(
                        jhn_ip_addr:decode(
                          jhn_ip_addr:encode(jhn_ip_addr:decode(IP, [Format]),
                                         [Return])),
                        [list])))} || Return <- [iolist, binary, list],
                                      Format <- [integer, tuple],
                                      IP <- ?IPv4 ++ ?IPv6
    ].



%% ===================================================================
%% Internal functions.
%% ===================================================================

