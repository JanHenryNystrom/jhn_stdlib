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
%%%   eunit unit tests for the plist library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(ip_addr_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(IPv4, ["127.0.0.1", "8.8.8.8"]).
-define(IPv6, ["1:2:3:4:5:6:7:8", "8:8:8:8:8:8:8:8",
               "a:b:c:d:e:f:16:17"
              ]).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% Decode/Encode/Decode/Encode
%%--------------------------------------------------------------------
encode_decode_encode_decode_test_() ->
    [{IP,
      ?_test(
         ?assertEqual(IP,
                      ip_addr:encode(
                        ip_addr:decode(
                          ip_addr:encode(ip_addr:decode(IP, [Format]),
                                         [Return])),
                        [list])))} || Return <- [iolist, binary, list],
                                      Format <- [integer, tuple],
                                      IP <- ?IPv4 ++ ?IPv6
    ].



%% ===================================================================
%% Internal functions.
%% ===================================================================

