%%==============================================================================
%% Copyright 2017-2020 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the timestamp library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2017-2020, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(timestamp_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(TENTHZ, #{year => 1977, month => 1, day => 1,
                  hour => 10, minute => 10, second => 10,
                  fraction => 0, offset => 'Z'}).
-define(TENTHZB, [<<"1977-01-01T10:10:10Z">>,
                  <<"1977-01-01t10:10:10Z">>,
                  <<"1977-01-01T10:10:10z">>,
                  <<"1977-01-01t10:10:10z">>]).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% Generate
%%--------------------------------------------------------------------
gen_1_test_() ->
    [{"gen([])",
      ?_test(?assertMatch(
                #{fraction := 0},
                timestamp:decode(iolist_to_binary(timestamp:gen()))))},
     {"gen([posix])",
      ?_test(?assertMatch(#{fraction := 0},
                          timestamp:decode(timestamp:gen([posix]))))},
     {"gen([posix, seconds])",
      ?_test(?assertMatch(#{fraction := 0},
                          timestamp:decode(timestamp:gen([posix, seconds]))))},
     {"gen([posix, milli])",
      ?_test(?assertMatch(#{},
                          timestamp:decode(timestamp:gen([posix, milli]),
                                           [milli])))},
     {"gen([posix, micro])",
      ?_test(?assertMatch(#{},
                          timestamp:decode(timestamp:gen([posix, micro]),
                                           [micro])))},
     {"gen([posix, nano])",
      ?_test(?assertMatch(#{},
                          timestamp:decode(timestamp:gen([posix, nano]),
                                           [nano])))},
     {"gen([iolist])",
      ?_test(?assertMatch(
                #{fraction := 0},
                timestamp:decode(iolist_to_binary(timestamp:gen([iolist])))))},
     {"gen([list])",
      ?_test(?assertMatch(
                #{fraction := 0},
                timestamp:decode(iolist_to_binary(timestamp:gen([list])))))},
     {"gen([binary])",
      ?_test(?assertMatch(#{fraction := 0},
                          timestamp:decode(timestamp:gen([binary]))))},
     {"gen([binary, seconds])",
      ?_test(?assertMatch(#{fraction := 0},
                          timestamp:decode(timestamp:gen([binary, seconds]))))},
     {"gen([binary, milli])",
      ?_test(?assertMatch(#{},
                          timestamp:decode(timestamp:gen([binary, milli]))))},
     {"gen([binary, micro])",
      ?_test(?assertMatch(#{},
                          timestamp:decode(timestamp:gen([binary, micro]))))},
     {"gen([binary, nano])",
      ?_test(?assertMatch(#{},
                          timestamp:decode(timestamp:gen([binary, nano]))))}
    ].

%%--------------------------------------------------------------------
%% Encode
%%--------------------------------------------------------------------
encode_2_test_() ->
    [{"2000-01-01T00:00:00Z(946684800)",
      ?_test(?assertEqual(946684800,
                          timestamp:encode(
                            #{year => 2000, month => 1, day => 1,
                              hour => 0, minute => 0, second => 0,
                              offset => 'Z'},
                            [posix])))},
     {"2000-01-01T00:00:00Z(946684800000)",
      ?_test(?assertEqual(946684800000,
                          timestamp:encode(
                            #{year => 2000, month => 1, day => 1,
                              hour => 0, minute => 0, second => 0,
                              offset => 'Z'},
                            [posix, milli])))},
     {"2000-01-01T00:00:00Z(946684800001)",
      ?_test(?assertEqual(946684800001,
                          timestamp:encode(
                            #{year => 2000, month => 1, day => 1,
                              hour => 0, minute => 0, second => 0,
                              offset => 'Z', fraction => 1},
                            [posix, milli])))},
     {"2000-01-01T00:00:00Z(946684800000001)",
      ?_test(?assertEqual(946684800000001,
                          timestamp:encode(
                            #{year => 2000, month => 1, day => 1,
                              hour => 0, minute => 0, second => 0,
                              offset => 'Z', fraction => 1},
                            [posix, micro])))},
     {"2000-01-01T00:00:00Z(946684800000000001)",
      ?_test(?assertEqual(946684800000000001,
                          timestamp:encode(
                            #{year => 2000, month => 1, day => 1,
                              hour => 0, minute => 0, second => 0,
                              offset => 'Z', fraction => 1},
                            [posix, nano])))},
     {"949363200(2000-02-01T00:00:00Z)",
      ?_test(?assertEqual(#{year => 2000, month => 2, day => 1,
                            hour => 0, minute => 0, second => 0,
                            fraction => 0, offset => 'Z'},
                          timestamp:decode(
                            iolist_to_binary(timestamp:encode(949363200)))))},
     {<<"1977-01-01T10:10:10+00:00">>,
      ?_test(?assertEqual(
                #{year => 1977, month => 1, day => 1,
                  hour => 10, minute => 10, second => 10,
                  fraction => 0, offset => 'Z'},
                timestamp:decode(
                  timestamp:encode(#{year => 1977, month => 1, day => 1,
                                     hour => 10, minute => 10, second => 10,
                                     offset => #{sign => '+',
                                                 hours => 0,
                                                 minutes => 0}}, [binary]))))},
     {<<"1977-01-01T10:10:10+01:00">>,
      ?_test(?assertEqual(
                #{year => 1977, month => 1, day => 1,
                  hour => 10, minute => 10, second => 10,
                  fraction => 0,
                  offset => #{sign => '+', hours => 1, minutes => 0}},
                timestamp:decode(
                  timestamp:encode(#{year => 1977, month => 1, day => 1,
                                     hour => 10, minute => 10, second => 10,
                                     offset => #{sign => '+',
                                                 hours => 1,
                                                 minutes => 0}}, [binary]))))},
     {<<"1977-01-01T10:10:10+01:00">>,
      ?_test(?assertEqual(
                #{year => 1977, month => 1, day => 1,
                  hour => 10, minute => 10, second => 10,
                  fraction => 0,
                  offset => #{sign => '-', hours => 1, minutes => 0}},
                timestamp:decode(
                  timestamp:encode(#{year => 1977, month => 1, day => 1,
                                     hour => 10, minute => 10, second => 10,
                                     offset => #{sign => '-',
                                                 hours => 1,
                                                 minutes => 0}}, [binary]))))}
    ].

%%--------------------------------------------------------------------
%% Decode
%%--------------------------------------------------------------------

decode_1_test_() ->
    [[{D,
       ?_test(?assertEqual(?TENTHZ, timestamp:decode(D)))} || D <- ?TENTHZB],
     {<<"1977-01-01T10:10:10.12Z">>,
      ?_test(?assertEqual(#{year => 1977, month => 1, day => 1,
                            hour => 10, minute => 10, second => 10,
                            fraction => 12, offset => 'Z'},
                          timestamp:decode(<<"1977-01-01T10:10:10.12Z">>)))},
     {<<"1977-01-01T10:10:10.12z">>,
      ?_test(?assertEqual(#{year => 1977, month => 1, day => 1,
                            hour => 10, minute => 10, second => 10,
                            fraction => 12, offset => 'Z'},
                          timestamp:decode(<<"1977-01-01T10:10:10.12z">>)))},
     {<<"1977-01-01T10:10:10-01:00">>,
      ?_test(?assertEqual(#{year => 1977, month => 1, day => 1,
                            hour => 10, minute => 10, second => 10,
                            fraction => 0,
                            offset => #{sign => '-', hours => 1, minutes => 0}},
                          timestamp:decode(<<"1977-01-01T10:10:10-01:00">>)))},
     {<<"1977-01-01T10:10:10-00:00">>,
      ?_test(?assertEqual(#{year => 1977, month => 1, day => 1,
                            hour => 10, minute => 10, second => 10,
                            fraction => 0,
                            offset => #{sign => '-', hours => 0, minutes => 0}},
                          timestamp:decode(<<"1977-01-01T10:10:10-00:00">>)))},
     {<<"1977-01-01T10:10:10+00:00">>,
      ?_test(?assertEqual(#{year => 1977, month => 1, day => 1,
                            hour => 10, minute => 10, second => 10,
                            fraction => 0,
                            offset => 'Z'},
                          timestamp:decode(<<"1977-01-01T10:10:10+00:00">>)))},
     {<<"1977-01-01T10:10:10.43+00:00">>,
      ?_test(?assertEqual(
                #{year => 1977, month => 1, day => 1,
                  hour => 10, minute => 10, second => 10,
                  fraction => 43,
                  offset => 'Z'},
                timestamp:decode(<<"1977-01-01T10:10:10.43+00:00">>)))},
     {<<"1977-01-01T10:10:10+10:22">>,
      ?_test(?assertEqual(#{year => 1977, month => 1, day => 1,
                            hour => 10, minute => 10, second => 10,
                            fraction => 0,
                            offset => #{sign => '+',
                                        hours => 10,
                                        minutes => 22}},
                          timestamp:decode(<<"1977-01-01T10:10:10+10:22">>)))},
     {<<"1977-01-01T10:10:10.33+10:22">>,
      ?_test(?assertEqual(
                #{year => 1977, month => 1, day => 1,
                  hour => 10, minute => 10, second => 10,
                  fraction => 33,
                  offset => #{sign => '+',
                              hours => 10,
                              minutes => 22}},
                timestamp:decode(<<"1977-01-01T10:10:10.33+10:22">>)))},
     {<<"1977-01-01T10:10:10.33-10:22">>,
      ?_test(?assertEqual(
                #{year => 1977, month => 1, day => 1,
                  hour => 10, minute => 10, second => 10,
                  fraction => 33,
                  offset => #{sign => '-',
                              hours => 10,
                              minutes => 22}},
                timestamp:decode(<<"1977-01-01T10:10:10.33-10:22">>)))}
    ].

decode_2_test_() ->
    [{D,
      ?_test(?assertEqual({?TENTHZ, <<>>}, timestamp:decode(D, [continue])))} ||
        D <- ?TENTHZB].

%% ===================================================================
%% Internal functions.
%% ===================================================================

