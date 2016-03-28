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
%%%   eunit unit tests for the syslog library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(syslog_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines

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
    [?_test(?assertMatch(#{header := #{facility := user,
                                       severity := info,
                                       version := 1
                                      }},
                         syslog:decode(iolist_to_binary(syslog:encode(#{}))))),
     ?_test(?assertMatch(#{header := #{facility := kern,
                                       severity := info,
                                       version := 1
                                      }},
                         syslog:decode(
                           iolist_to_binary(
                             syslog:encode(#{header => #{facility => kern}},
                                           [iolist]))))),
     ?_test(?assertMatch(#{header := #{facility := cron,
                                       severity := warning,
                                       version := 1
                                      }},
                         syslog:decode(
                           syslog:encode(#{header => #{facility => cron,
                                                       severity => warning}},
                                         [binary]),
                           [])))
    ].

%% ===================================================================
%% Encoding
%% ===================================================================

%% ===================================================================
%% Decoding
%% ===================================================================

%% ===================================================================
%% Bad options
%% ===================================================================
%% bad_option_test_() ->
%%     [?_test(?IS_BADARG(json:encode({}, [Option]))) || Option <- ?BAD_OPTS] ++
%%         [?_test(?IS_BADARG(json:decode(<<"{}">>, [Option]))) ||
%%             Option <- ?BAD_OPTS].

%% ===================================================================
%% Internal functions.
%% ===================================================================
