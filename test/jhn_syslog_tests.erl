%%==============================================================================
%% Copyright 2016-2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%% @copyright (C) 2016-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_syslog_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-record(transport, {type                 :: udp | tcp | tls,
                    role                 :: client | server,
                    port                 :: inet:port(),
                    ipv                  :: ipv4 | ipv6,
                    dest                 :: inet:ip_address() | inet:hostname(),
                    dest_port            :: inet:port(),
                    socket               :: gen_udp:socket() |
                                            gen_tcp:socket() |
                                            ssl:socket(),
                    listen_socket        :: gen_tcp:socket() | ssl:socket(),
                    buf           = <<>> :: binary()
                   }).

-define(UDP, 1601).
-define(UDPS, 1154).
-define(TLS, 6514).
-define(DTLS, 6515).


%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Open
%% ===================================================================

%%--------------------------------------------------------------------
%% open/0
%%--------------------------------------------------------------------
open_0_test_() ->
    [?_test(?assertMatch(#transport{}, jhn_syslog:open()))
    ].

%%--------------------------------------------------------------------
%% open/1
%%--------------------------------------------------------------------
open_1_client_test_() ->
    {setup,
     fun() ->
             {ok, Apps} = application:ensure_all_started(ssl),
             TCPS = {tcp, server_start(tcp, ?UDP, dummy)},
             TLSS = {tls, server_start(tls, ?TLS, dummy)},
             DTLSS = {dtls, server_start(dtls, ?DTLS, dummy)},
             {Apps, [TCPS , TLSS, DTLSS]}
     end,
     fun({Apps, Servers}) ->
             [server_stop(Type, Server) || {Type, Server} <- Servers],
             [application:stop(App) || App <- Apps]
     end,
    [?_test(?assertMatch(ok, jhn_syslog:close(jhn_syslog:open([])))),
     ?_test(?assertMatch(ok, jhn_syslog:close(jhn_syslog:open([udp, ipv4])))),
     ?_test(?assertMatch(ok, jhn_syslog:close(jhn_syslog:open([udp, client])))),
     ?_test(?assertMatch(ok, jhn_syslog:close(jhn_syslog:open([udp, ipv6])))),
     ?_test(?assertEqual(ok,
                         jhn_syslog:close(
                           jhn_syslog:open([dtls,
                                        {opts, [{verify, verify_none}]},
                                        {destination, "127.0.0.1"},
                                        {destination_port, ?DTLS}])))),
     ?_test(?assertEqual(ok,
                         jhn_syslog:close(
                           jhn_syslog:open([tcp,
                                        {destination, {127, 0, 0, 1}},
                                        {destination_port, ?UDP}])))),
     ?_test(?assertEqual(ok,
                         jhn_syslog:close(
                           jhn_syslog:open([tls,
                                        {opts, [{verify, verify_none}]},
                                        {destination, "127.0.0.1"},
                                        {destination_port, ?TLS}])))),
     ?_test(?assertEqual({error, {exit, badarg}},
                         jhn_syslog:open([{opts, [{ip, none}]}]))),
     ?_test(?assertMatch({error, _}, jhn_syslog:open([tcp]))),
     ?_test(?assertMatch({error, {exit, _}},
                         jhn_syslog:open([tcp, {opts, [{ip, none}]}]))),
     %% Connect so the server must be there for dlts
     ?_test(?assertMatch({error, _},
                         jhn_syslog:open([dtls,
                                      {destination, "::1"},
                                      {destination_port, ?DTLS}]))),
     ?_test(?assertMatch({error, _},
                         jhn_syslog:open([tls,
                                      {destination, "::1"},
                                      {destination_port, 4712}])))
    ]}.

open_1_client_timeout_test_() ->
    {setup,
     fun() ->
             {ok, Apps} = application:ensure_all_started(ssl),
             TCPS = {tcp, server_start(tcp, ?UDP, dummy)},
             TLSS = {tls, server_start(tls, ?TLS, dummy)},
             DTLSS = {dtls, server_start(dtls, ?DTLS, dummy)},
             {Apps, [TCPS , TLSS, DTLSS]}
     end,
     fun({Apps, Servers}) ->
             [server_stop(Type, Server) || {Type, Server} <- Servers],
             [application:stop(App) || App <- Apps]
     end,
    [?_test(?assertMatch(ok, jhn_syslog:close(jhn_syslog:open([])))),
     ?_test(?assertMatch(ok, jhn_syslog:close(jhn_syslog:open([udp, ipv4])))),
     ?_test(?assertMatch(ok, jhn_syslog:close(jhn_syslog:open([udp, client])))),
     ?_test(?assertMatch(ok, jhn_syslog:close(jhn_syslog:open([udp, ipv6])))),
     ?_test(?assertEqual(ok,
                         jhn_syslog:close(
                           jhn_syslog:open([dtls,
                                        {opts, [{verify, verify_none}]},
                                        {timeout, 500},
                                        {destination, {127, 0, 0, 1}},
                                        {destination_port, ?DTLS}])))),
     ?_test(?assertEqual(ok,
                         jhn_syslog:close(
                           jhn_syslog:open([tcp,
                                        {timeout, 500},
                                        {destination, {127, 0, 0, 1}},
                                        {destination_port, ?UDP}])))),
     ?_test(?assertEqual(ok,
                         jhn_syslog:close(
                           jhn_syslog:open([tls,
                                        {timeout, 500},
                                        {opts, [{verify, verify_none}]},
                                        {destination, {127, 0, 0, 1}},
                                        {destination_port, ?TLS}])))),
     ?_test(?assertMatch({error, _}, jhn_syslog:open([tcp, {timeout, 500}]))),
     ?_test(?assertMatch({error, {exit, _}},
                         jhn_syslog:open([tcp,
                                      {timeout, 500},
                                      {opts, [{ip, none}]}]))),
     ?_test(?assertMatch({error, _},
                         jhn_syslog:open([tls,
                                      {timeout, 500},
                                      {destination, {0, 0, 0, 0, 0, 0, 0, 1}},
                                      {destination_port, 4711}])))
    ]}.

open_1_server_test_() ->
    {setup,
     fun() -> application:ensure_all_started(ssl) end,
     fun({ok, Apps}) -> [application:stop(App) || App <- Apps] end,
     [?_test(
         ?assertMatch(ok,
                      jhn_syslog:close(
                        jhn_syslog:open([server, {port, ?UDPS}])))),
      ?_test(
         ?assertMatch(ok,
                      jhn_syslog:close(
                        jhn_syslog:open([udp, server, {port, ?UDPS}])))),
      ?_test(
         ?assertMatch(ok,
                      jhn_syslog:close(
                        jhn_syslog:open([udp, ipv6, server, {port, ?UDPS}])))),
      ?_test(
         ?assertMatch(ok,
                      jhn_syslog:close(
                        jhn_syslog:open([tcp, server, {port, ?UDP}])))),
      ?_test(
         ?assertMatch(ok,
                      jhn_syslog:close(
                        jhn_syslog:open([tcp, server, ipv6, {port, ?UDP}])))),
      ?_test(
         ?assertMatch(ok,
                      jhn_syslog:close(jhn_syslog:open([tls, ipv6, server])))),
      ?_test(
         ?assertMatch(ok,
                      jhn_syslog:close(
                        jhn_syslog:open([tls, server, {port, ?TLS}])))),
      %% ?_test(?assertEqual({error,eacces}, jhn_syslog:open([server, udp]))),
      %% ?_test(?assertEqual({error,eacces}, jhn_syslog:open([server, tcp]))),
      ?_test(
         ?assertMatch({error, {exit, _}},
                      jhn_syslog:open([tcp, server, {opts, [{ip, none}]}]))),
      ?_test(?assertMatch({error, _},
                          jhn_syslog:open([tls,
                                           server,
                                           {opts, [{depth, -1}]}]))),
      ?_test(?assertMatch({error, _},
                          jhn_syslog:open([tls, server, {opts, [{ip, none}]}])))
     ]}.

%%--------------------------------------------------------------------
%% send/2
%%--------------------------------------------------------------------
send_2_test_() ->
    ets:new(send_2_test, [named_table, public]),
    {setup,
     fun() ->
             {ok, Apps} = application:ensure_all_started(ssl),
             UDPS = {udp, server_start(udp, ?UDPS, send_2_test_)},
             UDPC = jhn_syslog:open([{destination, {127, 0, 0, 1}},
                                 {destination_port, ?UDPS}]),
             true = ets:insert(send_2_test, {udpc, UDPC}),
             TCPS = {tcp, server_start(tcp, ?UDP, send_2_test_)},
             TCPC = jhn_syslog:open([tcp,
                                 {destination, {127, 0, 0, 1}},
                                 {destination_port, ?UDP}]),
             true = ets:insert(send_2_test, {tcpc, TCPC}),
             TLSS = {tls, server_start(tls, ?TLS, send_2_test_)},
             TLSC = jhn_syslog:open([tls,
                                 {opts, [{verify, verify_none}]},
                                 {destination, {127, 0, 0, 1}},
                                 {destination_port, ?TLS}]),
             true = ets:insert(send_2_test, {tlsc, TLSC}),
             {Apps, [UDPC, TCPC, TLSC], [UDPS, TCPS , TLSS]}
     end,
     fun({Apps, Clients, Servers}) ->
             [jhn_syslog:close(Client) || Client <- Clients],
             [server_stop(Type, Server) || {Type, Server} <- Servers],
             [application:stop(App) || App <- Apps]
     end,
     [?_test(?assertEqual(true, register(send_2_test_, self()))),
      ?_test(?assertMatch(ok, jhn_syslog:send(sock(send_2_test, udpc), #{}))),
      ?_test(?assertMatch(#{}, jhn_syslog:decode(active(udp)))),
      ?_test(?assertMatch(ok,
                          jhn_syslog:send(sock(send_2_test, udpc),
                                      #{},
                                      [{destination, {127, 0, 0, 1}},
                                       {destination_port, ?UDPS}]))),
      ?_test(?assertMatch(#{}, jhn_syslog:decode(active(udp)))),
      ?_test(?assertMatch(ok, jhn_syslog:send(sock(send_2_test, tcpc), #{}))),
      ?_test(?assertMatch(#{},
                          jhn_syslog:decode(
                            element(1, jhn_syslog:unframe(tcp, active(tcp)))))),

      ?_test(?assertMatch(ok, jhn_syslog:send(sock(send_2_test, tlsc), #{}))),
      ?_test(?assertMatch(#{}, unframe(tls, active(tls)))),
      ?_test(
         ?assertEqual({error, function_clause},
                      jhn_syslog:send(#transport{type = udp}, #{}))),
      ?_test(
         ?assertEqual({error, function_clause},
                      jhn_syslog:send(#transport{type = udp},
                                  #{},
                                  [{destination, {127, 0, 0, 1}}]))),
      ?_test(
         ?assertEqual({error, function_clause},
                      jhn_syslog:send(#transport{type = tcp}, #{}))),
      ?_test(
         ?assertEqual({error, function_clause},
                      jhn_syslog:send(#transport{type = tls}, #{})))
     ]}.

%%--------------------------------------------------------------------
%% recv/0
%%--------------------------------------------------------------------
recv_1_test_() ->
    ets:new(recv_1_test, [named_table, public]),
    {setup,
     fun() ->
             {ok, Apps} = application:ensure_all_started(ssl),
             UDPS = server_start(udp, ?UDPS, recv_1_test_),
             UDPC = jhn_syslog:open([{destination, "127.0.0.1"},
                                 {destination_port, ?UDPS}]),
             true = ets:insert(recv_1_test, {udps, UDPS}),
             true = ets:insert(recv_1_test, {udpc, UDPC}),
             TCPS = server_start(tcp, ?UDP, recv_1_test_),
             TCPC = jhn_syslog:open([tcp,
                                 {destination, {127, 0, 0, 1}},
                                 {destination_port, ?UDP}]),
             true = ets:insert(recv_1_test, {tcps, TCPS}),
             true = ets:insert(recv_1_test, {tcpc, TCPC}),
             TLSS = server_start(tls, ?TLS, recv_1_test_),
             TLSC = jhn_syslog:open([tls,
                                 {opts, [{verify, verify_none}]},
                                 {destination, {127, 0, 0, 1}},
                                 {destination_port, ?TLS}]),
             true = ets:insert(recv_1_test, {tlss, TLSS}),
             true = ets:insert(recv_1_test, {tlsc, TLSC}),
             {Apps,
              [UDPC, TCPC, TLSC],
              [{udp, UDPS}, {tcp, TCPS}, {tls, TLSS}]}
     end,
     fun({Apps, Clients, Servers}) ->
             [jhn_syslog:close(Client) || Client <- Clients],
             [server_stop(Type, Server) || {Type, Server} <- Servers],
             [application:stop(App) || App <- Apps]
     end,
     [?_test(?assertEqual(true, register(recv_1_test_, self()))),
      ?_test(?assertMatch({error, _},
                          jhn_syslog:recv(sock(recv_1_test, udpc)))),
      ?_test(?assertMatch(passive, passify(pid(recv_1_test, udps)))),
      ?_test(?assertMatch(ok, jhn_syslog:send(sock(recv_1_test, udpc), #{}))),
      ?_test(?assertMatch({ok, #{}}, passive(pid(recv_1_test, udps)))),
      ?_test(?assertMatch({error, _},
                          jhn_syslog:recv(sock(recv_1_test, tcpc)))),
      ?_test(?assertMatch({error, _},
                          jhn_syslog:recv(sock(recv_1_test, tcpc),
                                      [{timeout, 5}]))),
      ?_test(?assertMatch(passive, passify(pid(recv_1_test, tcps)))),
      ?_test(?assertMatch(ok, jhn_syslog:send(sock(recv_1_test, tcpc), #{}))),
      ?_test(?assertMatch(#{}, passive(pid(recv_1_test, tcps)))),
      ?_test(?assertMatch({error, _},
                          jhn_syslog:recv(sock(recv_1_test, tlsc)))),
      ?_test(?assertMatch({error, _},
                          jhn_syslog:recv(sock(recv_1_test, tlsc),
                                      [{timeout, 5}]))),
      ?_test(?assertMatch(passive, passify(pid(recv_1_test, tlss)))),
      ?_test(?assertMatch(ok, jhn_syslog:send(sock(recv_1_test, tlsc), #{}))),
      ?_test(?assertMatch(#{},passive(pid(recv_1_test, tlss)))),
      ?_test(?assertMatch({error, _}, jhn_syslog:recv(#transport{type = udp}))),
      ?_test(?assertMatch({error, _}, jhn_syslog:recv(#transport{type = tcp}))),
      ?_test(?assertMatch({error, _}, jhn_syslog:recv(#transport{type = tls})))
     ]}.

recv_2_test_() ->
    [?_test(?assertMatch({error, _},
                         jhn_syslog:recv(#transport{type = udp},
                                         [{timeout, 5}]))),
     ?_test(?assertMatch({error, _},
                         jhn_syslog:recv(jhn_syslog:open(), [{timeout, 5}]))),
     ?_test(?assertMatch({error, _},
                         jhn_syslog:recv(#transport{type = tcp},
                                         [{timeout, 5}]))),
     ?_test(?assertMatch({error, _},
                         jhn_syslog:recv(#transport{type = tls},
                                         [{timeout, 5}])))
    ].

%%--------------------------------------------------------------------
%% close/1
%%--------------------------------------------------------------------
close_1_test_() ->
    [?_test(?assertMatch(ok, jhn_syslog:close(jhn_syslog:open()))),
     ?_test(?assertEqual({error, function_clause},
                         jhn_syslog:close(#transport{type = udp}))),
     ?_test(?assertEqual({error, function_clause},
                         jhn_syslog:close(#transport{type = tcp}))),
     ?_test(?assertEqual({error, function_clause},
                         jhn_syslog:close(#transport{type = tcp_listen}))),
     ?_test(?assertEqual({error, function_clause},
                         jhn_syslog:close(#transport{type = tls}))),
     ?_test(?assertEqual({error, function_clause},
                         jhn_syslog:close(#transport{type = tls_listen})))
    ].

%%--------------------------------------------------------------------
%% setopts/2
%%--------------------------------------------------------------------
setopts_2_test_() ->
    [?_test(?assertMatch({error, _},
                         jhn_syslog:setopts(#transport{type = udp}, self()))),
     ?_test(?assertMatch({error, _},
                         jhn_syslog:setopts(#transport{type = tcp}, self()))),
     ?_test(?assertMatch({error, _},
                         jhn_syslog:setopts(#transport{type = tls}, self())))
    ].

%%--------------------------------------------------------------------
%% controlling_process/2
%%--------------------------------------------------------------------
controlling_process_2_test_() ->
    [?_test(?assertMatch({error, _},
                         jhn_syslog:controlling_process(#transport{type = udp},
                                                    self()))),
     ?_test(?assertEqual({error, function_clause},
                         jhn_syslog:controlling_process(#transport{type = tcp},
                                                    self()))),
     ?_test(
        ?assertEqual({error, function_clause},
                     jhn_syslog:controlling_process(
                       #transport{type = tcp_listen},
                       self()))),
     ?_test(?assertEqual({error, function_clause},
                         jhn_syslog:controlling_process(#transport{type = tls},
                                                    self()))),
     ?_test(
        ?assertEqual({error, function_clause},
                     jhn_syslog:controlling_process(
                       #transport{type = tls_listen},
                       self())))
    ].

%%--------------------------------------------------------------------
%% accept/1
%%--------------------------------------------------------------------
accept_1_test_() ->
    [?_test(?assertEqual({error, function_clause},
                         jhn_syslog:accept(#transport{type = tcp_listen}))),
     ?_test(?assertEqual({error, function_clause},
                         jhn_syslog:accept(#transport{type = tls_listen})))
    ].

%%--------------------------------------------------------------------
%% accept/2
%%--------------------------------------------------------------------
accept_2_test_() ->
    [?_test(?assertEqual({error, function_clause},
                         jhn_syslog:accept(#transport{type = tcp_listen},
                                       [{timeout, 500}]))),
     ?_test(?assertEqual({error, function_clause},
                         jhn_syslog:accept(#transport{type = tls_listen},
                                       [{timeout, 500}])))
    ].


%% ===================================================================
%% Encode/Decode
%% ===================================================================

%%--------------------------------------------------------------------
%% encode/2/decode/1
%%--------------------------------------------------------------------
encode_2_decode_1_test_() ->
    {inparallel,
     [?_test(?assertMatch(#{header := #{facility := user,
                                        severity := info,
                                        version := 1
                                       }},
                          jhn_syslog:decode(
                            iolist_to_binary(jhn_syslog:encode(#{}))))),
      ?_test(?assertMatch(#{header := #{facility := kern,
                                        severity := info,
                                        version := 1
                                       }},
                          jhn_syslog:decode(
                            iolist_to_binary(
                              jhn_syslog:encode(
                                #{header => #{facility => kern}},
                                [iolist]))))),
      ?_test(?assertMatch(#{header := #{facility := cron,
                                        severity := warning,
                                        version := 1
                                       }},
                          jhn_syslog:decode(
                            jhn_syslog:encode(#{header => #{facility => cron,
                                                        severity => warning}},
                                          [binary]),
                            []))),
      ?_test(?assertMatch(#{header := #{facility := user,
                                        severity := info,
                                        version := 2
                                       }},
                          jhn_syslog:decode(
                            jhn_syslog:encode(#{header => #{version => 2}},
                                          [binary])))),
      [?_test(?assertMatch(#{header := #{facility := Facility,
                                         severity := Severity,
                                         version := 1
                                        }},
                           jhn_syslog:decode(
                             jhn_syslog:encode(
                               #{header => #{facility => Facility,
                                             severity => Severity}},
                               [binary])))) ||
          Severity <- [emerg, alert, crit, err, warning, notice, info, debug],
          Facility <- [kern, user, mail, daemon, auth, syslog, lpr, news, uucp,
                       cron, authpriv, ftp, ntp, audit, console, cron2,
                       local0, local1, local2, local3, local4, local5,
                       local6, local7]],
      [?_test(
          ?assertMatch(#{header := #{time_stamp := Timestamp}},
                       jhn_syslog:decode(
                         jhn_syslog:encode(
                           #{header => #{time_stamp => Timestamp}},
                           [binary])))) ||
          Timestamp <- [{{2016, 4, 1}, {18, 11, 42}},
                        {{2016, 12, 30}, {18, 0, 42}},
                        {{1939, 9, 30}, {0, 11, 42}},
                        {{2042, 5, 17}, {18, 11, 0}}]],
      [?_test(
          ?assertMatch(
             #{header := #{time_stamp := {{2016, 4, 1}, {18, 11, 42}},
                           fraction := Fraction}},
             jhn_syslog:decode(
               jhn_syslog:encode(#{header =>
                                   #{time_stamp => {{2016, 4, 1}, {18, 11, 42}},
                                     fraction => Fraction}},
                             [binary, milli])))) ||
          Fraction <- [0, 1, 10, 12, 123]],
      [?_test(
          ?assertMatch(
             #{header := #{time_stamp := {{2016, 4, 1}, {18, 11, 42}},
                           offset_sign := Sign,
                           offset := Offset}},
             jhn_syslog:decode(
               jhn_syslog:encode(#{header =>
                                   #{time_stamp => {{2016, 4, 1}, {18, 11, 42}},
                                     offset_sign => Sign,
                                     offset => Offset}},
                             [binary])))) ||
          Sign <- ['-', '+'], Offset <- [{7, 30}, {0, 10}]],
      [?_test(
          ?assertMatch(
             #{header := #{time_stamp := {{2016, 4, 1}, {18, 11, 42}},
                           offset_sign := '-',
                           offset := {0, 0}}},
             jhn_syslog:decode(
               jhn_syslog:encode(#{header =>
                                   #{time_stamp => {{2016, 4, 1}, {18, 11, 42}},
                                     offset_sign => '-',
                                     offset => {0, 0}}},
                             [binary]))))],
      [?_test(
          ?assertMatch(
             #{header := #{time_stamp := {{2016, 4, 1}, {18, 11, 42}},
                           fraction := 123,
                           offset_sign := Sign,
                           offset := {2, 0}}},
             jhn_syslog:decode(
               jhn_syslog:encode(#{header =>
                                   #{time_stamp => {{2016, 4, 1}, {18, 11, 42}},
                                     fraction => 123,
                                     offset_sign => Sign,
                                     offset => {2, 0}}},
                             [binary, milli])))) ||
          Sign <- ['-', '+']],
      ?_test(
         ?assertMatch(
            #{header := #{time_stamp := {{2016, 4, 1}, {18, 11, 42}}}},
            jhn_syslog:decode(
              jhn_syslog:encode(#{header =>
                                  #{time_stamp => {{2016, 4, 1}, {18, 11, 42}},
                                    offset_sign => 'Z'}},
                            [binary])))),
      ?_test(
         ?assertMatch(#{header := #{host_name := <<"Host">>,
                                    app_name := <<"App">>,
                                    proc_id := <<"Proc">>,
                                    msg_id := <<"Msg">>}},
                      jhn_syslog:decode(
                        jhn_syslog:encode(#{header =>
                                            #{host_name => <<"Host">>,
                                              app_name => <<"App">>,
                                              proc_id => <<"Proc">>,
                                              msg_id => <<"Msg">>}},
                                      [binary])))),
      ?_test(
         ?assertMatch(#{structured := [{<<"structured@id">>, []}]},
                      jhn_syslog:decode(
                        jhn_syslog:encode(#{structured =>
                                            [{<<"structured@id">>, []}]},
                                      [binary])))),
      ?_test(
         ?assertMatch(#{structured := [{<<"structured@id">>,
                                        [{<<"a">>, <<"]\"1\\ ">>}]}]},
                      jhn_syslog:decode(
                        jhn_syslog:encode(#{structured =>
                                            [{<<"structured@id">>,
                                              [{<<"a">>, "]\"1\\ "}]}]},
                                      [binary])))),
      ?_test(
         ?assertMatch(#{structured := [{<<"structured@id">>,
                                        [{<<"a">>, <<"[1]">>},
                                         {<<"b">>, <<"\"2\"">>}]}]},
                      jhn_syslog:decode(
                        jhn_syslog:encode(#{structured =>
                                            [{<<"structured@id">>,
                                              [{<<"a">>, <<"[1]">>},
                                               {<<"b">>, <<"\"2\"">>}]}]},
                                      [binary])))),
      ?_test(
         ?assertMatch(#{structured := [{<<"foo@id">>,
                                        [{<<"a">>, <<"\\1">>},
                                         {<<"b">>, <<"2">>}]},
                                       {<<"bar@id">>,
                                        [{<<"a">>, <<"1">>},
                                         {<<"b">>, <<"2">>}]}]},
                      jhn_syslog:decode(
                        jhn_syslog:encode(#{structured =>
                                            [{<<"foo@id">>,
                                              [{<<"a">>, <<"\\1">>},
                                               {<<"b">>, <<"2">>}]},
                                             {<<"bar@id">>,
                                              [{<<"a">>, ["1"]},
                                               {<<"b">>, [<<"2">>]}]}]},
                                      [binary])))),
      ?_test(
         ?assertMatch(#{msg := #{content := <<"foo">>}},
                      jhn_syslog:decode(
                        jhn_syslog:encode(#{msg => #{content => "foo"}},
                                          [binary])))),
      ?_test(
         ?assertMatch(
            #{msg := #{content := <<"foo">>}},
            jhn_syslog:decode(jhn_syslog:encode(#{msg =>
                                              #{type => utf8,
                                                content => <<"foo">>}},
                                        [binary])))),
      ?_test(
         ?assertMatch(#{structured := [{<<"structured@id">>, []}],
                        msg := #{content := <<"foo">>}},
                      jhn_syslog:decode(
                        jhn_syslog:encode(#{structured =>
                                            [{<<"structured@id">>, []}],
                                        msg =>
                                            #{type => utf8,
                                              content => <<"foo">>}},
                                      [binary]))))
     ]}.

%% ===================================================================
%% Encoding
%% ===================================================================

%% ===================================================================
%% Decoding
%% ===================================================================

%% ===================================================================
%% Bad options
%% ===================================================================
bad_option_test_() ->
    [?_test(?assertError(badarg, jhn_syslog:open([oops])))].

%% ===================================================================
%% Internal functions.
%% ===================================================================

dir() -> code:lib_dir(jhn_stdlib, test).

cert(File) -> filename:join([dir(), "certs", File]).

-define(SSL, [{verify, verify_none},
              {cacertfile, cert("ca_certificate.pem")},
              {keyfile, cert("server_rsa_key.pem")},
              {certfile, cert("server_certificate.pem")}]).

server_start(tls, Port, Parent) ->
    Transport =
        jhn_syslog:open([server,
                     tls,
                     {port, Port},
                     {opts, ?SSL}]),
    Pid = spawn_link(fun() -> server(tls, Parent) end),
    ok = jhn_syslog:controlling_process(Transport, Pid),
    Pid ! {transport, Transport},
    Pid;
server_start(dtls, Port, Parent) ->
    Transport =
        jhn_syslog:open([server,
                     dtls,
                     {port, Port},
                     {opts, ?SSL}]),
    Pid = spawn_link(fun() -> server(dtls, Parent) end),
    ok = jhn_syslog:controlling_process(Transport, Pid),
    Pid ! {transport, Transport},
    Pid;
server_start(Type, Port, Parent) ->
    Transport = jhn_syslog:open([server, Type, {port, Port}]),
    Pid = spawn_link(fun() -> server(Type, Parent) end),
    ok = jhn_syslog:controlling_process(Transport, Pid),
    Pid ! {transport, Transport},
    Pid.

server(udp, Parent) ->
    Transport = receive {transport, Transport0} -> Transport0 end,
    server_loop(Parent, Transport);
server(_, Parent) ->
    TransportL = receive {transport, Transport0} -> Transport0 end,
    Transport = jhn_syslog:accept(TransportL),
    server_loop(Parent, TransportL, Transport).

server_loop(Parent, Transport) ->
    receive
        stop -> jhn_syslog:close(Transport);
        passify ->
            jhn_syslog:setopts(Transport, [{active, false}]),
            Parent ! passive,
            server_loop(Parent, Transport);
        {udp, _, _, _, Message} ->
            Parent ! {active, Message},
            server_loop(Parent, Transport);
        recv ->
            Result = jhn_syslog:recv(Transport),
            Parent ! {passive, Result},
            server_loop(Parent, Transport)
    end.

server_loop(Parent, TransportL, Transport) ->
    receive
        stop ->
            jhn_syslog:close(TransportL),
            jhn_syslog:close(Transport);
        passify ->
            jhn_syslog:setopts(Transport, [{active, false}]),
            Parent ! passive,
            server_loop(Parent, TransportL, Transport);
        {tcp, _, Message} ->
            Parent ! {active, Message},
            server_loop(Parent, TransportL, Transport);
        {ssl, _, Message} ->
            Parent ! {active, Message},
            server_loop(Parent, TransportL, Transport);
        recv ->
            {ok, Result, Transport1} = jhn_syslog:recv(Transport),
            Parent ! {passive, Result},
            server_loop(Parent, TransportL, Transport1)
    end.

server_stop(_, Pid) -> Pid ! stop.

active(_) -> receive {active, Packet} -> Packet after 500 -> timeout end.

passify(Server) ->
    Server ! passify,
    receive passive -> passive after 500 -> timeout end.

passive(Server) ->
    Server ! recv,
    receive {passive, Packet} -> Packet
    after 500 -> timeout end.

sock(Table, Client) -> element(2, hd(ets:lookup(Table, Client))).

pid(Table, Server) -> element(2, hd(ets:lookup(Table, Server))).

unframe(Type, Frame) ->
    jhn_syslog:decode(element(1, jhn_syslog:unframe(Type, Frame))).
