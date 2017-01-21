%%==============================================================================
%% Copyright 2016-2017 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%% @copyright (C) 2016-2017, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(syslog_tests).
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
    [?_test(?assertMatch(#transport{}, syslog:open()))
    ].

%%--------------------------------------------------------------------
%% open/1
%%--------------------------------------------------------------------
open_1_client_test_() ->
    {setup,
     fun() ->
             {ok, Apps} = application:ensure_all_started(ssl),
             TCPS = {tcp, server_start(tcp, 1601, dummy)},
             TLSS = {tls, server_start(tls, 6514, dummy)},
             {Apps, [TCPS , TLSS]}
     end,
     fun({Apps, Servers}) ->
             [server_stop(Type, Server) || {Type, Server} <- Servers],
             [application:stop(App) || App <- Apps]
     end,
    [?_test(?assertMatch(ok, syslog:close(syslog:open([])))),
     ?_test(?assertMatch(ok, syslog:close(syslog:open([udp, ipv4])))),
     ?_test(?assertMatch(ok, syslog:close(syslog:open([udp, client])))),
     ?_test(?assertMatch(ok, syslog:close(syslog:open([udp, ipv6])))),
     ?_test(?assertEqual(ok,
                         syslog:close(
                           syslog:open([tcp,
                                        {destination, {127, 0, 0, 1}},
                                        {destination_port, 1601}])))),
     ?_test(?assertEqual(ok,
                         syslog:close(
                           syslog:open([tls,
                                        {destination, "127.0.0.1"},
                                        {destination_port, 6514}])))),
     ?_test(?assertEqual({error, {exit, badarg}},
                         syslog:open([{opts, [{ip, none}]}]))),
     ?_test(?assertMatch({error, _}, syslog:open([tcp]))),
     ?_test(?assertMatch({error, {exit, _}},
                         syslog:open([tcp, {opts, [{ip, none}]}]))),
     ?_test(?assertMatch({error, _},
                         syslog:open([tls,
                                      {destination, "::1"},
                                      {destination_port, 4711}])))
    ]}.

open_1_client_timeout_test_() ->
    {setup,
     fun() ->
             {ok, Apps} = application:ensure_all_started(ssl),
             TCPS = {tcp, server_start(tcp, 1601, dummy)},
             TLSS = {tls, server_start(tls, 6514, dummy)},
             {Apps, [TCPS , TLSS]}
     end,
     fun({Apps, Servers}) ->
             [server_stop(Type, Server) || {Type, Server} <- Servers],
             [application:stop(App) || App <- Apps]
     end,
    [?_test(?assertMatch(ok, syslog:close(syslog:open([])))),
     ?_test(?assertMatch(ok, syslog:close(syslog:open([udp, ipv4])))),
     ?_test(?assertMatch(ok, syslog:close(syslog:open([udp, client])))),
     ?_test(?assertMatch(ok, syslog:close(syslog:open([udp, ipv6])))),
     ?_test(?assertEqual(ok,
                         syslog:close(
                           syslog:open([tcp,
                                        {timeout, 500},
                                        {destination, {127, 0, 0, 1}},
                                        {destination_port, 1601}])))),
     ?_test(?assertEqual(ok,
                         syslog:close(
                           syslog:open([tls,
                                        {timeout, 500},
                                        {destination, {127, 0, 0, 1}},
                                        {destination_port, 6514}])))),
     ?_test(?assertMatch({error, _}, syslog:open([tcp, {timeout, 500}]))),
     ?_test(?assertMatch({error, {exit, _}},
                         syslog:open([tcp,
                                      {timeout, 500},
                                      {opts, [{ip, none}]}]))),
     ?_test(?assertMatch({error, _},
                         syslog:open([tls,
                                      {timeout, 500},
                                      {destination, {0, 0, 0, 0, 0, 0, 0, 1}},
                                      {destination_port, 4711}])))
    ]}.

open_1_server_test_() ->
    {setup,
     fun() -> application:ensure_all_started(ssl) end,
     fun({ok, Apps}) -> [application:stop(App) || App <- Apps] end,
     [?_test(
         ?assertMatch(ok, syslog:close(syslog:open([server, {port, 1154}])))),
      ?_test(
         ?assertMatch(ok,
                      syslog:close(syslog:open([udp, server, {port, 1154}])))),
      ?_test(
         ?assertMatch(ok,
                      syslog:close(
                        syslog:open([udp, ipv6, server, {port, 1154}])))),
      ?_test(
         ?assertMatch(ok,
                      syslog:close(syslog:open([tcp, server, {port, 1601}])))),
      ?_test(
         ?assertMatch(ok,
                      syslog:close(
                        syslog:open([tcp, server, ipv6, {port, 1601}])))),
      ?_test(?assertMatch(ok, syslog:close(syslog:open([tls, ipv6, server])))),
      ?_test(
         ?assertMatch(ok,
                      syslog:close(syslog:open([tls, server, {port, 6514}])))),
      ?_test(?assertEqual({error,eacces}, syslog:open([server, udp]))),
      ?_test(?assertEqual({error,eacces}, syslog:open([server, tcp]))),
      ?_test(?assertMatch({error, {exit, _}},
                          syslog:open([tcp, server, {opts, [{ip, none}]}]))),
      ?_test(?assertMatch({error, _},
                          syslog:open([tls, server, {opts, [{depth, -1}]}]))),
      ?_test(?assertMatch({error, _},
                          syslog:open([tls, server, {opts, [{ip, none}]}])))
     ]}.

%%--------------------------------------------------------------------
%% send/2
%%--------------------------------------------------------------------
send_2_test_() ->
    ets:new(send_2_test, [named_table, public]),
    {setup,
     fun() ->
             {ok, Apps} = application:ensure_all_started(ssl),
             UDPS = {udp, server_start(udp, 1154, send_2_test_)},
             UDPC = syslog:open([{destination, {127, 0, 0, 1}},
                                 {destination_port, 1154}]),
             true = ets:insert(send_2_test, {udpc, UDPC}),
             TCPS = {tcp, server_start(tcp, 1601, send_2_test_)},
             TCPC = syslog:open([tcp,
                                 {destination, {127, 0, 0, 1}},
                                 {destination_port, 1601}]),
             true = ets:insert(send_2_test, {tcpc, TCPC}),
             TLSS = {tls, server_start(tls, 6514, send_2_test_)},
             TLSC = syslog:open([tls,
                                 {opts, [{verify, verify_none}]},
                                 {destination, {127, 0, 0, 1}},
                                 {destination_port, 6514}]),
             true = ets:insert(send_2_test, {tlsc, TLSC}),
             {Apps, [UDPC, TCPC, TLSC], [UDPS, TCPS , TLSS]}
     end,
     fun({Apps, Clients, Servers}) ->
             [syslog:close(Client) || Client <- Clients],
             [server_stop(Type, Server) || {Type, Server} <- Servers],
             [application:stop(App) || App <- Apps]
     end,
     [?_test(?assertEqual(true, register(send_2_test_, self()))),
      ?_test(?assertMatch(ok, syslog:send(sock(send_2_test, udpc), #{}))),
      ?_test(?assertMatch(#{}, syslog:decode(active(udp)))),
      ?_test(?assertMatch(ok,
                          syslog:send(sock(send_2_test, udpc),
                                      #{},
                                      [{destination, {127, 0, 0, 1}},
                                       {destination_port, 1154}]))),
      ?_test(?assertMatch(#{}, syslog:decode(active(udp)))),
      ?_test(?assertMatch(ok, syslog:send(sock(send_2_test, tcpc), #{}))),
      ?_test(?assertMatch(#{},
                          syslog:decode(
                            element(1, syslog:unframe(tcp, active(tcp)))))),

      ?_test(?assertMatch(ok, syslog:send(sock(send_2_test, tlsc), #{}))),
      ?_test(?assertMatch(#{}, unframe(tls, active(tls)))),
      ?_test(
         ?assertEqual({error, function_clause},
                      syslog:send(#transport{type = udp}, #{}))),
      ?_test(
         ?assertEqual({error, function_clause},
                      syslog:send(#transport{type = udp},
                                  #{},
                                  [{destination, {127, 0, 0, 1}}]))),
      ?_test(
         ?assertEqual({error, function_clause},
                      syslog:send(#transport{type = tcp}, #{}))),
      ?_test(
         ?assertEqual({error, function_clause},
                      syslog:send(#transport{type = tls}, #{})))
     ]}.

%%--------------------------------------------------------------------
%% recv/0
%%--------------------------------------------------------------------
recv_1_test_() ->
    ets:new(recv_1_test, [named_table, public]),
    {setup,
     fun() ->
             {ok, Apps} = application:ensure_all_started(ssl),
             UDPS = server_start(udp, 1154, recv_1_test_),
             UDPC = syslog:open([{destination, "127.0.0.1"},
                                 {destination_port, 1154}]),
             true = ets:insert(recv_1_test, {udps, UDPS}),
             true = ets:insert(recv_1_test, {udpc, UDPC}),
             TCPS = server_start(tcp, 1601, recv_1_test_),
             TCPC = syslog:open([tcp,
                                 {destination, {127, 0, 0, 1}},
                                 {destination_port, 1601}]),
             true = ets:insert(recv_1_test, {tcps, TCPS}),
             true = ets:insert(recv_1_test, {tcpc, TCPC}),
             TLSS = server_start(tls, 6514, recv_1_test_),
             TLSC = syslog:open([tls,
                                 {opts, [{verify, verify_none}]},
                                 {destination, {127, 0, 0, 1}},
                                 {destination_port, 6514}]),
             true = ets:insert(recv_1_test, {tlss, TLSS}),
             true = ets:insert(recv_1_test, {tlsc, TLSC}),
             {Apps,
              [UDPC, TCPC, TLSC],
              [{udp, UDPS}, {tcp, TCPS}, {tls, TLSS}]}
     end,
     fun({Apps, Clients, Servers}) ->
             [syslog:close(Client) || Client <- Clients],
             [server_stop(Type, Server) || {Type, Server} <- Servers],
             [application:stop(App) || App <- Apps]
     end,
     [?_test(?assertEqual(true, register(recv_1_test_, self()))),
      ?_test(?assertMatch({error, _},
                          syslog:recv(sock(recv_1_test, udpc)))),
      ?_test(?assertMatch(passive, passify(pid(recv_1_test, udps)))),
      ?_test(?assertMatch(ok, syslog:send(sock(recv_1_test, udpc), #{}))),
      ?_test(?assertMatch({ok, #{}}, passive(pid(recv_1_test, udps)))),
      ?_test(?assertMatch({error, _},
                          syslog:recv(sock(recv_1_test, tcpc)))),
      ?_test(?assertMatch({error, _},
                          syslog:recv(sock(recv_1_test, tcpc),
                                      [{timeout, 5}]))),
      ?_test(?assertMatch(passive, passify(pid(recv_1_test, tcps)))),
      ?_test(?assertMatch(ok, syslog:send(sock(recv_1_test, tcpc), #{}))),
      ?_test(?assertMatch(#{}, passive(pid(recv_1_test, tcps)))),
      ?_test(?assertMatch({error, _},
                          syslog:recv(sock(recv_1_test, tlsc)))),
      ?_test(?assertMatch({error, _},
                          syslog:recv(sock(recv_1_test, tlsc),
                                      [{timeout, 5}]))),
      ?_test(?assertMatch(passive, passify(pid(recv_1_test, tlss)))),
      ?_test(?assertMatch(ok, syslog:send(sock(recv_1_test, tlsc), #{}))),
      ?_test(?assertMatch(#{},passive(pid(recv_1_test, tlss)))),
      ?_test(?assertMatch({error, _}, syslog:recv(#transport{type = udp}))),
      ?_test(?assertMatch({error, _}, syslog:recv(#transport{type = tcp}))),
      ?_test(?assertMatch({error, _}, syslog:recv(#transport{type = tls})))
     ]}.

recv_2_test_() ->
    [?_test(?assertMatch({error, _},
                         syslog:recv(#transport{type = udp}, [{timeout, 5}]))),
     ?_test(?assertMatch({error, _},
                         syslog:recv(syslog:open(), [{timeout, 5}]))),
     ?_test(?assertMatch({error, _},
                         syslog:recv(#transport{type = tcp}, [{timeout, 5}]))),
     ?_test(?assertMatch({error, _},
                         syslog:recv(#transport{type = tls}, [{timeout, 5}])))
    ].

%%--------------------------------------------------------------------
%% close/1
%%--------------------------------------------------------------------
close_1_test_() ->
    [?_test(?assertMatch(ok, syslog:close(syslog:open()))),
     ?_test(?assertEqual({error, function_clause},
                         syslog:close(#transport{type = udp}))),
     ?_test(?assertEqual({error, function_clause},
                         syslog:close(#transport{type = tcp}))),
     ?_test(?assertEqual({error, function_clause},
                         syslog:close(#transport{type = tcp_listen}))),
     ?_test(?assertEqual({error, function_clause},
                         syslog:close(#transport{type = tls}))),
     ?_test(?assertEqual({error, function_clause},
                         syslog:close(#transport{type = tls_listen})))
    ].

%%--------------------------------------------------------------------
%% setopts/2
%%--------------------------------------------------------------------
setopts_2_test_() ->
    [?_test(?assertEqual({error, function_clause},
                         syslog:setopts(#transport{type = udp}, self()))),
     ?_test(?assertEqual({error, function_clause},
                         syslog:setopts(#transport{type = tcp}, self()))),
     ?_test(?assertEqual({error, function_clause},
                         syslog:setopts(#transport{type = tls}, self())))
    ].

%%--------------------------------------------------------------------
%% controlling_process/2
%%--------------------------------------------------------------------
controlling_process_2_test_() ->
    [?_test(?assertEqual({error, function_clause},
                         syslog:controlling_process(#transport{type = udp},
                                                    self()))),
     ?_test(?assertEqual({error, function_clause},
                         syslog:controlling_process(#transport{type = tcp},
                                                    self()))),
     ?_test(
        ?assertEqual({error, function_clause},
                     syslog:controlling_process(#transport{type = tcp_listen},
                                                    self()))),
     ?_test(?assertEqual({error, function_clause},
                         syslog:controlling_process(#transport{type = tls},
                                                    self()))),
     ?_test(
        ?assertEqual({error, function_clause},
                     syslog:controlling_process(#transport{type = tls_listen},
                                                self())))
    ].

%%--------------------------------------------------------------------
%% accept/1
%%--------------------------------------------------------------------
accept_1_test_() ->
    [?_test(?assertEqual({error, function_clause},
                         syslog:accept(#transport{type = tcp_listen}))),
     ?_test(?assertEqual({error, function_clause},
                         syslog:accept(#transport{type = tls_listen})))
    ].

%%--------------------------------------------------------------------
%% accept/2
%%--------------------------------------------------------------------
accept_2_test_() ->
    [?_test(?assertEqual({error, function_clause},
                         syslog:accept(#transport{type = tcp_listen},
                                       [{timeout, 500}]))),
     ?_test(?assertEqual({error, function_clause},
                         syslog:accept(#transport{type = tls_listen},
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
                            []))),
      ?_test(?assertMatch(#{header := #{facility := user,
                                        severity := info,
                                        version := 2
                                       }},
                          syslog:decode(
                            syslog:encode(#{header => #{version => 2}},
                                          [binary])))),
      [?_test(?assertMatch(#{header := #{facility := Facility,
                                         severity := Severity,
                                         version := 1
                                        }},
                           syslog:decode(
                             syslog:encode(#{header => #{facility => Facility,
                                                         severity => Severity}},
                                           [binary])))) ||
          Severity <- [emerg, alert, crit, err, warning, notice, info, debug],
          Facility <- [kern, user, mail, daemon, auth, syslog, lpr, news, uucp,
                       cron, authpriv, ftp, ntp, audit, console, cron2,
                       local0, local1, local2, local3, local4, local5,
                       local6, local7]],
      [?_test(
          ?assertMatch(#{header := #{time_stamp := Timestamp}},
                       syslog:decode(
                         syslog:encode(#{header => #{time_stamp => Timestamp}},
                                       [binary])))) ||
          Timestamp <- [{{2016, 4, 1}, {18, 11, 42}},
                        {{2016, 12, 30}, {18, 0, 42}},
                        {{1939, 9, 30}, {0, 11, 42}},
                        {{2042, 5, 17}, {18, 11, 0}}]],
      [?_test(
          ?assertMatch(
             #{header := #{time_stamp := {{2016, 4, 1}, {18, 11, 42}},
                           fraction := Fraction}},
             syslog:decode(
               syslog:encode(#{header =>
                                   #{time_stamp => {{2016, 4, 1}, {18, 11, 42}},
                                     fraction => Fraction}},
                             [binary])))) ||
          Fraction <- [0, 1, 10, 12, 123, 6666]],
      [?_test(
          ?assertMatch(
             #{header := #{time_stamp := {{2016, 4, 1}, {18, 11, 42}},
                           offset_sign := Sign,
                           offset := Offset}},
             syslog:decode(
               syslog:encode(#{header =>
                                   #{time_stamp => {{2016, 4, 1}, {18, 11, 42}},
                                     offset_sign => Sign,
                                     offset => Offset}},
                             [binary])))) ||
          Sign <- ['-', '+'], Offset <- [{0, 0}, {7, 30}, {0, 10}]],
      [?_test(
          ?assertMatch(
             #{header := #{time_stamp := {{2016, 4, 1}, {18, 11, 42}},
                           fraction := 123,
                           offset_sign := Sign,
                           offset := {2, 0}}},
             syslog:decode(
               syslog:encode(#{header =>
                                   #{time_stamp => {{2016, 4, 1}, {18, 11, 42}},
                                     fraction => 123,
                                     offset_sign => Sign,
                                     offset => {2, 0}}},
                             [binary])))) ||
          Sign <- ['-', '+']],
      ?_test(
         ?assertMatch(
            #{header := #{time_stamp := {{2016, 4, 1}, {18, 11, 42}}}},
            syslog:decode(
              syslog:encode(#{header =>
                                  #{time_stamp => {{2016, 4, 1}, {18, 11, 42}},
                                    offset_sign => 'Z'}},
                            [binary])))),
      ?_test(
         ?assertMatch(#{header := #{host_name := <<"Host">>,
                                    app_name := <<"App">>,
                                    proc_id := <<"Proc">>,
                                    msg_id := <<"Msg">>}},
                      syslog:decode(
                        syslog:encode(#{header =>
                                            #{host_name => <<"Host">>,
                                              app_name => <<"App">>,
                                              proc_id => <<"Proc">>,
                                              msg_id => <<"Msg">>}},
                                      [binary])))),
      ?_test(
         ?assertMatch(#{structured := [{<<"structured@id">>, []}]},
                      syslog:decode(
                        syslog:encode(#{structured =>
                                            [{<<"structured@id">>, []}]},
                                      [binary])))),
      ?_test(
         ?assertMatch(#{structured := [{<<"structured@id">>,
                                        [{<<"a">>, <<"]\"1\\ ">>}]}]},
                      syslog:decode(
                        syslog:encode(#{structured =>
                                            [{<<"structured@id">>,
                                              [{<<"a">>, "]\"1\\ "}]}]},
                                      [binary])))),
      ?_test(
         ?assertMatch(#{structured := [{<<"structured@id">>,
                                        [{<<"a">>, <<"[1]">>},
                                         {<<"b">>, <<"\"2\"">>}]}]},
                      syslog:decode(
                        syslog:encode(#{structured =>
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
                      syslog:decode(
                        syslog:encode(#{structured =>
                                            [{<<"foo@id">>,
                                              [{<<"a">>, <<"\\1">>},
                                               {<<"b">>, <<"2">>}]},
                                             {<<"bar@id">>,
                                              [{<<"a">>, ["1"]},
                                               {<<"b">>, [<<"2">>]}]}]},
                                      [binary])))),
      ?_test(
         ?assertMatch(#{msg := #{content := <<"foo">>}},
                      syslog:decode(syslog:encode(#{msg => #{content => "foo"}},
                                                  [binary])))),
      ?_test(
         ?assertMatch(
            #{msg := #{content := <<"foo">>}},
            syslog:decode(syslog:encode(#{msg =>
                                              #{type => utf8,
                                                content => <<"foo">>}},
                                        [binary])))),
      ?_test(
         ?assertMatch(#{structured := [{<<"structured@id">>, []}],
                        msg := #{content := <<"foo">>}},
                      syslog:decode(
                        syslog:encode(#{structured =>
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
    [?_test(?assertError(badarg, syslog:open([oops])))].

%% ===================================================================
%% Internal functions.
%% ===================================================================

server_start(tls, Port, Parent) ->
    Transport =
        syslog:open([server,
                     tls,
                     {port, Port},
                     {opts, [{certfile, "../test/crt.pem"},
                             {keyfile, "../test/key.pem"}]}]),
    Pid = spawn_link(fun() -> server(tls, Parent) end),
    ok = syslog:controlling_process(Transport, Pid),
    Pid ! {transport, Transport},
    Pid;
server_start(Type, Port, Parent) ->
    Transport = syslog:open([server, Type, {port, Port}]),
    Pid = spawn_link(fun() -> server(Type, Parent) end),
    ok = syslog:controlling_process(Transport, Pid),
    Pid ! {transport, Transport},
    Pid.

server(udp, Parent) ->
    Transport = receive {transport, Transport0} -> Transport0 end,
    server_loop(Parent, Transport);
server(_, Parent) ->
    TransportL = receive {transport, Transport0} -> Transport0 end,
    Transport = syslog:accept(TransportL),
    server_loop(Parent, TransportL, Transport).

server_loop(Parent, Transport) ->
    receive
        stop -> syslog:close(Transport);
        passify ->
            syslog:setopts(Transport, [{active, false}]),
            Parent ! passive,
            server_loop(Parent, Transport);
        {udp, _, _, _, Message} ->
            Parent ! {active, Message},
            server_loop(Parent, Transport);
        recv ->
            Result = syslog:recv(Transport),
            Parent ! {passive, Result},
            server_loop(Parent, Transport)
    end.

server_loop(Parent, TransportL, Transport) ->
    receive
        stop ->
            syslog:close(TransportL),
            syslog:close(Transport);
        passify ->
            syslog:setopts(Transport, [{active, false}]),
            Parent ! passive,
            server_loop(Parent, TransportL, Transport);
        {tcp, _, Message} ->
            Parent ! {active, Message},
            server_loop(Parent, TransportL, Transport);
        {ssl, _, Message} ->
            Parent ! {active, Message},
            server_loop(Parent, TransportL, Transport);
        recv ->
            {ok, Result, Transport1} = syslog:recv(Transport),
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

unframe(Type, Frame) -> syslog:decode(element(1, syslog:unframe(Type, Frame))).
