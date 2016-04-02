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
    [?_test(?assertMatch(ok, syslog:close(syslog:open([])))),
     ?_test(?assertMatch(ok, syslog:close(syslog:open([udp, ipv4])))),
     ?_test(?assertMatch(ok, syslog:close(syslog:open([udp, client])))),
     ?_test(?assertMatch(ok, syslog:close(syslog:open([udp, ipv6])))),
     ?_test(?assertEqual({error, econnrefused}, syslog:open([tcp]))),
     ?_test(?assertEqual({error, econnrefused}, syslog:open([tls])))
    ].


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
      ?_test(
         ?assertMatch(ok,
                      syslog:close(
                        syslog:open([tls, server, ipv6, {port, 6514}])))),
      ?_test(
         ?assertMatch(ok,
                      syslog:close(syslog:open([tls, server, {port, 6514}]))))
     ]}.

%%--------------------------------------------------------------------
%% send/2
%%--------------------------------------------------------------------
send_2_test_() ->
    ets:new(send_2_test, [named_table, public]),
    {setup,
     fun() -> {ok, Apps} = application:ensure_all_started(ssl),
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
      ?_test(
         ?assertMatch(ok,
                      syslog:send(element(2, hd(ets:lookup(send_2_test, udpc))),
                                  #{}))),
      ?_test(?assertMatch(#{}, syslog:decode(active(udp)))),
      ?_test(
         ?assertMatch(ok,
                      syslog:send(element(2, hd(ets:lookup(send_2_test, tcpc))),
                                  #{}))),
      ?_test(?assertMatch(#{},
                          syslog:decode(
                            element(1, syslog:unframe(tcp, active(tcp))))))
     %% ,
     %%  ?_test(
     %%     ?assertMatch(ok,
     %%                  syslog:send(element(2, hd(ets:lookup(send_2_test, tlsc))),
     %%                              #{})))
     ]}.

%%--------------------------------------------------------------------
%% close/1
%%--------------------------------------------------------------------
close_1_test_() ->
    [?_test(?assertMatch(ok, syslog:close(syslog:open()))),
     ?_test(?assertMatch(ok, syslog:close(syslog:open()))),
     ?_test(?assertError(function_clause, syslog:close({error, eaccess})))
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
                                        [{<<"a">>, <<"1">>}]}]},
                      syslog:decode(
                        syslog:encode(#{structured =>
                                            [{<<"structured@id">>,
                                              [{<<"a">>, <<"1">>}]}]},
                                      [binary])))),
      ?_test(
         ?assertMatch(#{structured := [{<<"structured@id">>,
                                        [{<<"a">>, <<"1">>},
                                         {<<"b">>, <<"2">>}]}]},
                      syslog:decode(
                        syslog:encode(#{structured =>
                                            [{<<"structured@id">>,
                                              [{<<"a">>, <<"1">>},
                                               {<<"b">>, <<"2">>}]}]},
                                      [binary])))),
      ?_test(
         ?assertMatch(#{structured := [{<<"foo@id">>,
                                        [{<<"a">>, <<"1">>},
                                         {<<"b">>, <<"2">>}]},
                                       {<<"bar@id">>,
                                        [{<<"a">>, <<"1">>},
                                         {<<"b">>, <<"2">>}]}]},
                      syslog:decode(
                        syslog:encode(#{structured =>
                                            [{<<"foo@id">>,
                                              [{<<"a">>, <<"1">>},
                                               {<<"b">>, <<"2">>}]},
                                             {<<"bar@id">>,
                                              [{<<"a">>, <<"1">>},
                                               {<<"b">>, <<"2">>}]}]},
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
%% bad_option_test_() ->
%%     [?_test(?IS_BADARG(json:encode({}, [Option]))) || Option <- ?BAD_OPTS] ++
%%         [?_test(?IS_BADARG(json:decode(<<"{}">>, [Option]))) ||
%%             Option <- ?BAD_OPTS].

%% ===================================================================
%% Internal functions.
%% ===================================================================

server_start(Type, Port, Parent) ->
    Transport = syslog:open([server, Type, {port, Port}]),
    Pid = spawn_link(fun() ->  server(Type, Parent) end),
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
        {udp, _, _, _, Message} ->
            Parent ! {active, Message},
            server_loop(Parent, Transport)
    end.

server_loop(Parent, TransportL, Transport) ->
    receive
        stop -> syslog:close(TransportL), syslog:close(Transport);
        {tcp, _, Message} ->
            Parent ! {active, Message},
            server_loop(Parent, TransportL, Transport)
    end.

server_stop(_, Pid) -> Pid ! stop.

active(_) ->
        receive
            {active, Packet} -> Packet
        after
            500 -> timeout
        end.
