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
%%%   eunit unit tests for the json library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_amqp_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(BASIC, 60).

%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

%%--------------------------------------------------------------------
%% encode/1
%%--------------------------------------------------------------------
encode_1_test_() -> [].

%% ===================================================================
%% Decoding
%% ===================================================================

%%--------------------------------------------------------------------
%% decode/1
%%--------------------------------------------------------------------
decode_1_test_() -> [].

%% ===================================================================
%% Encode/Decode
%% ===================================================================

%%--------------------------------------------------------------------
%% decode(encode(Class, Method)
%%--------------------------------------------------------------------
encode_2_decode_test_() ->
    [
     [{"heartbeat",
       ?_test(?assertMatch({ok, #{frame := heartbeat}, _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(heartbeat, none)))))}],
     [{"connection " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := connection,
                              method := Method},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(connection, Method)))))} ||
         Method <- [start, tune, tune_ok, open, open_ok, close, close_ok]]
    ].

encode_3_decode_test_() ->
    ServerProperties = #{host => <<"coherentlight.se">>},
    [
     [{"content " ++ atom_to_list(Part),
       ?_test(?assertMatch({ok, #{frame := Part}, _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(content,
                                               Part,
                                               #{channel => 4711,
                                                 content_class => basic}))
                            )))} || Part <- [header, body]],
     {"connection start",
      ?_test(
         ?assertMatch({ok,
                       #{frame := method,
                         class := connection,
                         method := start,
                         server_properties := ServerProperties},
                       _},
                      jhn_amqp:decode(
                        iolist_to_binary(
                          jhn_amqp:encode(connection,
                                          start,
                                          #{server_properties =>
                                                ServerProperties})))))},
     [{"connection " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := connection,
                              method := Method,
                              response := <<"OPAQUE">>},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(connection,
                                               Method,
                                               #{response =>
                                                     <<"OPAQUE">>})))))}
      || Method <- [start_ok, secure_ok]],
     {"connection secure",
      ?_test(?assertMatch({ok,
                           #{frame := method,
                             class := connection,
                             method := secure,
                             challange := <<"OPAQUE">>},
                           _},
                          jhn_amqp:decode(
                            iolist_to_binary(
                              jhn_amqp:encode(connection,
                                              secure,
                                              #{challange =>
                                                    <<"OPAQUE">>})))))},
     [{"channel " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := channel,
                              channel := 1,
                              method := Method},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(channel,
                                               Method,
                                               #{channel => 1})))))} ||
         Method <- [open]],
     [{"channel " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := channel,
                              method := Method,
                              channel := 4711},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(channel,
                                               Method,
                                               #{channel => 4711})))))}
      || Method <- [open_ok, flow, flow_ok, close, close_ok]],
     [{"exchange " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := exchange,
                              method := Method,
                              channel := 4711,
                              exchange := <<"mix">>},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(exchange,
                                               Method,
                                               #{channel => 4711,
                                                 exchange => <<"mix">>
                                                })))))}
      || Method <- [declare, delete]],
     [{"exchange " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := exchange,
                              method := Method,
                              channel := 4711},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(exchange,
                                               Method,
                                               #{channel => 4711})))))}
      || Method <- [declare_ok, delete_ok]],
     [{"queue " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := queue,
                              method := Method,
                              channel := 4711,
                              queue := <<"mix">>},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(queue,
                                               Method,
                                               #{channel => 4711,
                                                 queue => <<"mix">>
                                                })))))}
      || Method <- [declare, declare_ok, purge, delete]],
     [{"queue " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := queue,
                              method := Method,
                              channel := 4711,
                              exchange := <<"mix">>},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(queue,
                                               Method,
                                               #{channel => 4711,
                                                 exchange => <<"mix">>
                                                })))))}
      || Method <- [bind]],
     [{"queue " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := queue,
                              method := Method,
                              channel := 4711,
                              queue := <<"mix">>,
                              exchange := <<"mix">>},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(queue,
                                               Method,
                                               #{channel => 4711,
                                                 queue => <<"mix">>,
                                                 exchange => <<"mix">>
                                                })))))}
      || Method <- [unbind]],
     [{"queue " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := queue,
                              method := Method,
                              channel := 4711},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(queue,
                                               Method,
                                               #{channel => 4711})))))}
      || Method <- [bind_ok, unbind_ok, purge_ok, delete_ok]],
     [{"basic " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := basic,
                              method := Method,
                              channel := 4711},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(basic,
                                               Method,
                                               #{channel => 4711})))))}
      || Method <- [qos, qos_ok,
                    publish, get_empty,
                    recover_async, recover, recover_ok]],
     [{"basic " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := basic,
                              method := Method,
                              channel := 4711,
                              queue := <<"mix">>},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(basic,
                                               Method,
                                               #{channel => 4711,
                                                 queue => <<"mix">>})))))}
      || Method <- [consume, get]],
     [{"basic " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := basic,
                              method := Method,
                              channel := 4711,
                              consumer_tag := <<"tag">>},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(basic,
                                               Method,
                                               #{channel => 4711,
                                                 consumer_tag =>
                                                     <<"tag">>})))))}
      || Method <- [consume_ok, cancel, cancel_ok]],
     [{"basic " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := basic,
                              method := Method,
                              channel := 4711,
                              code := 313},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(basic,
                                               Method,
                                               #{channel => 4711,
                                                 code => 313})))))}
      || Method <- [return]],
     [{"basic " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := basic,
                              method := Method,
                              channel := 4711,
                              consumer_tag := <<"foo">>,
                              delivery_tag := 100},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(basic,
                                               Method,
                                               #{channel => 4711,
                                                 consumer_tag => <<"foo">>,
                                                 delivery_tag => 100})))))}
      || Method <- [deliver]],
     [{"basic " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := basic,
                              method := Method,
                              channel := 4711,
                              delivery_tag := 100},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(basic,
                                               Method,
                                               #{channel => 4711,
                                                 delivery_tag => 100})))))}
      || Method <- [get_ok, ack, reject]],
     [{"tx " ++ atom_to_list(Method),
       ?_test(?assertMatch({ok,
                            #{frame := method,
                              class := tx,
                              method := Method,
                              channel := 4711},
                            _},
                           jhn_amqp:decode(
                             iolist_to_binary(
                               jhn_amqp:encode(tx,
                                               Method,
                                               #{channel => 4711})))))}
      || Method <- [select, select_ok, commit, commit_ok, rollback,rollback_ok]]
    ].

%% ===================================================================
%% Bad options
%% ===================================================================
bad_option_test_() -> [].

%% ===================================================================
%% Internal functions.
%% ===================================================================
