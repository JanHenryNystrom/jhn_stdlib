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
%%% The protocol encoding/decoding for AMQP.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016-2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_amqp).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions

%% Connections

%% Wire Protocol
-export([encode/2, encode/3, encode/4,
         decode/1]).

%% Includes

%% Types

%% Exported Types
-export_types([]).

%% Records

%% Defines

-define(LITERAL_AMQP, <<"AMQP">>).
-define(PROTOCOL_ID, <<0>>).
-define(PROTOCOL_VERSION, <<0, 9, 1>>).
-define(PROTOCOL_HEADER, [?LITERAL_AMQP, ?PROTOCOL_ID, ?PROTOCOL_VERSION]).

%% -----------
%% Field sizes.
%% -----------

-define(OCTET, 8/unsigned-integer).
-define(SHORT, 16/unsigned-integer).
-define(LONG, 32/unsigned-integer).
-define(LLONG, 64/unsigned-integer).

%% -----------
%% FRAME TYPES.
%% -----------
-define(FRAME_METHOD, 1).
-define(FRAME_HEADER, 2).
-define(FRAME_BODY, 3).
-define(FRAME_HEARTBEAT, 8).
-define(FRAME_END, 206).
-define(FRAME_MIN_SIZE, 4096).


-define(REPLY_SUCCESS, 200).

-define(NO_CHAN, 0).

%% Deprecated
-define(WEIGHT, 0).
-define(CAPABILITIES, 0).
-define(INSIST, 0).
-define(KNOWN_HOSTS, 0).
-define(OUT_OF_BAND, 0).
-define(CHANNEL_ID, 0).
-define(TICKET, 0).
-define(AUTO_DELETE, false).
-define(INTERVAL, false).
-define(CLUSTER_ID, 0).

%% Class
-define(CONNECTION, 10).
-define(CHANNEL, 20).
-define(EXCHANGE, 40).
-define(QUEUE, 50).
-define(BASIC, 60).
-define(TX, 90).

%% -----------
%% Method
%% -----------

%% Connection
-define(CONNECTION_START, 10).
-define(CONNECTION_START_OK, 11).
-define(CONNECTION_SECURE, 20).
-define(CONNECTION_SECURE_OK, 21).
-define(CONNECTION_TUNE, 30).
-define(CONNECTION_TUNE_OK, 31).
-define(CONNECTION_OPEN, 40).
-define(CONNECTION_OPEN_OK, 41).
-define(CONNECTION_CLOSE, 50).
-define(CONNECTION_CLOSE_OK, 51).

%% Channel
-define(CHANNEL_OPEN, 10).
-define(CHANNEL_OPEN_OK, 11).
-define(CHANNEL_FLOW, 20).
-define(CHANNEL_FLOW_OK, 21).
-define(CHANNEL_CLOSE, 40).
-define(CHANNEL_CLOSE_OK, 41).

%% Exchange
-define(EXCHANGE_DECLARE, 10).
-define(EXCHANGE_DECLARE_OK, 11).
-define(EXCHANGE_DELETE, 20).
-define(EXCHANGE_DELETE_OK, 21).

%% Queue
-define(QUEUE_DECLARE, 10).
-define(QUEUE_DECLARE_OK, 11).
-define(QUEUE_BIND, 20).
-define(QUEUE_BIND_OK, 21).
-define(QUEUE_UNBIND, 50).
-define(QUEUE_UNBIND_OK, 51).
-define(QUEUE_PURGE, 30).
-define(QUEUE_PURGE_OK, 31).
-define(QUEUE_DELETE, 40).
-define(QUEUE_DELETE_OK, 41).

%% Basic
-define(BASIC_QOS, 10).
-define(BASIC_QOS_OK, 11).
-define(BASIC_CONSUME, 20).
-define(BASIC_CONSUME_OK, 21).
-define(BASIC_CANCEL, 30).
-define(BASIC_CANCEL_OK, 31).
-define(BASIC_PUBLISH, 40).
-define(BASIC_RETURN, 50).
-define(BASIC_DELIVER, 60).
-define(BASIC_GET, 70).
-define(BASIC_GET_OK, 71).
-define(BASIC_GET_EMPTY, 72).
-define(BASIC_ACK, 80).
-define(BASIC_REJECT, 90).
-define(BASIC_RECOVER_ASYNC, 100).
-define(BASIC_RECOVER, 110).
-define(BASIC_RECOVER_OK, 111).

%% Basic Properties
-define(BASIC_PROPERTIES,
        [{content_type, short},
         {content_encoding, short},
         {headers, table},
         {delivery_mode, octet},
         {priority, octet},
         {correlation_id, short},
         {reply_to, short},
         {expiration, short},
         {message_id, short},
         {timestamp, timestamp},
         {type, short},
         {user_id, short},
         {app_id, short},
         {cluster_id, short}
        ]).

%% TX
-define(TX_SELECT, 10).
-define(TX_SELECT_OK, 11).
-define(TX_COMMIT, 20).
-define(TX_COMMIT_OK, 21).
-define(TX_ROLLBACK, 30).
-define(TX_ROLLBACK_OK, 31).

%% Error code tables.
-define(ERROR_CODES_TABLE,
        [{311, 'content­too­large', channel},
         {313, 'no­consumers', channel},
         {320, 'connection­forced', connection},
         {402, 'invalid­path', connection},
         {403, 'access­refused', channel},
         {404, 'not­found', channel},
         {405, 'resource­locked', channel},
         {406, 'precondition­failed', channel},
         {501, 'frame­error', connection},
         {502, 'syntax­error', connection},
         {503, 'command­invalid', connection},
         {504, 'channel­error', connection},
         {505, 'unexpected­frame', connection},
         {506, 'resource­error', connection},
         {530, 'not­allowed', connection},
         {540, 'not­implemented', connection},
         {541, 'internal­error', connection}
        ]).

%% ===================================================================
%% Library functions 
%% ===================================================================


%% ===================================================================
%% Library functions Wire Protocol
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode() -> iodata()
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec encode(atom(), atom()) -> iodata().
%%--------------------------------------------------------------------
encode(Class, Method) -> encode(Class, Method, #{}).

%%--------------------------------------------------------------------
%% Function: encode() -> iodata()
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec encode(atom(), atom(), map()) -> iodata().
%%--------------------------------------------------------------------
encode(Class, Method, Args) -> encode(Class, Method, Args, #{}).

%%--------------------------------------------------------------------
%% Function: encode() -> iolist() | binary.
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec encode(atom(), atom(), map(), map()) -> iodata().
%%--------------------------------------------------------------------
encode(Class, Method, Args, Options) ->
    case maps:get(binary, Options, false) of
        true -> iolist_to_binary(do_encode(Class, Method, Args));
        false -> do_encode(Class, Method, Args)
    end.

%%--------------------------------------------------------------------
%% Function: decode() -> _.
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> ok.
%%--------------------------------------------------------------------
decode(Bin) -> do_decode(Bin).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

%%
%% -------------------------------------------------------------------
%% Protocol-Header
%% -------------------------------------------------------------------
%%
do_encode(protocol, header, _) -> ?PROTOCOL_HEADER;
%%
%% -------------------------------------------------------------------
%% Content-Header
%% -------------------------------------------------------------------
%%
do_encode(content, header, Args) ->
    Default = #{content_body_size => 0, properties => #{}},
    #{content_class := Class, %% Method frames class
      content_body_size := BodySize,
      properties := Properties,
      channel := Channel} = maps:merge(Default, Args),
    {PropertyFlags, PropertyList} = encode_properties(Class, Properties),
    Payload =
        [<<(encode_class(Class)):?SHORT, ?WEIGHT:?SHORT, BodySize:?LLONG>>,
         PropertyFlags,
         PropertyList],
    Size = iolist_size(Payload),
    [<<?FRAME_HEADER, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
%%
%% -------------------------------------------------------------------
%% Content-Body
%% -------------------------------------------------------------------
%%
do_encode(content, body, Args) ->
    Default = #{payload => <<>>},
    #{payload := Payload, channel := Channel} = maps:merge(Default, Args),
    Size = iolist_size(Payload),
    [<<?FRAME_BODY, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
%%
%% -------------------------------------------------------------------
%% Heartbeat
%% -------------------------------------------------------------------
%%
do_encode(heartbeat, _, _) ->
    [<<?FRAME_HEARTBEAT, 0:?SHORT, 0:?LONG, ?FRAME_END>>];
%%
%% -------------------------------------------------------------------
%% Methods
%% -------------------------------------------------------------------
%%
%% Connection
%%
do_encode(connection, start, Args) ->
    Default = #{major => 0,
                minor => 9,
                server_properties => #{},
                mechanisms => <<"PLAIN">>,
                locales => <<"en_US">>},
    #{major := Major,
      minor := Minor,
      server_properties := Props,
      mechanisms := Mechanisms,
      locales := Locales} = maps:merge(Default, Args),
    Payload = [<<?CONNECTION:?SHORT, ?CONNECTION_START:?SHORT, Major, Minor>>,
               encode_table(Props),
               encode_long_string(Mechanisms),
               encode_long_string(Locales)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, ?NO_CHAN:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(connection, start_ok, Args) ->
    Default = #{client_properties => #{},
                mechanism => <<"PLAIN">>,
                locale => <<"en_US">>},
    #{client_properties := Props,
      mechanism := Mechanism,
      response := Response,
      locale := Locale} = maps:merge(Default, Args),
    Payload = [<<?CONNECTION:?SHORT, ?CONNECTION_START_OK:?SHORT>>,
               encode_table(#{capabilities => Props}),
               encode_short_string(Mechanism),
               encode_long_string(Response),
               encode_short_string(Locale)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, ?NO_CHAN:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(connection, secure, #{challange := Challange}) ->
    Payload = [<<?CONNECTION:?SHORT, ?CONNECTION_SECURE:?SHORT>>,
               encode_long_string(Challange)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, 0:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(connection, secure_ok, #{response := Response}) ->
    Payload = [<<?CONNECTION:?SHORT, ?CONNECTION_SECURE_OK:?SHORT>>,
               encode_long_string(Response)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, ?NO_CHAN:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(connection, tune, Args) ->
    Default = #{channel_max => 0, frame_max => 0, heartbeat => 0},
    #{channel_max := ChannelMax,
      frame_max := FrameMax,
      heartbeat := HeartBeat} = maps:merge(Default, Args),
    Payload = <<?CONNECTION:?SHORT, ?CONNECTION_TUNE:?SHORT,
                ChannelMax:?SHORT, FrameMax:?LONG, HeartBeat:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, ?NO_CHAN:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(connection, tune_ok, Args) ->
    Default = #{channel_max => 0, frame_max => 0, heartbeat => 0},
    #{channel_max := ChannelMax,
      frame_max := FrameMax,
      heartbeat := HeartBeat} = maps:merge(Default, Args),
    Payload = <<?CONNECTION:?SHORT, ?CONNECTION_TUNE_OK:?SHORT,
                ChannelMax:?SHORT, FrameMax:?LONG, HeartBeat:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, ?NO_CHAN:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(connection, open, Args) ->
    Default = #{virtual_host => <<"/">>},
    #{virtual_host := VHost} = maps:merge(Default, Args),
    Payload = [<<?CONNECTION:?SHORT, ?CONNECTION_OPEN:?SHORT>>,
               encode_short_string(VHost),
               <<?CAPABILITIES, ?INSIST>>],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, ?NO_CHAN:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(connection, open_ok, _) ->
    Payload = <<?CONNECTION:?SHORT, ?CONNECTION_OPEN_OK:?SHORT, ?KNOWN_HOSTS>>,
    Size = byte_size(Payload),
    [<<?FRAME_METHOD, ?NO_CHAN:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(connection, close, Args) ->
    Default = #{code => ?REPLY_SUCCESS, text => <<>>, class => 0, method => 0},
    #{code := Code, text := Text, class := Class, method := Method} =
        maps:merge(Default, Args),
    Payload = [<<?CONNECTION:?SHORT, ?CONNECTION_CLOSE:?SHORT, Code:?SHORT>>,
               encode_short_string(Text),
               <<Class:?SHORT, Method:?SHORT>>],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, ?NO_CHAN:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(connection, close_ok, #{}) ->
    Payload = <<?CONNECTION:?SHORT, ?CONNECTION_CLOSE_OK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, ?NO_CHAN:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
%%
%% Channel
%%
do_encode(channel, open, #{channel := Channel}) ->
    Payload = <<?CHANNEL:?SHORT, ?CHANNEL_OPEN:?SHORT, ?OUT_OF_BAND>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(channel, open_ok, #{channel := Channel}) ->
    Payload = <<?CHANNEL:?SHORT, ?CHANNEL_OPEN_OK:?SHORT, ?CHANNEL_ID:?LONG>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(channel, flow, Args) ->
    Default = #{active => true},
    #{channel := Channel, active := Active} = maps:merge(Default, Args),
    Payload =
        [<<?CHANNEL:?SHORT, ?CHANNEL_FLOW:?SHORT>>, encode_boolean(Active)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(channel, flow_ok, Args) ->
    Default = #{active => true},
    #{channel := Channel, active := Active} = maps:merge(Default, Args),
    Payload =
        [<<?CHANNEL:?SHORT, ?CHANNEL_FLOW_OK:?SHORT>>, encode_boolean(Active)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(channel, close, Args) ->
    Default = #{code => ?REPLY_SUCCESS, text => <<>>, class => 0, method => 0},
    #{channel := Channel,
      code := Code,
      text := Text,
      class := Class,
      method := Method} = maps:merge(Default, Args),
    Payload = [<<?CHANNEL:?SHORT, ?CHANNEL_CLOSE:?SHORT, Code:?SHORT>>,
               encode_short_string(Text),
               <<Class:?SHORT, Method:?SHORT>>],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(channel, close_ok, #{channel := Channel}) ->
    Payload = <<?CHANNEL:?SHORT, ?CHANNEL_CLOSE_OK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
%%
%% Exchange
%%
do_encode(exchange, declare, Args) ->
    Default = #{type => <<"direct">>,
                passive => false,
                durable => false,
                no_wait => false,
                arguments => #{}},
    #{channel := Channel,
      exchange := Exchange,
      type := Type,
      passive := Passive,
      durable := Durable,
      no_wait := NoWait,
      arguments := Arguments} = maps:merge(Default, Args),
    Payload = [<<?EXCHANGE:?SHORT, ?EXCHANGE_DECLARE:?SHORT, ?TICKET:?SHORT>>,
               encode_short_string(Exchange),
               encode_short_string(Type),
               encode_flags([Passive, Durable, ?AUTO_DELETE, ?INTERVAL,NoWait]),
               encode_table(Arguments)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(exchange, declare_ok, #{channel := Channel}) ->
    Payload = <<?EXCHANGE:?SHORT, ?EXCHANGE_DECLARE_OK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(exchange, delete, Args) ->
    Default = #{if_unused => false,
                no_wait => false},
    #{channel := Channel,
      exchange := Exchange,
      if_unused := IfUnused,
      no_wait := NoWait} = maps:merge(Default, Args),
    Payload = [<<?EXCHANGE:?SHORT, ?EXCHANGE_DELETE:?SHORT, ?TICKET:?SHORT>>,
               encode_short_string(Exchange),
               encode_flags([IfUnused, NoWait])],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(exchange, delete_ok, #{channel := Channel}) ->
    Payload = <<?EXCHANGE:?SHORT, ?EXCHANGE_DELETE_OK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
%%
%% Queue
%%
do_encode(queue, declare, Args) ->
    Default = #{passive => false,
                durable => false,
                exclusive => false,
                auto_delete => false,
                no_wait => false,
                arguments => #{}},
    #{channel := Channel,
      queue := Queue,
      passive := Passive,
      durable := Durable,
      exclusive := Exclusive,
      auto_delete := AutoDelete,
      no_wait := NoWait,
      arguments := Arguments} = maps:merge(Default, Args),
    Payload = [<<?QUEUE:?SHORT, ?QUEUE_DECLARE:?SHORT, ?TICKET:?SHORT>>,
               encode_short_string(Queue),
               encode_flags([Passive, Durable, Exclusive, AutoDelete, NoWait]),
               encode_table(Arguments)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(queue, declare_ok, Args) ->
    Default = #{message_count => 0, consumer_count => 0},
    #{channel := Channel,
      queue := Queue,
      message_count := Msgs,
      consumer_count := Consumers} = maps:merge(Default, Args),
    Payload = [<<?QUEUE:?SHORT, ?QUEUE_DECLARE_OK:?SHORT>>,
               encode_short_string(Queue),
               <<Msgs:?LONG, Consumers:?LONG>>],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(queue, bind, Args) ->
    Default = #{queue => <<>>,
                routing_key => <<>>,
                no_wait => false,
                arguments => #{}},
    #{channel := Channel,
      queue := Queue,
      exchange := Exchange,
      routing_key := RoutingKey,
      no_wait := NoWait,
      arguments := Arguments} = maps:merge(Default, Args),
    Payload = [<<?QUEUE:?SHORT, ?QUEUE_BIND:?SHORT, ?TICKET:?SHORT>>,
               encode_short_string(Queue),
               encode_short_string(Exchange),
               encode_short_string(RoutingKey),
               encode_flags([NoWait]),
               encode_table(Arguments)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(queue, bind_ok, #{channel := Channel}) ->
    Payload = <<?QUEUE:?SHORT, ?QUEUE_BIND_OK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(queue, unbind, Args) ->
    Default = #{routing_key => <<>>, arguments => #{}},
    #{channel := Channel,
      queue := Queue,
      exchange := Exchange,
      routing_key := RoutingKey,
      arguments := Arguments} = maps:merge(Default, Args),
    Payload = [<<?QUEUE:?SHORT, ?QUEUE_UNBIND:?SHORT, ?TICKET:?SHORT>>,
               encode_short_string(Queue),
               encode_short_string(Exchange),
               encode_short_string(RoutingKey),
               encode_table(Arguments)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(queue, unbind_ok, #{channel := Channel}) ->
    Payload = <<?QUEUE:?SHORT, ?QUEUE_UNBIND_OK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(queue, purge, Args) ->
    Default = #{no_wait => false},
    #{channel := Channel,
      queue := Queue,
      no_wait := NoWait} = maps:merge(Default, Args),
    Payload = [<<?QUEUE:?SHORT, ?QUEUE_PURGE:?SHORT, ?TICKET:?SHORT>>,
               encode_short_string(Queue),
               encode_flags([NoWait])],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(queue, purge_ok, Args) ->
    Default = #{message_count => 0},
    #{channel := Channel, message_count := Msgs} = maps:merge(Default, Args),
    Payload = <<?QUEUE:?SHORT, ?QUEUE_PURGE_OK:?SHORT, Msgs:?LONG>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(queue, delete, Args) ->
    Default = #{if_unused => false, if_emty => false, no_wait => false},
    #{channel := Channel,
      queue := Queue,
      if_unused := IfUnused,
      if_emty := IfEmpty,
      no_wait := NoWait} = maps:merge(Default, Args),
    Payload = [<<?QUEUE:?SHORT, ?QUEUE_DELETE:?SHORT, ?TICKET:?SHORT>>,
               encode_short_string(Queue),
               encode_flags([IfUnused, IfEmpty, NoWait])],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(queue, delete_ok, Args) ->
    Default = #{message_count => 0},
    #{channel := Channel, message_count := Msgs} = maps:merge(Default, Args),
    Payload = <<?QUEUE:?SHORT, ?QUEUE_DELETE_OK:?SHORT, Msgs:?LONG>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
%%
%% Basic
%%
do_encode(basic, qos, Args) ->
    Default = #{prefetch_size => 0, prefetch_count => 0, global => false},
    #{channel := Channel,
      prefetch_size := PrefetchSize,
      prefetch_count := PrefetchCount,
      global := Global} = maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT,?BASIC_QOS:?SHORT,
                 PrefetchSize:?LONG, PrefetchCount:?SHORT>>,
               encode_flags([Global])],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, qos_ok, #{channel := Channel}) ->
    Payload = <<?BASIC:?SHORT, ?BASIC_QOS_OK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, consume, Args) ->
    Default = #{consumer_tag => <<>>,
                no_local => false,
                no_ack => false,
                exclusive => false,
                no_wait => false,
                arguments => #{}},
    #{channel := Channel,
      queue := Queue,
      consumer_tag := Tag,
      no_local := NoLocal,
      no_ack := NoAck,
      exclusive := Exclusive,
      no_wait := NoWait,
      arguments := Arguments} = maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT, ?BASIC_CONSUME:?SHORT, ?TICKET:?SHORT>>,
               encode_short_string(Queue),
               encode_short_string(Tag),
               encode_flags([NoLocal, NoAck, Exclusive, NoWait]),
               encode_table(Arguments)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, consume_ok, #{channel := Channel, consumer_tag := Tag}) ->
    Payload = [<<?BASIC:?SHORT, ?BASIC_CONSUME_OK:?SHORT>>,
               encode_short_string(Tag)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, cancel, Args) ->
    Default = #{no_wait => false},
    #{channel := Channel,
      consumer_tag := Tag,
      no_wait := NoWait} = maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT, ?BASIC_CANCEL:?SHORT>>,
               encode_short_string(Tag), encode_flags([NoWait])],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, cancel_ok, #{channel := Channel, consumer_tag := Tag}) ->
    Payload = [<<?BASIC:?SHORT, ?BASIC_CANCEL_OK:?SHORT>>,
               encode_short_string(Tag)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, publish, Args) ->
    Default = #{exchange => <<>>,
                routing_key => <<>>,
                mandatory => false,
                immediate => false},
    #{channel := Channel,
      exchange := Exchange,
      routing_key := RoutingKey,
      mandatory := Mandatory,
      immediate := Immediate} = maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT, ?BASIC_PUBLISH:?SHORT, ?TICKET:?SHORT>>,
               encode_short_string(Exchange),
               encode_short_string(RoutingKey),
               encode_flags([Mandatory, Immediate])],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, return, Args) ->
    Default = #{text => <<>>, exchange => <<>>, routing_key => <<>>},
    #{channel := Channel,
      code := Code,
      text := Text,
      exchange := Exchange,
      routing_key := RoutingKey} = maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT, ?BASIC_RETURN:?SHORT, Code:?SHORT>>,
               encode_short_string(Text),
               encode_short_string(Exchange),
               encode_short_string(RoutingKey)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, deliver, Args) ->
    Default = #{redelivered => false,  exchange => <<>>, routing_key => <<>>},
    #{channel := Channel,
      consumer_tag := ConsumerTag,
      delivery_tag := DeliveryTag,
      redelivered := Redelivered,
      exchange := Exchange,
      routing_key := RoutingKey} = maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT, ?BASIC_DELIVER:?SHORT>>,
               encode_short_string(ConsumerTag),
               <<DeliveryTag:?LLONG>>,
               encode_flags([Redelivered]),
               encode_short_string(Exchange),
               encode_short_string(RoutingKey)],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, get, Args) ->
    Default = #{no_ack => false},
    #{channel := Channel,
      queue := Queue,
      no_ack := NoAck} = maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT, ?BASIC_GET:?SHORT, ?TICKET:?SHORT>>,
               encode_short_string(Queue), encode_flags([NoAck])],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, get_ok, Args) ->
    Default = #{redelivered => false,
                exchange => <<>>,
                routing_key => <<>>,
                message_count => 0},
    #{channel := Channel,
      delivery_tag := DeliveryTag,
      redelivered := Redelivered,
      exchange := Exchange,
      routing_key := RoutingKey,
      message_count := Count} = maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT, ?BASIC_GET_OK:?SHORT, DeliveryTag:?LLONG>>,
               encode_flags([Redelivered]),
               encode_short_string(Exchange),
               encode_short_string(RoutingKey),
               <<Count:?SHORT>>],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, get_empty, #{channel := Channel}) ->
    Payload = <<?BASIC:?SHORT, ?BASIC_GET_EMPTY:?SHORT, ?CLUSTER_ID>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, ack, Args) ->
    Default = #{multiple => false},
    #{channel := Channel, delivery_tag := DeliveryTag, multiple := Multiple} =
        maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT, ?BASIC_ACK:?SHORT, DeliveryTag:?LLONG>>,
               encode_flags([Multiple])],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, reject, Args) ->
    Default = #{requeue => false},
    #{channel := Channel, delivery_tag := DeliveryTag, requeue := Requeue} =
        maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT, ?BASIC_REJECT:?SHORT, DeliveryTag:?LLONG>>,
               encode_flags([Requeue])],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, recover_async, Args) ->
    Default = #{requeue => false},
    #{channel := Channel, requeue := Requeue} = maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT, ?BASIC_RECOVER_ASYNC:?SHORT>>,
               encode_flags([Requeue])],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, recover, Args) ->
    Default = #{requeue => false},
    #{channel := Channel, requeue := Requeue} = maps:merge(Default, Args),
    Payload = [<<?BASIC:?SHORT, ?BASIC_RECOVER:?SHORT>>,
               encode_flags([Requeue])],
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(basic, recover_ok, #{channel := Channel}) ->
    Payload = <<?BASIC:?SHORT, ?BASIC_RECOVER_OK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
%%
%% Connection
%%
do_encode(tx, select, #{channel := Channel}) ->
    Payload = <<?TX:?SHORT, ?TX_SELECT:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(tx, select_ok, #{channel := Channel}) ->
    Payload = <<?TX:?SHORT, ?TX_SELECT_OK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(tx, commit, #{channel := Channel}) ->
    Payload = <<?TX:?SHORT, ?TX_COMMIT:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(tx, commit_ok, #{channel := Channel}) ->
    Payload = <<?TX:?SHORT, ?TX_COMMIT_OK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(tx, rollback, #{channel := Channel}) ->
    Payload = <<?TX:?SHORT, ?TX_ROLLBACK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>];
do_encode(tx, rollback_ok, #{channel := Channel}) ->
    Payload = <<?TX:?SHORT, ?TX_ROLLBACK_OK:?SHORT>>,
    Size = iolist_size(Payload),
    [<<?FRAME_METHOD, Channel:?SHORT, Size:?LONG>>, Payload, <<?FRAME_END>>].

encode_class(connection) -> ?CONNECTION;
encode_class(channel) -> ?CHANNEL;
encode_class(exchange) -> ?EXCHANGE;
encode_class(queue) -> ?QUEUE;
encode_class(basic) -> ?BASIC;
encode_class(tx) -> ?TX.

encode_properties(_, #{}) -> {<<0:?SHORT>>, <<>>};
encode_properties(basic, Properties) ->
    encode_properties(?BASIC_PROPERTIES, Properties, <<>>, <<>>);
encode_properties(_, _) ->
    erlang:error(badarg).

encode_properties([], _, Flags, List) -> {<<Flags/binary, 0:2>>, List};
encode_properties([{Name, Type} | T], Properties, Flags, List) ->
    case maps:get(Name, Properties, undefined) of
        undefined ->
            encode_properties(T, Properties, <<Flags/binary, 0:1>>, List);
        Value ->
            List1 = encode_property(Type, Value, List),
            encode_properties(T, Properties, <<Flags/binary, 1:1>>, List1)
    end.

encode_property(octet, Octet, List) -> <<List/binary, Octet:?OCTET>>;
encode_property(timestamp, Stamp, List) -> <<List/binary, Stamp:?LLONG>>;
encode_property(short, String, List) ->
    <<List/binary, (encode_short_string(String))/binary>>;
encode_property(table, Table, List) ->
    <<List/binary, (encode_table(Table))/binary>>.

encode_table(Map) when map_size(Map) == 0 -> <<0:?LONG>>;
encode_table(Map) ->
    Encoded  = maps:fold(fun encode_table/3, [], Map),
    [<<(iolist_size(Encoded)):?LONG>>, Encoded].

encode_table(Key, Value, Acc) when is_atom(Key) ->
    String = atom_to_binary(Key, utf8),
    [encode_short_string(String), encode_table_elt(Value) | Acc];
encode_table(Key, Value, Acc) when is_binary(Key) ->
    [encode_short_string(Key), encode_table_elt(Value) | Acc].

encode_table_elt(true) -> <<$t, 1:8/unsigned-integer>>;
encode_table_elt(false) -> <<$t, 0:8/unsigned-integer>>;
encode_table_elt(I) when I =< 255, I >= 0 -> <<$B, I:8/unsigned-integer>>;
encode_table_elt(I) when I < 0, I >= -128 -> <<$b, I:8/signed-integer>>;
encode_table_elt(I) when I =< 65535, I >= 0 -> <<$u, I:16/unsigned-integer>>;
encode_table_elt(I) when I < 0, I >= -32768 -> <<$U, I:16/signed-integer>>;
encode_table_elt(I) when I =< 4294967295, I >= 0 ->
    <<$i, I:32/unsigned-integer>>;
encode_table_elt(I) when I < 0, I >= -2147483648 ->
    <<$I, I:32/signed-integer>>;
encode_table_elt(I) when I =< 18446744073709551615, I >= 0 ->
    <<$l, I:64/unsigned-integer>>;
encode_table_elt(I) when I < 0, I >= -9223372036854775808 ->
    <<$L, I:64/signed-integer>>;
encode_table_elt(D) when is_float(D) ->
    <<$d, D:64/float>>;
encode_table_elt({decimal, Scale, Value}) ->
    <<$D, Scale, Value:32/signed-integer>>;
encode_table_elt(String) when is_binary(String) ->
    case byte_size(String) of
        Size when Size < 256 -> <<$s, Size, String/binary>>;
        Size -> <<$S, Size:?LONG, String/binary>>
    end;
encode_table_elt(Table = #{}) ->
    [<<$F>>, encode_table(Table)];
encode_table_elt(Array = [_| _]) ->
    Encoded = [encode_table_elt(Elt) || Elt <- Array],
    [<<$A, (iolist_size(Encoded)):?LONG>>, Encoded];
encode_table_elt({timestamp, I}) ->
    <<$T, I:64/unsigned-integer>>;
encode_table_elt(undefined) ->
    <<$V>>.

encode_short_string(String) -> <<(byte_size(String)), String/binary>>.

encode_long_string(String) -> <<(byte_size(String)):?LONG, String/binary>>.

encode_boolean(true) -> <<1>>;
encode_boolean(false) -> <<0>>.

encode_flags(Flags) ->
    Padding = 8 - (length(Flags) rem 8),
    Flags1 = << <<(case X of true -> 1; _ -> 0 end):1>> ||
                 X <- lists:reverse(Flags) >>,
    <<0:Padding, Flags1/bits>>.

%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(<<?FRAME_HEARTBEAT, 0:?SHORT, 0:?LONG, ?FRAME_END, T/binary>>) ->
    {ok, #{frame => heartbeat}, T};
do_decode(<<?FRAME_HEADER, Chan:?SHORT, Size:?LONG,
            Payload:Size/bytes, ?FRAME_END, T/binary>>) ->
    <<Class:?SHORT,
      ?WEIGHT:?SHORT,
      BodySize:?LLONG,
      Flags:2/binary,
      List/binary>> = Payload,
    {ok,
     #{frame => header,
       channel => Chan,
       class => decode_class(Class),
       content_body_size => BodySize,
       properties => decode_properties(Class, Flags, List)},
     T};
do_decode(<<?FRAME_BODY, Chan:?SHORT, Size:?LONG,
            Payload:Size/bytes, ?FRAME_END, T/binary>>) ->
    {ok, #{frame => body, channel => Chan, payload => Payload}, T};
do_decode(<<?FRAME_METHOD, Chan:?SHORT, Size:?LONG,
            Payload:Size/bytes, ?FRAME_END, T/binary>>) ->
    <<Class:?SHORT, Method:?SHORT, Args/binary>> = Payload,
    {ok, maps:put(channel, Chan, decode_method(Class, Method, Args)), T};
do_decode(<<?FRAME_HEADER, _:? SHORT, Size:?LONG, T/binary>>)
  when byte_size(T) =< Size ->
    {more, Size + 1 - byte_size(T)};
do_decode(<<?FRAME_BODY, _:?SHORT, Size:?LONG, T/binary>>)
  when byte_size(T) =< Size ->
    {more, Size + 1 - byte_size(T)};
do_decode(<<?FRAME_METHOD, _:?SHORT, Size:?LONG, T/binary>>)
  when byte_size(T) =< Size ->
    {more, Size + 1 - byte_size(T)};
do_decode(Bin) when byte_size(Bin) < 8 ->
    {more, 8 - byte_size(Bin)};
do_decode(_)  ->
    {more, 0}.

decode_class(?CONNECTION) -> connection;
decode_class(?CHANNEL) -> channel;
decode_class(?EXCHANGE) -> exchange;
decode_class(?QUEUE) -> queue;
decode_class(?BASIC) -> basic;
decode_class(?TX) -> tx.


decode_properties(_, <<0:16>>, <<>>) ->#{};
decode_properties(basic, Flags, List) ->
    decode_properties(?BASIC_PROPERTIES, Flags, List, #{});
decode_properties(_, _, _) ->
    erlang:error(badarg).

decode_properties([], <<>>, <<>>, Properties) -> Properties;
decode_properties([_ | T], <<0:1, Flags/binary>>, List, Properties) ->
    decode_properties(T, Flags, List, Properties);
decode_properties([{Name, Type} | T], <<_:1, Flags/binary>>, List,Properties) ->
    {Value, List1} = decode_property(Type, List),
    decode_properties(T, Flags, List1, maps:put(Name, Value, Properties)).

decode_property(octet, <<Octet:?OCTET, T/binary>>) -> {Octet, T};
decode_property(timestamp, <<Stamp:?LLONG, T/binary>>) -> {Stamp, T};
decode_property(short, List) -> decode_short_string(List);
decode_property(table, List) -> decode_table(List).

%%
%% Connection
%%
decode_method(?CONNECTION, ?CONNECTION_START, <<Major, Minor, T/binary>>) ->
    {Props, T1} = decode_table(T),
    {Mechanisms, T2} = decode_long_string(T1),
    {Locales, <<>>} = decode_long_string(T2),
    #{frame => method,
      class => connection,
      method => start,
      major => Major,
      minor => Minor,
      server_properties => Props,
      mechanisms => Mechanisms,
      locales => Locales};
decode_method(?CONNECTION, ?CONNECTION_START_OK, Args) ->
    {Props, T1} = decode_table(Args),
    {Mechanism, T2} = decode_short_string(T1),
    {Response, T3} = decode_long_string(T2),
    {Locale, <<>>} = decode_short_string(T3),
    #{frame => method,
      class => connection,
      method => start_ok,
      client_properties => Props,
      mechanism => Mechanism,
      response => Response,
      locale => Locale};
decode_method(?CONNECTION, ?CONNECTION_SECURE, Args) ->
    {Challange, <<>>} = decode_long_string(Args),
    #{frame => method,
      class => connection,
      method => secure,
      challange => Challange};
decode_method(?CONNECTION, ?CONNECTION_SECURE_OK, Args) ->
    {Response, <<>>} = decode_long_string(Args),
    #{frame => method,
      class => connection,
      method => secure_ok,
      response => Response};
decode_method(?CONNECTION, ?CONNECTION_TUNE, Args) ->
    <<ChannelMax:?SHORT, FrameMax:?LONG, HeartBeat:?SHORT>> = Args,
    #{frame => method,
      class => connection,
      method => tune,
      channel_max => ChannelMax,
      frame_max => FrameMax,
      heartbeat => HeartBeat};
decode_method(?CONNECTION, ?CONNECTION_TUNE_OK, Args) ->
    <<ChannelMax:?SHORT, FrameMax:?LONG, HeartBeat:?SHORT>> = Args,
    #{frame => method,
      class => connection,
      method => tune_ok,
      channel_max => ChannelMax,
      frame_max => FrameMax,
      heartbeat => HeartBeat};
decode_method(?CONNECTION, ?CONNECTION_OPEN, Args) ->
    {VHost, T} = decode_short_string(Args),
    {<<>>, T1} = decode_short_string(T),
    {false, <<>>} = decode_boolean(T1),
    #{frame => method,
      class => connection,
      method => open,
      virtual_host => VHost};
decode_method(?CONNECTION, ?CONNECTION_OPEN_OK, Args) ->
    {<<>>, <<>>} = decode_short_string(Args),
    #{frame => method, class => connection, method => open_ok};
decode_method(?CONNECTION, ?CONNECTION_CLOSE, Args) ->
    <<Code:?SHORT, T/binary>> = Args,
    {Text, <<Class:?SHORT, Method:?SHORT>>} = decode_short_string(T),
    #{frame => method,
      class => connection,
      method => close,
      code => Code,
      text => Text,
      failing_class => Class,
      failing_method => Method};
decode_method(?CONNECTION, ?CONNECTION_CLOSE_OK, <<>>) ->
    #{frame => method, class => connection, method => close_ok};
%%
%% Channel
%%
decode_method(?CHANNEL, ?CHANNEL_OPEN, Args) ->
    {<<>>, <<>>} = decode_short_string(Args),
    #{frame => method, class => channel, method => open};
decode_method(?CHANNEL, ?CHANNEL_OPEN_OK, Args) ->
    {<<>>, <<>>} = decode_long_string(Args),
    #{frame => method, class => channel, method => open_ok};
decode_method(?CHANNEL, ?CHANNEL_FLOW, Args) ->
    {Active, <<>>} = decode_boolean(Args),
    #{frame => method, class => channel, method => flow, active => Active};
decode_method(?CHANNEL, ?CHANNEL_FLOW_OK, Args) ->
    {Active, <<>>} = decode_boolean(Args),
    #{frame => method, class => channel, method => flow_ok, active => Active};
decode_method(?CHANNEL, ?CHANNEL_CLOSE, Args) ->
    <<Code:?SHORT, T/binary>> = Args,
    {Text, <<Class:?SHORT, Method:?SHORT>>} = decode_short_string(T),
    #{frame => method,
      class => channel,
      method => close,
      code => Code,
      text => Text,
      failing_class => Class,
      failing_method => Method};
decode_method(?CHANNEL, ?CHANNEL_CLOSE_OK, <<>>) ->
    #{frame => method, class => channel, method => close_ok};
%%
%% Exchange
%%
decode_method(?EXCHANGE, ?EXCHANGE_DECLARE, <<_:?SHORT, Args/binary>>) ->
    {Exchange, T} = decode_short_string(Args),
    {Type, T1} = decode_short_string(T),
    <<_:3, NoWait:1, 0:2, Durable:1, Passive:1, T2/binary>> = T1,
    {Arguments, <<>>} = decode_table(T2),
    #{frame => method,
      class => exchange,
      method => declare,
      exchange => Exchange,
      type => Type,
      passive => decode_bit(Passive),
      durable => decode_bit(Durable),
      no_wait => decode_bit(NoWait),
      arguments => Arguments};
decode_method(?EXCHANGE, ?EXCHANGE_DECLARE_OK, <<>>) ->
    #{frame => method, class => exchange, method => declare_ok};
decode_method(?EXCHANGE, ?EXCHANGE_DELETE, <<_:?SHORT, Args/binary>>) ->
    {Exchange, T} = decode_short_string(Args),
    <<_:6, NoWait:1, IfUnused:1>> = T,
    #{frame => method,
      class => exchange,
      method => delete,
      exchange => Exchange,
      if_unused => decode_bit(IfUnused),
      no_wait => decode_bit(NoWait)};
decode_method(?EXCHANGE, ?EXCHANGE_DELETE_OK, <<>>) ->
    #{frame => method, class => exchange, method => delete_ok};
%%
%% Queue
%%
decode_method(?QUEUE, ?QUEUE_DECLARE, <<_:?SHORT, Args/binary>>) ->
    {Queue, T} = decode_short_string(Args),
    <<_:3,NoWait:1,AutoDelete:1,Exclusive:1,Durable:1,Passive:1,T1/binary>> = T,
    {Arguments, <<>>} = decode_table(T1),
    #{frame => method,
      class => queue,
      method => declare,
      queue => Queue,
      passive => decode_bit(Passive),
      durable => decode_bit(Durable),
      exclusive => decode_bit(Exclusive),
      auto_delete => decode_bit(AutoDelete),
      no_wait => decode_bit(NoWait),
      arguments => Arguments};
decode_method(?QUEUE, ?QUEUE_DECLARE_OK, Args) ->
    {Queue, <<Msgs:?LONG, Consumers:?LONG>>} = decode_short_string(Args),
    #{frame => method,
      class => queue,
      method => declare_ok,
      queue => Queue,
      message_count => Msgs,
      consumer_count => Consumers};
decode_method(?QUEUE, ?QUEUE_BIND, <<_:?SHORT, Args/binary>>) ->
    {Queue, T} = decode_short_string(Args),
    {Exchange, T1} = decode_short_string(T),
    {RoutingKey, T2} = decode_short_string(T1),
    <<_:7, NoWait:1, T3/binary>> = T2,
    {Arguments, <<>>} = decode_table(T3),
    #{frame => method,
      class => queue,
      method => bind,
      queue => Queue,
      exchange => Exchange,
      routing_key => RoutingKey,
      no_wait => decode_bit(NoWait),
      arguments => Arguments};
decode_method(?QUEUE, ?QUEUE_BIND_OK, <<>>) ->
    #{frame => method, class => queue, method => bind_ok};
decode_method(?QUEUE, ?QUEUE_UNBIND, <<_:?SHORT, Args/binary>>) ->
    {Queue, T} = decode_short_string(Args),
    {Exchange, T1} = decode_short_string(T),
    {RoutingKey, T2} = decode_short_string(T1),
    {Arguments, <<>>} = decode_table(T2),
    #{frame => method,
      class => queue,
      method => unbind,
      queue => Queue,
      exchange => Exchange,
      routing_key => RoutingKey,
      arguments => Arguments};
decode_method(?QUEUE, ?QUEUE_UNBIND_OK, <<>>) ->
    #{frame => method, class => queue, method => unbind_ok};
decode_method(?QUEUE, ?QUEUE_PURGE, <<_:?SHORT, Args/binary>>) ->
    {Queue, <<_:7, NoWait:1>>} = decode_short_string(Args),
    #{frame => method,
      class => queue,
      method => purge,
      queue => Queue,
      no_wait => decode_bit(NoWait)};
decode_method(?QUEUE, ?QUEUE_PURGE_OK, <<Msgs:?LONG>>) ->
    #{frame => method,
      class => queue,
      method => purge_ok,
      message_count => Msgs};
decode_method(?QUEUE, ?QUEUE_DELETE, <<_:?SHORT, Args/binary>>) ->
    {Queue, <<_:5, NoWait:1, IfEmpty:1, IfUnused:1>>} =
        decode_short_string(Args),
    #{frame => method,
      class => queue,
      method => delete,
      queue => Queue,
      if_unused => decode_bit(IfUnused),
      if_emty => decode_bit(IfEmpty),
      no_wait => decode_bit(NoWait)};
decode_method(?QUEUE, ?QUEUE_DELETE_OK, <<Msgs:?LONG>>) ->
    #{frame => method,
      class => queue,
      method => delete_ok,
      message_count => Msgs};
%%
%% Basic
%%
decode_method(?BASIC, ?BASIC_QOS, Args) ->
    <<PrefetchSize:?LONG, PrefetchCount:?SHORT, _:7, Global:1>> = Args,
    #{frame => method,
      class => basic,
      method => qos,
      prefetch_size => PrefetchSize,
      prefetch_count => PrefetchCount,
      global => decode_bit(Global)};
decode_method(?BASIC, ?BASIC_QOS_OK, <<>>) ->
    #{frame => method, class => basic, method => qos_ok};
decode_method(?BASIC, ?BASIC_CONSUME, <<_:?SHORT, Args/binary>>) ->
    {Queue, T} = decode_short_string(Args),
    {Tag, T1} = decode_short_string(T),
    <<_:4, NoWait:1, Exclusive:1, NoAck:1, NoLocal:1, T2/binary>> = T1,
    {Arguments, <<>>} = decode_table(T2),
    #{frame => method,
      class => basic,
      method => consume,
      queue => Queue,
      consumer_tag => Tag,
      no_local => decode_bit(NoLocal),
      no_ack => decode_bit(NoAck),
      exclusive => decode_bit(Exclusive),
      no_wait => decode_bit(NoWait),
      arguments => Arguments};
decode_method(?BASIC, ?BASIC_CONSUME_OK, Args) ->
    {Tag, <<>>} = decode_short_string(Args),
    #{frame => method,
      class => basic,
      method => consume_ok,
      consumer_tag => Tag};
decode_method(?BASIC, ?BASIC_CANCEL, Args) ->
    {Tag, <<_:7, NoWait:1>>} = decode_short_string(Args),
    #{frame => method,
      class => basic,
      method => cancel,
      consumer_tag => Tag,
      no_wait => decode_bit(NoWait)};
decode_method(?BASIC, ?BASIC_CANCEL_OK, Args) ->
    {Tag, <<>>} = decode_short_string(Args),
    #{frame => method, class => basic, method => cancel_ok,consumer_tag => Tag};
decode_method(?BASIC, ?BASIC_PUBLISH, <<_:?SHORT, Args/binary>>) ->
    {Exchange, T} = decode_short_string(Args),
    {RoutingKey, <<_:6, Immediate:1, Mandatory:1>>} = decode_short_string(T),
    #{frame => method,
      class => basic,
      method => publish,
      exchange => Exchange,
      routing_key => RoutingKey,
      mandatory => decode_bit(Mandatory),
      immediate => decode_bit(Immediate)};
decode_method(?BASIC, ?BASIC_RETURN, <<Code:?SHORT, Args/binary>>) ->
    {Text, T} = decode_short_string(Args),
    {Exchange, T1} = decode_short_string(T),
    {RoutingKey, <<>>} = decode_short_string(T1),
    #{frame => method,
      class => basic,
      method => return,
      code => Code,
      text => Text,
      exchange => Exchange,
      routing_key => RoutingKey};
decode_method(?BASIC, ?BASIC_DELIVER, Args) ->
    {ConsumerTag, <<DeliveryTag:?LLONG, _:7, Redelivered:1, T/binary>>} =
        decode_short_string(Args),
    {Exchange, T1} = decode_short_string(T),
    {RoutingKey, <<>>} = decode_short_string(T1),
    #{frame => method,
      class => basic,
      method => deliver,
      consumer_tag => ConsumerTag,
      delivery_tag => DeliveryTag,
      redelivered => decode_bit(Redelivered),
      exchange => Exchange,
      routing_key => RoutingKey};
decode_method(?BASIC, ?BASIC_GET, <<_:?SHORT, Args/binary>>) ->
    {Queue, <<_:7, NoAck:1>>} = decode_short_string(Args),
    #{frame => method,
      class => basic,
      method => get,
      queue => Queue,
      no_ack => decode_bit(NoAck)};
decode_method(?BASIC, ?BASIC_GET_OK, Args) ->
    <<DeliveryTag:?LLONG, _:7, Redelivered:1, T/binary>> = Args,
    {Exchange, T1} = decode_short_string(T),
    {RoutingKey, <<Count:?SHORT>>} = decode_short_string(T1),
    #{frame => method,
      class => basic,
      method => get_ok,
      delivery_tag => DeliveryTag,
      redelivered => decode_bit(Redelivered),
      exchange => Exchange,
      routing_key => RoutingKey,
      message_count => Count};
decode_method(?BASIC, ?BASIC_GET_EMPTY, <<?CLUSTER_ID>>) ->
    #{frame => method, class => basic, method => get_empty};
decode_method(?BASIC, ?BASIC_ACK, <<DeliveryTag:?LLONG, _:7, Multiple:1>>) ->
    #{frame => method,
      class => basic,
      method => ack,
      delivery_tag => DeliveryTag,
      multiple => decode_bit(Multiple)};
decode_method(?BASIC, ?BASIC_REJECT, <<DeliveryTag:?LLONG, _:7, Requeue:1>>) ->
    #{frame => method,
      class => basic,
      method => reject,
      delivery_tag => DeliveryTag,
      requeue => decode_bit(Requeue)};
decode_method(?BASIC, ?BASIC_RECOVER_ASYNC, <<_:7, Requeue:1>>) ->
    #{frame => method,
      class => basic,
      method => recover_async,
      requeue => decode_bit(Requeue)};
decode_method(?BASIC, ?BASIC_RECOVER, <<_:7, Requeue:1>>) ->
    #{frame => method,
      class => basic,
      method => recover,
      requeue => decode_bit(Requeue)};
decode_method(?BASIC, ?BASIC_RECOVER_OK, <<>>) ->
    #{frame => method, class => basic, method => recover_ok};
%%
%% TX
%%
decode_method(?TX, ?TX_SELECT, <<>>) ->
    #{frame => method, class => tx, method => select};
decode_method(?TX, ?TX_SELECT_OK, <<>>) ->
    #{frame => method, class => tx, method => select_ok};
decode_method(?TX, ?TX_COMMIT, <<>>) ->
    #{frame => method, class => tx, method => commit};
decode_method(?TX, ?TX_COMMIT_OK, <<>>) ->
    #{frame => method, class => tx, method => commit_ok};
decode_method(?TX, ?TX_ROLLBACK, <<>>) ->
    #{frame => method, class => tx, method => rollback};
decode_method(?TX, ?TX_ROLLBACK_OK, <<>>) ->
    #{frame => method, class => tx, method => rollback_ok}.

decode_table(<<0:?LONG, T/binary>>) -> {#{}, T};
decode_table(<<Size:?LONG, TABLE:Size/binary, T/binary>>) ->
    {decode_table(TABLE, []), T}.

decode_table(<<>>, Acc) -> maps:from_list(Acc);
decode_table(<<Size, Name:Size/binary, T/binary>>, Acc) ->
    {Value, T1} = decode_table_value(T),
    decode_table(T1, [{binary_to_atom(Name, utf8), Value} | Acc]).

decode_table_value(<<$t, 0:8/unsigned-integer, T/binary>>) -> {false, T};
decode_table_value(<<$t, _, T/binary>>) -> {true, T};
decode_table_value(<<$b, I:8/signed-integer, T/binary>>) -> {I, T};
decode_table_value(<<$B, I:8/unsigned-integer, T/binary>>) -> {I, T};
decode_table_value(<<$U, I:16/signed-integer, T/binary>>) -> {I, T};
decode_table_value(<<$u, I:16/unsigned-integer, T/binary>>) -> {I, T};
decode_table_value(<<$I, I:32/signed-integer, T/binary>>) -> {I, T};
decode_table_value(<<$i, I:32/unsigned-integer, T/binary>>) -> {I, T};
decode_table_value(<<$L, I:64/signed-integer, T/binary>>) -> {I, T};
decode_table_value(<<$l, I:64/unsigned-integer, T/binary>>) -> {I, T};
decode_table_value(<<$f, F:32/float, T/binary>>) -> {F, T};
decode_table_value(<<$d, F:64/float, T/binary>>) -> {F, T};
decode_table_value(<<$D,Scale,D:32/signed-integer,T/binary>>) -> {{Scale, D},T};
decode_table_value(<<$s, Size, S:Size/binary, T/binary>>) -> {S, T};
decode_table_value(<<$S, Size:?LONG, S:Size/binary, T/binary>>) -> {S, T};
decode_table_value(<<$F, T/binary>>) -> decode_table(T);
decode_table_value(<<$A, Size:?LONG, A:Size/binary, T/binary>>) ->
    {decode_array(A, []), T};
decode_table_value(<<$V, T/binary>>) ->
    {undefined, T}.

decode_short_string(<<Size, S:Size/bytes, T/binary>>) -> {S, T}.

decode_long_string(<<Size:?LONG, S:Size/bytes, T/binary>>) -> {S, T}.

decode_boolean(<<0, T/binary>>) -> {false, T};
decode_boolean(<<_, T/binary>>) -> {true, T}.

decode_bit(1) -> true;
decode_bit(0) -> false.

decode_array(<<>>, Acc) -> lists:reverse(Acc);
decode_array(T, Acc) ->
    {Value, T1} = decode_table_value(T),
    decode_array(T1, [Value | Acc]).

%% ===================================================================
%% Common parts
%% ===================================================================
