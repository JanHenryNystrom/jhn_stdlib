%%==============================================================================
%% Copyright 2016-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A Syslog library based on:
%%%    The Syslog Protocol                                             (rfc5424)
%%%    Transport Layer Security (TLS) Transport Mapping for Syslog     (rfc5425)
%%%    Transmission of Syslog Messages over UDP                        (rfc5426)
%%%    Textual Conventions for Syslog Management                       (rfc5427)
%%%    Transmission of Syslog Messages over TCP                        (rfc6587)
%%%
%%%  SYSLOG line is represented as follows:
%%%
%%%  line       : #{header => Header,
%%%                 structured => Structured,
%%%                 msg => Message
%%%                }
%%%
%%%  Header     : #{facility => Facility,
%%%                 severity => Severity,
%%%                 version => Version,
%%%                 time_stamp => TimeStamp,
%%%                 fraction => integer(),
%%%                 offset_sign => '+' | '-' | 'Z',
%%%                 offset => {Hour, Minute},
%%%                 host_name => iodata(),
%%%                 app_name => iodata(),
%%%                 proc_id => iodata(),
%%%                 msg_id => iodata()
%%%                }
%%%
%%%  Facility   : kern | user | mail | daemon | auth | syslog | lpr | news |
%%%               uucp | cron | authpriv | ftp | ntp | audit | console | cron2 |
%%%               local0 | local1 | local2 | local3 | local4 | local5 |
%%%               local6 | local7
%%%               default: user
%%%  Severity   : emerg | alert | crit | err | warning | notice | info | debug
%%%               default: info
%%%  Version    : integer()
%%%               default: 1
%%%  TimeStamp  : binary() |
%%%               {{Year, Month, Day}, {Hour, Minute, Second}} |
%%%               #{year => Year, month => Month, day => Day,
%%%                 hour =>Hour, minute => Minute, second => Second}
%%%  Year, Month, Day, Hour, Minute, Second : integer()
%%%
%%%  Structured : [{Id, [{Key, Value}]}]
%%%  Id, Key, Value : iodata()
%%%
%%%  Message    : #{type => utf8 | any,
%%%                 content => iodata()
%%%                }
%%%
%%%  All part of the map are optional. All iodata is in fact a binary when a
%%%  line is decoded. All header values that can be represented by the nil
%%%  element "-" has that as default and when decoded omitted from the
%%   returned map.
%%%
%%% N.B. TCP only supports Octet counting framing.
%%%      The TLS transport requires the ssl OTP lib which is not included in
%%       the application resource file.
%%%      Only supports R18 and later.
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(syslog).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% API
-export([open/0, open/1,
         accept/1, accept/2,
         send/2, send/3,
         recv/1, recv/2,
         setopts/2, controlling_process/2,
         close/1
        ]).

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2,
         frame/2, unframe/2
        ]).

%% Exported types
-export_type([transport/0, line/0]).

%% Includes

%% Records
-record(opts, {type        = udp     :: type(),
               role        = client  :: client | server,
               port                  :: integer(),
               opts        = []      :: [{atom(), _}],
               ipv         = ipv4    :: ipv4 | ipv6,
               dest                  :: inet:ip_address() | inet:hostname(),
               dest_port             :: inet:port(),
               precision   = seconds :: seconds | milli | micro,
               timeout               :: integer(),
               return_type = iolist  :: iolist | binary}).

-record(transport, {type                 :: type() | tcp_listen | tls_listen,
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

%% Types
-type opt() :: _.
-type transport() :: #transport{}.
-type line() :: map().
-type type() :: udp | tcp | tls.
-type socket_options() :: gen_udp:option() | gen_tcp:option() | ssl:option().

%% Defines
-define(UTF8_BOM, 239,187,191).

%% ===================================================================
%% API functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: open() -> Transport | Error.
%% @doc
%%   Opens an UDP client or server transport.
%%   Equivalent to open([]).
%% @end
%%--------------------------------------------------------------------
-spec open() -> transport() | {error, _}.
%%--------------------------------------------------------------------
open() -> open(#opts{}).

%%--------------------------------------------------------------------
%% Function: open(Options) -> Transport | Error.
%% @doc
%%   Opens a Transport returning a connected client connection or a listen
%%   port for TCP and TLS for the server, UDP the difference is quite moot.
%%
%%   Options are:
%%     client -> opens a client transport (default)
%%     server -> open a server transport
%%     udp -> uses the UDP protocol (default)
%%     tcp -> uses the TCP protocol
%%     tls -> uses TLS v1.2 over TCP
%%     ipv4 -> IPv4 addresses are used (default)
%%     ipv6 -> IPv6 addresses are used
%%     port -> The port used by client or server with the default in the
%%             client case is 0 (the underlying OS assigns an available
%%             port number) for the server the default ports are as follows:
%%             UDP/514, TCP/601, TLS/6514
%%     destination -> the IP address or hostname of the server to connect
%%                    to in the case TCP and TLS and the default server for
%%                    the UDP case
%%     destination_port -> the port of the server to connect to in the case
%%                         TCP and TLS and the default server for the UDP case
%%     opts -> options to the transport's UDP, TCP, or TLS, see the
%%             documentation of gen_udp, gen_tcp, ssl respectively
%%     timeout -> the time in milliseconds the request is allowed to complete
%% @end
%%--------------------------------------------------------------------
-spec open([opt()] | #opts{}) -> transport() | {error, _}.
%%--------------------------------------------------------------------
open(Opts = #opts{type = udp}) ->
    #opts{role = Role,
          port = Port,
          opts = TransportOpts,
          ipv = IPv,
          dest = Dest,
          dest_port = DestPort} = Opts,
    Port1 = port(udp, Port, Role),
    DestPort1 = port(udp, DestPort, inverse(Role)),
    TransportOpts1 = case IPv of
                         ipv4 -> [inet, binary | TransportOpts];
                         ipv6 -> [inet6, binary | TransportOpts]
                     end,
    try gen_udp:open(Port1, TransportOpts1) of
        {ok, Sock} -> #transport{type = udp,
                                 port = Port1,
                                 role = Role,
                                 socket = Sock,
                                 dest = dest(Dest, IPv),
                                 dest_port = DestPort1};
        Error -> Error
    catch
        Class:Error -> {error, {Class, Error}}
    end;
open(Opts = #opts{type = tcp}) ->
    #opts{role = Role,
          port = Port,
          opts = TransportOpts,
          ipv = IPv,
          dest = Dest,
          dest_port = DestPort,
          timeout = Timeout} = Opts,
    Port1 = port(tcp, Port, Role),
    Dest1 = dest(Dest, IPv),
    DestPort1 = port(tcp, DestPort, inverse(Role)),
    TransportOpts1 = case IPv of
                         ipv4 -> [inet, binary | TransportOpts];
                         ipv6 -> [inet6, binary | TransportOpts]
                     end,
    case {Role, Timeout} of
        {client, undefined} ->
            try gen_tcp:connect(Dest1, DestPort1, TransportOpts1) of
                {ok, Sock} -> #transport{type = tcp,
                                         role = client,
                                         socket = Sock,
                                         dest = Dest1,
                                         dest_port = DestPort1};
                Error -> Error
            catch
                Class:Error -> {error, {Class, Error}}
            end;
        {client, Timeout} ->
            try gen_tcp:connect(Dest1, DestPort1, TransportOpts1, Timeout) of
                {ok, Sock} -> #transport{type = tcp,
                                         role = client,
                                         socket = Sock,
                                         dest = Dest1,
                                         dest_port = DestPort1};
                Error -> Error
            catch
                Class:Error -> {error, {Class, Error}}
            end;
        {server, _} ->
            try gen_tcp:listen(Port1, TransportOpts1) of
                {ok, Sock} -> #transport{type = tcp_listen,
                                         role = server,
                                         listen_socket = Sock};
                Error -> Error
            catch
                Class:Error -> {error, {Class, Error}}
            end
    end;
open(Opts = #opts{type = tls}) ->
    #opts{role = Role,
          port = Port,
          opts = TransportOpts,
          ipv = IPv,
          dest = Dest,
          dest_port = DestPort,
          timeout = Timeout} = Opts,
    Port1 = port(tls, Port, Role),
    Dest1 = dest(Dest, IPv),
    DestPort1 = port(tls, DestPort, inverse(Role)),
    TransportOpts1 = case IPv of
                         ipv4 -> [inet, binary, {versions, ['tlsv1.2']} |
                                  TransportOpts];
                         ipv6 -> [inet6, binary, {versions, ['tlsv1.2']} |
                                  TransportOpts]
                     end,
    case {Role, Timeout} of
        {client, undefined} ->
            case ssl:connect(Dest1, DestPort1, TransportOpts1) of
                {ok, Sock} -> #transport{type = tls,
                                         role = client,
                                         socket = Sock,
                                         dest = Dest1,
                                         dest_port = DestPort1};
                Error -> Error
            end;
        {client, Timeout} ->
            case ssl:connect(Dest1, DestPort1, TransportOpts1, Timeout) of
                {ok, Sock} -> #transport{type = tls,
                                         role = client,
                                         socket = Sock,
                                         dest = Dest1,
                                         dest_port = DestPort1};
                Error -> Error
            end;
        {server, _} ->
            try ssl:listen(Port1, TransportOpts1) of
                {ok, Sock} -> #transport{type = tls_listen,
                                         role = server,
                                         listen_socket = Sock};
                Error -> Error
            catch
                Class:Error -> {error, {Class, Error}}
            end
    end;
open(Opts) -> open(parse_opts(Opts)).

%%--------------------------------------------------------------------
%% Function: accept(Transport) -> Transport | Error.
%% @doc
%%   Accepts an incoming connection request on an opened server transport
%%   for the TCP and TLS, it returns a connected transport.
%%   Equivalent to accept(Transport, []).
%% @end
%%--------------------------------------------------------------------
-spec accept(transport()) -> transport() | {error, _}.
%%--------------------------------------------------------------------
accept(Transport) -> accept(Transport, []).

%%--------------------------------------------------------------------
%% Function: accept(Transport, Options) -> Transport | Error.
%% @doc
%%   Accepts an incoming connection request on an opened server transport
%%   for the TCP and TLS, it returns a connected transport.
%%
%%   Options are:
%%     timeout -> the time in milliseconds the request is allowed to complete
%% @end
%%--------------------------------------------------------------------
-spec accept(transport(), [opt()]) -> transport() | {error, _}.
%%--------------------------------------------------------------------
accept(Transport = #transport{type = tcp_listen, listen_socket = LSock},Opts) ->
    case parse_opts(Opts) of
        #opts{timeout = undefined} ->
            try gen_tcp:accept(LSock) of
                {ok, Sock}  ->
                    Transport#transport{type = tcp,
                                        socket = Sock,
                                        listen_socket = undefined};
                Error ->
                    Error
            catch
                error:Error -> {error, Error}
            end;
        #opts{timeout = Timeout} ->
            try gen_tcp:accept(LSock, Timeout) of
                {ok, Sock}  ->
                    Transport#transport{type = tcp,
                                        socket = Sock,
                                        listen_socket = undefined};
                Error ->
                    Error
            catch
                error:Error -> {error, Error}
            end
    end;
accept(Transport = #transport{type = tls_listen, listen_socket = LSock},Opts) ->
    case parse_opts(Opts) of
        #opts{timeout = undefined} ->
            try ssl:transport_accept(LSock) of
                {ok, Sock}  ->
                    case ssl:handshake(Sock) of
                        {ok, SSock} ->
                            Transport#transport{type = tls,
                                                socket = SSock,
                                                listen_socket = undefined};
                        {ok, SSock, _} ->
                            Transport#transport{type = tls,
                                                socket = SSock,
                                                listen_socket = undefined};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            catch
                error:Error -> {error, Error}
            end;
        #opts{timeout = Timeout} ->
            try ssl:transport_accept(LSock, Timeout) of
                {ok, Sock}  ->
                    case ssl:handshake(Sock) of
                        {ok, SSock} ->
                            Transport#transport{type = tls,
                                                socket = SSock,
                                                listen_socket = Sock};
                        {ok, SSock, _} ->
                            Transport#transport{type = tls,
                                                socket = SSock,
                                                listen_socket = Sock};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            catch
                error:Error -> {error, Error}
            end
    end.

%%--------------------------------------------------------------------
%% Function: send(Transport, Line) -> ok | Error.
%% @doc
%%   Sends a message over the transport.
%%   Equivalent to Send(Transport, Line).
%% @end
%%--------------------------------------------------------------------
-spec send(transport(), line()) -> ok | {error, _}.
%%--------------------------------------------------------------------
send(Transport, Line) -> send(Transport, Line, #opts{}).

%%--------------------------------------------------------------------
%% Function: send(Transport, Line, Options) -> ok | Error.
%% @doc
%%   Sends a message over the transport.
%%
%%   Options are:
%%     seconds (default) -> second precision
%%     milli -> milli second precision
%%     micro -> micro second precision
%%     destination -> the IP address or hostname of the peer to send to
%%                    in the UDP case, otherwise the default provided in open
%%                    is used
%%     destination_port -> the port of the peer in the UDP case
%% @end
%%--------------------------------------------------------------------
-spec send(transport(), line(), [opt()] | #opts{}) -> ok | {error, _}.
%%--------------------------------------------------------------------
send(Transport = #transport{type = udp, socket = Sock}, Line, Opts) ->
    case parse_opts(Opts) of
        ParsedOpts = #opts{dest = undefined} ->
            #transport{dest = Dest, dest_port = Port} = Transport,
            try gen_udp:send(Sock, Dest, Port, encode(Line, ParsedOpts))
            catch error:Error -> {error, Error}
            end;
        ParsedOpts  = #opts{dest = Dest, dest_port = Port}->
            try gen_udp:send(Sock, Dest, Port, encode(Line, ParsedOpts))
            catch error:Error -> {error, Error}
            end
    end;
send(#transport{type = tcp, socket = Sock}, Line, Opts) ->
    try gen_tcp:send(Sock, frame(tcp, encode(Line, Opts)))
    catch error:Error -> {error, Error}
    end;
send(#transport{type = tls, socket = Sock}, Line, Opts) ->
    try ssl:send(Sock, frame(tls, encode(Line, Opts)))
    catch error:Error -> {error, Error}
    end.

%%--------------------------------------------------------------------
%% Function: recv(Transport) ->
%%               {ok, Line} | {ok, Transport, Line} | Error
%% @doc
%%   Receives a packet from a Transport socket in passive mode.
%%   A closed socket is indicated by a return value {error, closed}.
%%   Equivalent to recv(Line, []).
%% @end
%%--------------------------------------------------------------------
-spec recv(transport()) ->
                  {ok, line()} | {ok, line(), transport()} | {error, _}.
%%--------------------------------------------------------------------
recv(Transport) -> recv(Transport, #opts{}).

%%--------------------------------------------------------------------
%% Function: recv(Transport, Options) ->
%%               {ok, Line} | {ok, Transport, Line} | Error
%% @doc
%%   Receives a packet from a Transport socket in passive mode.
%%   A closed socket is indicated by a return value {error, closed}.
%%
%%   Options are:
%%     timeout -> the time in milliseconds the request is allowed to complete
%% @end
%%--------------------------------------------------------------------
-spec recv(transport(), [opt()] | #opts{}) ->
                  {ok, line()} | {ok, line(), transport()} | {error, _}.
%%--------------------------------------------------------------------
recv(#transport{type = udp, socket = Sock}, Opts) ->
    case parse_opts(Opts) of
        #opts{timeout = undefined} ->
            try gen_udp:recv(Sock, 0) of
                {ok, {_, _, Packet}} -> {ok, decode(Packet, Opts)};
                Error -> Error
            catch
                error:Error -> {error, Error}
            end;
        #opts{timeout = Timeout} ->
            try gen_udp:recv(Sock, Timeout) of
                {ok, {_, _, Packet}} -> {ok, decode(Packet, Opts)};
                Error -> Error
            catch
                error:Error -> {error, Error}
            end
    end;
recv(Transport = #transport{type = tcp, socket = Sock, buf = Buf}, Opts) ->
    case parse_opts(Opts) of
        #opts{timeout = undefined} ->
            try gen_tcp:recv(Sock, 0) of
                {ok, Packet} ->
                    case unframe(tcp, <<Buf/binary, Packet/binary>>) of
                        {more, Len} ->
                            try gen_tcp:recv(Sock, Len) of
                                {ok, {_, _, Packet1}} ->
                                    {Frame, Buf1} =
                                        unframe(tcp,
                                                <<Buf/binary,
                                                  Packet/binary,
                                                  Packet1/binary>>),
                                    {ok,
                                     decode(Frame, Opts),
                                     Transport#transport{buf = Buf1}};
                                Error -> Error
                            catch error:Error -> {error, Error}
                            end;
                        {Frame, Buf1} ->
                            {ok,
                             decode(Frame, Opts),
                             Transport#transport{buf = Buf1}}
                    end;
                Error ->
                    Error
            catch error:Error -> {error, Error}
            end;
        #opts{timeout = Timeout} ->
            try gen_tcp:recv(Sock, 0, Timeout) of
                {ok, Packet} ->
                    case unframe(tcp, <<Buf/binary, Packet/binary>>) of
                        {more, Len} ->
                            try gen_tcp:recv(Sock, Len, Timeout) of
                                {ok, {_, _, Packet1}} ->
                                    {Frame, Buf1} =
                                        unframe(tcp, <<Buf/binary,
                                                       Packet/binary,
                                                       Packet1/binary>>),
                                    {ok,
                                     decode(Frame, Opts),
                                     Transport#transport{buf = Buf1}};
                                Error -> Error
                            catch error:Error -> {error, Error}
                            end;
                        {Frame, Buf1} ->
                            {ok,
                             decode(Frame, Opts),
                             Transport#transport{buf = Buf1}}
                    end;
                Error ->
                    Error
            catch error:Error -> {error, Error}
            end
    end;
recv(Transport = #transport{type = tls, socket = Sock, buf = Buf}, Opts) ->
    case parse_opts(Opts) of
        #opts{timeout = undefined} ->
            try ssl:recv(Sock, 0) of
                {ok, Packet} ->
                    case unframe(tls, <<Buf/binary, Packet/binary>>) of
                        {more, Len} ->
                            try ssl:recv(Sock, Len) of
                                {ok, {_, _, Packet1}} ->
                                    {Frame, Buf1} =
                                        unframe(tls,
                                                <<Buf/binary,
                                                  Packet/binary,
                                                  Packet1/binary>>),
                                    {ok,
                                     decode(Frame, Opts),
                                     Transport#transport{buf = Buf1}};
                                Error -> Error
                            catch error:Error -> {error, Error}
                            end;
                        {Frame, Buf1} ->
                            {ok,
                             decode(Frame, Opts),
                             Transport#transport{buf = Buf1}}
                    end;
                Error ->
                    Error
            catch error:Error -> {error, Error}
            end;
        #opts{timeout = Timeout} ->
            try ssl:recv(Sock, 0, Timeout) of
                {ok, Packet} ->
                    case unframe(tls, <<Buf/binary, Packet/binary>>) of
                        {more, Len} ->
                            try ssl:recv(Sock, Len, Timeout) of
                                {ok, {_, _, Packet1}} ->
                                    {Frame, Buf1} =
                                        unframe(tls, <<Buf/binary,
                                                       Packet/binary,
                                                       Packet1/binary>>),
                                    {ok,
                                     decode(Frame, Opts),
                                     Transport#transport{buf = Buf1}};
                                Error -> Error
                            catch error:Error -> {error, Error}
                            end;
                        {Frame, Buf1} ->
                            {ok,
                             decode(Frame, Opts),
                             Transport#transport{buf = Buf1}}
                    end;
                Error ->
                    Error
            catch error:Error -> {error, Error}
            end
    end.

%%--------------------------------------------------------------------
%% Function: setopts(Transport, Options) -> ok | Error.
%% @doc
%%   Sets one or more options for a transport socket according to the transport.
%% @end
%%--------------------------------------------------------------------
-spec setopts(transport(), socket_options()) -> ok | {error, _}.
%%--------------------------------------------------------------------
setopts(#transport{type = udp, socket = Sock}, Options) ->
    try inet:setopts(Sock, Options)
    catch error:Error -> {error, Error}
    end;
setopts(#transport{type = tcp, socket = Sock}, Options) ->
    try inet:setopts(Sock, Options)
    catch error:Error -> {error, Error}
    end;
setopts(#transport{type = tls, socket = Sock}, Options) ->
    try ssl:setopts(Sock, Options)
    catch error:Error -> {error, Error}
    end.

%%--------------------------------------------------------------------
%% Function: controlling_process(Transport, Pid) -> ok | Error.
%% @doc
%%   Assigns a new controlling process Pid to the Transport socket.
%% @end
%%--------------------------------------------------------------------
-spec controlling_process(transport(), pid()) -> ok | {error, _}.
%%--------------------------------------------------------------------
controlling_process(#transport{type = udp, socket = Sock}, Pid) ->
    try gen_udp:controlling_process(Sock, Pid)
    catch error:Error -> {error, Error}
    end;
controlling_process(#transport{type = tcp, socket = Sock}, Pid) ->
    try gen_tcp:controlling_process(Sock, Pid)
    catch error:Error -> {error, Error}
    end;
controlling_process(#transport{type = tcp_listen, listen_socket = Sock}, Pid) ->
    try gen_tcp:controlling_process(Sock, Pid)
    catch error:Error -> {error, Error}
    end;
controlling_process(#transport{type = tls, socket = Sock}, Pid) ->
    try ssl:controlling_process(Sock, Pid)
    catch error:Error -> {error, Error}
    end;
controlling_process(#transport{type = tls_listen, listen_socket = Sock}, Pid) ->
    try ssl:controlling_process(Sock, Pid)
    catch error:Error -> {error, Error}
    end.

%%--------------------------------------------------------------------
%% Function: close(Transport) -> ok | Error.
%% @doc
%%   Closes an already established transport.
%% @end
%%--------------------------------------------------------------------
-spec close(transport()) -> ok | {error, _}.
%%--------------------------------------------------------------------
close(#transport{type = udp, socket = Sock}) ->
    try gen_udp:close(Sock) catch error:Error -> {error, Error} end;
close(#transport{type = tcp_listen, listen_socket = Sock}) ->
    try gen_tcp:close(Sock) catch error:Error -> {error, Error} end;
close(#transport{type = tcp, socket = Sock}) ->
    try gen_tcp:close(Sock) catch error:Error -> {error, Error} end;
close(#transport{type = tls_listen, listen_socket = Sock}) ->
    try ssl:close(Sock) catch error:Error -> {error, Error} end;
close(#transport{type = tls, socket = Sock}) ->
    try ssl:close(Sock) catch error:Error -> {error, Error} end.

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Line) -> Syslog line.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent to encode(Line, []).
%% @end
%%--------------------------------------------------------------------
-spec encode(line()) -> iolist().
%%--------------------------------------------------------------------
encode(Line) -> encode(Line, #opts{}).

%%--------------------------------------------------------------------
%% Function: encode(Line, Options) -> Syslog line
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     seconds (default) -> second precision
%%     milli -> milli second precision
%%     micro -> micro second precision
%%     binary -> a binary is returned
%%     iolist -> a iolist is returned
%% @end
%%--------------------------------------------------------------------
-spec encode(line(), [opt()] | #opts{}) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Line, Opts = #opts{}) -> do_encode(Line, Opts);
encode(Line, Opts) ->
    ParsedOpts = parse_opts(Opts, #opts{}),
    case ParsedOpts#opts.return_type of
        iolist-> do_encode(Line, ParsedOpts);
        binary -> iolist_to_binary(do_encode(Line, ParsedOpts))
    end.

%%--------------------------------------------------------------------
%% Function: decode(Binary) -> Line.
%% @doc
%%   Decodes the binary into a structured Erlang term.
%%   Equivalent to decode(Binary, [])
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> line().
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, #opts{}).

%%--------------------------------------------------------------------
%% Function: decode(Binary, Options) -> Line.
%% @doc
%%   Decodes the binary into a structured Erlang.
%%   Decode will give an exception if the binary is not well formed
%%   Syslog line.
%%
%%   Options are: currently none
%%   N.B. the timestamp is decode into a date time tuple.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), [opt()] | #opts{}) -> line().
%%--------------------------------------------------------------------
decode(Binary, Opts = #opts{}) -> do_decode(Binary, Opts);
decode(Binary, Opts) -> do_decode(Binary, parse_opts(Opts, #opts{})).

%%--------------------------------------------------------------------
%% Function: frame(TransportType, Line) -> Frame.
%% @doc
%%   Encloses an encoded line in an octet counting frame.
%% @end
%%--------------------------------------------------------------------
-spec frame(type(), iodata()) -> iodata().
%%--------------------------------------------------------------------
frame(_, Data) -> [integer_to_binary(iolist_size(Data)), $\s, Data].

%%--------------------------------------------------------------------
%% Function: unframe(TransportType, Frame) -> Line.
%% @doc
%%   Extracts the line from an octet counting frame.
%% @end
%%--------------------------------------------------------------------
-spec unframe(type(), iodata()) -> iodata().
%%--------------------------------------------------------------------
unframe(_, Data) -> unframe1(Data, <<>>).

unframe1(<<$\s, T/binary>>, Acc) ->
    case {binary_to_integer(Acc), byte_size(T)} of
        {Octets, Size} when Size < Octets -> {more, Octets - Size};
        {Octets, _} ->
            <<Frame:Octets/binary, T1/binary>> = T,
            {Frame, T1}
    end;
unframe1(<<H, T/binary>>, Acc) ->
    unframe1(T, <<Acc/binary, H>>).

%% ===================================================================
%% Internal functions.
%% ===================================================================

dest(undefined, ipv4) -> {127, 0, 0,1};
dest(undefined, ipv6) -> {0, 0, 0, 0, 0, 0, 0, 1};
dest(Address, _) -> Address.

port(udp, undefined, server) -> 514;
port(tcp, undefined, server) -> 601;
port(tls, undefined, server) -> 6514;
port(_, undefined, client) -> 0;
port(_, Port, _) when is_integer(Port) -> Port.

inverse(client) -> server;
inverse(server) -> client.

%% ===================================================================
%% Encoding
%% ===================================================================

do_encode(Line, Opts) ->
    [encode_header(Line, Opts), $\s,
     encode_structured_data(Line),
     encode_msg(Line)].

encode_header(Line, #opts{precision = Precision}) ->
    Header = maps:get(header, Line, #{}),
    Severity = encode_severity(maps:get(severity, Header, info)),
    Facility = encode_facility(maps:get(facility, Header, user)),
    Version = integer_to_binary(maps:get(version, Header, 1)),
    Timestamp =
        encode_time_stamp(maps:get(time_stamp, Header, '-'), Header, Precision),
    HostName = maps:get(host_name, Header, "-"),
    AppName = maps:get(app_name, Header, "-"),
    ProcId = maps:get(proc_id, Header, "-"),
    MsgId = maps:get(msg_id, Header, "-"),
    [$<, integer_to_binary(Severity + Facility), $>, Version,
     $\s, Timestamp, $\s, HostName, $\s, AppName, $\s, ProcId, $\s, MsgId].

encode_severity(emerg) -> 0;
encode_severity(alert) -> 1;
encode_severity(crit) -> 2;
encode_severity(err) -> 3;
encode_severity(warning) -> 4;
encode_severity(notice) -> 5;
encode_severity(info) -> 6;
encode_severity(debug) ->  7.

encode_facility(kern) -> 0;
encode_facility(user) -> 8;
encode_facility(mail) -> 16;
encode_facility(daemon) -> 24;
encode_facility(auth) -> 32;
encode_facility(syslog) -> 40;
encode_facility(lpr) -> 48;
encode_facility(news) -> 56;
encode_facility(uucp) -> 64;
encode_facility(cron) -> 72;
encode_facility(authpriv) -> 80;
encode_facility(ftp) -> 88;
encode_facility(ntp) -> 96;
encode_facility(audit) -> 104;
encode_facility(console) -> 112;
encode_facility(cron2) -> 120;
encode_facility(local0) -> 128;
encode_facility(local1) -> 136;
encode_facility(local2) -> 144;
encode_facility(local3) -> 152;
encode_facility(local4) -> 160;
encode_facility(local5) -> 168;
encode_facility(local6) -> 176;
encode_facility(local7) -> 184.

encode_time_stamp('-', _, _) -> "-";
encode_time_stamp(Bin, _, _) when is_binary(Bin) -> Bin;
encode_time_stamp(Map = #{}, Header, Precision) ->
    case maps:get(offset_sign, Header, 'Z') of
        'Z' -> timestamp:encode(Map, [Precision]);
        Sign ->
            {HO, MiO} = maps:get(offset, Header),
            T = Map#{offset => #{sign => Sign, hours => HO, minutes => MiO}},
            timestamp:encode(T, [Precision])
    end;
encode_time_stamp({{Y, M , D}, {H, Mi, S}}, Header, Precision) ->
    T = #{year => Y, month => M, day => D,
          hour => H, minute => Mi,second => S,
          fraction => maps:get(fraction, Header, 0)},
    case maps:get(offset_sign, Header, 'Z') of
        'Z' -> timestamp:encode(T, [Precision]);
        Sign ->
            {HO, MiO} = maps:get(offset, Header),
            T1 = T#{offset => #{sign => Sign, hours => HO, minutes => MiO}},
            timestamp:encode(T1, [Precision])
    end.

encode_structured_data(#{structured := Structured}) ->
    [encode_structured_data(Id, Params) || {Id, Params}  <- Structured];
encode_structured_data(_) ->
    [$-].

encode_structured_data(Id, Params) ->
    [$[, Id, [encode_structured_param(Param) || Param <- Params], $]].

encode_structured_param({Name, Value}) ->
    [$\s, Name, $=, $", escape(Value, <<>>), $"].

escape([], Acc) -> Acc;
escape([$" | T], Acc) -> escape(T, <<Acc/binary, $\\, $">>);
escape([$\\ | T], Acc) -> escape(T, <<Acc/binary, $\\, $\\>>);
escape([$] | T], Acc) -> escape(T, <<Acc/binary, $\\, $]>>);
escape([H | T], Acc) when is_list(H) -> escape(T, escape(H, Acc));
escape([H | T], Acc) when is_binary(H) -> escape(T, escape(H, Acc));
escape([H | T], Acc) -> escape(T, <<Acc/binary, H/utf8>>);
escape(<<>>, Acc) -> Acc;
escape(<<$", T/binary>>, Acc) -> escape(T, <<Acc/binary, $\\, $">>);
escape(<<$\\, T/binary>>, Acc) -> escape(T, <<Acc/binary, $\\, $\\>>);
escape(<<$], T/binary>>, Acc) -> escape(T, <<Acc/binary, $\\, $]>>);
escape(<<H/utf8, T/binary>>, Acc) -> escape(T, <<Acc/binary, H/utf8>>).

encode_msg(#{msg := #{type := utf8, content:=Msg}}) -> [$\s, <<?UTF8_BOM>>,Msg];
encode_msg(#{msg := #{content := Msg}}) -> [$\s, Msg];
encode_msg(_) -> [].

%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(<<$<, T/binary>>, _) ->
    {Header, T1} = decode_header(T, <<>>),
    case decode_structured_data(T1) of
        {nil, T2} ->
            case decode_msg(T2) of
                {any, <<>>} -> #{header => Header};
                {Type, Msg} ->
                    #{header => Header, msg => #{type => Type, content => Msg}}
            end;
        {Structured, T2} ->
            case decode_msg(T2) of
                {any, <<>>} -> #{header => Header, structured => Structured};
                {Type, Msg} ->
                    #{header => Header,
                      structured => Structured,
                      msg => #{type => Type, content => Msg}}
            end
    end.

decode_header(<<$>, T/binary>>, Acc) ->
    Priority = binary_to_integer(Acc),
    FacilityNum = Priority div 8 * 8,
    Facility = decode_facility(FacilityNum),
    Severity = decode_severity(Priority - FacilityNum),
    decode_version(T, #{facility => Facility, severity => Severity}, <<>>);
decode_header(<<H, T/binary>>, Acc) ->
    decode_header(T, <<Acc/binary, H>>).

decode_version(<<$\s, T/binary>>, Header, Acc) ->
    decode_timestamp(T, Header#{version => binary_to_integer(Acc)});
decode_version(<<H, T/binary>>, Header, Acc) ->
    decode_version(T, Header, <<Acc/binary ,H>>).

decode_timestamp(<<$-, $\s, T/binary>>, Header) -> decode_hostname(T, Header);
decode_timestamp(T, Header) ->
    {#{year := Y, month := M, day := D, hour := H, minute := Mi, second := S,
       fraction := Fraction, offset := Offset},
     <<_, T1/binary>>}
        = timestamp:decode(T, [continue]),
    Header1 = Header#{time_stamp => {{Y, M, D}, {H,Mi,S}},fraction => Fraction},
    case Offset of
        'Z' -> decode_hostname(T1, Header1);
        #{sign := Sign,  hours := HO, minutes := MO} ->
            decode_hostname(T1,Header1#{offset_sign => Sign,offset => {HO, MO}})
    end.

decode_hostname(<<$-, $\s, T/binary>>, Header) -> decode_appname(T, Header);
decode_hostname(Bin, Header) ->
    {HostName, T} = decode_string(Bin, <<>>),
    decode_appname(T, Header#{host_name => HostName}).

decode_appname(<<$-, $\s, T/binary>>, Header) -> decode_proc_id(T, Header);
decode_appname(Bin, Header) ->
    {AppName, T} = decode_string(Bin, <<>>),
    decode_proc_id(T, Header#{app_name => AppName}).

decode_proc_id(<<$-, $\s, T/binary>>, Header) -> decode_msg_id(T, Header);
decode_proc_id(Bin, Header) ->
    {ProcId, T} = decode_string(Bin, <<>>),
    decode_msg_id(T, Header#{proc_id => ProcId}).

decode_msg_id(<<$-, $\s, T/binary>>, Header) -> {Header, T};
decode_msg_id(Bin, Header) ->
    {ProcId, T} = decode_string(Bin, <<>>),
    {Header#{msg_id => ProcId}, T}.

decode_structured_data(<<$-, T/binary>>) -> {nil, T};
decode_structured_data(<<$[, T/binary>>) ->
    {Id, Params, T1} = decode_sd_id(T, <<>>),
    decode_structured_data_next(T1, [{Id, Params}]).

decode_structured_data_next(<<$[, T/binary>>, Acc) ->
    {Id, Params, T1} = decode_sd_id(T, <<>>),
    decode_structured_data_next(T1, [{Id, Params} | Acc]);
decode_structured_data_next(T, Acc) ->
    {lists:reverse(Acc), T}.

decode_sd_id(<<$], T/binary>>, Acc) -> {Acc, [], T};
decode_sd_id(Bin = <<$\s, _/binary>>, Acc) ->
    {Params, T1} = decode_sd_params(Bin, []),
    {Acc, Params, T1};
decode_sd_id(<<H, T/binary>>, Acc) ->
    decode_sd_id(T, <<Acc/binary, H>>).

decode_sd_params(<<$], T/binary>>, Acc) -> {lists:reverse(Acc), T};
decode_sd_params(<<$\s, T/binary>>, Acc) ->
    {Param, T1} = decode_sd_param(T, <<>>),
    decode_sd_params(T1, [Param | Acc]).

decode_sd_param(<<$=, $", T/binary>>, Acc) ->
    {Value, T1} = decode_sd_value(T, <<>>),
    {{Acc, Value}, T1};
decode_sd_param(<<H, T/binary>>, Acc) ->
    decode_sd_param(T, <<Acc/binary, H>>).

decode_sd_value(<<$", T/binary>>, Acc) -> {Acc, T};
decode_sd_value(<<$\\, $\", T/binary>>, Acc) ->
    decode_sd_value(T, <<Acc/binary, $">>);
decode_sd_value(<<$\\, $\\, T/binary>>, Acc) ->
    decode_sd_value(T, <<Acc/binary, $\\>>);
decode_sd_value(<<$\\, $], T/binary>>, Acc) ->
    decode_sd_value(T, <<Acc/binary, $]>>);
decode_sd_value(<<H/utf8, T/binary>>, Acc) ->
    decode_sd_value(T, <<Acc/binary, H/utf8>>).

decode_msg(<<>>) -> {any, <<>>};
decode_msg(<<$\s, ?UTF8_BOM, T/binary>>) -> {utf8, decode_msg_utf8(T, T)};
decode_msg(<<$\s, T/binary>>) -> {any, T}.

decode_msg_utf8(<<>>, Msg) -> Msg;
decode_msg_utf8(<<_/utf8, T/binary>>, Msg) -> decode_msg_utf8(T, Msg).

decode_string(<<$\s, T/binary>>, Acc) -> {Acc, T};
decode_string(<<H, T/binary>>, Acc) -> decode_string(T, <<Acc/binary, H>>).

decode_severity(0) -> emerg;
decode_severity(1) -> alert;
decode_severity(2) -> crit;
decode_severity(3) -> err;
decode_severity(4) -> warning;
decode_severity(5) -> notice;
decode_severity(6) -> info;
decode_severity(7) ->  debug.

decode_facility(0) -> kern;
decode_facility(8) -> user;
decode_facility(16) -> mail;
decode_facility(24) -> daemon;
decode_facility(32) -> auth;
decode_facility(40) -> syslog;
decode_facility(48) -> lpr;
decode_facility(56) -> news;
decode_facility(64) -> uucp;
decode_facility(72) -> cron;
decode_facility(80) -> authpriv;
decode_facility(88) -> ftp;
decode_facility(96) -> ntp;
decode_facility(104) -> audit;
decode_facility(112) -> console;
decode_facility(120) -> cron2;
decode_facility(128) -> local0;
decode_facility(136) -> local1;
decode_facility(144) -> local2;
decode_facility(152) -> local3;
decode_facility(160) -> local4;
decode_facility(168) -> local5;
decode_facility(176) -> local6;
decode_facility(184) -> local7.

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts(Rec = #opts{}) -> Rec;
parse_opts(Opts) -> parse_opts(Opts, #opts{}).

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(seconds, Opts) -> Opts#opts{precision = seconds};
parse_opt(milli, Opts) -> Opts#opts{precision = milli};
parse_opt(micro, Opts) -> Opts#opts{precision = micro};
parse_opt(udp, Opts) -> Opts#opts{type = udp};
parse_opt(tcp, Opts) -> Opts#opts{type = tcp};
parse_opt(tls, Opts) -> Opts#opts{type = tls};
parse_opt(client, Opts) -> Opts#opts{role = client};
parse_opt(server, Opts) -> Opts#opts{role = server};
parse_opt({port, Port}, Opts) -> Opts#opts{port = Port};
parse_opt({opts, TransportOpts}, Opts) -> Opts#opts{opts = TransportOpts};
parse_opt(ipv4, Opts) -> Opts#opts{ipv = ipv4};
parse_opt(ipv6, Opts) -> Opts#opts{ipv = ipv6};
parse_opt({timeout, Timeout}, Opts) -> Opts#opts{timeout = Timeout};
parse_opt({destination_port, Port}, Opts) -> Opts#opts{dest_port = Port};
parse_opt({destination, IP = {_, _, _, _}}, Opts) -> Opts#opts{dest = IP};
parse_opt({destination, IP = {_, _, _, _, _, _, _, _}}, Opts) ->
    Opts#opts{dest = IP};
parse_opt({destination, Dest = [C | _]}, Opts) when C >= $0, C =< $: ->
    try ip_addr:decode(Dest, [tuple]) of
        Dest1 -> Opts#opts{dest = Dest1}
    catch _:_ -> Opts#opts{dest = Dest}
    end;
parse_opt({destination, Dest = <<C,_/binary>>},Opts) when C >= $0,C =< $: ->
    try ip_addr:decode(Dest, [tuple]) of
        Dest1 -> Opts#opts{dest = Dest1}
    catch _:_ -> Opts#opts{dest = Dest}
    end;
parse_opt({destination, Dest}, Opts) ->
    Opts#opts{dest = Dest};
parse_opt(_, Opts) ->
    erlang:error(badarg, Opts).
