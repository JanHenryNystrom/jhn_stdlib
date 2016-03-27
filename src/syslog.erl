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
%%%  A Syslog library based on:
%%%    The Syslog Protocol                                             (rfc5424)
%%%    Transport Layer Security (TLS) Transport Mapping for Syslog     (rfc5425)
%%%    Transmission of Syslog Messages over UDP                        (rfc5426)
%%%    Textual Conventions for Syslog Management                       (rfc5427)
%%%    Transmission of Syslog Messages over TCP                        (rfc6587)
%%%
%%%    TCP only supports Octet counting framing.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(syslog).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% API
-export([open/0, open/1,
         close/1,
         send/2, send/3,
         recv/1, recv/2,
         accept/2,
         setopts/2
        ]).

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2,
         frame/2, unframe/2
        ]).

%% Exported types
-export_type([transport/0, entry/0]).

%% Includes

%% Records
-record(opts, {type        = udp    :: udp | tcp | tls,
               role        = client :: client | server,
               port                 :: integer(),
               opts        = []     :: [{atom(), _}],
               ipv         = ipv4   :: ipv4 | ipv6,
               dest                 :: inet:ip_address() | inet:hostname(),
               dest_port            :: inet:port(),
               timeout              :: integer(),
               return_type = iolist :: iolist | binary}).

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

%% Types
-type opt() :: none.
-type transport() :: #transport{}.
-type entry() :: map().
-type socket_options() :: gen_udp:option() | gen_tcp:option() | ssl:option().

%% Defines
-define(UTF8_BOM, 239,187,191).

%% ===================================================================
%% API functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec open() -> transport() | {error, _}.
%%--------------------------------------------------------------------
open() -> open(#opts{}).

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%
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
    Port1 = port(udp, Port),
    DestPort1 = port(udp, DestPort),
    TransportOpts1 = case IPv of
                         ipv4 -> [inet, binary | TransportOpts];
                         ipv6 -> [inet6, binary | TransportOpts]
                     end,
    try gen_udp:open(Port1, TransportOpts1) of
        {ok, Sock} -> #transport{port = Port1,
                                 role = Role,
                                 socket = Sock,
                                 dest = Dest,
                                 dest_port = DestPort1};
        Error -> Error
    catch
        error:Error -> {error, Error};
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
    Port1 = port(udp, Port),
    DestPort1 = port(udp, DestPort),
    TransportOpts1 = case IPv of
                         ipv4 -> [inet, binary | TransportOpts];
                         ipv6 -> [inet6, binary | TransportOpts]
                     end,
    case {Role, Timeout} of
        {client, undefined} ->
            try gen_tcp:connect(Dest, DestPort1, TransportOpts1) of
                {ok, Sock} -> #transport{type = tcp,
                                         role = client,
                                         socket = Sock,
                                         dest = Dest,
                                         dest_port = DestPort1};
                Error -> Error
            catch
                error:Error -> {error, Error};
                Class:Error -> {error, {Class, Error}}
            end;
        {client, Timeout} ->
            try gen_tcp:connect(Dest, DestPort1, TransportOpts1, Timeout) of
                {ok, Sock} -> #transport{type = tcp,
                                         role = client,
                                         socket = Sock,
                                         dest = Dest,
                                         dest_port = DestPort1};
                Error -> Error
            catch
                error:Error -> {error, Error};
                Class:Error -> {error, {Class, Error}}
            end;
        {server, _} ->
            try gen_tcp:listen(Port1, TransportOpts1) of
                {ok, Sock} -> #transport{type = tcp,
                                         role = server,
                                         listen_socket = Sock};
                Error -> Error
            catch
                error:Error -> {error, Error};
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
    Port1 = port(udp, Port),
    DestPort1 = port(udp, DestPort),
    TransportOpts1 = case IPv of
                         ipv4 -> [inet, binary, {versions, ['tlsv1.2']} |
                                  TransportOpts];
                         ipv6 -> [inet6, binary, {versions, ['tlsv1.2']} |
                                  TransportOpts]
                     end,
    case {Role, Timeout} of
        {client, undefined} ->
            try ssl:connect(Dest, DestPort1, TransportOpts1) of
                {ok, Sock} -> #transport{type = tls,
                                         role = client,
                                         socket = Sock,
                                         dest = Dest,
                                         dest_port = DestPort1};
                Error -> Error
            catch
                error:Error -> {error, Error};
                Class:Error -> {error, {Class, Error}}
            end;
        {client, Timeout} ->
            try ssl:connect(Dest, DestPort1, TransportOpts1, Timeout) of
                {ok, Sock} -> #transport{type = tls,
                                         role = client,
                                         socket = Sock,
                                         dest = Dest,
                                         dest_port = DestPort1};
                Error -> Error
            catch
                error:Error -> {error, Error};
                Class:Error -> {error, {Class, Error}}
            end;
        {server, _} ->
            try ssl:listen(Port1, TransportOpts1) of
                {ok, Sock} -> #transport{type = tls,
                                         role = server,
                                         listen_socket = Sock};
                Error -> Error
            catch
                error:Error -> {error, Error};
                Class:Error -> {error, {Class, Error}}
            end
    end;
open(Opts) -> open(parse_opts(Opts)).


%%--------------------------------------------------------------------
%% Function:
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec close(transport()) -> ok | {error, _}.
%%--------------------------------------------------------------------
close(#transport{type = udp, socket = Sock}) ->
    try gen_udp:close(Sock)
    catch error:Error -> {error, Error};
          Class:Error -> {error, {Class, Error}}
    end;
close(#transport{type = tcp, socket = Sock}) ->
    try gen_tcp:close(Sock)
    catch error:Error -> {error, Error};
          Class:Error -> {error, {Class, Error}}
    end;
close(#transport{type = tls, socket = Sock}) ->
    try ssl:close(Sock)
    catch error:Error -> {error, Error};
          Class:Error -> {error, {Class, Error}}
    end.

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec send(transport(), entry()) -> ok | {error, _}.
%%--------------------------------------------------------------------
send(Transport, Entry) -> send(Transport, Entry, #opts{}).

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec send(transport(), entry(), [opt()] | #opts{}) -> ok | {error, _}.
%%--------------------------------------------------------------------
send(Transport = #transport{type = udp, socket = Sock}, Entry, Opts) ->
    case parse_opts(Opts) of
        ParsedOpts = #opts{dest = undefined} ->
            #transport{dest = Dest, dest_port = Port} = Transport,
            try gen_udp:send(Sock, Dest, Port, encode(Entry, ParsedOpts))
            catch error:Error -> {error, Error};
                  Class:Error -> {error, {Class, Error}}
            end;
        ParsedOpts  = #opts{dest = Dest, dest_port = Port}->
            try gen_udp:send(Sock, Dest, Port, encode(Entry, ParsedOpts))
            catch error:Error -> {error, Error};
                  Class:Error -> {error, {Class, Error}}
            end
    end;
send(#transport{type = tcp, socket = Sock}, Entry, Opts) ->
    try gen_tcp:send(Sock, frame(tcp, encode(Entry, Opts)))
    catch error:Error -> {error, Error};
          Class:Error -> {error, {Class, Error}}
    end;
send(#transport{type = tls, socket = Sock}, Entry, Opts) ->
    try ssl:send(Sock, frame(tls, encode(Entry, Opts)))
    catch error:Error -> {error, Error};
          Class:Error -> {error, {Class, Error}}
    end.

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec recv(transport()) -> {ok, entry(), transport()} | {error, _}.
%%--------------------------------------------------------------------
recv(Transport) -> recv(Transport, #opts{}).

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec recv(transport(), [opt()] | #opts{}) ->
                  {ok, entry(), transport()} | {error, _}.
%%--------------------------------------------------------------------
recv(#transport{type = udp, socket = Sock}, Opts) ->
    case parse_opts(Opts) of
        #opts{timeout = undefined} ->
            try gen_udp:recv(Sock, 0) of
                {ok, {_, _, Packet}} -> {ok, decode(Packet, Opts)};
                Error -> Error
            catch error:Error -> {error, Error};
                  Class:Error -> {error, {Class, Error}}
            end;
        #opts{timeout = Timeout} ->
            try gen_udp:recv(Sock, Timeout) of
                {ok, {_, _, Packet}} -> {ok, decode(Packet, Opts)};
                Error -> Error
            catch error:Error -> {error, Error};
                  Class:Error -> {error, {Class, Error}}
            end
    end;
recv(Transport = #transport{type = tcp, socket = Sock, buf = Buf}, Opts) ->
    case parse_opts(Opts) of
        #opts{timeout = undefined} ->
            try gen_tcp:recv(Sock, 0) of
                {ok, {_, _, Packet}} ->
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
                            catch error:Error -> {error, Error};
                                  Class:Error -> {error, {Class, Error}}
                            end;
                        {Frame, Buf1} ->
                            {ok,
                             decode(Frame, Opts),
                             Transport#transport{buf = Buf1}}
                    end;
                Error ->
                    Error
            catch error:Error -> {error, Error};
                  Class:Error -> {error, {Class, Error}}
            end;
        #opts{timeout = Timeout} ->
            try gen_tcp:recv(Sock, 0, Timeout) of
                {ok, {_, _, Packet}} ->
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
                            catch error:Error -> {error, Error};
                                  Class:Error -> {error, {Class, Error}}
                            end;
                        {Frame, Buf1} ->
                            {ok,
                             decode(Frame, Opts),
                             Transport#transport{buf = Buf1}}
                    end;
                Error ->
                    Error
            catch error:Error -> {error, Error};
                  Class:Error -> {error, {Class, Error}}
            end
    end;
recv(Transport = #transport{type = tls, socket = Sock, buf = Buf}, Opts) ->
    case parse_opts(Opts) of
        #opts{timeout = undefined} ->
            try ssl:recv(Sock, 0) of
                {ok, {_, _, Packet}} ->
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
                            catch error:Error -> {error, Error};
                                  Class:Error -> {error, {Class, Error}}
                            end;
                        {Frame, Buf1} ->
                            {ok,
                             decode(Frame, Opts),
                             Transport#transport{buf = Buf1}}
                    end;
                Error ->
                    Error
            catch error:Error -> {error, Error};
                  Class:Error -> {error, {Class, Error}}
            end;
        #opts{timeout = Timeout} ->
            try ssl:recv(Sock, 0, Timeout) of
                {ok, {_, _, Packet}} ->
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
                            catch error:Error -> {error, Error};
                                  Class:Error -> {error, {Class, Error}}
                            end;
                        {Frame, Buf1} ->
                            {ok,
                             decode(Frame, Opts),
                             Transport#transport{buf = Buf1}}
                    end;
                Error ->
                    Error
            catch error:Error -> {error, Error};
                  Class:Error -> {error, {Class, Error}}
            end
    end.

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec accept(transport(), [opt()]) -> ok | {error, _}.
%%--------------------------------------------------------------------
accept(Transport = #transport{type = tcp, listen_socket = LSock}, Opts) ->
    case parse_opts(Opts) of
        #opts{timeout = undefined} ->
            try gen_tcp:accept(LSock) of
                {ok, Sock}  -> Transport#transport{socket = Sock};
                Error -> Error
            catch
                error:Error -> {error, Error};
                Class:Error -> {error, {Class, Error}}
            end;
        #opts{timeout = Timeout} ->
            try gen_tcp:accept(LSock, Timeout) of
                {ok, Sock}  -> Transport#transport{socket = Sock};
                Error -> Error
            catch
                error:Error -> {error, Error};
                Class:Error -> {error, {Class, Error}}
            end
    end;
accept(Transport = #transport{type = tls, listen_socket = LSock}, Opts) ->
    case parse_opts(Opts) of
        #opts{timeout = undefined} ->
            try ssl:transport_accept(LSock) of
                {ok, Sock}  ->
                    case ssl:ssl_accept(Sock) of
                        ok -> Transport#transport{socket = Sock};
                        Error -> Error
                    end;
                Error ->
                    Error
            catch
                error:Error -> {error, Error};
                Class:Error -> {error, {Class, Error}}
            end;
        #opts{timeout = Timeout} ->
            try ssl:transport_accept(LSock, Timeout) of
                {ok, Sock}  ->
                    case ssl:ssl_accept(Sock) of
                        ok -> Transport#transport{socket = Sock};
                        Error -> Error
                    end;
                Error ->
                    Error
            catch
                error:Error -> {error, Error};
                Class:Error -> {error, {Class, Error}}
            end
    end.

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec setopts(transport(), socket_options()) -> ok | {error, _}.
%%--------------------------------------------------------------------
setopts(#transport{type = udp, socket = Sock}, Options) ->
    try inet:setopts(Sock, Options)
    catch error:Error -> {error, Error};
          Class:Error -> {error, {Class, Error}}
    end;
setopts(#transport{type = tcp, socket = Sock}, Options) ->
    try inet:setopts(Sock, Options)
    catch error:Error -> {error, Error};
          Class:Error -> {error, {Class, Error}}
    end;
setopts(#transport{type = tls, socket = Sock}, Options) ->
    try ssl:setopts(Sock, Options)
    catch error:Error -> {error, Error};
          Class:Error -> {error, {Class, Error}}
    end.

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Entry) -> Syslog entry.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent of encode(Entry, []) -> Syslog encode.
%% @end
%%--------------------------------------------------------------------
-spec encode(entry()) -> iolist().
%%--------------------------------------------------------------------
encode(Entry) -> encode(Entry, #opts{}).

%%--------------------------------------------------------------------
%% Function: encode(Entry, Options) -> Syslog entry
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     iolist -> a iolist is returned
%% @end
%%--------------------------------------------------------------------
-spec encode(entry(), [opt()] | #opts{}) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Entry, Opts = #opts{}) -> do_encode(Entry, Opts);
encode(Entry, Opts) ->
    ParsedOpts = parse_opts(Opts, #opts{}),
    case ParsedOpts#opts.return_type of
        iolist-> do_encode(Entry, ParsedOpts);
        binary -> iolist_to_binary(do_encode(Entry, ParsedOpts))
    end.

%%--------------------------------------------------------------------
%% Function: decode(Binary) -> Entry.
%% @doc
%%   Decodes the binary into a structured Erlang term.
%%   Equivalent of decode(Binary, []) -> URI.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> entry().
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, #opts{}).

%%--------------------------------------------------------------------
%% Function: decode(Binary, Options) -> Entry.
%% @doc
%%   Decodes the binary into a structured Erlang.
%%   Decode will give an exception if the binary is not well formed
%%   Syslog entry.
%%   Options are:
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), [opt()] | #opts{}) -> entry().
%%--------------------------------------------------------------------
decode(Binary, Opts = #opts{}) -> do_decode(Binary, Opts);
decode(Binary, Opts) -> do_decode(Binary, parse_opts(Opts, #opts{})).

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec frame(atom(), iodata()) -> iodata().
%%--------------------------------------------------------------------
frame(_, Data) -> [integer_to_binary(iolist_size(Data)), $\s, Data].

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec unframe(atom(), iodata()) -> iodata().
%%--------------------------------------------------------------------
unframe(_, Data) -> unframe1(Data, <<>>).

unframe1(<<$\s, T/binary>>, Acc) ->
    case {binary_to_integer(Acc), byte_size(T)} of
        {Octets, Size} when Size < Octets -> {more, Octets - Size};
        {Octets, _} ->
            <<Frame:Octets/binary, T1>> = T,
            {Frame, T1}
    end;
unframe1(<<H, T/binary>>, Acc) ->
    unframe1(T, <<Acc/binary, H>>).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

do_encode(Entry, _) ->
    [encode_header(Entry), $\s,
     encode_structured_data(Entry),
     encode_msg(Entry)].

encode_header(Entry) ->
    Header = maps:get(header, Entry, #{}),
    Severity = encode_severity(maps:get(severity, Header, info)),
    Facility = encode_facility(maps:get(facility, Header, user)),
    Version = integer_to_binary(maps:get(version, Header, 1)),
    Timestamp = encode_time_stamp(maps:get(time_stamp, Header, '-'), Header),
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

encode_time_stamp('-', _) -> "-";
encode_time_stamp({{Y, M , D}, {H, Mi, S}}, Header) ->
    [integer_to_binary(Y), $-,
     integer_to_binary(M), $-,
     integer_to_binary(D), $T,
     integer_to_binary(H), $:,
     integer_to_binary(Mi), $:,
     integer_to_binary(S), $:,
     case maps:get(fraction, Header, undefined) of
         undefined -> [];
         Fraction -> [$., integer_to_binary(Fraction)]
     end,
     case maps:get(offset_sign, Header, undefined) of
         'Z' -> [$Z];
         '+' ->
             {HO, MiO} = maps:get(offset, Header),
             [$+, integer_to_binary(HO), $:, integer_to_binary(MiO)];
         '-' ->
             {HO, MiO} = maps:get(offset, Header),
             [$-, integer_to_binary(HO), $:, integer_to_binary(MiO)]
     end].

encode_structured_data(#{structured := Structured}) ->
    [encode_structured_data(Id, Params) || {Id, Params}  <- Structured];
encode_structured_data(_) ->
    [].

encode_structured_data(Id, Params) ->
    [$[, Id, [encode_structured_param(Param) || Param <- Params], $]].

%% TODO: Add escaping
encode_structured_param({Name, Value}) -> [$\s, Name, $=, $", Value, $"].

encode_msg(#{msg := #{type := utf8, content:=Msg}}) -> [$\s,<<?UTF8_BOM>>,Msg];
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
    Facility = decode_facility(Priority div 8),
    Severity = decode_severity(Priority - (Facility * 8)),
    decode_version(T, #{facility => Facility, severity => Severity}, <<>>);
decode_header(<<H, T/binary>>, Acc) ->
    decode_header(T, <<Acc/binary, H>>).

decode_version(<<$\s, T/binary>>, Header, Acc) ->
    decode_timestamp(T, Header#{version => binary_to_integer(Acc)});
decode_version(<<H, T/binary>>, Header, Acc) ->
    decode_version(T, Header, <<Acc/binary ,H>>).

decode_timestamp(<<$-, $\s, T/binary>>, Header) ->
    decode_hostname(T, Header#{time_stamp => nil});
decode_timestamp(<<Y:32, $-, M:2/bytes, $-, D:2/bytes, $T,
                   H:2/bytes, $:, Mi:2/bytes, $:, S:2/bytes,
                   T/binary>>,
                 Header) ->
    Header1 = Header#{time_stamp => {{Y,
                                      binary_to_integer(M),
                                      binary_to_integer(D)},
                                     {binary_to_integer(H),
                                      binary_to_integer(Mi),
                                      binary_to_integer(S)}}},
    decode_timestamp1(T, Header1).

decode_timestamp1(<<$Z, $\s, T/binary>>, Header) -> decode_hostname(T, Header);
decode_timestamp1(<<$., T/binary>>, Header) -> decode_fraction(T, Header, <<>>);
decode_timestamp1(<<$+, T/binary>>, Header) ->
    decode_offset(T, Header#{offset_sign => '+'});
decode_timestamp1(<<$-, T/binary>>, Header ) ->
    decode_offset(T, Header#{offset_sign => '-'}).

decode_fraction(<<$Z, $\s, T/binary>>, Header, Acc) ->
    decode_hostname(T, Header#{fraction => Acc}).

decode_offset(<<H:2/bytes, $:, M:2/bytes, $\s, T/binary>>, Header) ->
    decode_hostname(T, Header#{offset => {H, M}}).

decode_hostname(<<$-, $\s, T/binary>>, Header) ->
    decode_appname(T, Header#{host_name => nil});
decode_hostname(Bin, Header) ->
    {HostName, T} = decode_string(Bin, <<>>),
    decode_appname(T, Header#{host_name => HostName}).

decode_appname(<<$-, $\s, T/binary>>, Header) ->
    decode_proc_id(T, Header#{app_name => nil});
decode_appname(Bin, Header) ->
    {AppName, T} = decode_string(Bin, <<>>),
    decode_proc_id(T, Header#{app_name => AppName}).

decode_proc_id(<<$-, $\s, T/binary>>, Header) ->
    decode_msg_id(T, Header#{proc_id => nil});
decode_proc_id(Bin, Header) ->
    {ProcId, T} = decode_string(Bin, <<>>),
    decode_msg_id(T, Header#{proc_id => ProcId}).

decode_msg_id(<<$-, $\s, T/binary>>, Header) -> {Header#{msg_id => nil}, T};
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
    {Acc, T}.

decode_sd_id(<<$], T/binary>>, Acc) -> {Acc, [], T};
decode_sd_id(<<$\s, T/binary>>, Acc) ->
    {Params, T1} = decode_sd_params(T, []),
    {Acc, Params, T1}.

decode_sd_params(<<$], T/binary>>, Acc) -> {lists:reverse(Acc), T};
decode_sd_params(Bin, Acc) ->
    {Param, T} = decode_sd_param(Bin, <<>>),
    decode_sd_params(T, [Param | Acc]).

decode_sd_param(<<$=, $", T/binary>>, Acc) ->
    {Value, T1} = decode_sd_value(T, <<>>),
    {{Acc, Value}, T1};
decode_sd_param(<<H, T/binary>>, Acc) ->
    decode_sd_param(T, <<Acc/binary, H>>).

%% TODO: add handling of escape
decode_sd_value(<<$=, T/binary>>, Acc) -> {Acc, T};
decode_sd_value(<<H/utf8, T/binary>>, Acc) ->
    decode_sd_value(T, <<Acc/binary, H/utf8>>).

decode_msg(<<?UTF8_BOM, T>>) ->
    {utf8, decode_msg_utf8(T, T)};
decode_msg(Bin) ->
    {any, Bin}.

decode_msg_utf8(<<>>, Msg) -> Msg;
decode_msg_utf8(<<_/utf8, T>>, Msg) -> decode_msg_utf8(T, Msg).

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
%% Frame
%% ===================================================================

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts(Rec = #opts{}) -> Rec;
parse_opts(Opts) -> parse_opts(Opts, []).

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(udp, Opts) -> Opts#opts{type = udp};
parse_opt(tcp, Opts) -> Opts#opts{type = tcp};
parse_opt(tls, Opts) -> Opts#opts{type = tls};
parse_opt({port, Port}, Opts) -> Opts#opts{port = Port};
parse_opt({opts, TransportOpts}, Opts) -> Opts#opts{opts = TransportOpts};
parse_opt(ipv4, Opts) -> Opts#opts{ipv = ipv4};
parse_opt(ipv6, Opts) -> Opts#opts{ipv = ipv6};
parse_opt({destination, Destination}, Opts) -> Opts#opts{dest = Destination};
parse_opt({destination_port, Port}, Opts) -> Opts#opts{dest_port = Port};
parse_opt({timeout, Timeout}, Opts) -> Opts#opts{timeout = Timeout};
parse_opt(_, Opts) -> erlang:error(badarg, Opts).

port(udp, undefined) -> 154;
port(tcp, undefined) -> 601;
port(tls, undefined) -> 6514;
port(_, Port) -> Port.
