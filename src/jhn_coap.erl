%%==============================================================================
%% Copyright 2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%n
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%%  A Syslog library based on:
%%%    The Constrained Application Protocol (CoAP)                     (rfc7252)
%%%    Block-Wise Transfers in the Constrained Application Protocol (CoAP)
%%%                                                                    (rfc7959)
%%%    PATCH and FETCH Methods for the Constrained Application Protocol (CoAP)
%%%                                                                    (rfc8132)
%%%    CoAP (Constrained Application Protocol) over TCP, TLS, and WebSockets
%%%                                                                    (rfc8323)
%%%
%%%  https://www.iana.org/assignments/core-parameters/core-parameters.xhtml
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_coap).
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
         decode/1, decode/2
        ]).

%% Exported types
-export_type([transport/0, line/0]).

%% Includes

%% Records
-record(opts, {type        = udp       :: type(),
               role        = client    :: client | server,
               port                    :: integer() | undefined,
               opts        = []        :: [{atom(), _}],
               ipv         = ipv4      :: ipv4 | ipv6,
               dest                    :: inet:ip_address() | inet:hostname(),
               dest_port               :: inet:port(),
               timeout                 :: integer() | undefined,
               version     = 'tlsv1.2' :: 'tlsv1.2' | 'tlsv1.3',
               return_type = iolist    :: iolist | binary}).

-record(transport, {type                 :: type() | listen_type(),
                    role                 :: client | server,
                    port                 :: inet:port(),
                    ipv                  :: ipv4 | ipv6 | undefined,
                    dest                 :: inet:ip_address() | inet:hostname(),
                    dest_port            :: inet:port(),
                    socket               :: gen_udp:socket() |
                                            gen_tcp:socket() |
                                            ssl:socket(),
                    listen_socket        :: gen_tcp:socket() | ssl:socket(),
                    buf           = <<>> :: binary()
                   }).

-record(msg, {type    = non_conf :: message_type(),
              code               :: message_code(),
              id                 :: non_neg_integer(),
              token              :: non_neg_integer(),
              options = []       :: [message_option()],
              payload = <<>>     :: iodata()}

%% Types
-type opt() :: _.
-type transport() :: #transport{}.
-type line() :: map().
-type type() :: udp | dtls | tcp | tls.
-type listen_type() :: dtls_listen | tcp_listen | tls_listen.
-type socket_options() :: gen_udp:option() | gen_tcp:option() | ssl:option().

message_type()   :: conf | non_conf | ack | reset.
message_code()   :: _.
message_option() :: _.

%% Defines
-define(UTF8_BOM, 239,187,191).

-define(DGRAM_MAX, 65507).

%% Message format

%% Version (Ver): 2-bit unsigned integer.
-definr(VER, 1:2).

%% Type (T): 2-bit unsigned integer
-define(CON, 0).
-define(NON, 1).
-define(ACK, 2).
-define(RST, 3).

%% Token Length (TKL): 4-bit unsigned integer.

%% Code: 8-bit unsigned integer, split into a 3-bit class, and a 5-bit detail 
%% Class
%% Request
-define(REQUEST, 0).
%% Success
-define(SUCCESS, 2).
%% Client error
-define(CLIENT_ERROR), 4).
%% Server error
-define(SERVER_ERROR, 5).

%% EMPTY, special case 0.00
-define(EMPTY, {?REQUEST, 0}).

%% Request codes
%% rfc7252
-define(GET, {?REQUEST, 1}).
-define(POST, {?REQUEST, 2}).
-define(PUT, {?REQUEST, 3}).
-define(DELETE, {?REQUEST, 4}).
%% rfc8132
-define(FETCH, {?REQUEST, 5}).
-define(PATCH, {?REQUEST, 6}).
-define(I_PATCH, {?REQUEST, 7}).

%% Response codes
%% rfc7252
-define(CREATED, {?SUCCESS, 1}).
-define(DELETED, {?SUCCESS, 2}).
-define(VALID, {?SUCCESS, 3}).
-define(CHANGED, {?SUCCESS, 4}).
-define(CONTENT, {?SUCCESS, 5}).
%% rfc8323
-define(CONTINUE, {?SUCCESS, 31}).

%% Client Error Codes codes
%% rfc7252
-define(BAD_REQUEST, {?CLIENT_ERROR, 0}).
-define(UNAUTHORIZED, {?CLIENT_ERROR, 1}).
-define(BAD_OPTION, {?CLIENT_ERROR, 2}).
-define(FORBIDDEN, {?CLIENT_ERROR, 3}).
-define(NOT_FOUND, {?CLIENT_ERROR, 4}).
-define(METHOD_NOT_ALLOWED, {?CLIENT_ERROR, 5}).
-define(NOT_ACCEPTABLE, {?CLIENT_ERROR, 6}).
%%n
-define(PRECONDITION_FAILED, {?CLIENT_ERROR, 12}).
-define(REQUEST_ENTITY_TOO_LARGE, {?CLIENT_ERROR, 13}).
%%
-define(UNSUPPORTED_CONTENT_FORMAT, {?CLIENT_ERROR, 15}).
%% rfc7259
-define(REQUEST_ENTITY_INCOMPLETE, {?CLIENT_ERROR, 8}).
%% rfc8132
-define(CONFLICT, {?CLIENT_ERROR, 9}).
%%
-define(UNPROCESSABLE_ENTITY, {?CLIENT_ERROR, 22}).
%% rfc8516
-define(TOO_MANY_REQUESTS, {?CLIENT_ERROR, 29}).

%% Server Error Codes codes
%% rfc7252
-define(INTERNAL_SERVER_ERROR, {?SERVER_ERROR, 0}).
-define(NOT_IMPLEMENTED, {?SERVER_ERROR, 1}).
-define(BAD_GATEWAY, {?SERVER_ERROR, 2}).
-define(SERVICE_UNAVAILABLE, {?SERVER_ERROR, 3}).
-define(GATEWAY_TIMEOUT, {?SERVER_ERROR, 4}).
-define(PROXYING_NOT_SUPPORTED, {?SERVER_ERROR, 5}).
%% rfc8768
-define(HOP_LIMIT_REACHED, {?SERVER_ERROR, 8}).

%% Message ID: 16-bit unsigned integer in network byte order.

%% Token is any

%% Options

%% rfc7252
-define(IF_MATCH, 1).
%%
-define(URI_HOST, 3).
-define(ETAG, 4).
-define(IF_NONE_MATCH, 5).
-define(OBSERVE, 6).
-define(URI_PORT, 7).
-define(LOCATION_PATH, 8).
%%
-define(LOCATION_PATH, 11).
-define(CONTENT_FORMAT, 12).
%%
-define(MAX_AGE, 14).
-define(URI_QUERY, 15).
-define(ACCEPT, 17).
%%
-define(LOCATION_QUERY, 20).
%%
-define(PROXY_URI, 35).
%%
-define(PROXY_SCHEME, 39).
%%
-define(SIZE1, 60).
-define(, ).
%% rfc7959
-define(BLOCK2, 23).
%%
-define(BLOCK1, 27).
-define(SIZE2, 28).

%% rfc8768
-define(HOP_LIMIT, 16).
%% rfc9177
-define(Q_BLOCK1, 19).
%%
-define(Q_BLOCK2, 31).
%%
-define(PAYLOAD_MARKER, 16#FF:8).


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
%%   port for DTLS/TCP/TLS for the server, UDP the difference is quite moot.
%%
%%   Options are:
%%     client -> opens a client transport (default)
%%     server -> open a server transport
%%     udp -> uses the UDP protocol (default)
%%     dtls -> uses the DTLS v1.2 over UDP
%%     tcp -> uses the TCP protocol
%%     tls -> uses TLS v1.2(default) or TLS v1.3 over TCP
%%     ipv4 -> IPv4 addresses are used (default)
%%     ipv6 -> IPv6 addresses are used
%%     port -> The port used by client or server with the default in the
%%             client case is 0 (the underlying OS assigns an available
%%             port number) for the server the default ports are as follows:
%%             UDP/514, UDP/6514, TCP/601, TLS/6514
%%     destination -> the IP address or hostname of the server to connect
%%                    to in the case TCP and TLS and the default server for
%%                    the UDP/DTLS case
%%     destination_port -> the port of the server to connect to in the case
%%                         TCP/TLS and the default server for the UDP/DTLS case
%%     version -> the version used for TLS, v1.2(default) or v1.3.
%%     opts -> options to the transport's UDP, DTLS, TCP, or TLS, see the
%%             documentation of gen_udp, gen_tcp, ssl respectively
%%     timeout -> the time in milliseconds the request is allowed to complete
%% @end
%%--------------------------------------------------------------------
-spec open([opt()] | #opts{}) -> transport() | {error, _}.
%%--------------------------------------------------------------------
open(_) -> tbd.
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
accept(_, _) -> tbd.

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
send(_, _, _) -> tbd.

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
recv(_, _) -> tbd.

%%--------------------------------------------------------------------
%% Function: setopts(Transport, Options) -> ok | Error.
%% @doc
%%   Sets one or more options for a transport socket according to the transport.
%% @end
%%--------------------------------------------------------------------
-spec setopts(transport(), socket_options()) -> ok | {error, _}.
%%--------------------------------------------------------------------
setopts(_, _) ->tbd.

%%--------------------------------------------------------------------
%% Function: controlling_process(Transport, Pid) -> ok | Error.
%% @doc
%%   Assigns a new controlling process Pid to the Transport socket.
%% @end
%%--------------------------------------------------------------------
-spec controlling_process(transport(), pid()) -> ok | {error, _}.
%%--------------------------------------------------------------------
controlling_process(_, _) -> tbd.

%%--------------------------------------------------------------------
%% Function: close(Transport) -> ok | Error.
%% @doc
%%   Closes an already established transport.
%% @end
%%--------------------------------------------------------------------
-spec close(transport()) -> ok | {error, _}.
%%--------------------------------------------------------------------
close(_) -> tbd.

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
decode(Binary) -> decode(Binary, []).

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
decode(Binary, Opts) -> do_decode(Binary, parse_opts(Opts, #opts{})).


%% ===================================================================
%% Internal functions.
%% ===================================================================


%% ===================================================================
%% Encoding
%% ===================================================================

encode(Msg, _) ->
    #msg{type = Type,
         code = Code,
         id = Id,
         token = TK,
         options = Options,
         payload = Payload} = Msg,
    TKL = byte_size(TK),
    [<<?VER, (encode_type(Type)):2, TKL:4,
       (encode_code(Code)):8, Id:16, TK:(TKL * 8)>>,
     encode_options(Options),
     Payload
    ].

encode_type(conf) -> ?CON;
encode_type(non_conf) -> ?NON;
encode_type(ack) -> ?ACK;
encode_type(reset) -> ?RST.

%% Request
encode_code(get) -> ?GET;
encode_code(post) -> ?POST;
encode_code(put) -> ?PUT;
encode_code(delete) -> ?DELETE;
encode_code(fetch) -> ?FETCH;
encode_code(patch) -> ?PATCH;
encode_code(i_patch) -> ?I_PATCH;
%% Success
encode_code(created) -> ?CREATED;
encode_code(deleted) -> ?DELETED;
encode_code(valid) -> ?VALID;
encode_code(changed) -> ?CHANGED;
encode_code(content) -> ?CONTENT;
encode_code(continue) -> ?CONTINUE;
%% Client error
encode_code(bad_request) -> ?BAD_REQUEST;
encode_code(unauthorized) -> ?UNAUTHORIZED;
encode_code(bad_option) -> ?BAD_OPTION;
encode_code(forbidden) -> ?FORBIDDEN;
encode_code(not_found) -> ?NOT_FOUND;
encode_code(method_not_allowed) -> ?METHOD_NOT_ALLOWED;
encode_code(not_acceptable) -> ?NOT_ACCEPTABLE;
encode_code(precondition_failed) -> ?PRECONDITION_FAILED;
encode_code(request_entity_too_large) -> ?REQUEST_ENTITY_TOO_LARGE;
encode_code(unsupported_content_format) -> ?UNSUPPORTED_CONTENT_FORMAT;
encode_code(request_entity_incomplete) -> ?REQUEST_ENTITY_INCOMPLETE;
encode_code(conflict) -> ?CONFLICT;
encode_code(unprocessable_entity) -> ?UNPROCESSABLE_ENTITY;
encode_code(too_many_requests) -> ?TOO_MAY_REQUESTS;
%% Server error
encode_code(internal_server_error) -> ?INTERNAL_SERVER_ERROR;
encode_code(not_implemented) -> ?NOT_IMPLEMENTED;
encode_code(bad_gateway) -> ?BAD_GATEWAY;
encode_code(service_unavailable) -> ?SERVICE_UNAVAILABLE;
encode_code(gateway_timeout) -> ?GATEWAY_TIMEOUT;
encode_code(proxying_not_supported) ->  ?PROXYING_NOT_SUPPORTED;
encode_code(hop_limit_reached) -> ?HOP_LIMIT_REACHED.

%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(<<?VER, Type:2, TKL:4, Class:3, Detail:5, Id:16, TK:(TKL * 8),
            T/binary >>, _) ->
    {Options, Payload} = decode_options(T, 0, []),
    #msg{type = decode_type(Type),
         code = decode_code({Class, Detail}),
         id = Id,
         token = TK,
         options = Options,
         payload = Payload}.

decode_type(?CON) -> conf;
decode_type(?NON) -> non_conf;
decode_type(?ACK) -> ack;
decode_type(?RST) -> reset.

%% Request
decode_code(?GET) -> get;
decode_code(?POST) -> post;
decode_code(?PUT) -> put;
decode_code(?DELETE) -> delete;
decode_code(?FETCH) -> fetch;
decode_code(?PATCH) -> patch;
decode_code(?I_PATCH) -> i_patch;
%% Success
decode_code(?CREATED) -> created;
decode_code(?DELETED) -> deleted;
decode_code(?VALID) -> valid;
decode_code(?CHANGED) -> changed;
decode_code(?CONTENT) -> content;
decode_code(?CONTINUE) -> continue;
%% Client error
decode_code(?BAD_REQUEST) -> bad_request;
decode_code(?UNAUTHORIZED) -> unauthorized;
decode_code(?BAD_OPTION) -> bad_option;
decode_code(?FORBIDDEN) -> forbidden;
decode_code(?NOT_FOUND) -> not_found;
decode_code(?METHOD_NOT_ALLOWED) -> method_not_allowed;
decode_code(?NOT_ACCEPTABLE) -> not_acceptable;
decode_code(?PRECONDITION_FAILED) -> precondition_failed;
decode_code(?REQUEST_ENTITY_TOO_LARGE) -> request_entity_too_large;
decode_code(?UNSUPPORTED_CONTENT_FORMAT) -> unsupported_content_format;
decode_code(?REQUEST_ENTITY_INCOMPLETE) -> request_entity_incomplete;
decode_code(?CONFLICT) -> conflict;
decode_code(?UNPROCESSABLE_ENTITY) -> unprocessable_entity;
decode_code(?TOO_MANY_REQUESTS) -> too_may_requests;
%% Server error
decode_code(?INTERNAL_SERVER_ERROR) -> internal_server_error;
decode_code(?NOT_IMPLEMENTED) -> not_implemented;
decode_code(?BAD_GATEWAY) -> bad_gateway;
decode_code(?SERVICE_UNAVAILABLE) -> service_unavailable;
decode_code(?GATEWAY_TIMEOUT) -> gateway_timeout;
decode_code(?PROXYING_NOT_SUPPORTED) ->  proxying_not_supported;
decode_code(?HOP_LIMIT_REACHED) -> hop_limit_reached.

decode_options(<<>>, _, Acc) -> {lists:reverse(Acc), <<>>};
decode_options(<<_:4, 15:4, _/Binary>>, _, _) -> erlang:error(badarg, []);
decode_options(<<13:4, 13:4, D:8, L:8, V:(L * 8)/binary, T/binary>>, Opt,Acc) ->
    Opt1 = D + 13 + Opt,
    decode_options(T, Opt1, [decode_opt(Opt1, V) | Acc]);
decode_options(<<13:4, 14:4, D:8, L:16, V:(L * 8)/binary, T/binary>>,Opt,Acc) ->
    Opt1 = D + 13 + Opt,
    decode_options(T, Opt1, [decode_opt(Opt1, V) | Acc]);
decode_options(<<13:4, L:4, D:8, V:(L * 8)/binary, T/binary>>, Opt, Acc) ->
    Opt1 = D + 13 + Opt,
    decode_options(T, Opt1, [decode_opt(Opt1, V) | Acc]);
decode_options(<<14:4, 13:4, D:16,L:8, V:(L * 8)/binary, T/binary>>, Opt,Acc) ->
    Opt1 = D + 269 + Opt,
    decode_options(T, Opt1, [decode_opt(Opt1, V) | Acc]);
decode_options(<<14:4, 14:4, D:16,L:16, V:(L * 8)/binary, T/binary>>,Opt,Acc) ->
    Opt1 = D + 269 + Opt,
    decode_options(T, Opt1, [decode_opt(Opt1, V) | Acc]);
decode_options(<<14:4, L:4, D:16, V:(L * 8)/binary, T/binary>>, Opt,Acc) ->
    Opt1 = Delta + 269 + Opt;
decode_options(<<15:4, Payload/binary>>, Opt,Acc) ->
    {lists:revers(Acc), Payload};
decode_options(<<Delta:4, Len:4, Val:(Len * 4)/binary, T/binary>>, Opt, Acc) ->
    Opt1 = Opt + Delta,
    decode_options(T, Opt1, [decode_opt(Opt1, Val) | Acc]).

decode_opt() -> tbd.

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts(Rec = #opts{}) -> Rec;
parse_opts(Opts) -> parse_opts(Opts, #opts{}).

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(udp, Opts) -> Opts#opts{type = udp};
parse_opt(dtls, Opts) -> Opts#opts{type = dtls};
parse_opt(tcp, Opts) -> Opts#opts{type = tcp};
parse_opt(tls, Opts) -> Opts#opts{type = tls};
parse_opt(client, Opts) -> Opts#opts{role = client};
parse_opt(server, Opts) -> Opts#opts{role = server};
parse_opt({port, Port}, Opts) -> Opts#opts{port = Port};
parse_opt({opts, TransportOpts}, Opts) -> Opts#opts{opts = TransportOpts};
parse_opt({version, 'tlsv1.2'}, Opts) -> Opts#opts{version = 'tlsv1.2'};
parse_opt({version, 'tlsv1.3'}, Opts) -> Opts#opts{version = 'tlsv1.3'};
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
    catch _:_ -> Opts#opts{dest = binary_to_list(Dest)}
    end;
parse_opt({destination, Dest}, Opts) ->
    Opts#opts{dest = Dest};
parse_opt(_, Opts) ->
    erlang:error(badarg, Opts).
