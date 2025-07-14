%%==============================================================================
%% Copyright 2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A RESP3 library based on:
%%%    RESP3 specification                                             ()
%%%  https://github.com/redis/redis-specifications/blob/master/protocol/RESP3.md
%%%
%%%  RESP3 datatypes are represented as follows:
%%%
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_resp).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% RESP3 Command API
-export([%% Bitmap
         bitcount/2, bitcount/3,
         bitfield/3, bitfield/4, bitfield_ro/3, bitfield_ro/4,
         %% String
         append/3, append/4,
         decr/2, decr/3, decrby/3, decrby/4,
         get/2, get/3, getdel/2, getdel/3, getex/3, getrange/4, getrange/5,
         incr/2, incr/3, incrby/3, incrby/4, incrbyfloat/3, incrbyfloat/4,
         lcs/3, lcs/4,
         mget/2, mget/3,
         mset/2, mset/3, msetnx/2, msetnx/3,
         set/3, set/4, setrange/4, setrange/5,
         strlen/2, strlen/3
        ]).

%% API
-export([open/0, open/1, close/1]).

%% Library functions
-export([encode/1, encode/2, decode/1, decode/2]).

%% Exported types
-export_type([transport/0]).

%% Includes

%% Records

-record(opts,
        {type        = tcp       :: type(),
         role        = client    :: client | server,
         port                    :: integer() | undefined,
         opts        = []        :: [{atom(), _}],
         ipv         = ipv4      :: ipv4 | ipv6,
         dest                    :: inet:ip_address() | inet:hostname(),
         dest_port               :: inet:port_number() | undefined,
         timeout     = 5000      :: integer() | undefined,
         version     = 'tlsv1.2' :: 'tlsv1.2' | 'tlsv1.3',
         return_type = iolist    :: iolist | binary}).

-record(transport,
        {type                 :: type() | listen_type(),
         role                 :: client | server,
         port                 :: inet:port_number() | undefined,
         ipv                  :: ipv4 | ipv6 | undefined,
         dest                 :: inet:ip_address() | inet:hostname(),
         dest_port            :: inet:port_number() | undefined,
         socket               :: gen_tcp:socket() | ssl:sslsocket() | undefined,
         listen_socket        :: gen_tcp:socket() | ssl:sslsocket() | undefined,
         buf           = <<>> :: binary()
        }).

%% Types
-type opt() :: iolist | binary | {binary(), any()}.
-type transport() :: #transport{}.
-type type() :: tcp | tls.
-type listen_type() :: tcp_listen | tls_listen.
%% -type socket_options() :: gen_tcp:option() |
%%                           ssl:socket_option()| ssl:tls_option().

-type resp3() :: null | boolean() | integer() | float() |
                 simple_string() | blob() | verbatim() | error() | blob_error()|
                 array() | map() | sets:set() | attribute() |
                 push().

-type key() :: atom() | string() | binary().

%% RESP2
-type simple_string() :: {string, binary()}.
-type blob() :: {blob, binary()}.
-type error() :: {error, code(), binary()}.
-type array() :: list().

%% RESP3
-type blob_error() :: {blob_error, code(), binary()}.
-type code() :: binary().
-type verbatim() :: {verbatim, format(), binary()}.
-type format() :: binary().
-type attribute() :: {attribute, map(), resp3()}.
-type push() :: {push, binary(), array()}.
%% -type hello() :: map(). %% setup

%% API types
-type api_string() :: atom() | iodata() | integer() | float().
-type api_opts() :: map().
-type bitfield() :: bitfield_ro() |
                    {set, encoding(), offset(), integer()} |
                    {set, encoding(), offset(), integer(), overflow()} |
                    {incrby , encoding(), offset(), integer()} |
                    {incrby, encoding(), offset(), integer(), overflow()}.

-type bitfield_ro() :: {get, encoding(), offset()}.

-type encoding() :: {u | i, integer()}.
-type offset() :: {plus | times, integer()}.
-type overflow() :: wrap | sat | fail.

%% Defines
-define(LF, <<"\r\n">>).

-define(MAXINT, 16#7FFFFFFFFFFFFFFF).
-define(MININT, -16#8000000000000000).

-define(API_OPTS, #{sync => true}).

%% ===================================================================
%% RESP3 Command API
%% ===================================================================

%% -------------------------------------------------------------------
%% Bitmap
%% -------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec bitcount(key(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
bitcount(Key, Transport) -> bitcount(Key, #{sync => true}, Transport).


%%--------------------------------------------------------------------
-spec bitcount(key(), api_opts(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
bitcount(Key, Opts = #{sync := true}, Transport) ->
    case send_recv(bitcount, [Key], Opts, Transport) of
        {Integer, <<>>} when is_integer(Integer) -> Integer;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec bitfield(key(), [bitfield()], transport()) -> integer() | error().
%%--------------------------------------------------------------------
bitfield(Key, Bitfields, Transport) ->
    bitfield(Key, Bitfields, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec bitfield(key(), [bitfield()], api_opts(), transport()) ->
          integer() | error().
%%--------------------------------------------------------------------
bitfield(Key, Bitfields, Opts = #{sync := true}, Transport) ->
    case send_recv(bitfield, [Key, Bitfields], Opts, Transport) of
        {null, <<>>}  -> null;
        {Array, <<>>} when is_list(Array) -> Array;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec bitfield_ro(key(), [bitfield_ro()], transport()) -> integer() | error().
%%--------------------------------------------------------------------
bitfield_ro(Key, Bitfields, Transport) ->
    bitfield_ro(Key,  Bitfields, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec bitfield_ro(key(), [bitfield_ro()], api_opts(), transport()) ->
          integer() | error().
%%--------------------------------------------------------------------
bitfield_ro(Key, Bitfields, Opts = #{sync := true}, Transport) ->
    case send_recv(bitfield_ro, [Key, Bitfields], Opts, Transport) of
        {null, <<>>}  -> null;
        {Array, <<>>} when is_list(Array) -> Array;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%% -------------------------------------------------------------------
%% String
%% -------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec append(key(), api_string(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
append(Key, Value, Transport) -> append(Key, Value, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec append(key(), api_string(), api_opts(), transport()) ->
          integer() | error().
%%--------------------------------------------------------------------
append(Key, Value, Opts = #{sync := true}, Transport) ->
    case send_recv(append, [Key, Value], Opts, Transport) of
        {Integer, <<>>} when is_integer(Integer) -> Integer;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec decr(key(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
decr(Key, Transport) -> decr(Key, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec decr(key(), api_opts(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
decr(Key, Opts = #{sync := true}, Transport) ->
    case send_recv(decr, [Key], Opts, Transport) of
        {Integer, <<>>} when is_integer(Integer) -> Integer;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec decrby(key(), integer(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
decrby(Key, Value, Transport) -> decrby(Key, Value, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec decrby(key(), integer(), api_opts(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
decrby(Key, Value, Opts = #{sync := true}, Transport) when is_integer(Value) ->
    case send_recv(decrby, [Key, Value], Opts, Transport) of
        {Integer, <<>>} when is_integer(Integer) -> Integer;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec get(key(), transport()) -> binary() | null | error().
%%--------------------------------------------------------------------
get(Key, Transport) -> get(Key, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec get(key(), api_opts(), transport()) -> binary() | null | error().
%%--------------------------------------------------------------------
get(Key, Opts, Transport) ->
    case send_recv(get, [Key], Opts, Transport) of
        {{blob, Value}, <<>>} -> Value;
        {null, <<>>} -> null;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec getdel(key(), transport()) -> binary() | null | error().
%%--------------------------------------------------------------------
getdel(Key, Transport) -> getdel(Key, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec getdel(key(), api_opts(), transport()) -> binary() | null | error().
%%--------------------------------------------------------------------
getdel(Key, Opts = #{sync := true}, Transport) ->
    case send_recv(getdel, [Key], Opts, Transport) of
        {{blob, Value}, <<>>} -> Value;
        {null, <<>>} -> null;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec getex(key(), api_opts(), transport()) -> binary() | null | error().
%%--------------------------------------------------------------------
getex(Key, Opts = #{sync := true}, Transport) ->
    case send_recv(getex, [Key], Opts, Transport) of
        {{blob, Value}, <<>>} -> Value;
        {null, <<>>} -> null;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec getrange(key(), integer(), integer(), transport()) ->
          binary() | null | error().
%%--------------------------------------------------------------------
getrange(Key, Start, End, Transport) ->
    getrange(Key, Start, End, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec getrange(key(), integer(), integer(), api_opts(), transport()) ->
          binary() | null | error().
%%--------------------------------------------------------------------
getrange(Key, Start, End, Opts, Tr) when is_integer(Start), is_integer(End) ->
    case send_recv(getrange, [Key, Start, End], Opts, Tr) of
        {{blob, Value}, <<>>} -> Value;
        {null, <<>>} -> null;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec incr(key(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
incr(Key, Transport) -> incr(Key, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec incr(key(), api_opts(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
incr(Key, Opts = #{sync := true}, Transport) ->
    case send_recv(incr, [Key], Opts, Transport) of
        {Integer, <<>>} when is_integer(Integer) -> Integer;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec incrby(key(), integer(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
incrby(Key, Value, Transport) -> incrby(Key, Value, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec incrby(key(), integer(), api_opts(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
incrby(Key, Value, Opts = #{sync := true}, Transport) when is_integer(Value) ->
    case send_recv(incrby, [Key, Value], Opts, Transport) of
        {Integer, <<>>} when is_integer(Integer) -> Integer;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec incrbyfloat(key(), float() | integer(), transport()) -> float() | error().
%%--------------------------------------------------------------------
incrbyfloat(Key, Value, Transport) ->
    incrbyfloat(Key, Value, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec incrbyfloat(key(), float() | integer(), api_opts(), transport()) ->
          float() | error().
%%--------------------------------------------------------------------
incrbyfloat(Key, Value, Opts = #{sync := true}, Transport)
  when is_float(Value); is_integer(Value) ->
    case send_recv(incrbyfloat, [Key, Value], Opts, Transport) of
        {Float, <<>>} when is_float(Float) -> Float;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec lcs(key(), api_string(), transport()) ->
          binary() | null | integer() | array() | error().
%%--------------------------------------------------------------------
lcs(Key, Match, Transport) -> lcs(Key, Match, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec lcs(key(), api_string(), api_opts(), transport()) ->
          binary() | null | integer() | array() | error().
%%--------------------------------------------------------------------
lcs(Key, Match, Opts = #{sync := true}, Transport) ->
    case send_recv(lcs, [Key, Match], Opts, Transport) of
        {{blob, Value}, <<>>} -> Value;
        {null, <<>>} -> null;
        {I, <<>>} when is_integer(I) -> I;
        {Array, <<>>} when is_list(Array) -> Array;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec mget([key()], transport()) -> array() | error().
%%--------------------------------------------------------------------
mget(Keys, Transport) -> mget(Keys, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec mget([key()], api_opts(), transport()) -> array() | error().
%%--------------------------------------------------------------------
mget(Keys, Opts = #{sync := true}, Transport) ->
    case send_recv(mget, Keys, Opts, Transport) of
        {Array, <<>>} when is_list(Array) -> Array;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec mset([{key(), api_string()}], transport()) -> ok | error().
%%--------------------------------------------------------------------
mset(KeyVals, Transport) -> mset(KeyVals, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec mset([{key(), api_string()}], api_opts(), transport()) -> ok | error().
%%--------------------------------------------------------------------
mset(KeyVals, Opts = #{sync := true}, Transport) ->
    case send_recv(mset, KeyVals, Opts, Transport) of
        {{string, <<"OK">>}, <<>>} -> ok;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec msetnx([{key(), api_string()}], transport()) -> integer() | error().
%%--------------------------------------------------------------------
msetnx(KeyVals, Transport) -> msetnx(KeyVals, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec msetnx([{key(), api_string()}], api_opts(), transport()) ->
          integer() | error().
%%--------------------------------------------------------------------
msetnx(KeyVals, Opts = #{sync := true}, Transport) ->
    case send_recv(msetnx, KeyVals, Opts, Transport) of
        {I, <<>>} when is_integer(I) -> I;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec set(key(), api_string(), transport()) -> ok | error().
%%--------------------------------------------------------------------
set(Key, Value, Transport) -> set(Key, Value, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec set(key(), api_string(), api_opts(), transport()) ->
          ok | binary() | null | error().
%%--------------------------------------------------------------------
set(Key, Value, Opts = #{sync := true}, Transport) ->
    case send_recv(set, [Key, Value], Opts, Transport) of
        {{string, <<"OK">>}, <<>>} -> ok;
        {{blob, Value}, <<>>} -> Value;
        {null, <<>>} -> null;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec setrange(key(), integer(), api_string(), transport()) ->
          integer() | error().
%%--------------------------------------------------------------------
setrange(Key, Offest, Value, Transport) ->
    setrange(Key, Offest, Value, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec setrange(key(), integer(), api_string(), api_opts(), transport()) ->
          integer() | error().
%%--------------------------------------------------------------------
setrange(Key, Offest, Value, Opts = #{sync := true}, Transport)
  when is_integer(Offest) ->
    case send_recv(setrange, [Key, Offest, Value], Opts, Transport) of
        {I, <<>>} when is_integer(I) -> I;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%%--------------------------------------------------------------------
-spec strlen(key(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
strlen(Key, Transport) -> strlen(Key, ?API_OPTS, Transport).

%%--------------------------------------------------------------------
-spec strlen(key(), api_opts(), transport()) -> integer() | error().
%%--------------------------------------------------------------------
strlen(Key, Opts = #{sync := true}, Transport) ->
    case send_recv(strlen, [Key], Opts, Transport) of
        {I, <<>>} when is_integer(I) -> I;
        {Error = {error, _, _}, <<>>} -> Error
    end.

%% ===================================================================
%% API functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: open() -> Transport | Error.
%% @doc
%%   Opens an TCP client transport.
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
%%   port for TCP/TLS for the server.
%%
%%   Options are:
%%     client -> opens a client transport (default)
%%     server -> open a server transport
%%     tcp -> uses the TCP protocol
%%     tls -> uses TLS v1.2(default) or TLS v1.3 over TCP
%%     ipv4 -> IPv4 addresses are used (default)
%%     ipv6 -> IPv6 addresses are used
%%     port -> The port used by client or server with the default in the
%%             client case is 0 (the underlying OS assigns an available
%%             port number) for the server the default ports are as follows:
%%             TCP/601, TLS/6514
%%     destination -> the IP address or hostname of the server to connect
%%                    to in TCP and TLS.
%%     destination_port -> the port of the server to connect to TCP/TLS
%%     version -> the version used for TLS, v1.2(default) or v1.3.
%%     opts -> options to the transport's TCP, or TLS, see the
%%             documentation of gen_tcp, ssl respectively
%%     timeout -> the time in milliseconds the request is allowed to complete
%% @end
%%--------------------------------------------------------------------
-spec open([opt()] | #opts{}) -> transport() | {error, _}.
%%--------------------------------------------------------------------
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
          timeout = Timeout,
          version = Version} = Opts,
    Port1 = port(tls, Port, Role),
    Dest1 = dest(Dest, IPv),
    DestPort1 = port(tls, DestPort, inverse(Role)),
    TransportOpts1 = case IPv of
                         ipv4 -> [inet, binary, {versions, [Version]} |
                                  TransportOpts];
                         ipv6 -> [inet6, binary, {versions, [Version]} |
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
open(Opts) ->
    open(parse_opts(Opts)).

%%--------------------------------------------------------------------
%% Function: close(Transport) -> ok | Error.
%% @doc
%%   Closes an already established transport.
%% @end
%%--------------------------------------------------------------------
-spec close(transport()) -> ok | {error, _}.
%%--------------------------------------------------------------------
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
-spec encode(resp3()) -> iolist().
%%--------------------------------------------------------------------
encode(Resp) -> encode(Resp, []).

%%--------------------------------------------------------------------
-spec encode(resp3(), [opt()]) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Resp, Opts) ->
    case parse_opts(Opts) of
        #opts{return_type = iolist} -> do_encode(Resp);
        #opts{return_type = binary} -> iolist_to_binary(do_encode(Resp))
    end.

%%--------------------------------------------------------------------
-spec decode(binary()) -> resp3().
%%--------------------------------------------------------------------
decode(Binery) -> decode(Binery, []).

%%--------------------------------------------------------------------
-spec decode(binary(), [opt()]) -> resp3().
%%--------------------------------------------------------------------
decode(Binary, _Opts) -> do_decode(Binary).

%% ===================================================================
%% Internal functions.
%% ===================================================================

dest(undefined, ipv4) -> {127, 0, 0,1};
dest(undefined, ipv6) -> {0, 0, 0, 0, 0, 0, 0, 1};
dest(Address, _) -> Address.

port(tcp, undefined, server) -> 6379;
port(tls, undefined, server) -> 6380;
port(_, undefined, client) -> 0;
port(_, Port, _) when is_integer(Port) -> Port.

inverse(client) -> server;
inverse(server) -> client.

%% send(Data, #transport{type = tcp, socket = Sock}) ->
%%     try gen_tcp:send(Sock, Data) catch error:Error -> {error, Error} end.

send_recv(CMD, Args, Opts, #transport{type = tcp, socket = S}) ->
    #opts{timeout = T} = Opts,
    try gen_tcp:send(S, encode_command(CMD, Args, Opts)) of
        ok ->
            receive {tcp, S, Data} ->
                    case decode(Data) of
                        {error, C, E} -> {error, C, E};
                        {blob_error, C, E} -> {error, C, E};
                        Decoded -> Decoded
                    end
            after T -> {error, <<"TCP">>, timeout}
            end;
        {error, Error} ->
            {error, <<"TCP">>, Error}
    catch
        error:Error -> {error, <<"TCP">>, Error}
    end.

%% ===================================================================
%% Encoding
%% ===================================================================

%% -------------------------------------------------------------------
%% Command
%% -------------------------------------------------------------------

%% Bitmap
encode_command(bitcount, [Key], Opts) ->
    EOpts = 
        case Opts of
            #{start := Start, 'end' := End, index := byte} ->
                [Start, End, <<"BYTE">>];
            #{start := Start, 'end' := End, index := bit} ->
                [Start, End, <<"BIT">>];
            #{start := Start, 'end' := End} ->
                [Start, End];
            #{start := Start} ->
                [Start]
        end,
    do_encode([{blob, <<"BITCOUNT">>}, key(Key) | EOpts]);
encode_command(bitfield, [Key, Bitfields], #{}) ->
    Encoding = fun({i, I}) -> {blob, <<$i, (integer_to_binary(I))/binary>>};
                  ({u, I}) -> {blob, <<$i, (integer_to_binary(I))/binary>>}
               end,
    Offest = fun({plus, I}) -> {blob, integer_to_binary(I)};
                ({times, I}) -> {blob, <<$#, (integer_to_binary(I))/binary>>}
             end,
    Overflow = fun(wrap, F) -> [{blob, <<"OVERFLOW">>}, {blob, <<"WRAP">>} | F];
                  (sat, F) -> [{blob, <<"OVERFLOW">>}, {blob, <<"SAT">>} | F];
                  (fail, F) -> [{blob, <<"OVERFLOW">>}, {blob, <<"FAIL">>} | F]
               end,
    Enc = fun({get, E, O}) -> [{blob, <<"GET">>}, Encoding(E), Offest(O)];
             ({set, E, O, V}) ->
                  [{blob, <<"SET">>}, Encoding(E), Offest(O),
                   {blob, integer_to_binary(V)}];
             ({set, E, O, V, OV}) ->
                  Overflow(OV,
                           [{blob, <<"SET">>}, Encoding(E), Offest(O),
                            {blob, integer_to_binary(V)}]);
             ({incrby, E, O, I}) ->
                  [{blob, <<"INCRBY">>}, Encoding(E), Offest(O),
                   {blob, integer_to_binary(I)}];
             ({incrby, E, O, V, OV}) ->
                  Overflow(OV,
                           [{blob, <<"INCRBY">>}, Encoding(E), Offest(O),
                            {blob, integer_to_binary(V)}])
          end,
    EBitfields = lists:foldr(Enc, [], Bitfields),
    do_encode([{blob, <<"BITFIELD">>}, key(Key) | EBitfields]);
encode_command(bitfield_ro, [Key, Bitfields], #{}) ->
    Encoding = fun({i, I}) -> {blob, <<$i, (integer_to_binary(I))/binary>>};
                  ({u, I}) -> {blob, <<$i, (integer_to_binary(I))/binary>>}
               end,
    Offest = fun({plus, I}) -> {blob, integer_to_binary(I)};
                ({times, I}) -> {blob, <<$#, (integer_to_binary(I))/binary>>}
             end,
    Enc = fun({get, E, O}) -> [{blob, <<"GET">>}, Encoding(E), Offest(O)]
          end,
    EBitfields = lists:foldr(Enc, [], Bitfields),
    do_encode([{blob, <<"BITFIELD_RO">>}, key(Key) | EBitfields]);
%% String
encode_command(append, [Key, Value], #{}) ->
    do_encode([{blob, <<"APPEND">>}, key(Key), blob(Value)]);
encode_command(decr, [Key], #{}) ->
    do_encode([{blob, <<"DECR">>}, key(Key)]);
encode_command(decrby, [Key, Value], #{}) ->
    do_encode([{blob, <<"DECRBY">>}, key(Key), Value]);
encode_command(get, [Key], #{}) ->
    do_encode([{blob, <<"GET">>}, key(Key)]);
encode_command(getdel, [Key], #{}) ->
    do_encode([{blob, <<"GETDEL">>}, key(Key)]);
encode_command(getex, [Key], Opts) ->
    Expire = encode_expire(Opts),
    do_encode([{blob, <<"GETEX">>}, key(Key) | Expire]);
encode_command(getrange, [Key, Start, End], #{}) ->
    do_encode([{blob, <<"GETRANGE">>}, key(Key), Start, End]);
encode_command(incr, [Key], #{}) ->
    do_encode([{blob, <<"INCR">>}, key(Key)]);
encode_command(incrby, [Key, Value], #{}) ->
    do_encode([{blob, <<"INCRBY">>}, key(Key), Value]);
encode_command(incrbyfloat, [Key, Value], #{}) ->
    do_encode([{blob, <<"INCRBYFLOAT">>}, key(Key), Value]);
encode_command(lcs, [Key, Match], Opts) ->
    Len = case Opts of
              #{len := true} -> [{blob, <<"LEN">>}];
              _ -> []
          end,
    Idx = case Opts of
              #{idx := true} -> [{blob, <<"IDX">>}];
              _ -> []
          end,
    Minmatchlen = case Opts of
              #{minmatchlen := Min} -> [{blob, <<"MINMATCHLEN">>}, Min];
              _ -> []
          end,
    Withmatchlen = case Opts of
                       #{withmatchlen := true} -> [{blob, <<"WITHMATCHLEN">>}];
                       _ -> []
                   end,
    EOpts = lists:append([Len, Idx, Minmatchlen, Withmatchlen]),
    do_encode([{blob, <<"LCS">>}, key(Key), blob(Match) | EOpts]);
encode_command(mget, Keys, #{}) ->
    do_encode([{blob, <<"MGET">>} | [key(Key) || Key <- Keys]]);
encode_command(mset, KVs, #{}) ->
    EKVs = lists:foldr(fun({K, V}, A) -> [key(K), blob(V) | A] end, [], KVs),
    do_encode([{blob, <<"MSET">>} | EKVs]);
encode_command(msetnx, KVs, #{}) ->
    EKVs = lists:foldr(fun({K, V}, A) -> [key(K), blob(V) | A] end, [], KVs),
    do_encode([{blob, <<"MSETNX">>} | EKVs]);
encode_command(set, [Key, Value], Opts) ->
    Comp = encode_comp(Opts),
    Get = case Opts of
              #{get := true} -> [{blob, <<"GET">>}];
              _ -> []
          end,
    Expire = encode_expire(Opts),
    EOpts = lists:append([Comp, Get, Expire]),
    do_encode([{blob, <<"SET">>}, key(Key), blob(Value) | EOpts]);
encode_command(setrange, [Key, Offest, Value], #{}) ->
    do_encode([{blob, <<"SETRANGE">>}, key(Key), Offest, blob(Value)]);
encode_command(strlen, [Key], #{}) ->
    do_encode([{blob, <<"STRLEN">>}, key(Key)]).

encode_comp(#{nx := true}) -> [{blob, <<"NX">>}];
encode_comp(#{xx := true}) -> [{blob, <<"XX">>}];
encode_comp(#{ifeq := CompValue}) -> [{blob, <<"IFEQ">>}, blob(CompValue)];
encode_comp(_) -> [].

encode_expire(#{persist := true}) -> [{blob, <<"PERSIST">>}];
encode_expire(#{keepttl := true}) -> [{blob, <<"KEEPTTL">>}];
encode_expire(#{ex := S}) when is_integer(S) -> [{blob, <<"EX">>}, S];
encode_expire(#{px := MS}) when is_integer(MS) -> [{blob, <<"PX">>}, MS];
encode_expire(#{exat := S}) when is_integer(S) -> [{blob, <<"EXAT">>}, S];
encode_expire(#{pxat := MS}) when is_integer(MS) -> [{blob, <<"PXAT">>}, MS];
encode_expire(#{exat := TS}) ->
    S = jhn_timestamp:encode(jhn_timestamp:decode(TS), [posix]),
    [{blob, <<"EXAT">>}, S];
encode_expire(#{pxat := TS}) ->
    MS = jhn_timestamp:encode(jhn_timestamp:decode(TS), [posix, milli]),
    [{blob, <<"{PXAT">>}, MS].

key(A) when is_atom(A) -> {blob, atom_to_binary(A)};
key(Key = <<_/binary>>) -> {blob, Key};
key(Key = [_|_]) ->  {blob, iolist_to_binary(Key)}.

blob(Value = <<_/binary>>) -> {blob, Value};
blob(<<>>) -> {blob, <<>>};
blob(Value = [_|_]) -> {blob, iolist_to_binary(Value)};
blob(A) when is_atom(A) -> {blob, atom_to_binary(A)};
blob(I) when is_integer(I) -> {blob, integer_to_binary(I)};
blob(F) when is_float(F) -> {blob, float_to_binary(F, [short])}.

%% -------------------------------------------------------------------
%% Generic
%% -------------------------------------------------------------------

do_encode(null) -> <<"_\r\n">>;
do_encode(true) -> <<"#t\r\n">>;
do_encode(false) -> <<"#f\r\n">>;
do_encode(inf) -> <<",inf\r\n">>;
do_encode(neg_inf) -> <<",-inf\r\n">>;
do_encode(nan) -> <<",nan\r\n">>;
do_encode({string, String}) -> [$+, String, ?LF];
do_encode({error, Code, String}) -> [$-, Code, $\s, String, ?LF];
do_encode({blob, String}) -> encode_blob($$, String);
do_encode({verbatim, P, S}) -> encode_blob($=, <<P/binary, $:, S/binary>>);
do_encode({blob_error, C, S}) -> encode_blob($!, <<C/binary, $\s, S/binary>>);
do_encode(I) when is_integer(I), I >= ?MININT, I =< ?MAXINT ->
    [$:, integer_to_binary(I), ?LF];
do_encode(I) when is_integer(I) -> [$(, integer_to_binary(I), ?LF];
do_encode(F) when is_float(F) -> [$,, float_to_binary(F, [short]), ?LF];
do_encode(A) when is_list(A) -> encode_array($*, A);
do_encode(M) when is_map(M) -> encode_map($%, M);
do_encode({set, Set}) -> encode_array($~, sets:to_list(Set));
do_encode({attribute, A, Value}) -> [encode_map($|, A), do_encode(Value)];
do_encode({push, Type, Message}) -> encode_array($>, [Type | Message]).

encode_blob(Type, String) ->
    [Type, integer_to_binary(byte_size(String)), ?LF, String, ?LF].

encode_array(Type, A) ->
    [Type, integer_to_binary(length(A)), ?LF, [do_encode(E) || E <- A]].

encode_map(Type, M) ->
    [Type, integer_to_binary(maps:size(M)), ?LF,
     [[do_encode(K), do_encode(V)] || K := V <- M]].

%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(<<"_\r\n", T/binary>>) -> {null, T};
do_decode(<<"#t\r\n", T/binary>>) -> {true, T};
do_decode(<<"#f\r\n", T/binary>>) -> {false, T};
do_decode(<<",inf\r\n", T/binary>>) -> {inf, T};
do_decode(<<",-inf\r\n", T/binary>>) -> {neg_inf, T};
do_decode(<<",nan\r\n", T/binary>>) -> {nan, T};
do_decode(<<"+", T/binary>>) -> decode_string(T, <<>>);
do_decode(<<"-", T/binary>>) -> decode_error(T, <<>>);
do_decode(<<"$", T/binary>>) -> decode_blob(T);
do_decode(<<"=", T/binary>>) -> decode_verbatim(T);
do_decode(<<"!", T/binary>>) -> decode_blob_error(T);
do_decode(<<":", T/binary>>) -> decode_integer(T, <<>>);
do_decode(<<"(", T/binary>>) -> decode_integer(T, <<>>);
do_decode(<<",", T/binary>>) -> decode_float(T, <<>>);
do_decode(<<"*", T/binary>>) -> decode_array(T);
do_decode(<<"%", T/binary>>) -> decode_map(T);
do_decode(<<"~", T/binary>>) -> decode_set(T);
do_decode(<<"|", T/binary>>) -> decode_attribute(T);
do_decode(<<">", T/binary>>) -> decode_push(T).

decode_string(<<"\r\n", T/binary>>, Acc) -> {{string, Acc}, T};
decode_string(<<H, T/binary>>, Acc) -> decode_string(T, <<Acc/binary, H>>).

decode_error(<<" ", T/binary>>, Code) -> decode_error(T, Code, <<>>);
decode_error(<<"\r\n", T/binary>>, Code) -> {{error, Code, <<>>}, T};
decode_error(<<H, T/binary>>, Acc) -> decode_error(T, <<Acc/binary, H>>).

decode_error(<<"\r\n", T/binary>>, Code, Acc) -> {{error, Code, Acc}, T};
decode_error(<<H, T/binary>>, C, A) -> decode_error(T, C, <<A/binary, H>>).

decode_blob(T) ->
    {Size, T1} = decode_integer(T, <<>>),
    <<Blob:Size/binary, "\r\n", T2/binary>> = T1,
    {{blob, Blob}, T2}.

decode_verbatim(T) ->
    {Size, T1} = decode_integer(T, <<>>),
    <<Prefix:3/binary, ":", Blob:(Size - 4)/binary, "\r\n", T2/binary>> = T1,
    {{verbatim, Prefix, Blob}, T2}.

decode_blob_error(T) ->
    {Size, T1} = decode_integer(T, <<>>),
    <<Blob:Size/binary, "\r\n", T2/binary>> = T1,
    decode_blob_error(Blob, <<>>, T2).

decode_blob_error(<<" ", Blob/binary>>, Code, T) ->
    {{blob_error, Code, Blob}, T};
decode_blob_error(<<>>, Code, T) ->
    {{blob_error, Code, <<>>}, T};
decode_blob_error(<<H, Blob/binary>>, Acc, T) ->
    decode_blob_error(Blob, <<Acc/binary, H>>, T).

decode_integer(<<"\r\n", T/binary>>, Acc) -> {binary_to_integer(Acc), T};
decode_integer(<<H, T/binary>>, Acc) ->  decode_integer(T, <<Acc/binary, H>>).

decode_float(<<"\r\n", T/binary>>, Acc = <<".", _/binary>>) ->
    {binary_to_float(<<"0", Acc/binary>>), T};
decode_float(<<"\r\n", T/binary>>, Acc) ->
    case jhn_blist:member($., Acc) of
        true -> {binary_to_float(Acc), T};
        false -> {binary_to_float(<<Acc/binary, ".0">>), T}
    end;
decode_float(<<H, T/binary>>, Acc) ->
    decode_float(T, <<Acc/binary, H>>).

decode_array(T) ->
    {Size, T1} = decode_integer(T, <<>>),
    decode_array(Size, T1, []).

decode_array(0, T, Array) -> {lists:reverse(Array), T};
decode_array(N, T, Acc) ->
    {E, T1} = do_decode(T),
    decode_array(N - 1, T1, [E | Acc]).

decode_map(T) ->
    {Size, T1} = decode_integer(T, <<>>),
    decode_map(Size, T1, []).

decode_map(0, T, Acc) -> {maps:from_list(Acc), T};
decode_map(N, T, Acc) ->
    {K, T1} = do_decode(T),
    {V, T2} = do_decode(T1),
    decode_map(N - 1, T2, [{K, V} | Acc]).

decode_set(T) ->
    {S, T1} = decode_array(T),
    {{set, sets:from_list(S, [{version, 2}])}, T1}.

decode_attribute(T) ->
    {A, T1} = decode_map(T),
    {V, T2} = do_decode(T1),
    {{attribute, A, V}, T2}.

decode_push(T) ->
    {[Type | Message], T1} = decode_array(T),
    {{push, Type, Message}, T1}.

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts(Opts) -> lists:foldl(fun parse_opt/2, #opts{}, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
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
    try jhn_ip_addr:decode(Dest, [tuple]) of
        Dest1 -> Opts#opts{dest = Dest1}
    catch _:_ -> Opts#opts{dest = Dest}
    end;
parse_opt({destination, Dest = <<C,_/binary>>},Opts) when C >= $0,C =< $: ->
    try jhn_ip_addr:decode(Dest, [tuple]) of
        Dest1 -> Opts#opts{dest = Dest1}
    catch _:_ -> Opts#opts{dest = binary_to_list(Dest)}
    end;
parse_opt({destination, Dest}, Opts) ->
    Opts#opts{dest = Dest};
parse_opt(_, Opts) ->
    erlang:error(badarg, Opts).
