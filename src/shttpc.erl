%%==============================================================================
%% Copyright 2015-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A simple HTTP client based on:
%%%    Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing
%%%                                                                    (rfc7230)
%%%    Hypertext Transfer Protocol (HTTP/1.1): Semantics and Content   (rfc7231)
%%%    The Hypertext Transfer Protocol Status Code 308 (Permanent Redirect)
%%%                                                                    (rfc7538)
%%%    PATCH Method for HTTP                                           (rfc5789)
%%%    HTTP Over TLS                                                   (rfc2818)
%%%
%%% Options that are available to open/1, open/2 and all HTTP method functions:
%%%    timeout -> the time in milliseconds the request is allowed to complete
%%%    limit -> the endtime for the request as system_time in milliseconds
%%%    headers -> a map of HTTP headers where the headers name can be an
%%%               atom or binary and the value a binary
%%%    options -> options to the transport's (tcp or ssl) connection function
%%%    redirect -> a boolean that determines if redirect are automatically
%%%                followed for status codes 301, 302, 303, 307, 308 and in the
%%%                case of 303 use the GET as the method
%%%    close -> a boolean that determines if the connection should be closed by
%%%             this client upon completion of the function
%%%    connection -> if present provides an already established connection to
%%%                  server that will be used for the request
%%%
%%% Returned by open/1, open/2 and all HTTP method functions is a map with:
%%%     status -> {Code, Text} where the code is an integer and Text a binary
%%%     headers -> a map of HTTP headers from the server reply, recognised
%%                 names ones are returned as atoms
%%%     body -> the body of the server reply as a binary, possibly empty
%%%     connection -> present if neither the server ot the client closed the
%%%                   connection, the value is opaque and should used in later
%%%                   HTTP mehod calls
%%%
%%% N.B. The established connections do not time out, if not closed explicitly
%%%      they will remain and cause a resource leak.
%%%      For HTTPS requires the ssl OTP lib which is not included in the
%%%      application resource file.
%%%      Only supports R18 and later.
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2015-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(shttpc).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% API
-export([open/1, open/2, close/1]).

%% API Methods
-export([get/1, get/2,
         head/1, head/2,
         post/2, post/3,
         put/1, put/2, put/3,
         patch/2, patch/3,
         delete/1, delete/2,
         connect/1, connect/2,
         options/1, options/2,
         trace/1, trace/2]).

%% Exported types
-export_type([connection/0]).

%% Compiler options
-compile({no_auto_import, [put/2]}).

%% Includes
-include_lib("jhn_stdlib/include/uri.hrl").

%% Defines
-define(DEFAULT, #{headers => #{},
                   body => <<>>,
                   options => [],
                   timeout => 5000,
                   redirect => false,
                   limit => undefined
                  }).

-define(DENORM,
        #{<<"cache-control">> => 'Cache-Control',
          <<"connection">> => 'Connection',
          <<"date">> => 'Date',
          <<"pragma">> => 'Pragma',
          <<"transfer-encoding">> => 'Transfer-Encoding',
          <<"upgrade">> => 'Upgrade',
          <<"via">> => 'Via',
          <<"accept">> => 'Accept',
          <<"accept-charset">> => 'Accept-Charset',
          <<"accept-encoding">> =>'Accept-Encoding',
          <<"accept-language">> =>'Accept-Language',
          <<"authorization">> => 'Authorization',
          <<"from">> => 'From',
          <<"host">> => 'Host',
          <<"if-modified-since">> => 'If-Modified-Since',
          <<"if-match">> => 'If-Match',
          <<"if-none-match">> => 'If-None-Match',
          <<"If-Range">> => 'If-Range',
          <<"if-unmodified-since">> => 'If-Unmodified-Since',
          <<"max-forwards">> => 'Max-Forwards',
          <<"proxy-authorization">> => 'Proxy-Authorization',
          <<"range">> => 'Range',
          <<"referer">> => 'Referer',
          <<"user-agent">> => 'User-Agent',
          <<"age">> => 'Age',
          <<"location">> => 'Location',
          <<"proxy-authenticate">> => 'Proxy-Authenticate',
          <<"public">> => 'Public',
          <<"retry-after">> => 'Retry-After',
          <<"server">> => 'Server',
          <<"vary">> => 'Vary',
          <<"warning">> => 'Warning',
          <<"www-authenticate">> => 'Www-Authenticate',
          <<"allow">> => 'Allow',
          <<"content-base">> => 'Content-Base',
          <<"content-encoding">> => 'Content-Encoding',
          <<"content-language">> => 'Content-Language',
          <<"content-length">> => 'Content-Length',
          <<"content-location">> => 'Content-Location',
          <<"content-md5">> => 'Content-Md5',
          <<"content-range">> => 'Content-Range',
          <<"content-type">> => 'Content-Type',
          <<"etag">> => 'Etag',
          <<"expires">> => 'Expires',
          <<"last-modified">> => 'Last-Modified',
          <<"accept-ranges">> => 'Accept-Ranges',
          <<"set-cookie">> => 'Set-Cookie',
          <<"set-cookie2">> => 'Set-Cookie2',
          <<"x-forwarded-for">> => 'X-Forwarded-For',
          <<"cookie">> => 'Cookie',
           <<"keep-alive">> => 'Keep-Alive',
          <<"proxy-connection">> => 'Proxy-Connection',
          <<"Accept-Patch">> => 'Accept-Patch'}).

-define(FORMAT,
        #{<<"cache-control">> => <<"Cache-Control">>,
          <<"connection">> => <<"Connection">>,
          <<"date">> => <<"Date">>,
          <<"pragma">> => <<"Pragma">>,
          <<"transfer-encoding">> => <<"Transfer-Encoding">>,
          <<"upgrade">> => <<"Upgrade">>,
          <<"via">> => <<"Via">>,
          <<"accept">> => <<"Accept">>,
          <<"accept-charset">> => <<"Accept-Charset">>,
          <<"accept-encoding">> =><<"Accept-Encoding">>,
          <<"accept-language">> =><<"Accept-Language">>,
          <<"authorization">> => <<"Authorization">>,
          <<"from">> => <<"From">>,
          <<"host">> => <<"Host">>,
          <<"if-modified-since">> => <<"If-Modified-Since">>,
          <<"if-match">> => <<"If-Match">>,
          <<"if-none-match">> => <<"If-None-Match">>,
          <<"If-Range">> => <<"If-Range">>,
          <<"if-unmodified-since">> => <<"If-Unmodified-Since">>,
          <<"max-forwards">> => <<"Max-Forwards">>,
          <<"proxy-authorization">> => <<"Proxy-Authorization">>,
          <<"range">> => <<"Range">>,
          <<"referer">> => <<"Referer">>,
          <<"user-agent">> => <<"User-Agent">>,
          <<"age">> => <<"Age">>,
          <<"location">> => <<"Location">>,
          <<"proxy-authenticate">> => <<"Proxy-Authenticate">>,
          <<"public">> => <<"Public">>,
          <<"retry-after">> => <<"Retry-After">>,
          <<"server">> => <<"Server">>,
          <<"vary">> => <<"Vary">>,
          <<"warning">> => <<"Warning">>,
          <<"www-authenticate">> => <<"Www-Authenticate">>,
          <<"allow">> => <<"Allow">>,
          <<"content-base">> => <<"Content-Base">>,
          <<"content-encoding">> => <<"Content-Encoding">>,
          <<"content-language">> => <<"Content-Language">>,
          <<"content-length">> => <<"Content-Length">>,
          <<"content-location">> => <<"Content-Location">>,
          <<"content-md5">> => <<"Content-Md5">>,
          <<"content-range">> => <<"Content-Range">>,
          <<"content-type">> => <<"Content-Type">>,
          <<"etag">> => <<"Etag">>,
          <<"expires">> => <<"Expires">>,
          <<"last-modified">> => <<"Last-Modified">>,
          <<"accept-ranges">> => <<"Accept-Ranges">>,
          <<"set-cookie">> => <<"Set-Cookie">>,
          <<"set-cookie2">> => <<"Set-Cookie2">>,
          <<"x-forwarded-for">> => <<"X-Forwarded-For">>,
          <<"cookie">> => <<"Cookie">>,
          <<"keep-alive">> => <<"Keep-Alive">>,
          <<"proxy-connection">> => <<"Proxy-Connection">>,
          <<"accept-patch">> => <<"Accept-Patch">>}).

%% Records
-record(state, {transport = tcp :: tcp | ssl,
                host :: inet:ip_address(),
                port :: inet:port_number(),
                method :: method(),
                headers :: headers(),
                path :: [binary()],
                fragment :: binary(),
                query :: binary(),
                body :: iodata(),
                socket,
                limit = infinity :: timeout(),
                options = [] :: [any()],
                redirect :: boolean(),
                redirections = []
               }).

-record(connection, {transport = tcp :: tcp | ssl,
                     socket :: port() | ssl_socket()}).

%% Types
-type header() :: 'Cache-Control' | 'Connection' | 'Date' | 'Pragma' |
                  'Transfer-Encoding' | 'Upgrade' | 'Via' | 'Accept' |
                  'Accept-Charset' | 'Accept-Encoding' | 'Accept-Language' |
                  'Authorization' | 'From' | 'Host' | 'If-Modified-Since' |
                  'If-Match' | 'If-None-Match' | 'If-Range' |
                  'If-Unmodified-Since' | 'Max-Forwards' |
                  'Proxy-Authorization' | 'Range' | 'Referer' |
                  'User-Agent' | 'Age' | 'Location' | 'Proxy-Authenticate' |
                  'Public' | 'Retry-After' | 'Server' | 'Vary' | 'Warning' |
                  'Www-Authenticate' | 'Allow' | 'Content-Base' |
                  'Content-Encoding' | 'Content-Language' | 'Content-Length' |
                  'Content-Location' | 'Content-Md5' | 'Content-Range' |
                  'Content-Type' | 'Etag' | 'Expires' | 'Last-Modified' |
                  'Accept-Ranges' | 'Set-Cookie' | 'Set-Cookie2' |
                  'X-Forwarded-For' | 'Cookie' | 'Keep-Alive' |
                  'Proxy-Connection' | 'Accept-Patch' |
                  binary().

-type method() :: 'GET' | 'HEAD'| 'POST' | 'PUT' | 'PATCH' |
                  'DELETE' | 'CONNECT' | 'OPTIONS' | 'TRACE'.

-type headers() :: [{header(), iodata()}].

-type ssl_socket() :: _.

-opaque connection() :: #connection{}.

%% ===================================================================
%% API functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: open(URL) -> Connection | Error.
%% @doc
%%   Opens a connection for later use, uses the HTTP HEAD method.
%% @end
%%--------------------------------------------------------------------
-spec open(iodata()) -> connection() | {error, _}.
%%--------------------------------------------------------------------
open(URL) -> open(URL, 'HEAD').

%%--------------------------------------------------------------------
%% Function: open(URL, Method) -> Connection | Error.
%% @doc
%%   Opens a connection for later use, uses the HTTP Method.
%% @end
%%--------------------------------------------------------------------
-spec open(iodata(), method()) -> connection() | {error, _}.
%%--------------------------------------------------------------------
open(URL, Method) -> open(URL, Method, #{}).

%%--------------------------------------------------------------------
%% Function: open(URL, Method, Options) -> Connection | Error.
%% @doc
%%   Opens a connection for later use, uses the HTTP Method.
%% @end
%%--------------------------------------------------------------------
-spec open(iodata(), method(), map()) -> connection() | {error, _}.
%%--------------------------------------------------------------------
open(URL, Method, Options) ->
    case request(Options#{method => Method, uri => URL}) of
                Error = {error, _} -> Error;
                #{status := {200, _}, connection := Con} -> Con;
                #{status := {204, _}, connection := Con} -> Con;
                #{status := {Code, _}} -> {error, {wrong_status_code, Code}}
    end.

%%--------------------------------------------------------------------
%% Function: close(Connection) -> ok | Error.
%% @doc
%%   Closes an already established connection.
%% @end
%%--------------------------------------------------------------------
-spec close(connection()) -> ok | {error, _}.
%%--------------------------------------------------------------------
close(Connection = #connection{}) -> do_close(Connection).

%% ===================================================================
%% API Method functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: get(URL) -> ServerResponse | Error.
%% @doc
%%   Performs the GET HTTP method
%% @end
%%--------------------------------------------------------------------
-spec get(iodata()) -> map() | {error, _}.
%%--------------------------------------------------------------------
get(URL) -> get(URL, #{}).

%%--------------------------------------------------------------------
%% Function: get(URL, Options) -> ServerResponse | Error.
%% @doc
%%   Performs the GET HTTP method
%% @end
%%--------------------------------------------------------------------
-spec get(iodata(), map()) -> map() | {error, _}.
%%--------------------------------------------------------------------
get(URL, Opts) -> request(Opts#{method => 'GET', uri => URL}).

%%--------------------------------------------------------------------
%% Function: head(URL) -> ServerResponse | Error.
%% @doc
%%   Performs the HEAD HTTP method
%% @end
%%--------------------------------------------------------------------
-spec head(iodata()) -> map() | {error, _}.
%%--------------------------------------------------------------------
head(URL) -> head(URL, #{}).

%%--------------------------------------------------------------------
%% Function: head(URL, Options) -> ServerResponse | Error.
%% @doc
%%   Performs the HEAD HTTP method
%% @end
%%--------------------------------------------------------------------
-spec head(iodata(), map()) -> map() | {error, _}.
%%--------------------------------------------------------------------
head(URL, Opts) -> request(Opts#{method => 'HEAD', uri => URL}).

%%--------------------------------------------------------------------
%% Function: post(URL, Body) -> ServerResponse | Error.
%% @doc
%%   Performs the POST HTTP method
%% @end
%%--------------------------------------------------------------------
-spec post(iodata(), iodata()) -> map() | {error, _}.
%%--------------------------------------------------------------------
post(URL, Body) -> post(URL, Body, #{}).

%%--------------------------------------------------------------------
%% Function: post(URL, Body, Options) -> ServerResponse | Error.
%% @doc
%%   Performs the POST HTTP method
%% @end
%%--------------------------------------------------------------------
-spec post(iodata(), iodata(), map()) -> map() | {error, _}.
%%--------------------------------------------------------------------
post(URL, Body, Opts) ->
    request(Opts#{method => 'POST', body => Body, uri => URL}).

%%--------------------------------------------------------------------
%% Function: put(URL) -> ServerResponse | Error.
%% @doc
%%   Performs the PUT HTTP method
%% @end
%%--------------------------------------------------------------------
-spec put(iodata()) -> map() | {error, _}.
%%--------------------------------------------------------------------
put(URL) -> put(URL, <<>>).

%%--------------------------------------------------------------------
%% Function: put(URL, Body) -> ServerResponse | Error.
%% @doc
%%   Performs the PUT HTTP method
%% @end
%%--------------------------------------------------------------------
-spec put(iodata(), iodata()) -> map() | {error, _}.
%%--------------------------------------------------------------------
put(URL, Body) -> put(URL, Body, #{}).

%%--------------------------------------------------------------------
%% Function: put(URL, Body, Options) -> ServerResponse | Error.
%% @doc
%%   Performs the PUT HTTP method
%% @end
%%--------------------------------------------------------------------
-spec put(iodata(), iodata(), map()) -> map() | {error, _}.
%%--------------------------------------------------------------------
put(URL, Body, Opts) ->
    request(Opts#{method => 'PUT', body => Body, uri => URL}).

%%--------------------------------------------------------------------
%% Function: patch(URL, Body) -> ServerResponse | Error.
%% @doc
%%   Performs the PATCH HTTP method
%% @end
%%--------------------------------------------------------------------
-spec patch(iodata(), iodata()) -> map() | {error, _}.
%%--------------------------------------------------------------------
patch(URL, Body) -> patch(URL, Body, #{}).

%%--------------------------------------------------------------------
%% Function: patch(URL, Body, Options) -> ServerResponse | Error.
%% @doc
%%   Performs the PATCH HTTP method
%% @end
%%--------------------------------------------------------------------
-spec patch(iodata(), iodata(), map()) -> map() | {error, _}.
%%--------------------------------------------------------------------
patch(URL, Body, Opts) ->
    request(Opts#{method => 'PATCH', body => Body, uri => URL}).

%%--------------------------------------------------------------------
%% Function: delete(URL) -> ServerResponse | Error.
%% @doc
%%  Performs the DELETE HTTP method
%% @end
%%--------------------------------------------------------------------
-spec delete(iodata()) -> map() | {error, _}.
%%--------------------------------------------------------------------
delete(URL) -> delete(URL, #{}).

%%--------------------------------------------------------------------
%% Function: delete(URL, Options) -> ServerResponse | Error.
%% @doc
%%   Performs the DELET HTTP method
%% @end
%%--------------------------------------------------------------------
-spec delete(iodata(), map()) -> map() | {error, _}.
%%--------------------------------------------------------------------
delete(URL, Opts) -> request(Opts#{method => 'DELETE', uri => URL}).

%%--------------------------------------------------------------------
%% Function: connect(URL) -> ServerResponse | Error.
%% @doc
%%   Performs the CONNECT HTTP method
%% @end
%%--------------------------------------------------------------------
-spec connect(iodata()) -> map() | {error, _}.
%%--------------------------------------------------------------------
connect(URL) -> connect(URL, #{}).

%%--------------------------------------------------------------------
%% Function: connect(URL, Options) -> ServerResponse | Error.
%% @doc
%%   Performs the CONNECT HTTP method
%% @end
%%--------------------------------------------------------------------
-spec connect(iodata(), map()) -> map() | {error, _}.
%%--------------------------------------------------------------------
connect(URL, Opts) -> request(Opts#{method => 'CONNECT', uri => URL}).

%%--------------------------------------------------------------------
%% Function: options(URL) -> ServerResponse | Error.
%% @doc
%%   Performs the OPTIONS HTTP method
%% @end
%%--------------------------------------------------------------------
-spec options(iodata()) -> map() | {error, _}.
%%--------------------------------------------------------------------
options(URL) -> options(URL, #{}).

%%--------------------------------------------------------------------
%% Function: options(URL, Options) -> ServerResponse | Error.
%% @doc
%%   Performs the OPTIONS HTTP method
%% @end
%%--------------------------------------------------------------------
-spec options(iodata(), map()) -> map() | {error, _}.
%%--------------------------------------------------------------------
options(URL, Opts) -> request(Opts#{method => 'OPTIONS', uri => URL}).

%%--------------------------------------------------------------------
%% Function: trace(URL) -> ServerResponse | Error.
%% @doc
%%   Performs the TRACE HTTP method
%% @end
%%--------------------------------------------------------------------
-spec trace(iodata()) -> map() | {error, _}.
%%--------------------------------------------------------------------
trace(URL) -> trace(URL, #{}).

%%--------------------------------------------------------------------
%% Function: trace(URL, Options) -> ServerResponse | Error.
%% @doc
%%   Performs the TRACE HTTP method
%% @end
%%--------------------------------------------------------------------
-spec trace(iodata(), map()) -> map() | {error, _}.
%%--------------------------------------------------------------------
trace(URL, Opts) -> request(Opts#{method => 'TRACE', uri => URL}).

%%==============================================================================
%% Internal functions
%%==============================================================================

request(Req) ->
    Req1 =  maps:merge(?DEFAULT, Req),
    case maps:fold(fun validate/3, Req1, Req1) of
        Req2 = #{socket := Sock} ->
            #{uri := #uri{scheme = Scheme,
                          host = Host,
                          port = Port,
                          path = Path,
                          fragment = Fragment,
                          query = Query},
              method := Method,
              headers := Headers,
              body := Body,
              redirect := Redirect,
              limit := Limit} = Req2,
            State = #state{host = Host,
                           port = port(Scheme, Port),
                           transport = transport(Scheme),
                           method = Method,
                           headers = normalize_headers(Headers),
                           path = Path,
                           fragment = Fragment,
                           query = Query,
                           body = Body,
                           socket = Sock,
                           redirect = Redirect,
                           limit = Limit},
            case send_request(State, format_request(State)) of
                ok ->
                    setopts(State, [{packet, http_bin}]),
                    read_response(State);
                Error -> Error
            end;
        Req2 ->
            #{uri := #uri{scheme = Scheme,
                          host = Host,
                          port = Port,
                          path=Path,
                          fragment = Fragment,
                          query = Query},
              method := Method,
              headers := Headers,
              body := Body,
              options := Options,
              redirect := Redirect,
              limit := Limit} = Req2,
            State = #state{host = Host,
                           port = port(Scheme, Port),
                           transport = transport(Scheme),
                           method = Method,
                           headers = normalize_headers(Headers),
                           path = Path,
                           fragment = Fragment,
                           query = Query,
                           body = Body,
                           options = opts(Options, Host),
                           redirect = Redirect,
                           limit = Limit},
            case connect_server(State) of
                State1 = #state{} ->
                    case send_request(State1, format_request(State)) of
                        ok -> read_response(State1);
                        Error -> Error
                    end;
                Error ->
                    Error
            end
    end.

%%------------------------------------------------------------------------------
%% Arguments
%%------------------------------------------------------------------------------

validate(timeout, infinity, Req) -> Req;
validate(timeout, Ms, Req) when is_integer(Ms), Ms >= 0 -> Req;
validate(limit, infinity, Req) -> Req;
validate(limit,undefined,Req=#{timeout := infinity}) -> Req#{limit => infinity};
validate(limit, undefined, Req = #{timeout := Ms}) -> Req#{limit => limit(Ms)};
validate(limit, Ms, Req) when is_integer(Ms),Ms >= 0 -> Req;
validate(body, _, Req = #{method := 'POST'}) -> Req;
validate(body, _, Req = #{method := 'PUT'}) -> Req;
validate(body, _, Req = #{method := 'PATCH'}) -> Req;
validate(body, <<>>, Req) -> Req;
validate(uri, URI, Req) -> decode(URI, Req);
validate(headers, Headers, Req) when is_map(Headers) -> Req;
validate(options, Opts, Req) when is_list(Opts) -> Req;
validate(method, 'GET', Req) -> Req;
validate(method, 'HEAD', Req) -> Req;
validate(method, 'POST', Req) -> Req;
validate(method, 'PUT', Req) -> Req;
validate(method, 'PATCH', Req) -> Req;
validate(method, 'DELETE', Req) -> Req;
validate(method, 'CONNECT', Req) -> Req;
validate(method, 'OPTIONS', Req) -> Req;
validate(method, 'TRACE', Req) -> Req;
validate(redirect, Redirect, Req) when is_boolean(Redirect) -> Req;
validate(close, false, Req) -> Req;
validate(close, true, Req = #{headers := Headers}) ->
    Req#{headers => Headers#{'Connection' => <<"Close">>}};
validate(connection, #connection{transport = tcp, socket = Soscket}, Req)
  when is_port(Soscket) ->
    Req#{socket => Soscket};
validate(connection, #connection{transport = ssl, socket = Sock}, Req) ->
    Req#{socket => Sock};
validate(Opt, Value, _) -> erlang:error({bad_option, {Opt, Value}}).

decode(URI, Req = #{connection := #connection{transport = Transport}}) ->
    U  = #uri{scheme = Scheme} = uri(URI),
    case transport(Scheme) of
        Transport -> Req#{uri => U};
        _ -> erlang:error({bad_option, {uri, URI}})
    end;
decode(URI, Req = #{method := Method}) ->
    case uri(URI) of
        U  = #uri{userinfo = []} -> Req#{uri => U};
        U  = #uri{userinfo = UserInfo} when Method == 'CONNECT' ->
            Headers = maps:get(headers, Req, #{}),
            Basic = base64:encode(bstring:join(UserInfo, <<$:>>)),
            Authorization = <<"Basic ", Basic/binary>>,
            Req#{uri => U,
                 headers => Headers#{'Proxy-Authorization' => Authorization}};
        U  = #uri{userinfo = UserInfo} ->
            Headers = maps:get(headers, Req, #{}),
            Basic = base64:encode(bstring:join(UserInfo, <<$:>>)),
            Authorization = <<"Basic ", Basic/binary>>,
            Req#{uri => U,
                 headers => Headers#{'Authorization' => Authorization}}
    end.

port(_, Port) when is_integer(Port) -> Port;
port(http, _) -> 80;
port(https, _) -> 443.

normalize_headers(Map) -> maps:fold(fun normalize_header/3, [], Map).

normalize_header(Key, Value, Acc) ->
    [{bstring:to_lower(to_bin(Key)),
      bstring:strip(to_bin(Value), right)} | Acc].

to_bin(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
to_bin(List) when is_list(List) -> list_to_binary(List);
to_bin(Bin) ->  Bin.

transport(http) -> tcp;
transport(https) -> ssl.

limit(infinity) -> infinity;
limit(Timeout) -> erlang:system_time(milli_seconds) + Timeout.

%%------------------------------------------------------------------------------
%% options
%%------------------------------------------------------------------------------
opts(Opts, Host) ->
    Opts1 = [binary, {packet, http_bin}, {active, false} | Opts],
    case inet_opt(Opts) of
        true -> Opts1;
        false ->
            case ipv6_host(Host) of
                true -> [inet6 | Opts1];
                false -> Opts1
            end
    end.

inet_opt([]) -> false;
inet_opt([inet | _]) -> true;
inet_opt([inet6| _]) -> true;
inet_opt([_ | T]) -> inet_opt(T).

ipv6_host({_, _, _, _, _, _, _, _}) -> true;
ipv6_host(_) -> false.

%%------------------------------------------------------------------------------
%% Headers
%%------------------------------------------------------------------------------

header_normalized(Header, Headers) ->  header_normalized(Header, Headers, <<>>).

header_normalized(Header, Headers, Default) ->
    bstring:to_lower(plist:find(Header, Headers, Default)).

%%------------------------------------------------------------------------------
%% Transport
%%------------------------------------------------------------------------------

connect_server(State) ->
    try do_connect1(State) of
        {ok, Socket} -> State#state{socket = Socket};
        {error, etimedout} -> {error, connect_timeout};
        {error, timeout} -> {error, connect_timeout};
        {error, 'record overflow'} -> {error, ssl_error};
        Error -> Error
    catch
        exit:{{{badmatch, {error, {asn1, _}}}, _}, _} ->
            {error, ssl_decode_error};
        error:Error ->
            {error, {connection, Error}};
        Class:Error ->
            {error, {connection, Class, Error}}
    end.

do_connect1(State = #state{transport = tcp}) ->
    #state{host = Host,
           port = Port,
           limit = Limit,
           options = Opts} = State,
    case diff(Limit) of
        timeout -> erlang:error(connect_timeout);
        Timeout -> gen_tcp:connect(Host, Port, Opts, Timeout)
    end;
do_connect1(State = #state{transport = ssl}) ->
    #state{host = Host,
           port = Port,
           limit = Limit,
           options = Opts} = State,
    case diff(Limit) of
        timeout -> erlang:error(connect_timeout);
        Timeout -> ssl:connect(Host, Port, Opts, Timeout)
    end.

recv(State) -> recv(State, 0).

recv(#state{transport = tcp, socket = Sock, limit = infinity}, Len) ->
    case gen_tcp:recv(Sock, Len) of
        OK = {ok, _} -> OK;
        {error, closed} -> closed;
        {error, Error} -> erlang:error(Error)
    end;
recv(#state{transport = tcp, socket = Sock, limit = Limit}, Len) ->
    case diff(Limit) of
        timeout -> erlang:error(recv_timeout);
        Timeout ->
            case gen_tcp:recv(Sock, Len, Timeout) of
                OK = {ok, _} -> OK;
                {error, closed} -> closed;
                {error, Error} -> erlang:error(Error)
            end
    end;
recv(#state{transport = ssl, socket = Sock, limit = infinity}, Len) ->
    case ssl:recv(Sock, Len) of
        OK = {ok, _} -> OK;
        {error, closed} -> closed;
        {error, Error} -> erlang:error(Error)
    end;
recv(#state{transport = ssl, socket = Sock, limit = Limit}, Len) ->
    case diff(Limit) of
        timeout -> erlang:error(recv_timeout);
        Timeout ->
            case ssl:recv(Sock, Len, Timeout) of
                OK = {ok, _} -> OK;
                {error, closed} -> closed;
                {error, Error} -> erlang:error(Error)
            end
    end.

send_request(State, Data) ->
    try send(State, Data) of
        ok -> ok;
        Error ->
            safe_close(State),
            Error
    catch
        error:Error ->
            safe_close(State),
            {error, Error};
        Class:Error ->
            safe_close(State),
            {error, {Class, Error}}
    end.

send(#state{transport = tcp, socket = Sock, limit = infinity}, Data) ->
    gen_tcp:send(Sock, Data);
send(#state{transport = tcp, socket = Sock, limit = Limit}, Data) ->
    case diff(Limit) of
        timeout -> {error, send_timeout};
        Timeout ->
            inet:setopts(Sock, [{send_timeout, Timeout}]),
            gen_tcp:send(Sock, Data)
    end;
send(#state{transport = ssl, socket = Sock, limit = infinity}, Data) ->
    ssl:send(Sock, Data);
send(#state{transport = ssl, socket = Sock, limit = Limit}, Data) ->
    case diff(Limit) of
        timeout -> {error, send_timeout};
        Timeout ->
            ssl:setopts(Sock, [{send_timeout, Timeout}]),
            ssl:send(Sock, Data)
    end.

setopts(#state{transport = tcp, socket=Sock}, Opts) -> inet:setopts(Sock, Opts);
setopts(#state{transport = ssl, socket=Sock}, Opts) -> ssl:setopts(Sock, Opts).

safe_close(State) -> try do_close(State) catch _:_ -> ok end.

do_close(#state{transport = tcp, socket = Sock}) -> gen_tcp:close(Sock);
do_close(#state{transport = ssl, socket = Sock}) -> ssl:close(Sock);
do_close(#connection{transport = tcp, socket = Sock}) -> gen_tcp:close(Sock);
do_close(#connection{transport = ssl, socket = Sock}) -> ssl:close(Sock).

diff(infinity) -> infinity;
diff(Limit) ->
    case Limit - erlang:system_time(milli_seconds) of
        Timeout when Timeout > 0 -> Timeout;
        _ -> timeout
    end.

%%------------------------------------------------------------------------------
%% Format
%%------------------------------------------------------------------------------

format_request(State) ->
    #state{method = Method,
           host = Host,
           port = Port,
           headers = Headers,
           path = Path,
           fragment = Fragment,
           query = Query,
           body = Body} = State,
    Headers1 = case lists:member(Method, ['POST', 'PUT', 'PATCH']) of
                   false -> Headers;
                   true ->
                       case plist:member(<<"content-length">>, Headers) of
                           true -> Headers;
                           false ->
                               Size = integer_to_binary(iolist_size(Body)),
                               [{<<"content-length">>, Size} | Headers]
                       end
               end,
    Headers2 = case plist:member(<<"host">>, Headers) of
                   true -> Headers1;
                   false -> [{<<"host">>, format_host(Host, Port)} | Headers1]

               end,
    Fragment1 = case Fragment of
                     <<>> -> <<>>;
                     _ -> [$#, Fragment]
                 end,
    Query1 = case Query of
                 <<>> -> <<>>;
                 _ -> [$?, Query]
             end,
    [atom_to_binary(Method, utf8), " ", format_path(Path), Fragment1, Query1,
     " HTTP/1.1\r\n", format_headers(Headers2), "\r\n", Body].

format_headers(Headers) ->
    Fun = fun(H, V) -> [maps:get(H, ?FORMAT, H), <<": ">>, V, <<"\r\n">>] end,
    [Fun(Key, Value) || {Key, Value} <- Headers].

format_path([]) -> "/";
format_path(Parts) -> [["/", Part] || Part <- Parts].

format_host(Host = {_, _, _, _, _, _, _, _}, Port) ->
    [$[,
     [bstring:join([integer_to_binary(I) || I <- tuple_to_list(Host)], <<$:>>),
      $:, integer_to_binary(Port)],
     $]];
format_host(Host = {_, _, _, _}, Port) ->
    [bstring:join([integer_to_binary(I) || I <- tuple_to_list(Host)], <<$.>>),
     $:, integer_to_binary(Port)];
format_host(Host, Port) ->
    [Host, $:, integer_to_binary(Port)].

%%------------------------------------------------------------------------------
%% Response
%%------------------------------------------------------------------------------

read_response(State = #state{transport = Transport, socket = Socket}) ->
    try read_response(State, undefined, {undefined, undefined}, []) of
        closed -> {error, closed};
        {false, Response = #{}}  ->
            Response#{connection => #connection{transport = Transport,
                                                socket = Socket}};
        {true, Response = #{}} ->
            do_close(State),
            Response;
        Error ->
            safe_close(State),
            Error
    catch
        error:{badmatch, closed} ->
            safe_close(State),
            {error, connection_closed};
        error:Error ->
            safe_close(State),
            {error, Error};
        Class:Error ->
            safe_close(State),
            {error, {Class, Error}}
    end.

read_response(State, Vsn, Status = {Code, _}, Headers) ->
    case recv(State) of
        closed -> closed;
        {ok, {http_response, Vsn1, Code1, Reason}} ->
            read_response(State, Vsn1, {Code1, Reason}, Headers);
        {ok, {http_header, _, Name, _, Value}} ->
            read_response(State,
                          Vsn,
                          Status,
                          normalize_header(Name, Value, Headers));
        {ok, http_eoh} when Code >= 100, Code =< 199 ->
            read_response(State, undefined, {undefined, undefined}, []);
        {ok, http_eoh} when Code == 301, State#state.redirect ->
            redirect(State, Headers);
        {ok, http_eoh} when Code == 302, State#state.redirect ->
            redirect(State, Headers);
        {ok, http_eoh} when Code == 303, State#state.redirect ->
            redirect(State#state{method = 'GET'}, Headers);
        {ok, http_eoh} when Code == 307, State#state.redirect ->
            redirect(State, Headers);
        {ok, http_eoh} when Code == 308, State#state.redirect ->
            redirect(State, Headers);
        {ok, http_eoh} ->
            setopts(State, [{packet, raw}]),
            {Body, Trailers} = response_body(State, Vsn, Status, Headers),
            Headers1 = Trailers ++ Headers,
            {should_close(Vsn, State#state.headers, Headers1),
             #{status => Status,
               headers => denormalize_headers(Headers1),
               body => Body}}
    end.

response_body(#state{method = 'HEAD'}, _, _, _) -> {<<>>, []};
response_body(#state{method = 'CONNECT'}, _, _, _) -> {<<>>, []};
response_body(_, _, {204, _}, _) ->  {<<>>, []};
response_body(_, _, {304, _}, _) ->  {<<>>, []};
response_body(State = #state{method = 'OPTIONS'}, Vsn, _, Headers) ->
    ContentLength = plist:member(<<"content-length">>, Headers),
    TransferEncoding = plist:member(<<"transfer-encoding">>, Headers),
    case {ContentLength, TransferEncoding} of
        {false, false} -> {<<>>, []};
        {_, _} -> read_body(Vsn, Headers, State, body_type(Headers))
    end;
response_body(State, Vsn, _, Headers) ->
    read_body(Vsn, Headers, State, body_type(Headers)).

body_type(Headers) ->
    case plist:find(<<"content-length">>, Headers) of
        undefined ->
            case header_normalized(<<"transfer-encoding">>, Headers) of
                <<"chunked">> -> chunked;
                _ -> infinite
            end;
        Length -> {fixed_length, binary_to_integer(Length)}
    end.

read_body(_, _, _, {fixed_length, 0}) -> {<<>>, []};
read_body(_, _, State, {fixed_length, Length}) ->
    {read_length(State, Length), []};
read_body(_, _, State, chunked) -> read_chunked(State, []);
read_body(Vsn, Headers, State, infinite) ->
    check_infinite(Vsn, Headers),
    {read_infinite(State, <<>>), []}.

check_infinite({1, 1}, Headers) ->
    case header_normalized(<<"connection">>, Headers, <<"keep-alive">>) of
        <<"close">> -> ok;
        _ -> erlang:error(no_content_length)
    end;
check_infinite(_, Headers) ->
    case header_normalized(<<"connection">>, Headers, <<"close">>) of
        <<"keep-alive">> -> erlang:error(no_content_length);
        _ -> ok
    end.

read_length(State, Length) ->
    {ok, Data} = recv(State, Length),
    Data.

read_infinite(State, Acc) ->
    case recv(State) of
        closed -> Acc;
        {ok, Body} -> read_infinite(State, <<Acc/binary, Body/binary>>)
    end.

read_chunked(State, Chunks) ->
    case read_chunk_size(State) of
        0 -> {list_to_binary(lists:reverse(Chunks)), read_trailers(State, [])};
        Size -> read_chunked(State, [read_chunk(State, Size) | Chunks])
    end.

read_chunk_size(State) ->
    setopts(State, [{packet, line}]),
    {ok, ChunkSizeExt} = recv(State),
    chunk_size(ChunkSizeExt).

chunk_size(Bin) -> binary_to_integer(chunk_size(Bin, <<>>), 16).

chunk_size(<<$;, _/binary>>, Acc) -> Acc;
chunk_size(<<"\r\n", _/binary>>, Acc) -> Acc;
chunk_size(<<$\s, T/binary>>, Acc) -> chunk_size(T, Acc);
chunk_size(<<H, T/binary>>, Acc) -> chunk_size(T, <<Acc/binary, H>>).

read_chunk(State, Size) ->
    setopts(State, [{packet, raw}]),
    case recv(State, Size + 2) of
        {ok, <<Chunk:Size/binary, "\r\n">>} -> Chunk;
        {ok, Data} -> erlang:error({invalid_chunk, Data})
    end.

read_trailers(State, Trailers) ->
    setopts(State, [{packet, httph_bin}]),
    case recv(State) of
        {ok, http_eoh} -> Trailers;
        {ok, {http_header, _, Name, _, Value}} ->
            read_trailers(State, normalize_header(Name, Value, Trailers));
        {error, {http_error, Data}} ->
            erlang:error({bad_trailer, Data})
    end.

should_close({1, 1}, Client, Server) ->
    case header_normalized(<<"connection">>, Client, <<"keep-alive">>) of
        <<"close">> -> true;
        _ ->
            case header_normalized(<<"connection">>,Server,<<"keep-alive">>) of
                <<"close">> -> true;
                _ ->
                    false
            end
    end;
should_close(_, Client, Server) ->
    case header_normalized(<<"connection">>, Client, <<"keep-alive">>) of
        <<"close">> -> true;
        _ ->
            case header_normalized(<<"connection">>,Server,<<"close">>) of
                <<"keep-alive">> -> false;
                _ -> true
            end
    end.

denormalize_headers(Headers) ->
    maps:from_list([{maps:get(Key, ?DENORM, Key), Value} ||
                       {Key, Value} <- Headers]).

%%------------------------------------------------------------------------------
%% Redirect
%%------------------------------------------------------------------------------
redirect(State = #state{redirections = Redirections}, Headers) ->
    do_close(State),
    case plist:find(<<"location">>, Headers) of
        undefined -> erlang:error(missing_redirect);
        URI ->
            #uri{scheme = Scheme,
                 host = Host,
                 port = Port,
                 path = Path,
                 fragment = Fragment,
                 query = Query} = uri(URI),
            Redirect = {Scheme, Host, Port, Path},
            case lists:member(Redirect, Redirections) of
                true -> erlang:error(redirection_loop);
                false ->
                    State1 =
                        State#state{host = Host,
                                    port = port(Scheme, Port),
                                    transport = transport(Scheme),
                                    path = Path,
                                    fragment = Fragment,
                                    query = Query,
                                    redirections = [Redirect | Redirections]},
                    case connect_server(State1) of
                        State2 = #state{} ->
                            case send_request(State2, format_request(State1)) of
                                ok -> read_response(State2);
                                Error -> Error
                            end;
                        Error -> Error
                    end
            end
    end.

%%------------------------------------------------------------------------------
%% URI
%%------------------------------------------------------------------------------

uri(U = #uri{host = Host}) -> U#uri{host = host_to_address(Host)};
uri(IoData) -> uri(uri:decode(IoData)).

host_to_address(T = {_, _, _, _}) -> T;
host_to_address(T = {_, _, _, _, _, _, _, _}) -> T;
host_to_address(Binary) ->
    try ip_addr:decode(Binary, [tuple])
    catch _:_ -> binary_to_list(Binary)
    end.

