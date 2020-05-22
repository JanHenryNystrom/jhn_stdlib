%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%% @author Oscar Hellström <oscar@hellstrom.st>
%%% @author Magnus Henoch <magnus@erlang-consulting.com>
%%% @author Jan Henry Nyström <JanHenryNystrom@gmail.com>
%%% @end

-module(shttpc_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_STRING, "Great success!").

%%------------------------------------------------------------------------------
%% Setup
%%------------------------------------------------------------------------------

start_app() ->
    {ok, Started} = application:ensure_all_started(ssl),
    Started.

stop_app(Started) -> [application:stop(App) || App <- Started].

tcp_test_() ->
    {inorder,
     {setup,
      fun start_app/0,
      fun stop_app/1,
      [{"Open Close", ?_test(open_close())},
       {"Simple GET", ?_test(simple_get())},
%% Broken IPv6 support ni travis
%%       {"Simple GET Ipv6", ?_test(simple_get_ipv6())},
       {"Empty GET", ?_test(empty_get())},
       {"Basic auth", ?_test(basic_auth())},
       {"Missing Basic auth", ?_test(missing_basic_auth())},
       {"Wrong basic auth", ?_test(wrong_basic_auth())},
       {"Post mandatory headers", ?_test(post_with_mandatory_hdrs())},
       {"Post mandatory headers atoms",
        ?_test(post_with_mandatory_hdrs_by_atoms())},
       {"GET connections options", ?_test(get_with_connect_options())},
       {"No contect length", ?_test(no_content_length())},
       {"No contect length HTTTP/1.0", ?_test(no_content_length_1_0())},
       {"Get not modified", ?_test(get_not_modified())},
       {"Simple HEAD", ?_test(simple_head())},
       {"DELETE non content", ?_test(delete_no_content())},
       {"DELETE content", ?_test(delete_content())},
       {"OPTIONS content", ?_test(options_content())},
       {"OPTIONS no content", ?_test(options_no_content())},
       {"Server connection close", ?_test(server_connection_close())},
       {"Client connection close", ?_test(client_connection_close())},
       {"Client connection close option",?_test(client_connection_close_opt())},
       {"HTTP/1.0 server connection", ?_test(pre_1_1_server_connection())},
       {"HTTP/1.0 server keep alive", ?_test(pre_1_1_server_keep_alive())},
       {"Simple PUT", ?_test(simple_put())},
       {"PATCH", ?_test(patch())},
       {"POST", ?_test(post())},
       {"POST 100-continue", ?_test(post_100_continue())},
       {"CONNECT", ?_test(connect())},
       {"CONNECT auth", ?_test(connect_auth())},
       {"TRACE", ?_test(trace())},
       {"Bad URL", ?_test(bad_url())},
       {"Persistant connection", ?_test(persistent_connection())},
       {"Request timeout", ?_test(request_timeout())},
       {"Chunked encoding", ?_test(chunked_encoding())},
       {"Redirect", ?_test(redirect())},
       {"Redirect allowed", ?_test(redirect_allowed())},
       {"Close connection", ?_test(close_connection())},
       {"Queue", ?_test(message_queue())},
       {"Trailing space header", ?_test(trailing_space_header())}
      ]}
    }.

ssl_test_() ->
    {inorder,
     {setup,
      fun start_app/0,
      fun stop_app/1,
      [{"Simple GET", ?_test(ssl_get())},
%% Broken IPv6 support ni travisx
%%       {"Simple GET IPv6", ?_test(ssl_get_ipv6())},
       {"Simple POST", ?_test(ssl_post())},
       {"Chunked encoding", ?_test(ssl_chunked())}
      ]}
    }.

other_test_() ->
    [?_test(invalid_options())].

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

message_queue() ->
    receive X -> erlang:error({unexpected_message, X}) after 0 -> ok end.

open_close() ->
    Port = start(gen_tcp, [fun head_response/5, fun simple_response/5]),
    URL = url(Port, "/open_close"),
    Response1 = shttpc:open(URL),
    ?assertNotMatch({error, _}, Response1),
    ?assertMatch({_, _, _}, Response1),
    Response2 = shttpc:get(URL, #{connection => Response1}),
    ?assertEqual({200, <<"OK">>}, status(Response2)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response2)),
    ?assertEqual(ok, shttpc:close(Response1)).

simple_get() -> simple(get).

simple_get_ipv6() -> simple(get, inet6).

empty_get() ->
    Port = start(gen_tcp, [fun empty_body/5]),
    URL = url(Port, "/empty"),
    Response = shttpc:get(URL),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

basic_auth() ->
    User = "foo",
    Passwd = "bar",
    Port = start(gen_tcp, [basic_auth_responder(User, Passwd)]),
    URL = url(Port, "/empty", User, Passwd),
    Response = shttpc:get(URL),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<"OK">>, body(Response)).

missing_basic_auth() ->
    User = "foo",
    Passwd = "bar",
    Port = start(gen_tcp, [basic_auth_responder(User, Passwd)]),
    URL = url(Port, "/empty"),
    Response = shttpc:get(URL),
    ?assertEqual({401, <<"Unauthorized">>}, status(Response)),
    ?assertEqual(<<"missing_auth">>, body(Response)).

wrong_basic_auth() ->
    User = "foo",
    Passwd = "bar",
    Port = start(gen_tcp, [basic_auth_responder(User, Passwd)]),
    URL = url(Port, "/empty", User, "wrong_password"),
    Response = shttpc:get(URL),
    ?assertEqual({401, <<"Unauthorized">>}, status(Response)),
    ?assertEqual(<<"wrong_auth">>, body(Response)).

post_with_mandatory_hdrs() ->
    Port = start(gen_tcp, [fun simple_response/5]),
    URL = url(Port, "/host"),
    Body = <<?DEFAULT_STRING>>,
    Hdrs = #{<<"content-length">> => integer_to_list(size(Body)),
             <<"host">> => "localhost"},
    Response = shttpc:post(URL, Body, #{headers => Hdrs}),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

post_with_mandatory_hdrs_by_atoms() ->
    Port = start(gen_tcp, [fun simple_response/5]),
    URL = url(Port, "/host"),
    Body = <<?DEFAULT_STRING>>,
    Hdrs = #{'Content-Length' => integer_to_list(size(Body)),
             'Host' => "localhost"},
    Response = shttpc:post(URL, Body, #{headers => Hdrs}),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

get_with_connect_options() ->
    Port = start(gen_tcp, [fun empty_body/5]),
    URL = url(Port, "/empty"),
    Response = shttpc:get(URL, #{options => [{ip, {127, 0, 0, 1}}, {port, 0}]}),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

no_content_length() ->
    Port = start(gen_tcp, [fun no_content_length/5]),
    URL = url(Port, "/no_cl"),
    Response = shttpc:get(URL),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

no_content_length_1_0() ->
    Port = start(gen_tcp, [fun no_content_length_1_0/5]),
    URL = url(Port, "/no_cl"),
    Response = shttpc:get(URL),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

%% Check the header value is trimming spaces on header values
%% which can cause crash in lhttpc_client:body_type when Content-Length
%% is converted from list to integer
trailing_space_header() ->
    Port = start(gen_tcp, [fun trailing_space_header/5]),
    URL = url(Port, "/no_cl"),
    Response = shttpc:get(URL),
    ContentLength = maps:get('Content-Length', headers(Response)),
    ?assertEqual(<<"14">>, ContentLength).

get_not_modified() ->
    Port = start(gen_tcp, [fun not_modified_response/5]),
    URL = url(Port, "/not_modified"),
    Response = shttpc:get(URL),
    ?assertEqual({304, <<"Not Modified">>}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

simple_head() ->
    Port = start(gen_tcp, [fun head_response/5]),
    URL = url(Port, "/HEAD"),
    Response = shttpc:head(URL),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

delete_no_content() ->
    Port = start(gen_tcp, [fun no_content_response/5]),
    URL = url(Port, "/delete_no_content"),
    Response = shttpc:delete(URL),
    ?assertEqual({204, <<"OK">>}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

delete_content() ->
    Port = start(gen_tcp, [fun simple_response/5]),
    URL = url(Port, "/delete_content"),
    Response = shttpc:delete(URL),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

options_no_content() ->
    Port = start(gen_tcp, [fun head_response/5]),
    URL = url(Port, "/options_no_content"),
    Response = shttpc:options(URL),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

options_content() ->
    Port = start(gen_tcp, [fun simple_response/5]),
    URL = url(Port, "/options_content"),
    Response = shttpc:options(URL),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

server_connection_close() ->
    Port = start(gen_tcp, [fun respond_and_close/5]),
    URL = url(Port, "/close"),
    Response = shttpc:put(URL, pid_to_list(self())),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)),
    receive closed -> ok end.

client_connection_close() ->
    Port = start(gen_tcp, [fun respond_and_wait/5]),
    URL = url(Port, "/close"),
    Headers = #{<<"Connection">> => <<"close">>},
    #{} = shttpc:put(URL, pid_to_list(self()), #{headers => Headers}),
    %% Wait for the server to see that socket has been closed
    receive closed -> ok end.

client_connection_close_opt() ->
    Port = start(gen_tcp, [fun respond_and_wait/5]),
    URL = url(Port, "/close"),
    #{} = shttpc:put(URL, pid_to_list(self()), #{close => true}),
    %% Wait for the server to see that socket has been closed
    receive closed -> ok end.

pre_1_1_server_connection() ->
    Port = start(gen_tcp, [fun pre_1_1_server/5]),
    URL = url(Port, "/close"),
    #{} = shttpc:put(URL, pid_to_list(self())),
    %% Wait for the server to see that socket has been closed.
    %% The socket should be closed by us since the server responded with a
    %% 1.0 version, and not the Connection: keep-alive header.
    receive closed -> ok end.

pre_1_1_server_keep_alive() ->
    Port = start(gen_tcp, [fun pre_1_1_server_keep_alive/5,
                           fun pre_1_1_server/5]),
    URL = url(Port, "/close"),
    Response1 = #{connection := Con} = shttpc:get(URL),
    Response2 = shttpc:put(URL, pid_to_list(self()), #{connection => Con}),
    ?assertEqual({200, <<"OK">>}, status(Response1)),
    ?assertEqual({200, <<"OK">>}, status(Response2)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response1)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response2)),
    %% Wait for the server to see that socket has been closed.
    %% The socket should be closed by us since the server responded with a
    %% 1.0 version, and not the Connection: keep-alive header.
    receive closed -> ok end.

simple_put() -> simple(put).

patch() ->
    Port = start(gen_tcp, [fun no_content_response/5]),
    URL = url(Port, "/patch"),
    Response = shttpc:patch(URL, <<"Change">>),
    ?assertEqual({204, <<"OK">>}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

post() ->
    Port = start(gen_tcp, [fun copy_body/5]),
    URL = url(Port, "/post"),
    {X, Y, Z} = os:timestamp(),
    Body = ["This is a rather simple post :)",
            integer_to_list(X),
            integer_to_list(Y),
            integer_to_list(Z)],
    Response = shttpc:post(URL, Body),
    {StatusCode, ReasonPhrase} = status(Response),
    ?assertEqual(200, StatusCode),
    ?assertEqual(<<"OK">>, ReasonPhrase),
    ?assertEqual(iolist_to_binary(Body), body(Response)).

trace() ->
    Port = start(gen_tcp, [fun simple_response/5]),
    URL = url(Port, "/trace"),
    Response = shttpc:trace(URL),
    {StatusCode, ReasonPhrase} = status(Response),
    ?assertEqual(200, StatusCode),
    ?assertEqual(<<"OK">>, ReasonPhrase),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

connect() ->
    Port = start(gen_tcp, [fun no_content_response/5]),
    URL = url(Port, "/connect"),
    Response = shttpc:connect(URL),
    {StatusCode, ReasonPhrase} = status(Response),
    ?assertEqual(204, StatusCode),
    ?assertEqual(<<"OK">>, ReasonPhrase),
    ?assertEqual(<<>>, body(Response)).

connect_auth() ->
    User = "foo",
    Passwd = "bar",
    Port = start(gen_tcp, [connect_auth_responder(User, Passwd)]),
    URL = url(Port, "/connect_auth", User, Passwd),
    Response = shttpc:connect(URL),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

post_100_continue() ->
    Port = start(gen_tcp, [fun copy_body_100_continue/5]),
    URL = url(Port, "/post"),
    {X, Y, Z} = os:timestamp(),
    Body = ["This is a rather simple post :)",
            integer_to_list(X),
            integer_to_list(Y),
            integer_to_list(Z)],
    Response = shttpc:post(URL, Body),
    {StatusCode, ReasonPhrase} = status(Response),
    ?assertEqual(200, StatusCode),
    ?assertEqual(<<"OK">>, ReasonPhrase),
    ?assertEqual(iolist_to_binary(Body), body(Response)).

bad_url() -> ?assertError(_, shttpc:get(ost)).

persistent_connection() ->
    Port = start(gen_tcp, [fun simple_response/5,
                           fun simple_response/5,
                           fun copy_body/5]),
    URL = url(Port, "/persistent"),
    FirstResponse = #{connection := Con} = shttpc:get(URL),
    Headers = #{<<"KeepAlive">> => <<"300">>}, %% shouldn't be needed
    SecondResponse =
        shttpc:get(URL, #{connection => Con, headers => Headers}),
    ThirdResponse = shttpc:post(URL, <<>>, #{connection => Con}),
    ?assertEqual({200, <<"OK">>}, status(FirstResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(FirstResponse)),
    ?assertEqual({200, <<"OK">>}, status(SecondResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(SecondResponse)),
    ?assertEqual({200, <<"OK">>}, status(ThirdResponse)),
    ?assertEqual(<<>>, body(ThirdResponse)).

request_timeout() ->
    Port = start(gen_tcp, [fun very_slow_response/5]),
    URL = url(Port, "/slow"),
    ?assertEqual({error, timeout}, shttpc:get(URL, #{timeout => 50})).

chunked_encoding() ->
    Port = start(gen_tcp,
                 [fun chunked_response/5,
                  fun chunked_response_t/5]),
    URL = url(Port, "/chunked"),
    FirstResponse = #{connection := Con} = shttpc:get(URL),
    ?assertEqual({200, <<"OK">>}, status(FirstResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(FirstResponse)),
    ?assertEqual(<<"chunked">>,
                 maps:get('Transfer-Encoding', headers(FirstResponse))),
    SecondResponse = shttpc:get(URL, #{connection => Con}),
    ?assertEqual({200, <<"OK">>}, status(SecondResponse)),
    ?assertEqual(<<"Again, great success!">>, body(SecondResponse)),
    ?assertEqual(<<"ChUnKeD">>,
                 maps:get('Transfer-Encoding', headers(SecondResponse))),
    ?assertEqual(<<"1">>, maps:get(<<"trailer-1">>, headers(SecondResponse))),
    ?assertEqual(<<"2">>, maps:get(<<"trailer-2">>, headers(SecondResponse))).

redirect() ->
    Port1 = start(gen_tcp, [fun redirect_response/5]),
    Port2 = start(gen_tcp, [fun simple_response/5]),
    URL1 = url(Port1, "/redirect"),
    FirstResponse = #{connection := Con} =
        shttpc:post(URL1, integer_to_binary(Port2)),
    ?assertEqual(ok, shttpc:close(Con)),
    ?assertEqual({302, <<"Found">>}, status(FirstResponse)),
    ?assertEqual(<<>>, body(FirstResponse)),
    ?assertEqual(iolist_to_binary(url(Port2, "/the_redirect")),
                 maps:get('Location', headers(FirstResponse), <<>>)),
    URL2 = maps:get('Location', headers(FirstResponse)),
    SecondResponse = shttpc:get(URL2),
    ?assertEqual({200, <<"OK">>}, status(SecondResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(SecondResponse)).

redirect_allowed() ->
    Port1 = start(gen_tcp, [fun redirect_response/5]),
    Port2 = start(gen_tcp, [fun simple_response/5]),
    URL1 = url(Port1, "/redirect"),
    FirstResponse =
        shttpc:post(URL1, integer_to_binary(Port2), #{redirect => true}),
    ?assertEqual({200, <<"OK">>}, status(FirstResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(FirstResponse)).

close_connection() ->
    Port = start(gen_tcp, [fun close_connection/5]),
    URL = url(Port, "/close"),
    ?assertEqual({error, connection_closed}, shttpc:get(URL)).

ssl_get() ->
    Port = start(ssl, [fun simple_response/5]),
    URL = ssl_url(Port, "/simple"),
    Response = shttpc:get(URL),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

ssl_get_ipv6() ->
    Port = start(ssl, [fun simple_response/5], inet6),
    URL = ssl_url(inet6, Port, "/simple"),
    Response = shttpc:get(URL),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

ssl_post() ->
    Port = start(ssl, [fun copy_body/5]),
    URL = ssl_url(Port, "/simple"),
    Body = "SSL Test <o/",
    BinaryBody = list_to_binary(Body),
    Response = shttpc:post(URL, Body),
    ?assertEqual({200, <<"OK">>}, status(Response)),
    ?assertEqual(BinaryBody, body(Response)).

ssl_chunked() ->
    Port = start(ssl, [fun chunked_response/5,
                       fun chunked_response_t/5]),
    URL = ssl_url(Port, "/ssl_chunked"),
    FirstResponse = #{connection := Con} = shttpc:get(URL),
    ?assertEqual({200, <<"OK">>}, status(FirstResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(FirstResponse)),
    ?assertEqual(<<"chunked">>,
                 maps:get('Transfer-Encoding', headers(FirstResponse))),
    SecondResponse = shttpc:get(URL, #{connection => Con}),
    ?assertEqual({200, <<"OK">>}, status(SecondResponse)),
    ?assertEqual(<<"Again, great success!">>, body(SecondResponse)),
    ?assertEqual(<<"ChUnKeD">>,
                 maps:get('Transfer-Encoding', headers(SecondResponse))),
    ?assertEqual(<<"1">>,
                 maps:get(<<"trailer-1">>, headers(SecondResponse))),
    ?assertEqual(<<"2">>,
                 maps:get(<<"trailer-2">>, headers(SecondResponse))).

invalid_options() ->
    ?assertError({bad_option, {bad_option, foo}},
                 shttpc:get("http://localhost/", #{bad_option => foo})).

%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

simple(Method) -> simple(Method, inet).

simple(Method, Family) ->
    Port = start(gen_tcp, [fun simple_response/5], Family),
    URL = url(Family, Port, "/simple"),
    #{status := {StatusCode, ReasonPhrase},
      body := Body} = shttpc:Method(URL),
    ?assertEqual(200, StatusCode),
    ?assertEqual(<<"OK">>, ReasonPhrase),
    ?assertEqual(<<?DEFAULT_STRING>>, Body).

url(Port, Path) -> url(inet, Port, Path).

url(inet, Port, Path) -> "http://localhost:" ++ integer_to_list(Port) ++ Path;
url(inet6, Port, Path) -> "http://[::1]:" ++ integer_to_list(Port) ++ Path.

url(Port, Path, User, Password) -> url(inet, Port, Path, User, Password).

url(inet, Port, Path, User, Password) ->
    "http://" ++ User ++ ":" ++ Password ++
        "@localhost:" ++ integer_to_list(Port) ++ Path;
url(inet6, Port, Path, User, Password) ->
    "http://" ++ User ++ ":" ++ Password ++
        "@[::1]:" ++ integer_to_list(Port) ++ Path.

ssl_url(Port, Path) -> "https://localhost:" ++ integer_to_list(Port) ++ Path.

ssl_url(inet6, Port, Path) -> "https://[::1]:" ++ integer_to_list(Port) ++ Path.

status(#{status := Status}) -> Status.

body(#{body := Body}) -> Body.

headers(#{headers := Headers}) -> Headers.

%%------------------------------------------------------------------------------
%% Responders
%%------------------------------------------------------------------------------

simple_response(Module, Socket, _Request, _Headers, Body) ->
    Module:send(Socket,
                ["HTTP/1.1 200 OK\r\n"
                 "Content-type: text/plain\r\nContent-length: 14\r\n"
                 "X-Test-Orig-Body: ", Body, "\r\n\r\n"
                 ?DEFAULT_STRING]).

head_response(Module, Socket, _Request, _Headers, _Body) ->
    Module:send(Socket,
                "HTTP/1.1 200 OK\r\n"
                "Server: Test server!\r\n\r\n").

no_content_response(Module, Socket, _Request, _Headers, _Body) ->
    Module:send(Socket,
                "HTTP/1.1 204 OK\r\n"
                "Server: Test server!\r\n\r\n").

empty_body(Module, Socket, _, _, _) ->
    Module:send(Socket,
                "HTTP/1.1 200 OK\r\n"
                "Content-type: text/plain\r\nContent-length: 0\r\n\r\n").

copy_body(Module, Socket, _, _, Body) ->
    Module:send(Socket,
                ["HTTP/1.1 200 OK\r\n"
                 "Content-type: text/plain\r\nContent-length: ",
                 integer_to_list(size(Body)), "\r\n\r\n",
                 Body]).

copy_body_100_continue(Module, Socket, _, _, Body) ->
    Module:send(Socket,
                ["HTTP/1.1 100 Continue\r\n\r\n"
                 "HTTP/1.1 200 OK\r\n"
                 "Content-type: text/plain\r\nContent-length: ",
                 integer_to_list(size(Body)), "\r\n\r\n",
                 Body]).

respond_and_close(Module, Socket, _, _, Body) ->
    Module:send(Socket,
                "HTTP/1.1 200 OK\r\n"
                "Connection: close\r\n"
                "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"
                ?DEFAULT_STRING),
    {error, closed} = Module:recv(Socket, 0),
    list_to_pid(binary_to_list(Body)) ! closed,
    Module:close(Socket).

respond_and_wait(Module, Socket, _, _, Body) ->
    Module:send(Socket,
                "HTTP/1.1 200 OK\r\n"
                "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"
                ?DEFAULT_STRING),
    {error, closed} = Module:recv(Socket, 0),
    list_to_pid(binary_to_list(Body)) ! closed,
    Module:close(Socket).

pre_1_1_server(Module, Socket, _, _, Body) ->
    Module:send(Socket,
                "HTTP/1.0 200 OK\r\n"
                "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"
                ?DEFAULT_STRING),
    {error, closed} = Module:recv(Socket, 0),
    list_to_pid(binary_to_list(Body)) ! closed,
    Module:close(Socket).

pre_1_1_server_keep_alive(Module, Socket, _, _, _) ->
    Module:send(Socket,
                "HTTP/1.0 200 OK\r\n"
                "Content-type: text/plain\r\n"
                "Connection: Keep-Alive\r\n"
                "Content-length: 14\r\n\r\n"
                ?DEFAULT_STRING).

very_slow_response(Module, Socket, _, _, _) ->
    timer:sleep(1000),
    Module:send(Socket,
                "HTTP/1.1 200 OK\r\n"
                "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"
                ?DEFAULT_STRING).

no_content_length(Module, Socket, _, _, _) ->
    Module:send(Socket,
                "HTTP/1.1 200 OK\r\n"
                "Content-type: text/plain\r\nConnection: close\r\n\r\n"
                ?DEFAULT_STRING).

no_content_length_1_0(Module, Socket, _, _, _) ->
    Module:send(Socket,
                "HTTP/1.0 200 OK\r\n"
                "Content-type: text/plain\r\n\r\n"
                ?DEFAULT_STRING).

chunked_response(Module, Socket, _, _, _) ->
    Module:send(Socket,
                "HTTP/1.1 200 OK\r\n"
                "Content-type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n"
                "5\r\n"
                "Great\r\n"
                "1\r\n"
                " \r\n"
                "8\r\n"
                "success!\r\n"
                "0\r\n"
                "\r\n").

chunked_response_t(Module, Socket, _, _, _) ->
    Module:send(Socket,
                "HTTP/1.1 200 OK\r\n"
                "Content-type: text/plain\r\nTransfer-Encoding: ChUnKeD\r\n\r\n"
                "7\r\n"
                "Again, \r\n"
                "E\r\n"
                "great success!\r\n"
                "0\r\n"
                "Trailer-1: 1\r\n"
                "Trailer-2: 2\r\n"
                "\r\n").

close_connection(Module, Socket, _, _, _) ->
    Module:send(Socket,
                "HTTP/1.1 200 OK\r\n"
                "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"),
    Module:close(Socket).

not_modified_response(Module, Socket, _Request, _Headers, _Body) ->
    Module:send(Socket,
		["HTTP/1.1 304 Not Modified\r\n"
                 "Date: Tue, 15 Nov 1994 08:12:31 GMT\r\n\r\n"]).

basic_auth_responder(User, Passwd) ->
    fun(Module, Socket, _, Headers, _) ->
            case plist:find("Authorization", Headers) of
                undefined ->
                    Module:send(Socket,
                                ["HTTP/1.1 401 Unauthorized\r\n",
                                 "Content-Type: text/plain\r\n",
                                 "Content-Length: 12\r\n\r\n",
                                 "missing_auth"]);
                "Basic " ++ Auth ->
                    [U, P] =
                        bstring:tokens(base64:decode(list_to_binary(Auth)),
                                       <<":">>),
                    case {binary_to_list(U), binary_to_list(P)} of
                        {User, Passwd} ->
                            Module:send(Socket,
                                        ["HTTP/1.1 200 OK\r\n",
                                         "Content-Type: text/plain\r\n",
                                         "Content-Length: 2\r\n\r\n",
                                         "OK"
                                        ]);
                        _ ->
                            Module:send(Socket,
                                        ["HTTP/1.1 401 Unauthorized\r\n",
                                         "Content-Type: text/plain\r\n",
                                         "Content-Length: 10\r\n\r\n",
                                         "wrong_auth"])
                    end
            end
    end.

connect_auth_responder(User, Passwd) ->
    fun(Module, Socket, _, Headers, _) ->
            "Basic " ++ Auth = plist:find("Proxy-Authorization", Headers),
            [U, P] =
                bstring:tokens(base64:decode(list_to_binary(Auth)), <<":">>),
            {User, Passwd} = {binary_to_list(U), binary_to_list(P)},
            Module:send(Socket,
                        ["HTTP/1.1 200 OK\r\n",
                         "Content-Type: text/plain\r\n",
                         "Content-Length: 0\r\n\r\n"])
    end.

trailing_space_header(Module, Socket, _, _, _) ->
    Module:send(Socket,
                "HTTP/1.1 200 OK\r\n"
                "Content-type: text/plain\r\n"
                "Content-Length: 14 \r\n\r\n"
                ?DEFAULT_STRING).

redirect_response(Module, Socket, _, _, Body) ->
    URL = url(binary_to_integer(Body), "/the_redirect"),
    Module:send(Socket,
                ["HTTP/1.1 302 Found\r\n"
                 "Content-type: text/plain\r\nContent-length: 0\r\n"
                 "Location: ", URL, "\r\n\r\n"]).

%%------------------------------------------------------------------------------
%% Webserver
%%------------------------------------------------------------------------------

dir() -> code:lib_dir(jhn_stdlib, test).

file(File) -> filename:join([dir(), File]).


-define(TCP, [{packet, http}, binary, {active, false}]).
-define(SSL, [{verify, 0},
              {keyfile, file("key.pem")},
              {certfile, file("crt.pem")} | ?TCP]).

start(Mod, Responders) -> start(Mod, Responders, inet).

start(Mod, Responders, Family) ->
    {ok, Addr} = inet:getaddr("localhost", Family) ,
    {ok, LS} = listen(Mod, Addr, Family),
    spawn_link(fun() -> accept_connection(self(), Mod, LS, Responders) end),
    port(Mod, LS).

accept_connection(Parent, Mod, LS, Responders) ->
    server_loop(Mod, accept(Mod, LS), undefined, [], Responders),
    unlink(Parent).

server_loop(Mod, Socket, _, _, []) -> Mod:close(Socket);
server_loop(Mod, Socket, Request, Headers, Responders) ->
    case Mod:recv(Socket, 0) of
        {ok, Request1 = {http_request, _, _, _}} ->
            server_loop(Mod, Socket, Request1, Headers, Responders);
        {ok, {http_header, _, Field, _, Value}} when is_atom(Field) ->
            NewHeaders = [{atom_to_list(Field), Value} | Headers],
            server_loop(Mod, Socket, Request, NewHeaders, Responders);
        {ok, {http_header, _, Field, _, Value}} when is_list(Field) ->
            NewHeaders = [{Field, Value} | Headers],
            server_loop(Mod, Socket, Request, NewHeaders, Responders);
        {ok, http_eoh} ->
            Body =
                case plist:find("Content-Length", Headers) of
                    undefined -> <<>>;
                    "0" -> <<>>;
                    SLength ->
                        Length = list_to_integer(SLength),
                        setopts(Mod, Socket, [{packet, raw}]),
                        {ok, Body0} = Mod:recv(Socket, Length),
                        setopts(Mod, Socket, [{packet, http}]),
                        Body0
                end,
            Responder = hd(Responders),
            Responder(Mod, Socket, Request, Headers, Body),
            server_loop(Mod, Socket, none, [], tl(Responders))
    end.

listen(ssl, Addr, Family) -> ssl:listen(0, [Family, {ip, Addr} | ?SSL]);
listen(Mod, Addr, Family) -> Mod:listen(0, [Family, {ip, Addr} | ?TCP]).

accept(ssl, LS) ->
    {ok, Socket} = ssl:transport_accept(LS, 10000),
    ok = ssl:ssl_accept(Socket),
    Socket;
accept(Mod, LS) -> element(2,  Mod:accept(LS, 1000)).


setopts(ssl, Socket, Options) -> ssl:setopts(Socket, Options);
setopts(_, Socket, Options) -> inet:setopts(Socket, Options).

port(ssl, Socket) ->
    {ok, {_, Port}} = ssl:sockname(Socket),
    Port;
port(_, Socket) ->
    {ok, Port} = inet:port(Socket),
    Port.
