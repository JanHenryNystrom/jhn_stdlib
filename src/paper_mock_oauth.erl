%% -*-erlang-*-
%%==============================================================================
%% Copyright 2026 Kivra AB <dev@kivra.com>
%%==============================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%
%% @copyright (C) 2025, Kivra AB <dev@kivra.com>
%%%-------------------------------------------------------------------
-module(paper_mock_oauth).
-copyright('Kivra AB <dev@kivra.com>').

-export([
   start_link/0,
   stop/1,
   set_answer/1
]).

-define(OPTS, [inet, {packet, http_bin}, binary, {active, false}]).
-define(REPLY_200,
   "HTTP/1.1 200 OK\r\nServer: Test server!\r\n"
   "Content-Length: 32\r\n\r\n"
   "{\"access_token\": \"AAccessToken\"}"
).
-define(REPLY_503,
   "HTTP/1.1 503 Service Unavailable\r\n"
   "Content-length: 0\r\n"
   "Server: Test server!\r\n\r\n"
).
-define(TIMEOUT, 1000_000_000).

start_link() ->
    {ok, ListenSocket} = gen_tcp:listen(0, ?OPTS),
    Fun = fun() ->
                  {ok, Socket} = gen_tcp:accept(ListenSocket, ?TIMEOUT),
                  loop(Socket, ListenSocket, none, [])
          end,
    spawn_link(Fun),
    {ok, Port} = inet:port(ListenSocket),
    Port.

stop(Port) ->
   jhn_shttpc:delete(["http://127.0.0.1:",
                      integer_to_list(Port),
                      "/jhn_shadow"]).

set_answer(Answer) -> application:set_env(paper, mock_answer, Answer).

loop(Socket, LS, Request, Headers) ->
   case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
       {error, closed} ->
           {ok, Socket1} = gen_tcp:accept(LS, ?TIMEOUT),
           loop(Socket1, LS, none, []);
       {ok, {http_error, Error}} ->
           io:format("~nErrrort:~n    ~p~n", [Error]),
           gen_tcp:close(Socket),
           {ok, Socket1} = gen_tcp:accept(LS, ?TIMEOUT),
           loop(Socket1, LS, none, []);
       {ok, {http_request, 'DELETE', {abs_path, ~"/jhn_shadow"}, _}} ->
           gen_tcp:close(Socket);
       {ok, Request1 = {http_request, 'POST', URI, {1, 1}}} ->
           Path = case URI of
                      '*' -> ~"";
                      {absoluteURI, _, Host, _, Path0} -> Path0;
                      {abs_path, Path0} -> Path0;
                      {scheme, _, URI0} ->
                          #{path := Path0} = uri_string:parse(URI0),
                          Path0;
                      URI0 ->
                          #{path := Path0} = uri_string:parse(URI0),
                          Path0
                  end,
           io:format("~nPath:~n    ~p~n", [Path]),
           io:format("~nRequest:~n    ~p~n", [Request1]),
           loop(Socket, LS, Request1, Headers);
       {ok, H = {http_header, _, _, Field, Value}} ->
           io:format("~nHeader:~n    ~p~n", [H]),
           loop(Socket, LS, Request, [{Field, Value} | Headers]);
       {ok, http_eoh} ->
%%           Headers1 = [{jhn_bstring:to_lower(K), V} || {K, V} <- Headers],
           case lists:keyfind(~"Content-Length", 1, Headers) of
               false -> ok;
               {_, ~"0"} -> ok;
               {_, Len} ->
                   inet:setopts(Socket, [{packet, raw}]),
                   {ok, Body} =
                       gen_tcp:recv(Socket, binary_to_integer(Len), ?TIMEOUT),
                   io:format("~nBody:~n    ~pn~n", [Body]),
                   inet:setopts(Socket, [{packet, http_bin}])
           end,
           Answer = application:get_env(paper, mock_answer, 200),
           case Answer of
               200 -> gen_tcp:send(Socket, ?REPLY_200);
               503 -> gen_tcp:send(Socket, ?REPLY_503)
           end,
           loop(Socket, LS, none, [])
   end.
