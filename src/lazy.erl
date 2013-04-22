%%==============================================================================
%% Copyright 2013 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A lazy lib.
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(lazy).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([create/1, create/2]).

-export([prepend/2, append/2]).

-export([list_to_data/1, iolist_to_data/1,
         tcp_to_data/2, tcp_to_data/3, tcp_socket_to_data/1,
         file_to_data/2, file_stream_to_data/2
        ]).

%% Types
-type data(Type) :: fun(([timeout()]) -> {Type, data(Type)}) | eol.

-type promise(Type) :: fun(([timeout()]) -> Type | eol).
-type promise(Type, State) :: fun(([timeout()], State) -> {Type, State} | eol).

%% Exported Types
-export_type([data/1, promise/1, promise/2]).


%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec create(promise(Type)) -> data(Type).
%%--------------------------------------------------------------------
create(F) ->
    fun(Timeout) ->
            case F(Timeout) of
                eol -> eol;
                Data -> {Data, create(F)}
            end
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec create(promise(Type, State), State) -> data(Type).
%%--------------------------------------------------------------------
create(F, State) ->
    fun(Timeout) ->
            case F(Timeout, State) of
                {Data, State1} -> {Data, create(F, State1)};
                eol -> eol
            end
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec prepend(Type, data(Type)) -> data(Type).
%%--------------------------------------------------------------------
prepend(Data, Lazy) -> fun(_) -> {Data, Lazy} end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec append(Type, data(Type)) -> data(Type).
%%--------------------------------------------------------------------
append(Data, Lazy) ->
    fun(Timeout) ->
            case Lazy(Timeout) of
                eol -> {Data, fun(_) -> eol end};
                {Data1, Lazy1} -> {Data1, append(Data, Lazy1)}
            end
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec list_to_data([Type]) -> data(Type).
%%--------------------------------------------------------------------
list_to_data(List) ->
    Promise = fun(_, []) -> eol;
                 (_, [H | T]) -> {H, T}
              end,
    create(Promise, List).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec iolist_to_data(iolist()) -> data(binary()).
%%--------------------------------------------------------------------
iolist_to_data(List) ->
    Promise = fun(_, []) -> eol;
                 (_, [H | T]) when is_binary(H) -> {H, iolist_to_data(T)};
%% TODO should have deep traversal
                 (_, [H | T]) -> {iolist_to_binary(H), iolist_to_data(T)}
              end,
    create(Promise, List).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec tcp_to_data(HostName, Port) -> data(binary()) | {error, inet:posix()} when
      HostName:: inet:ip_address() | inet:hostname(),
      Port ::inet:port_number().
%%--------------------------------------------------------------------
tcp_to_data(HostName, Port) ->
    Options = [{packet, 0}, binary, {active, false}],
    case gen_tcp:connect(HostName, Port, Options) of
        {ok, Socket} -> tcp_socket_to_data(Socket);
        Error = {error, _}  -> Error
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec tcp_to_data(HostName, Port, timeout()) ->
          data(binary()) | {error, inet:posix()} when
      HostName:: inet:ip_address() | inet:hostname(),
      Port ::inet:port_number().
%%--------------------------------------------------------------------
tcp_to_data(HostName, Port, Timeout) ->
    Options = [{packet, 0}, binary, {active, false}],
    case gen_tcp:connect(HostName, Port, Options, Timeout) of
        {ok, Socket} -> tcp_socket_to_data(Socket);
        Error = {error, _}  -> Error
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec tcp_socket_to_data(inet:socket()) -> data(binary()).
%%--------------------------------------------------------------------
tcp_socket_to_data(TCPSocket) ->
    Promise = fun(Timeout, Socket) ->
                      case gen_tcp:recv(Socket, 0, Timeout) of
                          {ok, Packet} -> {Packet, Socket};
                          {error, timeout} -> {<<>>, Socket};
                          {error, closed} -> eol;
                          {error, _} -> tcp:close(Socket), eol
                      end
              end,
    create(Promise, TCPSocket).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec file_to_data(line | integer(), file:filename()) ->
          data(binary()) | {error, file:posix() | badarg | system_limit}.
%%--------------------------------------------------------------------
file_to_data(Type, Name) ->
    case file:open(Name, [read, raw, read_ahead]) of
        {ok, Device} -> file_stream_to_data(Type, Device);
        Error = {error, _} -> Error
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec file_stream_to_data(line | integer(), file:io_device()) -> data(binary()).
%%--------------------------------------------------------------------
file_stream_to_data(line, FileStream) ->
    Promise = fun(_, Stream) ->
                      case file:read_line(Stream) of
                          {ok, Data} -> {Data, Stream};
                          {error, _} -> file:close(Stream), eol;
                          eof -> file:close(Stream), eol
                      end
              end,
    create(Promise, FileStream);
file_stream_to_data(ChunkSize, FileStream) ->
    Promise = fun(_, Stream) ->
                      case file:read(Stream, ChunkSize) of
                          {ok, Data} -> {Data, Stream};
                          {error, _} -> file:close(Stream), eol;
                          eof -> file:close(Stream), eol
                      end
              end,
    create(Promise, FileStream).


%% ===================================================================
%% Internal functions.
%% ===================================================================
