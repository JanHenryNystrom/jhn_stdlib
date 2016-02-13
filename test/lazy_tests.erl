%%==============================================================================
%% Copyright 2013-2016 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the lazy library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2016, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(lazy_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(LISTS,
        [[],
         [a],
         lists:seq(1, 100),
         "Foo Fi Fum"
        ]).

-define(IOLISTS,
        [[],
         [<<"a">>],
         [<<"a">>, <<"b">>],
         [[<<"a">>], [1, 2, 3, <<"b">>]],
         [<<I>> || I <- lists:seq(1, 100)],
         <<"Foo Fi Fum">>
        ]).

-define(FILES,
        ["lazy_file.txt"]).

-define(PORT, 8888).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% prepend/2
%%--------------------------------------------------------------------
prepend_2_test_() ->
    [?_test(
        ?assertEqual([head | List],
                     exhaust(
                       lazy:prepend(head, lazy:list_to_data(List)), []))) ||
        List <- ?LISTS].

%%--------------------------------------------------------------------
%% append/2
%%--------------------------------------------------------------------
append_2_test_() ->
    [?_test(
        ?assertEqual(List ++ [tail],
                     exhaust(
                       lazy:append(tail, lazy:list_to_data(List)), []))) ||
        List <- ?LISTS].

%%--------------------------------------------------------------------
%% list_to_data/1
%%--------------------------------------------------------------------
list_to_data_1_test_() ->
    [?_test(?assertEqual(List, exhaust(lazy:list_to_data(List), []))) ||
        List <- ?LISTS].

%%--------------------------------------------------------------------
%% iolist_to_data/1
%%--------------------------------------------------------------------
iolist_to_data_1_test_() ->
    [?_test(
        ?assertEqual(iolist_to_binary(IOList),
                     iolist_to_binary(
                       exhaust(lazy:iolist_to_data(IOList), [])))) ||
        IOList <- ?IOLISTS].

%%--------------------------------------------------------------------
%% file_to_data_line/1
%%--------------------------------------------------------------------
file_to_data_1_line_test_() ->
    [?_test(
        ?assertEqual(element(2, file:read_file(abs_name(File))),
                     iolist_to_binary(
                       exhaust(
                         lazy:file_to_data(line, abs_name(File)), [])))) ||
        File <- ?FILES].

%%--------------------------------------------------------------------
%% tcp_to_data/2
%%--------------------------------------------------------------------
tcp_to_data_2_test_() ->
    {setup,
     fun() -> spawn_source(?IOLISTS, 0) end,
     fun(Pid) -> unlink(Pid), exit(Pid, shutdown) end,
     [{timeout, 120,
      ?_test(
         ?assertEqual(iolist_to_binary(?IOLISTS),
                      iolist_to_binary(
                        exhaust(lazy:tcp_to_data("127.0.0.1", ?PORT), []))))}
     ]
    }.

%%--------------------------------------------------------------------
%% tcp_reconnect_to_data/3
%%--------------------------------------------------------------------
tcp_reconnect_to_data_3_test_() ->
    {setup,
     fun() -> spawn_source(?IOLISTS, 1) end,
     fun(Pid) -> unlink(Pid), exit(Pid, shutdown) end,
     [{timeout, 120,
      ?_test(
         ?assertEqual(iolist_to_binary(?IOLISTS ++ ?IOLISTS),
                      iolist_to_binary(
                        exhaust(lazy:tcp_reconnect_to_data("127.0.0.1",
                                                           ?PORT,
                                                           100),
                                []))))}
     ]
    }.

%%--------------------------------------------------------------------
%% file_to_data_chunk/1
%%--------------------------------------------------------------------
file_to_data_1_chunk_test_() ->
    [?_test(
        ?assertEqual(element(2, file:read_file(abs_name(File))),
                     iolist_to_binary(
                       exhaust(
                         lazy:file_to_data(Chunk, abs_name(File)), [])))) ||
        File <- ?FILES,
        Chunk <- [1, 2, 3, 10, 1024]].

%% ===================================================================
%% Internal functions.
%% ===================================================================

exhaust(Lazy, Acc) ->
    case Lazy(100) of
        eol -> lists:reverse(Acc);
        {Data, Lazy1} -> exhaust(Lazy1, [Data | Acc])
    end.

abs_name(File) ->
    filename:join([code:lib_dir(jhn_stdlib), "test", File]).

spawn_source(Data, N) ->
    Me = self(),
    Pid = spawn_link(fun() -> source(Me, Data, N) end),
    receive setup -> Pid after 10000 -> exit(source_timeout) end.

source(Parent, Template, N) when is_pid(Parent) ->
    {ok, LSock} =
        gen_tcp:listen(?PORT, [binary, {packet, 0}, {reuseaddr, true}]),
    Parent ! setup,
    {ok, Sock} = gen_tcp:accept(LSock),
    source(Template, N, Sock, LSock, Template).

source([], 0, Sock, _, _) ->
    gen_tcp:close(Sock);
source([], N, Sock, LSock, Template) ->
    gen_tcp:close(Sock),
    {ok, Sock1} = gen_tcp:accept(LSock),
    source(Template, N - 1, Sock1, LSock, Template);
source([H | T], N, Sock, LSock, Template) ->
    timer:sleep(100),
    gen_tcp:send(Sock, H),
    source(T, N,  Sock, LSock, Template).



