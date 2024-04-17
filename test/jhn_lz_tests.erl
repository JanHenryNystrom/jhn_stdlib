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
%%%   eunit unit tests for the jhn_lz library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_lz_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(TEXTS, [<<"TOBEORNOTTOBE">>,
                <<"YABBADABBADABBADOO">>,
                <<"AAAAAAAAAAAAAAAAAA">>,
                <<"abcbbcbaaaaaa">>]).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% snappy_compress/1 <-> snappy_uncompress/1 Block
%%--------------------------------------------------------------------
snappy_compress_1_snappy_uncompress_1_test_() ->
    [?_test(?assertEqual(T,
                         jhn_lz:snappy_uncompress(
                           iolist_to_binary(jhn_lz:snappy_compress(T))))) ||
        T <- [rfc(2732), rfc(2818) | ?TEXTS]
    ].

%%--------------------------------------------------------------------
%% lz77_compress/1 <-> lz77_uncompress/1
%%--------------------------------------------------------------------
lz77_compress_1_lz77_uncompress_1_test_() ->
    [?_test(?assertEqual(T, jhn_lz:lz77_uncompress(jhn_lz:lz77_compress(T)))) ||
        T <- [rfc(2732), rfc(2818) | ?TEXTS]].

%%--------------------------------------------------------------------
%% lz77d_compress/1 <-> lz77_uncompress/1
%%--------------------------------------------------------------------
lz77d_compress_1_lz77_uncompress_1_test_() ->
    [?_test(?assertEqual(T,
                         jhn_lz:lz77_uncompress(jhn_lz:lz77d_compress(T)))) ||
        T <- [file(alice29), rfc(2732), rfc(2818) | ?TEXTS]].

%%--------------------------------------------------------------------
%% lz78_compress/1 <-> lz78w_uncompress/1
%%--------------------------------------------------------------------
lz78_compress_1_lz78w_uncompress_1_test_() ->
    [?_test(
        ?assertEqual(T,
                     iolist_to_binary(
                       jhn_lz:lz78w_uncompress(jhn_lz:lz78_compress(T))))) ||
        T <- [rfc(2732), rfc(2818) | ?TEXTS]
    ].

%%--------------------------------------------------------------------
%% lz78_compress/1 <-> lz78w_uncompress/2
%%--------------------------------------------------------------------
lz78_compress_1_lz78w_uncompress_2_test_() ->
    [?_test(
        ?assertEqual(T,
                     jhn_lz:lz78w_uncompress(jhn_lz:lz78_compress(T),
                                             [binary]))) ||
        T <- [rfc(2732), rfc(2818) | ?TEXTS]
    ] ++
        [?_test(
            ?assertEqual(T,
                         iolist_to_binary(
                           jhn_lz:lz78w_uncompress(jhn_lz:lz78_compress(T),
                                                   [iolist])))) ||
            T <- [rfc(2732), rfc(2818) | ?TEXTS]
        ].

%%--------------------------------------------------------------------
%% lz78_init/0 lz78w_cont/2 lz78w_end/1 <-> lz78w_uncompress/2
%%--------------------------------------------------------------------
lz78_init_0_lz78w_cont_2_lz78w_end_1_lz78w_uncompress_2_test_() ->
    [?_test(
        ?assertEqual(T,
                     jhn_lz:lz78w_uncompress(
                       jhn_lz:lz78w_end(
                         jhn_lz:lz78w_cont(T, jhn_lz:lz78_init())),
                       [binary]))) ||
        T <- [rfc(2732), rfc(2818) | ?TEXTS]
    ].

%%--------------------------------------------------------------------
%% lz78_init/0 lz78w_cont/2 lz78w_end/2 <-> lz78w_uncompress/2
%%--------------------------------------------------------------------
lz78_init_0_lz78w_cont_2_lz78w_end_2_lz78w_uncompress_2_test_() ->
    [?_test(
        ?assertEqual(<<T/binary, T/binary>>,
                     jhn_lz:lz78w_uncompress(
                       jhn_lz:lz78w_end(
                         T,
                         jhn_lz:lz78w_cont(T, jhn_lz:lz78_init())),
                       [binary]))) ||
        T <- [rfc(2732), rfc(2818) | ?TEXTS]
    ] ++
        [?_test(
            ?assertEqual(<<T/binary, T/binary>>,
                         jhn_lz:lz78w_uncompress(
                           jhn_lz:lz78w_end(
                             chunk(T),
                             jhn_lz:lz78w_cont(chunk(T), jhn_lz:lz78_init())),
                           [binary]))) ||
            T <- [rfc(2732), rfc(2818) | ?TEXTS]
        ].

%%--------------------------------------------------------------------
%% lzw_compress/1 <-> lz78w_uncompress/2
%%--------------------------------------------------------------------
lzw_compress_1_lz78w_uncompress_2_test_() ->
    [?_test(
        ?assertEqual(T,
                     jhn_lz:lz78w_uncompress(
                       jhn_lz:lzw_compress(T),
                       [binary]))) ||
        T <- [rfc(2732), rfc(2818) | ?TEXTS]
    ].

%%--------------------------------------------------------------------
%% lzw_compress/2 <-> lz78w_uncompress/2
%%--------------------------------------------------------------------
lzw_compress_2_lz78w_uncompress_2_test_() ->
    Dict = lists:foldl(fun(C, Acc) -> maps:put(<<C:8/integer>>, C, Acc) end,
                       #{},
                       lists:seq(32, 127)),
    [?_test(
        ?assertEqual(T,
                     jhn_lz:lz78w_uncompress(
                       jhn_lz:lzw_compress(T, Dict),
                       [binary]))) ||
        T <- [rfc(2732), rfc(2818) | ?TEXTS]
    ].

%%--------------------------------------------------------------------
%% Performance
%%--------------------------------------------------------------------
performance_test_() ->
    [{timeout, 10,
      ?_test(
         ?assertEqual(
            ok,
            begin
                {Timec, D} =
                    timer:tc(jhn_lz, snappy_compress, [T, [{min_match, 8}]]),
                D1 = iolist_to_binary(D),
                {Timeu, _} = timer:tc(jhn_lz, snappy_uncompress, [D1]),
                Size = byte_size(T),
                Percent = trunc(100* (byte_size(D1) / Size)),
                Speedc = (1000000 / Timec) * (Size / math:pow(2, 20)),
                Speedu = (1000000 / Timeu) * (Size / math:pow(2, 20)),
                ?debugFmt("~nsnappy ~p:~p ~p% ~.2..f:~.2..f MB/s ~pBytes~n",
                          [Timec, Timeu, Percent, Speedc, Speedu, Size]),
                ok
            end))} ||
        T <- [jpeg(fireworks), file(alice29), file(html), rfc(2732), rfc(2818)]
    ] ++
        [{timeout, 10,
          ?_test(
             ?assertEqual(
                ok,
                begin
                    {Timec, D} = timer:tc(jhn_lz, lz77_compress, [T]),
                    {Timeu, _} = timer:tc(jhn_lz, lz77_uncompress, [D]),
                    ?debugFmt("~nlz77 ~p:~p~n", [Timec, Timeu]),
                    ok
                end))} ||
            T <- [file(alice29), file(html), rfc(2732), rfc(2818)]
        ] ++
        [{timeout, 10,
          ?_test(
             ?assertEqual(
                ok,
                begin
                    {Timec, D} = timer:tc(jhn_lz, lz78_compress, [T]),
                    {Timeu, _} = timer:tc(jhn_lz, lz78w_uncompress, [D]),
                    Size = byte_size(T),
                    {Bs, _, Map} = D,
                    Percent = trunc(100 * (byte_size(Bs) / Size)),
                    Speedc = (1000000 / Timec) * (Size / math:pow(2, 20)),
                    Speedu = (1000000 / Timeu) * (Size / math:pow(2, 20)),
                    MaxSize =
                        maps:fold(fun(_, V, Acc) when V > Acc -> V;
                                     (_, _, Acc) -> Acc
                                  end,
                                  0,
                                  maps:map(fun(K, _) -> byte_size(K) end, Map)),
                    ?debugFmt(
                       "~nlz78 ~p:~p ~p% ~.2..f:~.2..f MB/s ~pBytes "
                       "Dict:~p:~p~n",
                       [Timec, Timeu, Percent, Speedc, Speedu, Size,
                        maps:size(Map), MaxSize]),
                    ok

                end))} ||
            T <- [file(alice29), file(html), rfc(2732), rfc(2818)]
        ] ++
        [{timeout, 10
         ,?_test(
             ?assertEqual(
                ok,
                begin
                    {Timec, D} = timer:tc(jhn_lz, lz77d_compress, [T]),
                    {Timeu, _} = timer:tc(jhn_lz, lz77_uncompress, [D]),
                    ?debugFmt("~nlz77d ~p:~p~n",
                              [Timec, Timeu]),
                    ok
                end))} ||
            T <- [file(alice29), file(html), rfc(2732), rfc(2818)]
        ].


%% ===================================================================
%% Internal functions.
%% ===================================================================

rfc(Int) ->
    Dir = code:lib_dir(jhn_stdlib, test),
    FileName = "rfc" ++ integer_to_list(Int) ++ ".txt",
    {ok, File} = file:read_file(filename:join([Dir, "jhn_lz", FileName])),
    File.

file(Name) when is_atom(Name) ->
    Dir = code:lib_dir(jhn_stdlib, test),
    FileName = atom_to_list(Name) ++ ".txt",
    {ok, File} = file:read_file(filename:join([Dir, "jhn_lz", FileName])),
    File.

jpeg(Name) when is_atom(Name) ->
    Dir = code:lib_dir(jhn_stdlib, test),
    FileName = atom_to_list(Name) ++ ".jpeg",
    {ok, File} = file:read_file(filename:join([Dir, "jhn_lz", FileName])),
    File.

-define(FIB, [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]).

chunk(Data) -> chunk(Data, ?FIB, []).

chunk(Data, [], Acc) -> chunk(Data, ?FIB, Acc);
chunk(Data, [H | _], Acc) when H > byte_size(Data)-> lists:reverse([Data|Acc]);
chunk(Data, [H | T], Acc) ->
    <<Chunk:H/binary, Data1/binary>> = Data,
    chunk(Data1, T, [Chunk | Acc]).
