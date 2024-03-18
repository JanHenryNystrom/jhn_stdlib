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
%% lz77_compress/1 <-> lz77_uncompress/1
%%--------------------------------------------------------------------
lz77_compress_1_lz77_uncompress_1_test_() ->
    [?_test(?assertEqual(T, jhn_lz:lz77_uncompress(jhn_lz:lz77_compress(T)))) ||
        T <- [rfc(2732), rfc(2818) | ?TEXTS]
    ].


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

%% ===================================================================
%% Internal functions.
%% ===================================================================

rfc(Int) ->
    Dir = code:lib_dir(jhn_stdlib, test),
    FileName = "rfc" ++ integer_to_list(Int) ++ ".txt",
    {ok, File} = file:read_file(filename:join([Dir, "jhn_lz", FileName])),
    File.
