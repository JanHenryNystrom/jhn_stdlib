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
%%%   eunit unit tests for the jhn_snappy library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_snappy_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(TEXTS, [<<"TOBEORNOTTOBE">>,
                <<"YABBADABBADABBADOO">>,
                <<"AAAAAAAAAAAAAAAAAA">>,
                <<"abcbbcbaaaaaa">>
               ]).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% compress/1 <-> uncompress/1 Block
%%--------------------------------------------------------------------
compress_1_uncompress_1_test_() ->
    [?_test(?assertEqual(T, jhn_snappy:uncompress(jhn_snappy:compress(T)))) ||
        T <- [jpeg(fireworks),
              file(alice29), file(html),
              rfc(2732), rfc(2818) | ?TEXTS]
    ].

%%--------------------------------------------------------------------
%% compress/1 <-> uncompress/1 Frame
%%--------------------------------------------------------------------
compress_1_frame_uncompress_1_test_() ->
    [?_test(
        ?assertEqual(
           T,
           jhn_snappy:uncompress(
             jhn_snappy:compress(T, [{type, frame}, {return_type, binary}]),
             [{return_type, binary}]))) ||
        T <- [jpeg(fireworks),
              file(alice29), file(html),
              rfc(2732), rfc(2818) | ?TEXTS]
    ] ++
    [?_test(
        ?assertEqual(
           T,
           jhn_snappy:uncompress(
             jhn_snappy:compress(T, [iwa, binary]), [iwa, binary]))) ||
        T <- [file(html)]
    ].

%%--------------------------------------------------------------------
%% Performance
%%--------------------------------------------------------------------
%% performance_test_() ->
%%     [{timeout, 10,
%%       ?_test(
%%          ?assertEqual(
%%             ok,
%%             begin
%%                 {Timec, D} =
%%                     timer:tc(jhn_snappy, compress, [T, [{min_match, 8}]]),
%%                 D1 = iolist_to_binary(D),
%%                 {Timeu, _} = timer:tc(jhn_snappy, uncompress, [D1]),
%%                 Size = byte_size(T),
%%                 Percent = trunc(100* (byte_size(D1) / Size)),
%%                 Speedc = (1000000 / Timec) * (Size / math:pow(2, 20)),
%%                 Speedu = (1000000 / Timeu) * (Size / math:pow(2, 20)),
%%                 ?debugFmt("~nsnappy ~p:~p ~p% ~.2..f:~.2..f MB/s ~pBytes~n",
%%                           [Timec, Timeu, Percent, Speedc, Speedu, Size]),
%%                 ok
%%             end))} ||
%%         T <- [jpeg(fireworks), file(alice29), file(html), rfc(2732), rfc(2818)]
%%     ].


%% ===================================================================
%% Internal functions.
%% ===================================================================

rfc(Int) ->
    Dir = code:lib_dir(jhn_stdlib),
    FileName = "rfc" ++ integer_to_list(Int) ++ ".txt",
    {ok, File} =
        file:read_file(filename:join([Dir, test, "jhn_snappy", FileName])),
    File.

file(Name) when is_atom(Name) ->
    Dir = code:lib_dir(jhn_stdlib),
    FileName = atom_to_list(Name) ++ ".txt",
    {ok, File} =
        file:read_file(filename:join([Dir, test, "jhn_snappy", FileName])),
    File.

jpeg(Name) when is_atom(Name) ->
    Dir = code:lib_dir(jhn_stdlib),
    FileName = atom_to_list(Name) ++ ".jpeg",
    {ok, File} =
        file:read_file(filename:join([Dir, test, "jhn_snappy", FileName])),
    File.
