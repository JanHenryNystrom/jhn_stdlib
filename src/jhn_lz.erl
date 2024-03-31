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
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_lz).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').


%%
%% rfc7932 Brotli Compressed Data Format
%% rfc1950 ZLIB Compressed Data Format Specification version 3.3
%% rfc1951 DEFLATE Compressed Data Format Specification version 1.3
%% rfc1952 GZIP file format specification version 4.3
%%
%% Zstd rfc8878.txt Zstandard Compression and the 'application/zstd' Media Type
%%
%%
%% LZ4 https://github.com/lz4/lz4
%%
%% LZO http://www.infradead.org/~mchehab/kernel_docs/staging/lzo.html
%% LZO-RLE
%%
%% LZMA https://www.7-zip.org/sdk.html,
%%      https://en.wikipedia.org/wiki/Lempel-Ziv-Markov_chain_algorithm

%% Library functions.

%% sNaPpY
-export([snappy_compress/1, snappy_compress/2,
         %% lz77_init/0, lz77_cont/2, lz77_end/1, lz77_end/2,
         snappy_uncompress/1, snappy_uncompress/2
        ]).

%% LZ77
-export([lz77_compress/1, lz77_compress/2,
         %% lz77_init/0, lz77_cont/2, lz77_end/1, lz77_end/2,
         lz77_uncompress/1
        ]).

-export([lz77d_compress/1, lz77d_compress/2
         %% lz77d_init/0, lz77d_cont/2, lz77d_end/1, lz77d_end/2,
        ]).

%% LZ78
-export([lz78_compress/1,
         lz78_init/0
        ]).

%% LZW
-export([lzw_compress/1, lzw_compress/2,
         lzw_init/0, lzw_init/1
        ]).

%% LZ78/LZW
-export([lz78w_cont/2,
         lz78w_end/1, lz78w_end/2,
         lz78w_uncompress/1, lz78w_uncompress/2
        ]).

%% Defines.

%% LZ77
-define(BYTE, 8/integer).

%% LZW
-define(W_DICT,
        #{<<155>> => 155, <<"G">> => 71, <<142>> => 142,
          <<"¢">> => 162, <<"ç">> => 231, <<6>> => 6,
          <<"\n">> => 10, <<18>> => 18, <<150>> => 150, <<21>> => 21,
          <<"ü">> => 252, <<"W">> => 87, <<"Ö">> => 214, <<"é">> => 233,
          <<144>> => 144, <<"Î">> => 206, <<"#">> => 35, <<"Ç">> => 199,
          <<"1">> => 49, <<"î">> => 238, <<"ï">> => 239, <<"]">> => 93,
          <<"Ñ">> => 209, <<"w">> => 119, <<"c">> => 99, <<137>> => 137,
          <<134>> => 134, <<140>> => 140, <<"­">> => 173, <<146>> => 146,
          <<131>> => 131, <<"ä">> => 228, <<" ">> => 160, <<1>> => 1,
          <<"C">> => 67, <<15>> => 15, <<133>> => 133, <<"O">> => 79,
          <<"Á">> => 193, <<"ß">> => 223, <<145>> => 145, <<"X">> => 88,
          <<"^">> => 94, <<"%">> => 37, <<"\v">> => 11, <<"L">> => 76,
          <<30>> => 30, <<"¬">> => 172, <<"+">> => 43, <<"T">> => 84,
          <<2>> => 2, <<5>> => 5, <<"È">> => 200, <<"\t">> => 9,
          <<"~">> => 126, <<"É">> => 201, <<"J">> => 74, <<"$">> => 36,
          <<"{">> => 123, <<"ð">> => 240, <<147>> => 147, <<130>> => 130,
          <<"\b">> => 8, <<"ê">> => 234, <<"¥">> => 165, <<"@">> => 64,
          <<"¡">> => 161, <<23>> => 23, <<"u">> => 117, <<31>> => 31,
          <<"Y">> => 89, <<"V">> => 86, <<"h">> => 104, <<")">> => 41,
          <<"Û">> => 219, <<"ÿ">> => 255, <<"6">> => 54, <<"`">> => 96,
          <<"±">> => 177, <<"Ä">> => 196, <<"*">> => 42, <<"|">> => 124,
          <<154>> => 154, <<"B">> => 66, <<"¾">> => 190, <<"ó">> => 243,
          <<"¤">> => 164, <<"å">> => 229, <<"y">> => 121, <<"º">> => 186,
          <<19>> => 19, <<"f">> => 102, <<159>> => 159, <<"®">> => 174,
          <<"}">> => 125, <<"¨">> => 168, <<"e">> => 101, <<149>> => 149,
          <<"Ó">> => 211, <<"k">> => 107, <<"b">> => 98, <<"Ô">> => 212,
          <<136>> => 136, <<"=">> => 61, <<"°">> => 176, <<20>> => 20,
          <<"l">> => 108, <<",">> => 44, <<143>> => 143, <<".">> => 46,
          <<"÷">> => 247, <<"µ">> => 181, <<22>> => 22, <<"\"">> => 34,
          <<"ù">> => 249, <<"¸">> => 184, <<"ú">> => 250, <<"_">> => 95,
          <<156>> => 156, <<"\\">> => 92, <<"õ">> => 245, <<16>> => 16,
          <<"z">> => 122, <<3>> => 3, <<"t">> => 116, <<"D">> => 68,
          <<">">> => 62, <<"o">> => 111, <<"Þ">> => 222, <<"I">> => 73,
          <<"Ü">> => 220, <<"ë">> => 235, <<"v">> => 118, <<"\r">> => 13,
          <<"x">> => 120, <<"Ý">> => 221, <<4>> => 4, <<"ã">> => 227,
          <<"!">> => 33, <<"´">> => 180, <<14>> => 14, <<"\f">> => 12,
          <<"'">> => 39, <<"N">> => 78, <<17>> => 17, <<"Ã">> => 195,
          <<"S">> => 83, <<";">> => 59, <<"2">> => 50, <<158>> => 158,
          <<29>> => 29, <<"ô">> => 244, <<"û">> => 251, <<"¦">> => 166,
          <<"ø">> => 248, <<28>> => 28, <<"U">> => 85, <<"¹">> => 185,
          <<"<">> => 60, <<"0">> => 48, <<"à">> => 224, <<"¿">> => 191,
          <<148>> => 148, <<151>> => 151, <<"Ì">> => 204, <<153>> => 153,
          <<"7">> => 55, <<"9">> => 57, <<"Í">> => 205, <<157>> => 157,
          <<"§">> => 167, <<"R">> => 82, <<"3">> => 51, <<"Ï">> => 207,
          <<"/">> => 47, <<"5">> => 53, <<129>> => 129, <<"ª">> => 170,
          <<127>> => 127, <<"8">> => 56, <<"?">> => 63, <<"p">> => 112,
          <<"Æ">> => 198, <<128>> => 128, <<"»">> => 187, <<"æ">> => 230,
          <<"j">> => 106, <<"ò">> => 242, <<141>> => 141, <<"Ð">> => 208,
          <<"·">> => 183, <<"è">> => 232, <<135>> => 135, <<152>> => 152,
          <<"Õ">> => 213, <<"ñ">> => 241, <<"á">> => 225, <<132>> => 132,
          <<"Q">> => 81, <<"þ">> => 254, <<"F">> => 70, <<":">> => 58,
          <<"d">> => 100, <<138>> => 138, <<"4">> => 52, <<"í">> => 237,
          <<"Ë">> => 203, <<"â">> => 226, <<"(">> => 40, <<"M">> => 77,
          <<"r">> => 114, <<"Ú">> => 218, <<"q">> => 113, <<"Ø">> => 216,
          <<"[">> => 91, <<"©">> => 169, <<"H">> => 72, <<139>> => 139,
          <<"³">> => 179, <<"Â">> => 194, <<" ">> => 32, <<"×">> => 215,
          <<25>> => 25, <<7>> => 7, <<"«">> => 171, <<"A">> => 65,
          <<"ì">> => 236, <<"K">> => 75, <<"£">> => 163, <<"¯">> => 175,
          <<26>> => 26, <<"ý">> => 253, <<"&">> => 38, <<"Ù">> => 217,
          <<"Z">> => 90, <<"²">> => 178, <<"m">> => 109, <<24>> => 24,
          <<"-">> => 45, <<"n">> => 110, <<"E">> => 69, <<"½">> => 189,
          <<"s">> => 115, <<"a">> => 97, <<"À">> => 192, <<"Ò">> => 210,
          <<"¼">> => 188, <<"g">> => 103, <<"ö">> => 246, <<"i">> => 105,
          <<"¶">> => 182, <<"Ê">> => 202, <<"Å">> => 197, <<"P">> => 80,
          <<"\e">> => 27}).

%% LZ78 and LZW
-define(W_POWER_TABLE,
        [{4, 16}, {5, 32}, {6, 64}, {7, 128}, {8, 256},
         {9, 512}, {10, 1024}, {11,2048}, {12, 4096}, {13, 8192}, {14, 16384},
         {15, 32768}, {16, 65536}, {17, 131072}, {18, 262144}, {19, 524288},
         {20, 1048576}, {21, 2097152}, {22, 4194304}, {8388608, 23},
         {24, 16777216}, {25, 33554432}, {26, 67108864}, {27, 134217728},
         {28, 268435456}, {29, 536870912}, {30, 1073741824}, {31, 2147483648}]).

%% Records

%% Snappy
-record(snappy, {type      = block  :: snappy_type(),
                 return    = iolist :: return_type()
                }).

%% LZ77
-record(s77, {search    = 16#FFF :: non_neg_integer(),
              lookahead = 16#F   :: non_neg_integer(),
              min_match = 1      :: 1..8,
              acc       = []     :: [match()],
              return    = raw    :: return_type()
             }).

-record(lz77c, {matches  :: [match()],
                pos_bits :: non_neg_integer(),
                len_bits :: non_neg_integer()
               }).

%% LZ78 and LZW
-record(s78w, {buf       = <<>>      :: binary(),
               buf_code  = undefined :: code() | undefined,
               dict      = #{}       :: map(),
               max_code  = 0         :: code(),
               acc       = []        :: [code()],
               stack     = []        :: stack(),
               cont      = true      :: boolean(),
               return    = raw       :: return_type()
              }).

-record(lz78w_stream, {state :: #s78w{}}).

%% sNaPpY
-type snappy_block() :: binary().
-type snappy_frame() :: binary().

-type snappy_type() :: block | frame.

%% Types

%% LZ77

-type lz77_compressed() :: #lz77c{}.

-type match() :: {non_neg_integer(), non_neg_integer(), byte() | -1}.


%% LZ78/LZW

-type code() :: byte().

-type stack() :: list(data()).

-type lz78w_stream() :: #lz78w_stream{}.

-type lz78w_compressed() :: {bitstring(), non_neg_integer(), map()}.

%% LZ77/LZ78/LZW

-type opt() :: return_type() |
               {search_length, non_neg_integer()} |
               {lookahead_length, non_neg_integer()} |
               {min_match, 1..4} |
               {type, snappy_type()}.

-type data() :: binary() | [data()].

-type return_type() :: raw | iolist | binary.

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec snappy_compress(data()) -> snappy_block().
%%--------------------------------------------------------------------
snappy_compress(Data) -> snappy_compress(Data, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec snappy_compress(data(), [opt()]) ->
          iolist() | snappy_block() | snappy_frame().
%%--------------------------------------------------------------------
snappy_compress(Data, Opts) ->
    #snappy{type = block, return = Return} = parse_opts(Opts, #snappy{}),
    LZ77Opts = [{return_type, raw} | delete_opt(return_type, Opts)],
    #lz77c{matches = Matches} = lz77d_compress(Data, LZ77Opts),
    {Literal, Matches1} = gather_literal(Matches, 0, <<>>),
    Block = gather(Matches1, [Literal, varint(byte_size(Data))]),
    case Return of
        iolist -> Block;
        binary -> iolist_to_binary(Block)
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec snappy_uncompress(snappy_block() | snappy_frame()) -> binary().
%%--------------------------------------------------------------------
snappy_uncompress(Frame) -> snappy_uncompress(Frame, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec snappy_uncompress(snappy_block() | snappy_frame(), [opt()]) -> binary().
%%--------------------------------------------------------------------
snappy_uncompress(Block, _Opts) ->
    snappy_uncompress_block(snappy_drop_varint(Block), <<>>).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz77_compress(data()) -> lz77_compressed().
%%--------------------------------------------------------------------
lz77_compress(Data) -> lz77_compress(Data, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz77_compress(data(), [opt()]) -> lz77_compressed().
%%--------------------------------------------------------------------
lz77_compress(Data, Options) ->
    Acc = lz77_compress(Data, 0, byte_size(Data), parse_opts(Options, #s77{})),
    {RevAcc, PMax, LMax} =
        lists:foldl(fun(H = {Pos, Len, _}, {A1, PMax, LMax}) when Pos > PMax,
                                                                  Len > LMax ->
                            {[H | A1], Pos, Len};
                       (H = {Pos, _, _}, {A1, PMax, LMax}) when Pos > PMax ->
                            {[H | A1], Pos, LMax};
                       (H = {_, Len, _}, {A1, PMax, LMax}) when Len > LMax ->
                            {[H | A1], PMax, Len};
                       (H, {A1, PMax, LMax}) ->
                            {[H | A1], PMax, LMax}
                    end,
                    {[], 0, 0},
                    Acc),
    PBits = code_bits(PMax, ?W_POWER_TABLE),
    LBits = code_bits(LMax, ?W_POWER_TABLE),
   #lz77c{matches = RevAcc, pos_bits = PBits, len_bits = LBits}.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz77_uncompress(lz77_compressed()) -> binary().
%%--------------------------------------------------------------------
lz77_uncompress(#lz77c{matches = Matches}) -> lz77_uncompress(Matches, <<>>).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz77d_compress(data()) -> lz77_compressed().
%%--------------------------------------------------------------------
lz77d_compress(Data) -> lz77d_compress(Data, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz77d_compress(data(), [opt()]) -> lz77_compressed().
%%--------------------------------------------------------------------
lz77d_compress(Data, Options) ->
    Acc = lz77d_compress(Data,
                         0,
                         byte_size(Data),
                         #{},
                         parse_opts(Options, #s77{min_match = 4})),
    {RevAcc, PMax, LMax} =
        lists:foldl(fun(H = {Pos, Len, _}, {A1, PMax, LMax}) when Pos > PMax,
                                                                  Len > LMax ->
                            {[H | A1], Pos, Len};
                       (H = {Pos, _, _}, {A1, PMax, LMax}) when Pos > PMax ->
                            {[H | A1], Pos, LMax};
                       (H = {_, Len, _}, {A1, PMax, LMax}) when Len > LMax ->
                            {[H | A1], PMax, Len};
                       (H, {A1, PMax, LMax}) ->
                            {[H | A1], PMax, LMax}
                    end,
                    {[], 0, 0},
                    Acc),
    PBits = code_bits(PMax, ?W_POWER_TABLE),
    LBits = code_bits(LMax, ?W_POWER_TABLE),
   #lz77c{matches = RevAcc, pos_bits = PBits, len_bits = LBits}.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz78_compress(data()) -> lz78w_compressed().
%%--------------------------------------------------------------------
lz78_compress(Data) -> lz78w_end(Data, lz78_init()).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz78_init() -> lz78w_stream().
%%--------------------------------------------------------------------
lz78_init() -> #lz78w_stream{state = #s78w{}}.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lzw_compress(data()) -> lz78w_compressed().
%%--------------------------------------------------------------------
lzw_compress(Data) -> lz78w_end(Data, lzw_init()).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lzw_compress(data(), map()) -> lz78w_compressed().
%%--------------------------------------------------------------------
lzw_compress(Data, Dict) -> lz78w_end(Data, lzw_init(Dict)).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lzw_init() -> lz78w_stream().
%%--------------------------------------------------------------------
lzw_init() ->
    #lz78w_stream{state = #s78w{dict = ?W_DICT,max_code = maps:size(?W_DICT)}}.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lzw_init(map()) -> lz78w_stream().
%%--------------------------------------------------------------------
lzw_init(Dict) ->
    <<Max:?BYTE>> =
        maps:fold(fun(K, _, A) when K > A -> K; (_, _, A) -> A end, 0, Dict),
    #lz78w_stream{state = #s78w{dict = Dict, max_code = Max}}.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz78w_cont(data(), lz78w_stream()) -> lz78w_stream().
%%--------------------------------------------------------------------
lz78w_cont(Data, #lz78w_stream{state = State}) -> lz78w_compress(Data, State).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz78w_end(lz78w_stream()) -> lz78w_compressed().
%%--------------------------------------------------------------------
lz78w_end(Stream) -> lz78w_end(<<>>, Stream).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz78w_end(data(), lz78w_stream()) -> lz78w_compressed().
%%--------------------------------------------------------------------
lz78w_end(Data, #lz78w_stream{state = State}) ->
    lz78w_compress(Data, State#s78w{cont = false}).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz78w_uncompress(lz78w_compressed()) -> iolist().
%%--------------------------------------------------------------------
lz78w_uncompress({C, Bits, Dict}) ->
    Dict1 = maps:fold(fun(K, V, A) -> maps:put(V, K, A) end, #{}, Dict),
    [maps:get(K, Dict1) || <<K:Bits/integer>> <= C].

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz78w_uncompress(lz78w_compressed(), [opt()]) -> binary().
%%--------------------------------------------------------------------
lz78w_uncompress(Compressed, [iolist]) -> lz78w_uncompress(Compressed);
lz78w_uncompress({C, Bits, Dict}, [binary]) ->
    RDict = maps:fold(fun(K, V, A) -> maps:put(V, K, A) end, #{}, Dict),
    << (maps:get(K, RDict)) || <<K:Bits/integer>> <= C >>.

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% --------------------------------------------------------------------
%% sNaPpY
%% --------------------------------------------------------------------

parse_snappy_opt({type, T}, State) -> State#snappy{type = T};
parse_snappy_opt({search_length, _}, State) -> State;
parse_snappy_opt({lookahead_length, L}, State) when L =< 64 -> State;
parse_snappy_opt({min_match, L}, State) when L > 3, L < 9 -> State;
parse_snappy_opt({return_type, R}, State) -> State#snappy{return = R}.

varint(I) when I =< 127 -> <<I>>;
varint(I) -> <<1:1, (I band 127):7, (varint(I bsr 7))/binary>>.

gather([], Acc) -> lists:reverse(Acc);
gather(Matches = [{0, 0, _} | _], Acc) ->
    {Literal, Matches1} = gather_literal(Matches, 0, <<>>),
    gather(Matches1, [Literal | Acc]);
gather(Matches, Acc) ->
    case gather_copy(Matches) of
        {Copy, -1, Matches1} -> gather(Matches1, [Copy | Acc]);
        {Copy, C, Matches1} ->
            {Literal, Matches2} = gather_literal(Matches1, 1, <<C:?BYTE>>),
            gather(Matches2, [Literal, Copy | Acc])
    end.

gather_literal([{0, 0, -1} | Matches], Len, Acc) ->
    gather_literal(Matches, Len, Acc);
gather_literal([{0, 0, C} | Matches], Len, Acc) ->
    gather_literal(Matches, Len + 1, <<Acc/binary, C:?BYTE>>);
gather_literal(Matches, Len, Acc) ->
    {Tag, L} =
        case Len - 1 of
            Len1 when Len1 < 60 -> {Len1, <<>>};
            Len1 when Len1 < 256 -> {60, <<Len1:8/integer>>};
            Len1 when Len1 < 65536 -> {61, <<Len1:16/integer-little>>};
            Len1 when Len1 < 16777216 -> {62, <<Len1:24/integer-little>>};
            Len1 when Len1 < 4294967296 -> {63, <<Len1:32/integer-little>>}
        end,
    {<<Tag:6/integer, 0:2/integer, L/binary, Acc/binary>>, Matches}.

gather_copy([{Pos, Len, C} | Matches]) when Len > 3, Len < 12, Pos < 2048 ->
    Len4 = Len - 4,
    <<PosU:3/integer, PosL:8/integer>> = <<Pos:11/integer>>,
    {<<PosU:3/integer, Len4:3, 1:2, PosL:8/integer>>, C, Matches};
gather_copy([{Pos, Len, C} | Matches]) when Pos < 65536 ->
    Len1 = Len - 1,
    {<<Len1:6, 2:2, Pos:16/integer-little>>, C, Matches};
gather_copy([{Pos, Len, C} | Matches]) ->
    Len1 = Len - 1,
    {<<Len1:6, 3:2, Pos:32/integer-little>>, C, Matches}.

snappy_drop_varint(<<1:1, _:7, T/binary>>) -> snappy_drop_varint(T);
snappy_drop_varint(<<0:1, _:7, T/binary>>) -> T.

snappy_uncompress_block(<<>>, Acc) -> Acc;
snappy_uncompress_block(<<S:6/integer, 0:2/integer, T/binary>>, Acc) ->
    {Len1, T2} = case {S, T} of
                    {60, <<Len:8/integer, T1/binary>>} -> {Len, T1};
                    {61, <<Len:16/integer-little, T1/binary>>} -> {Len, T1};
                    {62, <<Len:24/integer-little, T1/binary>>} -> {Len, T1};
                    {63, <<Len:32/integer-little, T1/binary>>} -> {Len, T1};
                    {Len, T1} -> {Len, T1}
                end,
    <<Lit:(Len1 + 1)/binary, T3/binary>> = T2,
    snappy_uncompress_block(T3, <<Acc/binary, Lit/binary>>);
snappy_uncompress_block(<<PosU:3/integer,Len4:3,1:2,PosL:8/integer,T/binary>>,
                        Acc) ->
    <<Pos:11/integer>> = <<PosU:3/integer, PosL:8/integer>>,
    snappy_uncompress_copy(Pos, Len4 + 4, T, Acc);
snappy_uncompress_block(<<Len1:6, 2:2, Pos:16/integer-little,T/binary>>, Acc) ->
    snappy_uncompress_copy(Pos, Len1 + 1, T, Acc);
snappy_uncompress_block(<<Len1:6, 3:2, Pos:32/integer-little,T/binary>>, Acc) ->
    snappy_uncompress_copy(Pos, Len1 + 1, T, Acc).

snappy_uncompress_copy(Pos, Len, T, Acc) when Pos > Len ->
    C1 = binary_part(Acc, byte_size(Acc) - Pos, Len),
    snappy_uncompress_block(T, <<Acc/binary, C1/binary>>);
snappy_uncompress_copy(Pos, Len, T, Acc) ->
    Start = byte_size(Acc) - Pos,
    C1 = binary:copy(binary_part(Acc, Start, Pos), Len div Pos),
    C2 = binary:part(Acc, Start, Len rem Pos),
    snappy_uncompress_block(T, <<Acc/binary, C1/binary, C2/binary>>).

%% --------------------------------------------------------------------
%% LZ77
%% --------------------------------------------------------------------

parse_s77_opt({search_length, L}, State) -> State#s77{search = L};
parse_s77_opt({lookahead_length, L}, State) -> State#s77{lookahead = L};
parse_s77_opt({min_match, L}, State) -> State#s77{min_match = L};
parse_s77_opt({return_type, R}, State) -> State#s77{return = R}.

lz77_compress(_, DLen, DLen, #s77{acc = Acc}) -> Acc;
lz77_compress(Data, Pos, DLen, State) ->
    #s77{lookahead = L, search = S, acc = Acc, min_match = M} = State,
    LL = min(DLen - Pos, L),
    Lookahead = [C | _] = binary_to_list(binary:part(Data, Pos, LL)),
    case LL of
        1 -> [{0, 0, binary:last(Data)} | Acc];
        LL when LL < M ->
            lists:reverse([{0, 0, X} || <<X:?BYTE>> <= Lookahead]) ++ Acc;
        _ ->
            Start = case Pos < S of
                        true -> 0;
                        false -> Pos - S
                    end,
            Match = {_, MLen, C1} =
                match(Data, Lookahead, Start, Pos, Pos - Start, {0, 0, C}),
            Pos1 = case C1 of
                       -1  -> Pos + MLen;
                       _ -> Pos + MLen + 1
                   end,
            lz77_compress(Data, Pos1, DLen, State#s77{acc = [Match | Acc]})
    end.

match(_, _, End, End, _, Match) -> Match;
match(Data, L = [C | T], Curr, End, Pos, Match = {_, MLen, _}) ->
    Curr1 = Curr + 1,
    case binary:at(Data, Curr) of
        C ->
            Match1 = case match_span(Data, T, Curr1, End, 1, [C]) of
                         {Len, C1} when Len >= MLen -> {Pos, Len, C1};
                         _ -> Match
                     end,
            match(Data, L, Curr1, End, Pos - 1, Match1);
        _ ->
            match(Data, L, Curr1, End, Pos - 1, Match)
    end.

match_span(_, [C], _, _, Len, _) -> {Len, C};
match_span(_, [C | _], End, End, Len, []) -> {Len, C};
match_span(_, L, End, End, Len, M) ->
    Match = lists:reverse(M),
    match_run(Match, Match, L, Len);
match_span(Data, [C | T], Curr, End, Len, Match) ->
    case binary:at(Data, Curr) of
        C -> match_span(Data, T, Curr + 1, End, Len + 1, [C | Match]);
        _ -> {Len, C}
    end.

match_run([], Match, L, Len) -> match_run(Match, Match, L, Len);
match_run(_, _, [], Len) -> {Len, -1};
match_run([C | M], Match, [C | L], Len) -> match_run(M, Match, L, Len + 1);
match_run(_, _, [C | _], Len) -> {Len, C}.

lz77_uncompress([], UnCompressed) -> UnCompressed;
lz77_uncompress([{0, 0, -1} | T], U) -> lz77_uncompress(T, U);
lz77_uncompress([{0, 0, C} | T], U) -> lz77_uncompress(T, <<U/binary,C:?BYTE>>);
lz77_uncompress([{Pos, Len, C} | T], U) ->
    X = case Pos > Len of
            true -> binary_part(U, byte_size(U) - Pos, Len);
            false ->
                Start = byte_size(U) - Pos,
                C1 = binary:copy(binary_part(U, Start, Pos), Len div Pos),
                C2 = binary:part(U, Start, Len rem Pos),
                <<C1/binary, C2/binary>>
        end,
    case C of
        -1 -> lz77_uncompress(T, <<U/binary, X:Len/binary>>);
        _ -> lz77_uncompress(T, <<U/binary, X:Len/binary, C:?BYTE>>)
    end.

%% --------------------------------------------------------------------
%% LZ77d
%% --------------------------------------------------------------------

lz77d_compress(_, DLen, DLen, _, #s77{acc = Acc}) -> Acc;
lz77d_compress(Data, Pos, DLen, Dict, State) ->
    #s77{lookahead = L, acc = Acc, min_match = M} = State,
    LL = min(DLen - Pos, L),
    Lookahead = <<C:?BYTE, _/binary>> = binary:part(Data, Pos, LL),
    case LL of
        1 -> [{0, 0, C} | Acc];
        LL when LL < M ->
            lists:reverse([{0, 0, X} || <<X:?BYTE>> <= Lookahead]) ++ Acc;
        _ ->
            <<F:M/binary, T/binary>> = Lookahead,
            {Dict1, Match} = {_, {_, MLen, C1}} = matchd(F, T, Pos, M, C, Dict),
            Pos1 = case C1 of
                       -1  -> Pos + MLen;
                       _ -> Pos + MLen + 1
                   end,
            State1 = State#s77{acc = [Match | Acc]},
            lz77d_compress(Data, Pos1, DLen, Dict1, State1)
    end.

matchd(F, <<>>, Pos, M, C, Dict) ->
    case maps:get(F, Dict, undefined) of
        undefined -> {maps:put(F, Pos, Dict), {0, 0, C}};
        MPos -> {Dict, {Pos - MPos, M, -1}}
    end;
matchd(F, <<H:?BYTE, T/binary>>, Pos, M, C, Dict) ->
    case maps:get(F, Dict, undefined) of
        undefined -> {maps:put(F, Pos, Dict), {0, 0, C}};
        MPos -> matchd1(<<F/binary, H:?BYTE>>, T, Pos, M, MPos, H, Dict)
    end.

matchd1(F, <<>>, Pos, Len, MPos, H, Dict) ->
    case maps:get(F, Dict, undefined) of
        undefined -> {maps:put(F, Pos, Dict), {Pos - MPos, Len, H}};
        MPos1 -> {Dict, {Pos - MPos1, Len + 1, -1}}
    end;
matchd1(F, <<H1:?BYTE, T/binary>>, Pos, Len, MPos, H, Dict) ->
    case maps:get(F, Dict, undefined) of
        undefined -> {maps:put(F, Pos, Dict), {Pos - MPos, Len, H}};
        %% %% MPos1 when MPos1 + Len = Pos
        MPos1 ->
            matchd1(<<F/binary, H1:?BYTE>>, T, Pos, Len + 1, MPos1, H1, Dict)
    end.

%% matchd_run([], Match, L, Len) -> matchd_run(Match, Match, L, Len);
%% matchd_run(_, _, [], Len) -> {Len, -1};
%% matchd_run([C | M], Match, [C | L], Len) -> matchd_run(M, Match, L, Len + 1);
%% matchd_run(_, _, [C | _], Len) -> {Len, C}.

%% --------------------------------------------------------------------
%% LZ78/LZW
%% --------------------------------------------------------------------
lz78w_compress([], State = #s78w{stack = [], cont=false}) -> lz78w_stop(State);
lz78w_compress(<<>>, State = #s78w{stack=[], cont=false}) -> lz78w_stop(State);
lz78w_compress([], State = #s78w{stack = []}) -> #lz78w_stream{state = State};
lz78w_compress(<<>>, State = #s78w{stack = []}) -> #lz78w_stream{state=State};
lz78w_compress([], State = #s78w{stack = [H | T]}) ->
    lz78w_compress(H, State#s78w{stack = T});
lz78w_compress(<<>>, State = #s78w{stack = [H | T]}) ->
    lz78w_compress(H, State#s78w{stack = T});
lz78w_compress([H | T], St = #s78w{stack = S}) ->
    lz78w_compress(H, St#s78w{stack = [T | S]});
lz78w_compress(I = <<H:?BYTE, T/binary>>, St) ->
     lz78w_next(I, H, T, St).

lz78w_next(I, H, T, St = #s78w{buf = Buf, dict = Dict}) ->
    Buf1 = <<Buf/binary, H/integer>>,
    case maps:get(Buf1, Dict, undefined) of
        undefined ->
            #s78w{max_code = Max, buf_code = BufCode, acc = Acc} = St,
            Max1 = Max + 1,
            Acc1 = case Buf of
                       <<>> -> Acc;
                       _ -> [BufCode | Acc]
                   end,
            St1 = St#s78w{buf = <<>>,
                          dict = maps:put(Buf1, Max1, Dict),
                          buf_code = undefined,
                          max_code = Max1,
                          acc = Acc1},
            lz78w_compress(I, St1);
        Buf1Code ->
            lz78w_compress(T, St#s78w{buf = Buf1, buf_code = Buf1Code})
    end.

lz78w_stop(St) ->
    #s78w{buf = Buf, buf_code = BCode,dict = Dict,max_code = Max, acc=Acc} = St,
    Acc1 = case Buf of
               <<>> -> Acc;
               _ -> [BCode | Acc]
           end,
    Bits = code_bits(Max, ?W_POWER_TABLE),
    C = << <<Cd:Bits/integer>> || Cd <- lists:reverse(Acc1) >>,
    {C, Bits, Dict}.

%% --------------------------------------------------------------------
%% LZ77/LZ78/LZW
%% --------------------------------------------------------------------
code_bits(Code, [{_, Max} | T]) when Code > Max -> code_bits(Code, T);
code_bits(_, [{Bits, _} | _]) -> Bits.

%%
parse_opts(Opts, Rec = #s77{}) -> lists:foldl(fun parse_s77_opt/2, Rec, Opts);
parse_opts(Opts, Rec = #snappy{}) ->
    lists:foldl(fun parse_snappy_opt/2, Rec, Opts).

delete_opt(_, []) -> [];
delete_opt(Opt, [{Opt, _} | T]) -> T;
delete_opt(Opt, [H | T]) -> [H | delete_opt(Opt, T)].

