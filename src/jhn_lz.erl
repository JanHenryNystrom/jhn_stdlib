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
%% rfc1951 DEFLATE Compressed Data Format Specification version 1.3
%%
%%
%%
%%
%%
%%
%%
%%
%%

%% LZ78
-export([lz77_compress/1, lz77_compress/2,
         %% lz77_init/0, lz77_cont/2, lz77_end/1, lz77_end/2,
         lz77_uncompress/1
        ]).

%% Library functions.

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

%% LZ78 and LZW
-record(state, {buf       = <<>>      :: binary(),
                buf_code  = undefined :: code() | undefined,
                dict      = #{}       :: map(),
                max_code  = 0         :: code(),
                acc       = []        :: [code()],
                stack     = []        :: stack(),
                cont      = true      :: boolean()
               }).

-record(lz78w_stream, {state :: #state{}}).

-record(s77, {buf :: binary() | binary(),
              search = 16#FFF :: non_neg_integer(),
              lookahead = 16#F :: non_neg_integer(),
              acc = []:: [match()]
             }).

%% Types
-type opt() :: iolist | binary.

-type code() :: non_neg_integer().

-type stack() :: list(iodata() | '$end').

-type lz78w_stream() :: #lz78w_stream{}.

-type lz78w_compressed() :: {binary(), non_neg_integer(), map()}.

-type lz77_compressed() :: {binary(), non_neg_integer()}.

-type match() :: {non_neg_integer(), non_neg_integer(),non_neg_integer()}.

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz77_compress(iodata()) -> lz77_compressed().
%%--------------------------------------------------------------------
lz77_compress(Data) -> lz77_compress(Data, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz77_compress(iodata(), [opt()]) -> lz77_compressed().
%%--------------------------------------------------------------------
lz77_compress(Data = <<C:?BYTE, T/binary>>, Options) ->
    State = #s77{buf = Data, acc = [{0, 0, C}]},
    State1 = #s77{lookahead = L} = parse_opts(Options, State),
    Acc = lz77_compress1(T, 1, State1),
    Bits = code_bits(L, ?W_POWER_TABLE),
    {lists:reverse(Acc),
%%    {iolist_to_binary([<<Pos:Bits/integer, Len:Bits/integer, Byte:?BYTE>> ||
%%                          {Pos, Len, Byte} <- lists:reverse(Acc)]),
     Bits}.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz77_uncompress(lz77_compressed()) -> binary().
%%--------------------------------------------------------------------
lz77_uncompress({Compressed, Bits}) -> lz77_uncompress(Compressed, Bits, <<>>).

lz77_uncompress(<<>>, _, UnCompressed) -> UnCompressed;
lz77_uncompress(Compressed, Bits, Un) ->
    case Compressed of
        <<0:Bits/integer, 0:Bits/integer, C:?BYTE, T/binary>> ->
            lz77_uncompress(T, Bits, <<Un/binary, C:?BYTE>>);
        <<Pos:Bits/integer, Len:Bits/integer, Byte:?BYTE, T/binary>> ->
            X = erlang:binary_part(Un, {byte_size(Un) - Pos,Len}),
            lz77_uncompress(T, Bits, <<Un/binary, X:Len/binary, Byte:?BYTE>>)
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz78_compress(iodata()) -> lz78w_compressed().
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
lz78_init() -> #lz78w_stream{state = #state{}}.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lzw_compress(iodata()) -> lz78w_compressed().
%%--------------------------------------------------------------------
lzw_compress(Data) -> lz78w_end(Data, lzw_init()).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lzw_compress(iodata(), map()) -> lz78w_compressed().
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
    #lz78w_stream{state = #state{dict = ?W_DICT,max_code = maps:size(?W_DICT)}}.

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
    #lz78w_stream{state = #state{dict = Dict, max_code = Max}}.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz78w_cont(iodata(), lz78w_stream()) -> lz78w_stream().
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
-spec lz78w_end(iodata(), lz78w_stream()) -> lz78w_compressed().
%%--------------------------------------------------------------------
lz78w_end(Data, #lz78w_stream{state = State}) ->
    lz78w_compress(Data, State#state{cont = false}).

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
%% LZ77
%% --------------------------------------------------------------------
parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt({search_length, L}, State) -> State#s77{search = L};
parse_opt({lookahead_length, L}, State) -> State#s77{lookahead = L}.

lz77_compress1(<<>>, _, #s77{acc = Acc}) -> Acc;
lz77_compress1(<<C:?BYTE>>, _, #s77{acc = Acc}) ->  [{0, 0, C} | Acc];
lz77_compress1(Data, P, State = #s77{search = P}) -> lz77_compress1(Data,State);
lz77_compress1(Data = <<C:?BYTE, _/binary>>, Pos, State) ->
    #s77{buf = Buf, lookahead = L, search = S, acc = Acc} = State,
    Lookahead = lookahead(Data, L),
    Match = {_, Len, _} =  match(Buf, Lookahead, min(Pos, S), {0, 0, C}),
    <<_:(Len + 1)/binary, Data1/binary>> = Data,
    <<_:(Len + 1)/binary, Buf1/binary>> = Buf,
    lz77_compress1(Data1,
                   Pos + Len + 1,
                   State#s77{buf = Buf1, acc = [Match | Acc]}).

lz77_compress1(<<>>, #s77{acc = Acc}) -> Acc;
lz77_compress1(<<C:?BYTE>>, #s77{acc = Acc}) ->  [{0, 0, C} | Acc];
lz77_compress1(Data = <<C:?BYTE, _/binary>>, State) ->
    #s77{buf = Buf, lookahead = L, search = S, acc = Acc} = State,
    Lookahead = lookahead(Data, L),
    Match = {_, Len, _} = match(Buf, Lookahead, S, {0, 0, C}),
    <<_:Len/binary, Data1/binary>> = Data,
    <<_:Len/binary, Buf1/binary>> = Buf,
    lz77_compress1(Data1, State#s77{buf = Buf1, acc = [Match | Acc]}).

lookahead(Data, L) ->
    case Data of
        <<Lookahead:L/binary, _/binary>> -> Lookahead;
        _ -> Data
    end.

match(_, _, 0, Match) -> Match;
match(<<>>, _, _, Match) -> Match;
match(_, <<_:?BYTE>>, _, Match) -> Match;
match(<<C:?BYTE, B/binary>>, <<C:?BYTE, L/binary>>, Pos, Match = {_, X, _}) ->
    Match1 = case match_span(B, L, 1) of
                 {Len, C1} when Len >= X -> {Pos, Len, C1};
                 _ ->
                     Match
             end,
    match(B, L, Pos + 1, Match1);
match(B, L, Pos, Match) ->
    match(B, L, Pos - 1, Match).

match_span(<<C:?BYTE, _/binary>>, <<C:?BYTE>>, Len) -> {Len, C};
match_span(<<C:?BYTE, B/binary>>, <<C:?BYTE, L/binary>>, Len) ->
    match_span(B, L, Len + 1);
match_span(_, <<C:?BYTE, _/binary>>, Len) ->
    {Len, C}.

%% --------------------------------------------------------------------
%% LZ78/LZW
%% --------------------------------------------------------------------
lz78w_compress([], State = #state{stack = [], cont=false}) -> lz78w_stop(State);
lz78w_compress(<<>>, State = #state{stack=[], cont=false}) -> lz78w_stop(State);
lz78w_compress([], State = #state{stack = []}) -> #lz78w_stream{state = State};
lz78w_compress(<<>>, State = #state{stack = []}) -> #lz78w_stream{state=State};
lz78w_compress(<<>>, State = #state{stack = [H | T]}) ->
    lz78w_compress(H, State#state{stack = T});
lz78w_compress([], State = #state{stack = [H | T]}) ->
    lz78w_compress(H, State#state{stack = T});
lz78w_compress([[] | T], St) -> lz78w_compress(T, St);
lz78w_compress([<<>> | T], St) -> lz78w_compress(T, St);
lz78w_compress([H  = [_ | _] | T], St = #state{stack = S}) ->
    lz78w_compress(H, St#state{stack = [T | S]});
lz78w_compress([H  = <<_/binary>> | T], St = #state{stack = S}) ->
    lz78w_compress(H, St#state{stack = [T | S]});
lz78w_compress(I = [H | T], St) -> 
     lz78w_next(I, H, T, St);
lz78w_compress(I = <<H:?BYTE, T/binary>>, St) ->
     lz78w_next(I, H, T, St).

lz78w_next(I, H, T, St = #state{buf=Buf,dict=Dict}) ->
    Buf1 = <<Buf/binary, H/integer>>,
    case maps:get(Buf1, Dict, undefined) of
        undefined ->
            #state{max_code = Max, buf_code = BufCode, acc = Acc} = St,
            Max1 = Max + 1,
            Acc1 = case Buf of
                       <<>> -> Acc;
                       _ -> [BufCode | Acc]
                   end,
            St1 = St#state{buf = <<>>,
                           dict = maps:put(Buf1, Max1, Dict),
                           buf_code = undefined,
                           max_code = Max1,
                           acc = Acc1},
            lz78w_compress(I, St1);
        Buf1Code ->
            lz78w_compress(T, St#state{buf = Buf1, buf_code = Buf1Code})
    end.

lz78w_stop(St) ->
    #state{buf = Buf,buf_code = BCode,dict = Dict,max_code = Max,acc=Acc} = St,
    Acc1 = case Buf of
               <<>> -> Acc;
               _ -> [BCode | Acc]
           end,
    Bits = code_bits(Max, ?W_POWER_TABLE),
    C = << <<Cd:Bits/integer>> || Cd <- lists:reverse(Acc1) >>,
    {C, Bits, Dict}.

code_bits(Code, [{_, Max} | T]) when Code > Max -> code_bits(Code, T);
code_bits(_, [{Bits, _} | _]) -> Bits.
