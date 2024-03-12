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
-export([ lz78w_cont/2,
          lz78w_end/1,
          lz78w_uncompress/1, lz78w_uncompress/2
        ]).

%% Defines.

%% W
-define(W_DICT, #{}).

%% LZ78 and LZW
-define(W_POWER_TABLE,
        [{9, 512}, {10, 1024}, {11,2048}, {12, 4096}, {13, 8192}, {14, 16384},
         {15, 32768}, {16, 65536}, {17, 131072}, {18, 262144}, {19, 524288},
         {20, 1048576}, {21, 2097152}, {22, 4194304}, {8388608, 23},
         {24, 16777216}, {25, 33554432}, {26, 67108864}, {27, 134217728},
         {28, 268435456}, {29, 536870912}, {30, 1073741824}, {31, 2147483648}]).

%% Records

%% LZ78 and LZW
-record(state, {buf   = <<>>      :: binary(),
                code  = undefined :: code() | undefined,
                dict  = #{}       :: map(),
                size  = 0         :: integer(),
                acc   = []        :: [code()],
                stack = []        :: stack(),
                cont  = true      :: boolean()
               }).

-record(lz78w_stream, {state :: #state{}}).

%% Types
-type opt() :: iolist | binary.

-type code() :: non_neg_integer().

-type stack() :: list(iodata() | '$end').

-type lz78w_stream() :: #lz78w_stream{}.

-type lz78w_compressed() :: {binary(), non_neg_integer(), map()}.

%% ===================================================================
%% Library functions.
%% ===================================================================

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
    #lz78w_stream{state = #state{dict = ?W_DICT, size = maps:size(?W_DICT)}}.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lzw_init(map()) -> lz78w_stream().
%%--------------------------------------------------------------------
lzw_init(Dict) ->
    #lz78w_stream{state = #state{dict = Dict, size = maps:size(Dict)}}.

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
lz78w_uncompress({C, Dict, CodeBits}) ->
    Dict1 = maps:fold(fun(K, V, A) -> maps:put(V, K, A) end, #{}, Dict),
    [maps:get(K, Dict1) || <<K:CodeBits/integer>> <= C].

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec lz78w_uncompress(lz78w_compressed(), [opt()]) -> binary().
%%--------------------------------------------------------------------
lz78w_uncompress(Compressed, [iolist]) -> lz78w_uncompress(Compressed);
lz78w_uncompress({C, Dict, CodeBits}, [binary]) ->
    RDict = maps:fold(fun(K, V, A) -> maps:put(V, K, A) end, #{}, Dict),
    << (maps:get(K, RDict)) || <<K:CodeBits/integer>> <= C >>.

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% --------------------------------------------------------------------
%% LZW
%% --------------------------------------------------------------------
lz78w_compress([], State = #state{stack = [], cont=false}) -> lz78w_end(State);
lz78w_compress(<<>>, State = #state{stack = [],cont=false}) -> lz78w_end(State);
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
lz78w_compress(I = [H | T], St = #state{buf=Buf,dict=Dict}) ->
    Buf1 = <<Buf/binary, H/integer>>,
    lz78w_next(maps:get(Buf1, Dict, undefined), I, T, Buf, Buf1, Dict, St);
lz78w_compress(I = <<H:8/integer, T/binary>>, St = #state{buf=Buf,dict=Dict}) ->
    Buf1 = <<Buf/binary, H/integer>>,
    lz78w_next(maps:get(Buf1, Dict, undefined), I, T, Buf, Buf1, Dict, St).

lz78w_next(undefined, I, _, Buf, Buf1, Dict, St) ->
    #state{size = Size, code = BufCode, acc = Acc} = St,
    Code1 = Size + 1,
    Acc1 = case I of
               Buf -> Acc;
               _ -> [BufCode | Acc]
           end,
    St1 = St#state{buf = <<>>,
                   dict = maps:put(Buf1, Code1, Dict),
                   code = undefined,
                   size = Code1,
                   acc = Acc1},
    lz78w_compress(I, St1);
lz78w_next(Buf1Code, _, T, _, Buf1, _, St) ->
    lz78w_compress(T, St#state{buf = Buf1, code = Buf1Code}).

lz78w_end(St) ->
    #state{buf = Buf, code = BCode, dict = Dict, size = Size, acc = Acc} = St,
    Acc1 = case Buf of
               <<>> -> Acc;
               _ -> [BCode | Acc]
           end,
    Bits = code_bits(Size, ?W_POWER_TABLE),
    C = << <<Code:Bits/integer>> || Code <- lists:reverse(Acc1) >>,
    {C, Bits, Dict}.

code_bits(Size, [{_, Max} | T]) when Size > Max-> code_bits(Size, T);
code_bits(_, [{Bits, _} | _]) -> Bits.
