%%==============================================================================
%% Copyright 2026 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%% @copyright (C) 2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_snappy).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%%
%%  https://github.com/google/snappy
%%  https://github.com/google/snappy/blob/main/framing_format.txt
%%  https://github.com/google/snappy/blob/main/format_description.txt
%%

%% Library functions
-export([compress/1, compress/2, pad/3, uncompress/1, uncompress/2]).

%% Defines.

-define(STREAM, 16#FF).
-define(COMPRESSED, 16#00).
-define(UNCOMPRESSED, 16#01).
-define(PAD, 16#FE).
-define(IDENTIFIER, ?STREAM, 6:24/integer-little, "sNaPpY").

-define(MAX_FRAME, 65536).
-define(CONST, 16#A282EAD8).
-define(MAX_COMPRESSED, 65536).

-define(MIN_MATCH, 8).

-define(CHECK, 32/integer-little).
-define(SIZE, 24/integer-little).
-define(BYTE, 8/integer).

%% Records

-record(state, {type      = block  :: type(),
                search    = 4095   :: 1024..65536,
                lookahead = 15     :: 4..64,
                acc       = []     :: [match()],
                return    = iolist :: return_type()
               }).

%% Types

-type block() :: binary().
-type frames() :: iodata().
-type iwa() :: iodata().

-type type() :: block | frame | iwa.

-type match() :: {non_neg_integer(), non_neg_integer(), byte() | -1}.

-type opt() :: return_type() | type() |
               {return_type, return_type()} | {type, type()} |
               {search_length, 1..65536} |
               {lookahead_length, 1..64} |
               {type, type()}.

-type data() :: binary() | [data()].

-type return_type() :: iolist | binary.

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec compress(data()) -> block().
%%--------------------------------------------------------------------
compress(Data) -> compress(Data, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec compress(data(), [opt()]) ->
          iolist() | block() | frames().
%%--------------------------------------------------------------------
compress(Data = <<_/binary>>, Opts) ->
    case parse_opts(Opts) of
        State = #state{type = block} ->
            Matches = compress_lz77d(Data, State),
            {Literal, Matches1} = gather_literal(Matches, 0, <<>>),
            return(gather(Matches1, [Literal, varint(byte_size(Data))]), State);
        State = #state{type = frame} ->
            return(compress_frame(Data, State, [<<?IDENTIFIER>>]), State);
        State = #state{type = iwa} ->
            return(compress_iwa(Data, State, []), State)
    end;
compress(Data, Opts) ->
    compress(collapse(Data, <<>>), Opts).


%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec pad(frames(), integer(), [opt()]) -> frames().
%%--------------------------------------------------------------------
pad(Frames, Size, Opts) when Size > 4 ->
    PSize = Size - 4,
    Frame = <<?PAD, PSize:?SIZE, 0:(PSize * 8)/integer>>,
    return([Frames, Frame], parse_opts(Opts)).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec uncompress(block() | frames()) -> binary().
%%--------------------------------------------------------------------
uncompress(Frame) -> uncompress(Frame, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec uncompress(block() | frames() | iwa() , [opt()]) ->
          iolist() | binary().
%%--------------------------------------------------------------------
uncompress(IoList = [_ | _], Opts) ->
    uncompress(iolist_to_binary(IoList), Opts);
uncompress(Data, Opts) ->
    case {parse_opts(Opts), Data} of
        {State, <<?IDENTIFIER, T/binary>>} ->
            return(uncompress_frames(T, []), State);
        {State = #state{type = iwa}, _} ->
            return(uncompress_iwa(Data, []), State);
        {#state{type = block}, _} ->
            uncompress_stream(Data)
    end.

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% --------------------------------------------------------------------
%% Compress
%% --------------------------------------------------------------------

compress_frame(Data, State, Acc) when byte_size(Data) =< ?MAX_FRAME ->
    Matches = compress_lz77d(Data, State),
    {Literal, Matches1} = gather_literal(Matches, 0, <<>>),
    Compressed = gather(Matches1, [Literal, varint(byte_size(Data))]),
    Max = trunc(byte_size(Data) * 0.98),
    Frame = select_frame(iolist_size(Compressed), Max, Compressed, Data),
    lists:reverse([Frame | Acc]);
compress_frame(<<H:?MAX_FRAME/binary, T/binary>>, State, Acc) ->
    Matches = compress_lz77d(H, State),
    {Literal, Matches1} = gather_literal(Matches, 0, <<>>),
    Compressed = gather(Matches1, [Literal, varint(byte_size(H))]),
    Size = iolist_size(Compressed),
    Frame = select_frame(Size, ?MAX_COMPRESSED, Compressed, H),
    compress_frame(T, State, [Frame | Acc]).

compress_iwa(Data, State, Acc) when byte_size(Data) =< ?MAX_FRAME ->
    Matches = compress_lz77d(Data, State),
    {Literal, Matches1} = gather_literal(Matches, 0, <<>>),
    Compressed = gather(Matches1, [Literal, varint(byte_size(Data))]),
    Frame = [<<?COMPRESSED, (iolist_size(Compressed) + 4):?SIZE>>, Compressed],
    lists:reverse([Frame | Acc]);
compress_iwa(<<H:?MAX_FRAME/binary, T/binary>>, State, Acc) ->
    Matches = compress_lz77d(H, State),
    {Literal, Matches1} = gather_literal(Matches, 0, <<>>),
    Compressed = gather(Matches1, [Literal, varint(byte_size(H))]),
    Frame = [<<?COMPRESSED, (iolist_size(Compressed) + 4):?SIZE>>, Compressed],
    compress_iwa(T, State, [Frame | Acc]).

select_frame(Size, Max, Compressed, Data) when Size < Max ->
    [<<?COMPRESSED,
       (iolist_size(Compressed) + 4):?SIZE,
       (checksum(Data)):?CHECK>>,
     Compressed];
select_frame(_, _, _, Data) ->
    [<<?UNCOMPRESSED, (iolist_size(Data) + 4):?SIZE, (checksum(Data)):?CHECK>>,
     Data].

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

%% --------------------------------------------------------------------
%% LZ77 dictionary compress
%% --------------------------------------------------------------------

compress_lz77d(Data, State) ->
    lists:reverse(compress_lz77d(Data, 0, byte_size(Data), #{}, State)).

compress_lz77d(_, DLen, DLen, _, #state{acc = Acc}) -> Acc;
compress_lz77d(Data, Pos, DLen, Dict, State) ->
    #state{lookahead = L, acc = Acc} = State,
    LL = min(DLen - Pos, L),
    Lookahead = <<C:?BYTE, _/binary>> = binary:part(Data, Pos, LL),
    case LL of
        1 -> [{0, 0, C} | Acc];
        LL when LL < ?MIN_MATCH ->
            lists:reverse([{0, 0, X} || <<X:?BYTE>> <= Lookahead]) ++ Acc;
        _ ->
            <<F:?MIN_MATCH/binary, T/binary>> = Lookahead,
            {Dict1, Match} = {_, {_, MLen, C1}} = matchd(F, T, Pos, C, Dict),
            Pos1 = case C1 of
                       -1  -> Pos + MLen;
                       _ -> Pos + MLen + 1
                   end,
            State1 = State#state{acc = [Match | Acc]},
            compress_lz77d(Data, Pos1, DLen, Dict1, State1)
    end.

matchd(F, <<>>, Pos, C, Dict) ->
    case maps:get(F, Dict, undefined) of
        undefined -> {maps:put(F, Pos, Dict), {0, 0, C}};
        MPos -> {Dict, {Pos - MPos, ?MIN_MATCH, -1}}
    end;
matchd(F, <<H:?BYTE, T/binary>>, Pos, C, Dict) ->
    case maps:get(F, Dict, undefined) of
        undefined -> {maps:put(F, Pos, Dict), {0, 0, C}};
        MPos ->
            matchd1(<<F/binary, H:?BYTE>>, T, Pos, ?MIN_MATCH, MPos, H, Dict)
    end.

matchd1(F, <<>>, Pos, Len, MPos, H, Dict) ->
    case maps:get(F, Dict, undefined) of
        undefined -> {maps:put(F, Pos, Dict), {Pos - MPos, Len, H}};
        MPos1 -> {Dict, {Pos - MPos1, Len + 1, -1}}
    end;
matchd1(F, L = <<H1:?BYTE, T/binary>>, Pos, Len, MPos, H, Dict) ->
    case maps:get(F, Dict, undefined) of
        undefined -> {maps:put(F, Pos, Dict), {Pos - MPos, Len, H}};
        MPos1 when MPos1 + Len == Pos ->
            matchd_run(F, F, L, Len);
        MPos1 ->
            matchd1(<<F/binary, H1:?BYTE>>, T, Pos, Len + 1, MPos1, H1, Dict)
    end.

matchd_run(<<>>, Match, L, Len) -> matchd_run(Match, Match, L, Len);
matchd_run(_, _, <<>>, Len) -> {Len, -1};
matchd_run(<<C:?BYTE,  M/binary>>, Match, <<C:?BYTE,  L/binary>>, Len) ->
    matchd_run(M, Match, L, Len + 1);
matchd_run(_, _, <<C:?BYTE, _/binary>>, Len) ->
    {Len, C}.

collapse([], Acc) -> Acc;
collapse(<<>>, Acc) -> Acc;
collapse(B = <<_:?BYTE, _/binary>>, Acc) -> <<Acc/binary, B/binary>>;
collapse([H | T], Acc) -> collapse(T, collapse(H, Acc)).

%% --------------------------------------------------------------------
%% Uncompress
%% --------------------------------------------------------------------

uncompress_frames(<<>>, Acc) -> lists:reverse(Acc);
uncompress_frames(<<?COMPRESSED, T/binary>>, Acc) ->
    <<Size:?SIZE, Checksum:?CHECK, B:(Size - 4)/binary, T1/binary>> = T,
    Data = uncompress_stream(B),
    Checksum = checksum(Data),
    uncompress_frames(T1, [Data | Acc]);
uncompress_frames(<<?UNCOMPRESSED, T/binary>>, Acc) ->
    <<Size:?SIZE, Checksum:?CHECK, U:(Size - 4)/binary, T1/binary>> = T,
    Checksum = checksum(U),
    uncompress_frames(T1, [U | Acc]);
uncompress_frames(<<?PAD, Size:?SIZE, _:Size/binary, T/binary>>, Acc) ->
    uncompress_frames(T, Acc);
uncompress_frames(<<Type, T/binary>>, Acc) when Type >= 16#80, Type =< 16#FD ->
    <<Size:?SIZE, _:Size/binary, T1/binary>> = T,
    uncompress_frames(T1, Acc);
uncompress_frames(<<Type, _/binary>>, _) ->
    erlang:error(badarg, [unskippabale_chunk, Type]).

uncompress_iwa(<<>>, Acc) -> lists:reverse(Acc);
uncompress_iwa(<<?COMPRESSED, T/binary>>, Acc) ->
    <<Size:?SIZE, B:Size/binary, T1/binary>> = T,
    Data = uncompress_stream(B),
    uncompress_iwa(T1, [Data | Acc]);
uncompress_iwa(_, _) ->
    erlang:error(badarg, no_compressed_iwa_frame).

uncompress_stream(Data) -> uncompress_block(drop_varint(Data), ~"").

uncompress_block(<<>>, Acc) -> Acc;
uncompress_block(<<S:6/integer, 0:2/integer, T/binary>>, Acc) ->
    {Len1, T2} = case {S, T} of
                    {60, <<Len:8/integer, T1/binary>>} -> {Len, T1};
                    {61, <<Len:16/integer-little, T1/binary>>} -> {Len, T1};
                    {62, <<Len:24/integer-little, T1/binary>>} -> {Len, T1};
                    {63, <<Len:32/integer-little, T1/binary>>} -> {Len, T1};
                    {Len, T1} -> {Len, T1}
                end,
    <<Lit:(Len1 + 1)/binary, T3/binary>> = T2,
    uncompress_block(T3, <<Acc/binary, Lit/binary>>);
uncompress_block(<<PosU:3/integer,Len4:3,1:2,PosL:8/integer,T/binary>>, Acc) ->
    <<Pos:11/integer>> = <<PosU:3/integer, PosL:8/integer>>,
    uncompress_copy(Pos, Len4 + 4, T, Acc);
uncompress_block(<<Len1:6, 2:2, Pos:16/integer-little,T/binary>>, Acc) ->
    uncompress_copy(Pos, Len1 + 1, T, Acc);
uncompress_block(<<Len1:6, 3:2, Pos:32/integer-little,T/binary>>, Acc) ->
    uncompress_copy(Pos, Len1 + 1, T, Acc).

uncompress_copy(Pos, Len, T, Acc) when Pos > Len ->
    C1 = binary_part(Acc, byte_size(Acc) - Pos, Len),
    uncompress_block(T, <<Acc/binary, C1/binary>>);
uncompress_copy(Pos, Len, T, Acc) ->
    Start = byte_size(Acc) - Pos,
    C1 = binary:copy(binary_part(Acc, Start, Pos), Len div Pos),
    C2 = binary:part(Acc, Start, Len rem Pos),
    uncompress_block(T, <<Acc/binary, C1/binary, C2/binary>>).

drop_varint(<<1:1, _:7, T/binary>>) -> drop_varint(T);
drop_varint(<<0:1, _:7, T/binary>>) -> T.

%% --------------------------------------------------------------------
%% Common Parts
%% --------------------------------------------------------------------
parse_opts(Opts) -> lists:foldl(fun parse_opt/2, #state{}, Opts).

parse_opt(frame, State) -> State#state{type = frame};
parse_opt(block, State) -> State#state{type = block};
parse_opt(iwa, State) -> State#state{type = iwa};
parse_opt(iolist, State) -> State#state{return = iolist};
parse_opt(binary, State) -> State#state{return = binary};
parse_opt({type, block}, State) -> State#state{type = block};
parse_opt({type, frame}, State) -> State#state{type = frame};
parse_opt({type, iwa}, State) -> State#state{type = iwa};
parse_opt({return_type, iolist}, State) -> State#state{return = iolist};
parse_opt({return_type, binary}, State) -> State#state{return = binary};
parse_opt({search_length, L}, State) when L >= 1024 , L =< 65536 ->
    State#state{search = L};
parse_opt({lookahead_length, L}, State) when L >= 4, L =< 64 ->
    State#state{lookahead = L}.

checksum(Data) ->
    CRC = jhn_hash:crc32c(Data),
    (jhn_math:rotr32(CRC, 15) + ?CONST) rem 16#100000000.

return(Value, #state{return = iolist}) -> Value;
return(Value, _) -> iolist_to_binary(Value).
