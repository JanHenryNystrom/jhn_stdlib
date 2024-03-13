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
%%%   A few checksum algorithms, CRC32-C and xxHash-32.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_hash).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions.
-export([crc32c/1,
         xxh32/1, xxh32/2]).

%% Defines

%% CRC32-C
-define(TABLE,
        {16#00000000, 16#F26B8303, 16#E13B70F7, 16#1350F3F4,
         16#C79A971F, 16#35F1141C, 16#26A1E7E8, 16#D4CA64EB,
         16#8AD958CF, 16#78B2DBCC, 16#6BE22838, 16#9989AB3B,
         16#4D43CFD0, 16#BF284CD3, 16#AC78BF27, 16#5E133C24,
         16#105EC76F, 16#E235446C, 16#F165B798, 16#030E349B,
         16#D7C45070, 16#25AFD373, 16#36FF2087, 16#C494A384,
         16#9A879FA0, 16#68EC1CA3, 16#7BBCEF57, 16#89D76C54,
         16#5D1D08BF, 16#AF768BBC, 16#BC267848, 16#4E4DFB4B,
         16#20BD8EDE, 16#D2D60DDD, 16#C186FE29, 16#33ED7D2A,
         16#E72719C1, 16#154C9AC2, 16#061C6936, 16#F477EA35,
         16#AA64D611, 16#580F5512, 16#4B5FA6E6, 16#B93425E5,
         16#6DFE410E, 16#9F95C20D, 16#8CC531F9, 16#7EAEB2FA,
         16#30E349B1, 16#C288CAB2, 16#D1D83946, 16#23B3BA45,
         16#F779DEAE, 16#05125DAD, 16#1642AE59, 16#E4292D5A,
         16#BA3A117E, 16#4851927D, 16#5B016189, 16#A96AE28A,
         16#7DA08661, 16#8FCB0562, 16#9C9BF696, 16#6EF07595,
         16#417B1DBC, 16#B3109EBF, 16#A0406D4B, 16#522BEE48,
         16#86E18AA3, 16#748A09A0, 16#67DAFA54, 16#95B17957,
         16#CBA24573, 16#39C9C670, 16#2A993584, 16#D8F2B687,
         16#0C38D26C, 16#FE53516F, 16#ED03A29B, 16#1F682198,
         16#5125DAD3, 16#A34E59D0, 16#B01EAA24, 16#42752927,
         16#96BF4DCC, 16#64D4CECF, 16#77843D3B, 16#85EFBE38,
         16#DBFC821C, 16#2997011F, 16#3AC7F2EB, 16#C8AC71E8,
         16#1C661503, 16#EE0D9600, 16#FD5D65F4, 16#0F36E6F7,
         16#61C69362, 16#93AD1061, 16#80FDE395, 16#72966096,
         16#A65C047D, 16#5437877E, 16#4767748A, 16#B50CF789,
         16#EB1FCBAD, 16#197448AE, 16#0A24BB5A, 16#F84F3859,
         16#2C855CB2, 16#DEEEDFB1, 16#CDBE2C45, 16#3FD5AF46,
         16#7198540D, 16#83F3D70E, 16#90A324FA, 16#62C8A7F9,
         16#B602C312, 16#44694011, 16#5739B3E5, 16#A55230E6,
         16#FB410CC2, 16#092A8FC1, 16#1A7A7C35, 16#E811FF36,
         16#3CDB9BDD, 16#CEB018DE, 16#DDE0EB2A, 16#2F8B6829,
         16#82F63B78, 16#709DB87B, 16#63CD4B8F, 16#91A6C88C,
         16#456CAC67, 16#B7072F64, 16#A457DC90, 16#563C5F93,
         16#082F63B7, 16#FA44E0B4, 16#E9141340, 16#1B7F9043,
         16#CFB5F4A8, 16#3DDE77AB, 16#2E8E845F, 16#DCE5075C,
         16#92A8FC17, 16#60C37F14, 16#73938CE0, 16#81F80FE3,
         16#55326B08, 16#A759E80B, 16#B4091BFF, 16#466298FC,
         16#1871A4D8, 16#EA1A27DB, 16#F94AD42F, 16#0B21572C,
         16#DFEB33C7, 16#2D80B0C4, 16#3ED04330, 16#CCBBC033,
         16#A24BB5A6, 16#502036A5, 16#4370C551, 16#B11B4652,
         16#65D122B9, 16#97BAA1BA, 16#84EA524E, 16#7681D14D,
         16#2892ED69, 16#DAF96E6A, 16#C9A99D9E, 16#3BC21E9D,
         16#EF087A76, 16#1D63F975, 16#0E330A81, 16#FC588982,
         16#B21572C9, 16#407EF1CA, 16#532E023E, 16#A145813D,
         16#758FE5D6, 16#87E466D5, 16#94B49521, 16#66DF1622,
         16#38CC2A06, 16#CAA7A905, 16#D9F75AF1, 16#2B9CD9F2,
         16#FF56BD19, 16#0D3D3E1A, 16#1E6DCDEE, 16#EC064EED,
         16#C38D26C4, 16#31E6A5C7, 16#22B65633, 16#D0DDD530,
         16#0417B1DB, 16#F67C32D8, 16#E52CC12C, 16#1747422F,
         16#49547E0B, 16#BB3FFD08, 16#A86F0EFC, 16#5A048DFF,
         16#8ECEE914, 16#7CA56A17, 16#6FF599E3, 16#9D9E1AE0,
         16#D3D3E1AB, 16#21B862A8, 16#32E8915C, 16#C083125F,
         16#144976B4, 16#E622F5B7, 16#F5720643, 16#07198540,
         16#590AB964, 16#AB613A67, 16#B831C993, 16#4A5A4A90,
         16#9E902E7B, 16#6CFBAD78, 16#7FAB5E8C, 16#8DC0DD8F,
         16#E330A81A, 16#115B2B19, 16#020BD8ED, 16#F0605BEE,
         16#24AA3F05, 16#D6C1BC06, 16#C5914FF2, 16#37FACCF1,
         16#69E9F0D5, 16#9B8273D6, 16#88D28022, 16#7AB90321,
         16#AE7367CA, 16#5C18E4C9, 16#4F48173D, 16#BD23943E,
         16#F36E6F75, 16#0105EC76, 16#12551F82, 16#E03E9C81,
         16#34F4F86A, 16#C69F7B69, 16#D5CF889D, 16#27A40B9E,
         16#79B737BA, 16#8BDCB4B9, 16#988C474D, 16#6AE7C44E,
         16#BE2DA0A5, 16#4C4623A6, 16#5F16D052, 16#AD7D5351
        }).

%% xxH32
-define(PRIME1, 16#9E3779B1).
-define(PRIME2, 16#85EBCA77).
-define(PRIME3, 16#C2B2AE3D).
-define(PRIME4, 16#27D4EB2F).
-define(PRIME5, 16#165667B1).

-define(LANE, 32/little).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: crc32c(Data) -> Checksum
%% @doc
%%   Generate a CRC32-C checksum from iodata.
%% @end
%%--------------------------------------------------------------------
-spec crc32c(iodata()) -> non_neg_integer().
%%--------------------------------------------------------------------
crc32c(Data) -> crc32c(iolist_to_binary(Data), 16#FFFFFFFF).

%%--------------------------------------------------------------------
%% Function: xxh32(Data) -> Checksum
%% @doc
%%   Generate a xxH32 checksum from iodata with Seed 0.
%% @end
%%--------------------------------------------------------------------
-spec xxh32(iodata()) -> non_neg_integer().
%%--------------------------------------------------------------------
xxh32(Data) -> xxh32(Data, 0).

%%--------------------------------------------------------------------
%% Function: xxh32(Data, Seed) -> Checksum
%% @doc
%%   Generate a xxH32 checksum from iodata using Seed.
%% @end
%%--------------------------------------------------------------------
-spec xxh32(iodata(), non_neg_integer()) -> non_neg_integer().
%%--------------------------------------------------------------------
xxh32(Data, Seed) -> xxh32_step1(iolist_to_binary(Data), Seed).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% --------------------------------------------------------------------
%% CRC32-C
%% --------------------------------------------------------------------

crc32c(<<>>, Acc) -> Acc bxor 16#FFFFFFFF;
crc32c(<<H, T/bits>>, Acc) ->
    crc32c(T, (Acc bsr 8) bxor element(1 + ((Acc bxor H) band 16#FF), ?TABLE)).

%% --------------------------------------------------------------------
%% XXH32
%% --------------------------------------------------------------------

xxh32_step1(Binary, Seed) when byte_size(Binary) < 16 ->
    xxh32_step4(Binary, plus(Seed, ?PRIME5));
xxh32_step1(Binary, Seed) ->
    xxh32_step2(Binary,
                plus(Seed, plus(?PRIME1, ?PRIME2)),
                plus(Seed, ?PRIME2),
                Seed,
                plus(Seed, ?PRIME1)).

xxh32_step2(<<L1:?LANE,L2:?LANE,L3:?LANE,L4:?LANE,T/binary>>, A1, A2, A3, A4) ->
    xxh32_step2(T, step2(A1, L1), step2(A2, L2), step2(A3, L3), step2(A4, L4));
xxh32_step2(B, Acc1, Acc2, Acc3, Acc4) ->
    Acc = plus(rotl(Acc1, 1),
               plus(rotl(Acc2, 7),
                    plus(rotl(Acc3, 12),
                         rotl(Acc4, 18)))),
    xxh32_step4(B, Acc).

step2(A, L) -> times(rotl(plus(A, times(L, ?PRIME2)), 13), ?PRIME1).

xxh32_step4(B, Acc) -> xxh32_step5(B, plus(Acc, byte_size(B))).

xxh32_step5(<<>>, Acc) -> xxh32_step6(Acc);
xxh32_step5(<<L:32/little, T/binary>>, Acc) ->
    xxh32_step5(T, times(rotl(plus(Acc, times(L, ?PRIME3)), 17), ?PRIME4));
xxh32_step5(<<L:8, T/binary>>, Acc) ->
    xxh32_step5(T, times(rotl(plus(Acc, times(L, ?PRIME5)), 11), ?PRIME1)).

xxh32_step6(Acc) ->
    Acc1 = Acc bxor (Acc bsr 15),
    Acc2 = times(Acc1, ?PRIME2),
    Acc3 = Acc2 bxor (Acc2 bsr 13),
    Acc4 = times(Acc3, ?PRIME3),
    Acc4 bxor (Acc4 bsr 16).

plus(0, B) -> B;
plus(A, B) -> mod(A + B).

times(A, B) -> mod(A * B).

mod(N) -> N rem 16#100000000.

rotl(I, N) -> jhn_math:rotl32(I, N).
