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
%%%  A binary data encodinglibrary providing a bumber of algorithms
%%%
%%% Base 32 default standard, standard
%%%  Aglorithm standard
%%%    Alfabet standard
%%%      b32 - The Base16, Base32, and Base64 Data Encodings - rfc4648
%%%    Alfabet hex
%%%      b32hex - The Base16, Base32, and Base64 Data Encodings - rfc4648
%%%    Alfabet
%%%      b32geo - https://github.com/nesterenko-kv/geohash/blob/main/README.md
%%%               https://en.wikipedia.org/wiki/Geohash
%%%               https://en.wikipedia.org/wiki/Base32
%%%  Crockford
%%%    https://datatracker.ietf.org/doc/draft-crockford-davis-base32-for-humans/
%%%
%%%  z-base-32
%%%       https://philzimmermann.com/docs/human-oriented-base-32-encoding.txt
%%%
%%%
%%%
%%% Base 45 default standard, standard
%%% b45 - The Base45 Data Encoding - rfc9285
%%%
%%% Base 85 default z85, standard
%%% z85 - ZeroMQ spec:32/Z85
%%%
%%% @end
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_base).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/2, encode/3,
         decode/2, decode/3]).

%% Types
-type algo() :: standard | hex | z85.
-type base() :: 32 | 45 | 85.
-type alfabet() :: standard | hex | geohash.
-type opt()     :: return_type() | {return_type, return_type()} |
                   {alpfabet, alfabet()}.

-type return_type() :: iolist | binary.

%% Records
-record(opts, {algo = standard :: algo(),
               alfabet     = standard :: alfabet() ,
               return_type = iolist   :: return_type()
              }).


%% Defines

%% B32
-define(B32_ALFABET,
        {$A, $B, $C, $D, $E, $F, $G, $H, $I, $J,
         $K, $L, $M, $N, $O, $P, $Q, $R, $S, $T,
         $U, $V, $W, $X, $Y, $Z, $2, $3, $4, $5,
         $6, $7}).
-define(B32_DECODE,
        {u, u, u, u, u, u, u, u, u, u,
         u, u, u, u, u, u, u, u, u, u,
         u, u, u, u, u, u, u, u, u, u,
         u, u, u, u, u, u, u, u, u, u,
         u, u, u, u, u, u, u, u, u,
         26, 27, 28, 29, 30, 31,
         u, u, u, u, u, u, u, u, u,
         0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
         10, 11, 12, 13, 14, 15, 16,
         17, 18, 19, 20, 21, 22, 23,
         24, 25}).

%% B32hex
-define(B32HEX_ALFABET,
        {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
         $A, $B, $C, $D, $E, $F, $G, $H, $I, $J,
         $K, $L, $M, $N, $O, $P, $Q, $R, $S, $T,
         $U, $V}).
-define(B32HEX_DECODE,
        {u, u, u, u, u, u, u, u, u, u,
         u, u, u, u, u, u, u, u, u, u,
         u, u, u, u, u, u, u, u, u, u,
         u, u, u, u, u, u, u, u, u, u,
         u, u, u, u, u, u, u,
         0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
         u, u,  u, u, u, u, u,
         10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
         20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
         30, 31}).

%% B32geo
-define(B32GEO_ALFABET,
        {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
         $b, $c, $d, $e, $f, $g, $h, $j, $k, $m,
         $n, $p, $q, $r, $s, $t, $u, $v, $w, $x,
         $y, $z}).
-define(B32GEO_DECODE,
        {u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,0,1,2,3,4,5,6,7,8,9,u,u,
         u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,u,10,11,12,13,14,15,16,u,17,18,u,19,20,u,21,
         22,23,24,25,26,27,28,29,30,31}).

%% B45
-define(B45_ALFABET,
        {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
         $A, $B, $C, $D, $E, $F, $G, $H, $I, $J,
         $K, $L, $M, $N, $O, $P, $Q, $R, $S, $T,
         $U, $V, $W, $X, $Y, $Z, $\s, $$, $%, $*,
         $+, $-, $\., $\/, $:
        }).
-define(B45_DECODE,
        {36,
         u, u, u,
         37, 38,
         u, u, u, u,
         39, 40,
         u,
         41, 42, 43, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 44,
         u, u, u, u, u, u,
         10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
         23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35}).

%% Z85
-define(Z85_ALFABET,
        {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
         $a, $b, $c, $d, $e, $f, $g, $h, $i, $j,
         $k, $l, $m, $n, $o, $p, $q, $r, $s, $e,
         $u, $v, $w, $x, $y, $z, $A, $B, $C, $D,
         $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
         $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X,
         $Y, $Z, $\., $-, $:, $+, $=, $^, $\!, $\/,
         $*, $?, $&, $<, $>, $(, $), $[, $], ${,
         $}, $@, $%, $$, $#}).
-define(Z85_DECODE,
        {16#00, 16#44, 16#00, 16#54, 16#53, 16#52, 16#48, 16#00,
         16#4B, 16#4C, 16#46, 16#41, 16#00, 16#3F, 16#3E, 16#45,
         16#00, 16#01, 16#02, 16#03, 16#04, 16#05, 16#06, 16#07,
         16#08, 16#09, 16#40, 16#00, 16#49, 16#42, 16#4A, 16#47,
         16#51, 16#24, 16#25, 16#26, 16#27, 16#28, 16#29, 16#2A,
         16#2B, 16#2C, 16#2D, 16#2E, 16#2F, 16#30, 16#31, 16#32,
         16#33, 16#34, 16#35, 16#36, 16#37, 16#38, 16#39, 16#3A,
         16#3B, 16#3C, 16#3D, 16#4D, 16#00, 16#4E, 16#43, 16#00,
         16#00, 16#0A, 16#0B, 16#0C, 16#0D, 16#0E, 16#0F, 16#10,
         16#11, 16#12, 16#13, 16#14, 16#15, 16#16, 16#17, 16#18,
         16#19, 16#1A, 16#1B, 16#1C, 16#1D, 16#1E, 16#1F, 16#20,
         16#21, 16#22, 16#23, 16#4F, 16#00, 16#50, 16#00, 16#00}).
-define(Z85_85, [52200625, 614125,  7225, 85, 1]).
-define(Z85_256, [16777216, 65536, 256, 1]).


%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec encode(algo(), iodata()) -> iolist().
%%--------------------------------------------------------------------
encode(Algo, Data) -> encode(Algo, Data, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec encode(algo(), iodata(), [opt()]) -> iolist().
%%--------------------------------------------------------------------
encode(Algo, Data = [_ | _], Opts) ->
    encode(Algo, iolist_to_binary(Data), Opts);
encode(Algo, Data = <<_/binary>>, Opts) ->
    Opts1 = #opts{return_type = Return} = lists:foldr(fun opt/2, #opts{}, Opts),
    Result = do_encode(Algo, Data, Opts1),
    case Return of
        iolist -> Result;
        binary -> iolist_to_binary(Result)
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(algo(), iodata()) -> iolist().
%%--------------------------------------------------------------------
decode(Algo, Data) -> decode(Algo, Data, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(algo(), iodata(), [opt()]) -> iolist().
%%--------------------------------------------------------------------
decode(Algo, Data = [_ | _], Opts) ->
    decode(Algo, iolist_to_binary(Data), Opts);
decode(Algo, Data = <<_/binary>>, Opts) ->
    Opts1 = #opts{return_type = Return} = lists:foldr(fun opt/2, #opts{}, Opts),
    Result = do_decode(Algo, Data, Opts1),
    case Return of
        iolist -> Result;
        binary -> iolist_to_binary(Result)
    end.

%% ===================================================================
%% Internal functions.
%% ===================================================================

opt(iolist, Opts) -> Opts#opts{return_type = iolist};
opt(binary, Opts) -> Opts#opts{return_type = binary};
opt({return_type, iolist}, Opts) -> Opts#opts{return_type = iolist};
opt({return_type, binary}, Opts) -> Opts#opts{return_type = binary};
opt({alfabet, standard}, Opts) -> Opts#opts{alfabet = standard};
opt({alfabet, hex}, Opts) -> Opts#opts{alfabet = hex};
opt({alfabet, geohash}, Opts) -> Opts#opts{alfabet = geohash}.

%% --------------------------------------------------------------------
%% Encode
%% --------------------------------------------------------------------

do_encode(b32, B, #opts{alfabet = standard}) -> encode_b32(B, ?B32_ALFABET, []);
do_encode(b32, B, #opts{alfabet = hex}) -> encode_b32(B, ?B32HEX_ALFABET, []);
do_encode(b32, B, #opts{alfabet = geohash}) ->
    encode_b32(B, ?B32GEO_ALFABET, []);
do_encode(b45, B, _) -> encode_b45(B, []);
do_encode(z85, B, _) when (byte_size(B) rem 4) == 0 -> encode_z85(B, []).

%% b32
encode_b32(<<>>, _, Acc) -> lists:reverse(Acc);
encode_b32(<<A:5, B:3>>, Alfabet, Acc) ->
    <<B1:5>> = <<B:3, 0:2>>,
    Elt = [element(A + 1, Alfabet),
           element(B1 + 1, Alfabet),
           $=, $=, $=, $=, $=, $=],
    encode_b32(<<>>, Alfabet, [Elt | Acc]);
encode_b32(<<A:5, B:5, C:5, D:1>>, Alfabet, Acc) ->
    <<D1:5>> = <<D:1, 0:4>>,
    Elt = [element(A + 1, Alfabet),
           element(B + 1, Alfabet),
           element(C + 1, Alfabet),
           element(D1 + 1, Alfabet),
           $=, $=, $=, $=],
    encode_b32(<<>>, Alfabet, [Elt | Acc]);
encode_b32(<<A:5, B:5, C:5, D:5, E:4>>, Alfabet, Acc) ->
    <<E1:5>> = <<E:4, 0:1>>,
    Elt = [element(A + 1, Alfabet),
           element(B + 1, Alfabet),
           element(C + 1, Alfabet),
           element(D + 1, Alfabet),
           element(E1 + 1, Alfabet),
           $=, $=, $=],
    encode_b32(<<>>, Alfabet, [Elt | Acc]);
encode_b32(<<A:5, B:5, C:5, D:5, E:5, F:5, G:2>>, Alfabet, Acc) ->
    <<G1:5>> = <<G:2, 0:3>>,
    Elt = [element(A + 1, Alfabet),
           element(B + 1, Alfabet),
           element(C + 1, Alfabet),
           element(D + 1, Alfabet),
           element(E + 1, Alfabet),
           element(F + 1, Alfabet),
           element(G1 + 1, Alfabet),
           $=],
    encode_b32(<<>>, Alfabet, [Elt | Acc]);
encode_b32(<<A:5, B:5, C:5, D:5, E:5, F:5, G:5, H:5, T/binary>>, Alfa, Acc) ->
    Elt = [element(A + 1, Alfa),
           element(B + 1, Alfa),
           element(C + 1, Alfa),
           element(D + 1, Alfa),
           element(E + 1, Alfa),
           element(F + 1, Alfa),
           element(G + 1, Alfa),
           element(H + 1, Alfa)],
    encode_b32(T, Alfa, [Elt | Acc]).


%% --------------------------------------------------------------------

%% b45
encode_b45(<<>>, Acc) -> lists:reverse(Acc);
encode_b45(<<A>>, Acc) ->
    C = e_b45(A rem 45 + 1),
    D = e_b45((A div 45) rem 45 + 1),
    encode_b45(<<>>, [[C, D] | Acc]);
encode_b45(<<A:16, T/binary>>, Acc) ->
    C = e_b45(A rem 45 + 1),
    D = e_b45((A div 45) rem 45 + 1),
    E = e_b45(A div 45 div 45 + 1),
    encode_b45(T, [[C, D, E] | Acc]).

e_b45(X) -> element(X, ?B45_ALFABET).
%% --------------------------------------------------------------------

%% Z85
encode_z85(<<>>, Acc) -> lists:reverse(Acc);
encode_z85(<<A, B, C, D, T/binary>>, Acc) ->
    V = (((((A * 256) + B) * 256) + C) * 256) + D,
    Es = [element((V div Div) rem 85 + 1, ?Z85_ALFABET) || Div <- ?Z85_85],
    encode_z85(T, [Es | Acc]).
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Decode
%% --------------------------------------------------------------------

do_decode(b32, B, #opts{alfabet = standard}) -> decode_b32(B, ?B32_DECODE, []);
do_decode(b32, B, #opts{alfabet = hex}) -> decode_b32(B, ?B32HEX_DECODE, []);
do_decode(b32, B, #opts{alfabet = geohash}) ->
    decode_b32(B, ?B32GEO_DECODE, []);
do_decode(b45, B45, _) -> decode_b45(B45, []);
do_decode(z85, Z85, _) when (byte_size(Z85) rem 5) == 0 -> decode_z85(Z85, []).

%% b32
decode_b32(<<>>, _, Acc) -> lists:reverse(Acc);
decode_b32(<<A, B, $=, $=, $=,$=, $=, $=>>, Alfabet, Acc) ->
    <<B1:3, 0:2>> = <<(element(B, Alfabet)):5>>,
    Elt = <<(element(A, Alfabet)):5, B1:3>>,
    decode_b32(<<>>, Alfabet, [Elt | Acc]);
decode_b32(<<A, B, C, D, $=, $=, $=, $=>>, Alfabet, Acc) ->
    <<D1:1, 0:4>> = <<(element(D, Alfabet)):5>>,
    Elt = <<(element(A, Alfabet)):5,
            (element(B, Alfabet)):5,
            (element(C, Alfabet)):5,
            D1:1>>,
    decode_b32(<<>>, Alfabet, [Elt | Acc]);
decode_b32(<<A, B, C, D, E, $=, $=, $=>>, Alfabet, Acc) ->
    <<E1:4, 0:1>> = <<(element(E, Alfabet)):5>>,
    Elt = <<(element(A, Alfabet)):5,
            (element(B, Alfabet)):5,
            (element(C, Alfabet)):5,
            (element(D, Alfabet)):5,
            E1:4>>,
    decode_b32(<<>>, Alfabet, [Elt | Acc]);
decode_b32(<<A, B, C, D, E, F, G, $=>>, Alfabet, Acc) ->
    <<G1:2, 0:3>> = <<(element(G, Alfabet)):5>>,
    Elt = <<(element(A, Alfabet)):5,
            (element(B, Alfabet)):5,
            (element(C, Alfabet)):5,
            (element(D, Alfabet)):5,
            (element(E, Alfabet)):5,
            (element(F, Alfabet)):5,
            G1:2>>,
    decode_b32(<<>>, Alfabet, [Elt | Acc]);
decode_b32(<<A, B, C, D, E, F, G, H, T/binary>>, Alfa, Acc) ->
    Elt = <<(element(A, Alfa)):5,
            (element(B, Alfa)):5,
            (element(C, Alfa)):5,
            (element(D, Alfa)):5,
            (element(E, Alfa)):5,
            (element(F, Alfa)):5,
            (element(G, Alfa)):5,
            (element(H, Alfa)):5>>,
    decode_b32(T, Alfa, [Elt | Acc]).

%% b45
decode_b45(<<>>, Acc) -> lists:reverse(Acc);
decode_b45(<<C, D>>, Acc) ->
    decode_b45(<<>>, [d_b45(C) + 45 * d_b45(D) | Acc]);
decode_b45(<<C, D, E, T/binary>>, Acc) ->
    N = d_b45(C) + 45 * (d_b45(D) + 45 * d_b45(E)),
    <<A, B>> = <<N:16>>,
    decode_b45(T, [[A, B] | Acc]).

d_b45(C) -> element(C - 31, ?B45_DECODE).
%% --------------------------------------------------------------------

%% Z85
decode_z85(<<>>, Acc) -> lists:reverse(Acc);
decode_z85(<<A, B, C, D, E, T/binary>>, Acc) ->
    V = lists:foldl(fun d_z85/2, 0, [A, B, C, D, E]),
    decode_z85(T, [[(V div Div) rem 256 || Div <- ?Z85_256] | Acc]).

d_z85(V, Pre) -> (Pre * 85) + element(V - 31, ?Z85_DECODE).
%% --------------------------------------------------------------------
