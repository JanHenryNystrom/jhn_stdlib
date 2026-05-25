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
%%%  A binary data encodinglibrary providing support for a number of
%%%  bases, algorithms and alphabets.
%%%  If nothing else is specified the "standard" algorithm is used .
%%%  The algorithm and alphabet to use in encoding or decoding can be specified
%%%  with options provided to encode/3 and decode/3. Further options are the
%%%  for the return values of either an iolist or binary, with the exception of
%%%  the Z-Base-32 algorithm that returns a bitstring.
%%%
%%%  The following documents are the base for the library
%%%
%%%  rfc4648: The Base16, Base32, and Base64 Data Encodings
%%%   * Base32 standard algorithm (standard)
%%%   * Base32 standard and hex alphabets (standard, hex)
%%%
%%%  The Base32 wikipage: https://en.wikipedia.org/wiki/Base32
%%%   * The Geohash alphabet (geohash)
%%%
%%%  Crockford's Base32:
%%%    https://datatracker.ietf.org/doc/draft-crockford-davis-base32-for-humans/
%%%    * Crockford base32 algorithm (crockford)
%%%
%%%  Clockwork: https://github.com/szktty/go-clockwork-base32
%%%    * The clockwork algorithm (clockwork)
%%%
%%%  Z-Base-32:
%%%    https://philzimmermann.com/docs/human-oriented-base-32-encoding.txt
%%%    * The Z-Base-32 algorithm (zbase)
%%%    Does not support binary/iolist deoding since it returns a bitstring only
%%%
%%%  rfc9285: The Base45 Data Encoding
%%%     * Base 45 standard algorithm (standard)
%%%
%%%  Ascii85: https://en.wikipedia.org/wiki/Ascii85
%%%     * The Base85 algorithm
%%%
%%%  ZeroMQ spec:32/Z85: https://rfc.zeromq.org/spec/32/
%%%     * The ZeroMQ base85 algorithm (z85)
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
-type algo() ::
        %% base85, base45, base85
        standard |
        %% base32
        crockford | clockwork | zbase |
        %% base85
        z85.
-type base() :: 32 | 45 | 85.
-type alphabet() :: standard | hex | geohash.
-type opt()     :: return_type() | {return_type, return_type()} |
                   {alpfabet, alphabet()} | {algo, algo()}.

-type return_type() :: iolist | binary.

%% Records
-record(opts, {algo        = standard :: algo(),
               alphabet     = standard :: alphabet() ,
               return_type = iolist   :: return_type()
              }).

%% Defines

%% B32
-define(B32_ALPHABET,
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
-define(B32HEX_ALPHABET,
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
-define(B32GEO_ALPHABET,
        {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
         $b, $c, $d, $e, $f, $g, $h, $j, $k, $m,
         $n, $p, $q, $r, $s, $t, $u, $v, $w, $x,
         $y, $z}).
-define(B32GEO_DECODE,
        {u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,
         0,1,2,3,4,5,6,7,8,9,
         u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,u,u,u,
         10,11,12,13,14,15,16,
         u,
         17,18,
         u,
         19,20,
         u,
         21,22,23,24,25,26,27,28,29,30,31}).

%% B32Crockford
-define(B32CROCKFORD_ALPHABET,
        {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
         $A, $B, $C, $D, $E, $F, $G, $H, $J, $K,
         $M, $N, $P, $Q, $R, $S, $T, $V, $W, $X,
         $Y, $Z}).
-define(B32CROCKFORD_DECODE,
        {u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,u,u,u,
         u,u,u,u,u,u,u,
         0,1,2,3,4,5,6,7,8,9,
         u,u, u,u,u,u,u,
         10,11,12,13,14,15,16,17,
         u,
         18,19,
         u,
         20,21,
         u,
         22,23,24,25,26,
         u,
         27,28,29,30,31}).

%% B32Z
-define(B32Z_ALPHABET,
        {$y, $b, $n, $d, $r, $f, $g, $8, $e,
         $j, $k, $m, $c, $p, $q, $x, $o, $t,
         $1, $u, $w, $i, $s, $z, $a, $3, $4,
         $5, $h, $7, $6, $9}).
-define(B32Z_DECODE,
        {u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,
   u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,18,u,25,26,27,30,29,7,
   31,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,
   u,u,u,u,u,u,u,u,u,u,u,24,1,12,3,8,5,6,28,21,9,10,u,11,2,16,
   13,14,4,22,17,19,u,20,15,0,23}).
%% B45
-define(B45_ALPHABET,
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
-define(Z85_ALPHABET,
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
%% Function: encode(Base, Data) -> Encoded
%% @doc
%%   Encodes the data as an iolist using Base default algorithm and alphabet,
%%   equivalent to encode(Base, Data, []).
%% @end
%%--------------------------------------------------------------------
-spec encode(base(), iodata()) -> iolist().
%%--------------------------------------------------------------------
encode(Base, Data) -> encode(Base, Data, []).

%%--------------------------------------------------------------------
%% Function: encode(Base, Data, Opts) -> Encoded
%% @doc
%%   Encodes the data as an iolist or binary according to  the Base.
%%   Options
%%     binary : returns a binary,
%%     iolist : returns an iolist (default)
%%     {return_type, binary | iolist}: returns what is specified
%%     {algo, Algo}: uses the algorithm Algo for encoding
%%     {alphabet, A}: uses the alphabet for the encoding, must be
%%                    compatible with the algorithm (no checks are done).
%% @end
%%--------------------------------------------------------------------
-spec encode(base(), iodata()| bitstring(), [opt()]) -> iolist().
%%--------------------------------------------------------------------
encode(Base, Data = [_ | _], Opts) ->
    encode(Base, iolist_to_binary(Data), Opts);
encode(Base, Data, Opts) ->
    Opts1 = #opts{return_type = Return} = lists:foldr(fun opt/2, #opts{}, Opts),
    Result = do_encode(Base, Data, Opts1),
    case Return of
        iolist -> Result;
        binary -> iolist_to_binary(Result)
    end.

%%--------------------------------------------------------------------
%% Function: decode(Base, Encoded) -> Data
%% @doc
%%   Decodes the data as an iolist using Base default algorithm and alphabet,
%%   equivalent to decode(Base, Encoded, []).
%% @end
%%--------------------------------------------------------------------
-spec decode(base(), iodata()) -> iolist() | bitstring().
%%--------------------------------------------------------------------
decode(Base, Data) -> decode(Base, Data, []).

%%--------------------------------------------------------------------
%% Function: decode(Base, Encoded, Opts) -> Data
%% @doc
%%   Decodes the data as an iolist, bitstring or binary according to  the Base.
%%   Options
%%     binary : returns a binary, not viable for Z-Base-32
%%     iolist : returns an iolist (default)
%%     {return_type, binary | iolist}: returns what is specified
%%     {algo, Algo}: uses the algorithm Algo for decoding
%%     {alphabet, A}: uses the alphabet for the decoding, must be
%%                    compatible with the algorithm (no checks are done).
%% @end
%%--------------------------------------------------------------------
-spec decode(base(), iodata(), [opt()]) -> iolist() | bitstring().
%%--------------------------------------------------------------------
decode(Base, Data = [_ | _], Opts) ->
    decode(Base, iolist_to_binary(Data), Opts);
decode(Base, Data = <<_/binary>>, Opts) ->
    Opts1 = #opts{return_type = Return} = lists:foldr(fun opt/2, #opts{}, Opts),
    Result = do_decode(Base, Data, Opts1),
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
opt({alphabet, standard}, Opts) -> Opts#opts{alphabet = standard};
opt({alphabet, hex}, Opts) -> Opts#opts{alphabet = hex};
opt({alphabet, geohash}, Opts) -> Opts#opts{alphabet = geohash};
opt({algo, standard}, Opts) -> Opts#opts{algo = standard};
opt({algo, crockford}, Opts) -> Opts#opts{algo = crockford};
opt({algo, clockwork}, Opts) -> Opts#opts{algo = clockwork};
opt({algo, zbase}, Opts) -> Opts#opts{algo = zbase};
opt({algo, z85}, Opts) -> Opts#opts{algo = z85}.

%% --------------------------------------------------------------------
%% Encode
%% --------------------------------------------------------------------

do_encode(32, B, #opts{algo = crockford}) ->
    encode_b32(B, ?B32CROCKFORD_ALPHABET, false, false, []);
do_encode(32, B, #opts{algo = clockwork}) ->
    encode_b32(B, ?B32CROCKFORD_ALPHABET, false, false, []);
do_encode(32, B, #opts{algo = zbase}) ->
    encode_zbase(B, []);
do_encode(32, B, #opts{alphabet = standard}) ->
    encode_b32(B, ?B32_ALPHABET, false, true, []);
do_encode(32, B, #opts{alphabet = hex}) ->
    encode_b32(B, ?B32HEX_ALPHABET, false, true, []);
do_encode(32, B, #opts{alphabet = geohash}) ->
    encode_b32(B, ?B32GEO_ALPHABET, false, true, []);
do_encode(45, B, _) ->
    encode_b45(B, []);
do_encode(85, B, #opts{algo = z85}) when (byte_size(B) rem 4) == 0 ->
    encode_z85(B, []);
do_encode(85, B, _) when (byte_size(B) rem 4) == 0 ->
    encode_b85(B, []).


%% --------------------------------------------------------------------
%% b32
encode_b32(<<>>, _, _, _, Acc) -> lists:reverse(Acc);
encode_b32(<<A:5, B:3>>, Alphabet, S, P, Acc) ->
    <<B1:5>> = <<B:3, 0:2>>,
    encode_b32(<<>>, Alphabet, S, P, [e_b32(Alphabet, [A, B1], P, 6) | Acc]);
encode_b32(<<A:5, B:5, C:5, D:1>>, Alphabet, S, P, Acc) ->
    <<D1:5>> = <<D:1, 0:4>>,
    encode_b32(<<>>, Alphabet, S, P, [e_b32(Alphabet, [A, B, C, D1], P,4)|Acc]);
encode_b32(<<A:5, B:5, C:5, D:5, E:4>>, Alphabet, S, P, Acc) ->
    <<E1:5>> = <<E:4, 0:1>>,
    encode_b32(<<>>, Alphabet, S, P, [e_b32(Alphabet, [A, B,C, D,E1],P,3)|Acc]);
encode_b32(<<A:5, B:5, C:5, D:5, E:5, F:5, G:2>>, Alphabet, S, P, Acc) ->
    <<G1:5>> = <<G:2, 0:3>>,
    Elt = e_b32(Alphabet, [A, B, C, D, E, F, G1], P, 1),
    encode_b32(<<>>, Alphabet, S, P, [Elt | Acc]);
encode_b32(<<A:5, B:5, C:5, D:5, E:5, F:5, G:5, H:5,T/binary>>,Alpha,S,P,Acc) ->
    Elt = e_b32(Alpha, [A, B, C, D, E, F, G, H], P, 0),
    encode_b32(T, Alpha, S, P, [Elt | Acc]).

e_b32(Alphabet, Parts, false, _) -> [element(X + 1, Alphabet) || X <- Parts];
e_b32(Alphabet, Parts, true, Pad) ->
    [[element(X + 1, Alphabet) || X <- Parts], lists:duplicate(Pad, $=)].

%% --------------------------------------------------------------------
%% zbase
encode_zbase(<<>>, Acc) -> lists:reverse(Acc);
encode_zbase(<<A:1>>, Acc) ->
    <<A1:5>> = <<A:1, 0:4>>,
    encode_zbase(<<>>, [element(A1 + 1, ?B32Z_ALPHABET) | Acc]);
encode_zbase(<<A:2>>, Acc) ->
    <<A1:5>> = <<A:2, 0:3>>,
    encode_zbase(<<>>, [element(A1 + 1, ?B32Z_ALPHABET) | Acc]);
encode_zbase(<<A:3>>, Acc) ->
    <<A1:5>> = <<A:3, 0:2>>,
    encode_zbase(<<>>, [element(A1 + 1, ?B32Z_ALPHABET) | Acc]);
encode_zbase(<<A:4>>, Acc) ->
    <<A1:5>> = <<A:4, 0:1>>,
    encode_zbase(<<>>, [element(A1 + 1, ?B32Z_ALPHABET) | Acc]);
encode_zbase(<<A:5, T/bits>>, Acc) ->
    encode_zbase(T, [element(A + 1, ?B32Z_ALPHABET) | Acc]).

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

e_b45(X) -> element(X, ?B45_ALPHABET).

%% --------------------------------------------------------------------
%% b85
encode_b85(<<>>, Acc) -> lists:reverse(Acc);
encode_b85(<<0:32, T/binary>>, Acc) ->
    encode_b85(T, [$z | Acc]);
encode_b85(<<A, B, C, D, T/binary>>, Acc) ->
    V = (((((A * 256) + B) * 256) + C) * 256) + D,
    Es = [(V div Div) rem 85 + 33 || Div <- ?Z85_85],
    encode_b85(T, [Es | Acc]).

%% --------------------------------------------------------------------
%% Z85
encode_z85(<<>>, Acc) -> lists:reverse(Acc);
encode_z85(<<A, B, C, D, T/binary>>, Acc) ->
    V = (((((A * 256) + B) * 256) + C) * 256) + D,
    Es = [element((V div Div) rem 85 + 1, ?Z85_ALPHABET) || Div <- ?Z85_85],
    encode_z85(T, [Es | Acc]).

%% --------------------------------------------------------------------
%% Decode
%% --------------------------------------------------------------------

do_decode(32, B, #opts{algo = crockford}) ->
    decode_b32(crockford_filtermap(B, <<>>), ?B32CROCKFORD_DECODE, []);
do_decode(32, B, #opts{algo = clockwork}) ->
    decode_b32(clockwork_filtermap(B, <<>>), ?B32CROCKFORD_DECODE, []);
do_decode(32, B, #opts{algo = zbase}) ->
    decode_zbase(B, <<>>);
do_decode(32, B, #opts{alphabet = standard}) ->
    decode_b32(B, ?B32_DECODE, []);
do_decode(32, B, #opts{alphabet = hex}) ->
    decode_b32(B, ?B32HEX_DECODE, []);
do_decode(32, B, #opts{alphabet = geohash}) ->
    decode_b32(B, ?B32GEO_DECODE, []);
do_decode(45, B45, _) ->
    decode_b45(B45, []);
do_decode(85, Z85, #opts{algo = z85}) when (byte_size(Z85) rem 5) == 0 ->
    decode_z85(Z85, []);
do_decode(85, B85, _) when (byte_size(B85) rem 5) == 0 ->
    decode_b85(remove_whitespace(B85, <<>>), []).

%% --------------------------------------------------------------------
%% b32
decode_b32(<<>>, _, Acc) -> lists:reverse(Acc);
%% With padding 
decode_b32(<<A, B, $=, $=, $=,$=, $=, $=>>, Alphabet, Acc) ->
    <<B1:3, 0:2>> = <<(element(B, Alphabet)):5>>,
    decode_b32(<<>>, Alphabet, [d_b32(Alphabet, [A], <<B1:3>>) | Acc]);
decode_b32(<<A, B, C, D, $=, $=, $=, $=>>, Alphabet, Acc) ->
    <<D1:1, 0:4>> = <<(element(D, Alphabet)):5>>,
    decode_b32(<<>>, Alphabet, [d_b32(Alphabet, [A, B, C], <<D1:1>>) | Acc]);
decode_b32(<<A, B, C, D, E, $=, $=, $=>>, Alphabet, Acc) ->
    <<E1:4, 0:1>> = <<(element(E, Alphabet)):5>>,
    decode_b32(<<>>, Alphabet, [d_b32(Alphabet, [A, B, C, D], <<E1:4>>) | Acc]);
decode_b32(<<A, B, C, D, E, F, G, $=>>, Alphabet, Acc) ->
    <<G1:2, 0:3>> = <<(element(G, Alphabet)):5>>,
    Elt = d_b32(Alphabet, [A, B, C, D, E, F], <<G1:2>>),
    decode_b32(<<>>, Alphabet, [Elt | Acc]);
%% Without padding
decode_b32(<<A, B>>, Alphabet, Acc) ->
    <<B1:3, 0:2>> = <<(element(B, Alphabet)):5>>,
    decode_b32(<<>>, Alphabet, [d_b32(Alphabet, [A], <<B1:3>>) | Acc]);
decode_b32(<<A, B, C, D>>, Alphabet, Acc) ->
    <<D1:1, 0:4>> = <<(element(D, Alphabet)):5>>,
    decode_b32(<<>>, Alphabet, [d_b32(Alphabet, [A, B, C], <<D1:1>>) | Acc]);
decode_b32(<<A, B, C, D, E>>, Alphabet, Acc) ->
    <<E1:4, 0:1>> = <<(element(E, Alphabet)):5>>,
    decode_b32(<<>>, Alphabet, [d_b32(Alphabet, [A, B, C, D], <<E1:4>>) | Acc]);
decode_b32(<<A, B, C, D, E, F, G>>, Alphabet, Acc) ->
    <<G1:2, 0:3>> = <<(element(G, Alphabet)):5>>,
    Elt = d_b32(Alphabet, [A, B, C, D, E, F], <<G1:2>>),
    decode_b32(<<>>, Alphabet, [Elt | Acc]);
%% Recursive
decode_b32(<<A, B, C, D, E, F, G, H, T/binary>>, Alphabet, Acc) ->
    Elt = d_b32(Alphabet, [A, B, C, D, E, F, G, H], <<>>),
    decode_b32(T, Alphabet, [Elt | Acc]).

d_b32(Alphabet, Parts, End) ->
    << (<< <<(element(X, Alphabet)):5>> || X <- Parts>>)/bits, End/bits>>.

%% --------------------------------------------------------------------
%% crockford

crockford_filtermap(<<>>, Acc) -> Acc;
%%  Filter
crockford_filtermap(<<$-, T/binary>>, Acc) -> crockford_filtermap(T, Acc);
%%  Map
%%  -> 0
crockford_filtermap(<<$0, T/binary>>, Acc) ->
    crockford_filtermap(T, <<Acc/binary, $0>>);
crockford_filtermap(<<$O, T/binary>>, Acc) ->
    crockford_filtermap(T, <<Acc/binary, $0>>);
crockford_filtermap(<<$o, T/binary>>, Acc) ->
    crockford_filtermap(T, <<Acc/binary, $0>>);
%%  -> 1
crockford_filtermap(<<$1, T/binary>>, Acc) ->
    crockford_filtermap(T, <<Acc/binary, $1>>);
crockford_filtermap(<<$I, T/binary>>, Acc) ->
    crockford_filtermap(T, <<Acc/binary, $1>>);
crockford_filtermap(<<$L, T/binary>>, Acc) ->
    crockford_filtermap(T, <<Acc/binary, $1>>);
crockford_filtermap(<<$i, T/binary>>, Acc) ->
    crockford_filtermap(T, <<Acc/binary, $1>>);
crockford_filtermap(<<$l, T/binary>>, Acc) ->
    crockford_filtermap(T, <<Acc/binary, $1>>);
%% -> Uppercase
crockford_filtermap(<<H, T/binary>>, Acc)
  when H >= 50, H =< 57; H >= 65, H =< 90 ->
    crockford_filtermap(T, <<Acc/binary, H>>);
crockford_filtermap(<<H, T/binary>>, Acc) when H >= 97, H =< 122 ->
    crockford_filtermap(T, <<Acc/binary, (H - 32)>>).

%% --------------------------------------------------------------------
%% clockwork

clockwork_filtermap(<<>>, Acc) -> Acc;
clockwork_filtermap(<<$0>>, Acc) -> Acc;
%%  Map
%%  -> 0
clockwork_filtermap(<<$0, T/binary>>, Acc) ->
    clockwork_filtermap(T, <<Acc/binary, $0>>);
clockwork_filtermap(<<$O, T/binary>>, Acc) ->
    clockwork_filtermap(T, <<Acc/binary, $0>>);
clockwork_filtermap(<<$o, T/binary>>, Acc) ->
    clockwork_filtermap(T, <<Acc/binary, $0>>);
%%  -> 1
clockwork_filtermap(<<$1, T/binary>>, Acc) ->
    clockwork_filtermap(T, <<Acc/binary, $1>>);
clockwork_filtermap(<<$I, T/binary>>, Acc) ->
    clockwork_filtermap(T, <<Acc/binary, $1>>);
clockwork_filtermap(<<$L, T/binary>>, Acc) ->
    clockwork_filtermap(T, <<Acc/binary, $1>>);
clockwork_filtermap(<<$i, T/binary>>, Acc) ->
    clockwork_filtermap(T, <<Acc/binary, $1>>);
clockwork_filtermap(<<$l, T/binary>>, Acc) ->
    clockwork_filtermap(T, <<Acc/binary, $1>>);
%% -> Uppercase
clockwork_filtermap(<<H, T/binary>>, Acc)
  when H >= 50, H =< 57; H >= 65, H =< 90 ->
    clockwork_filtermap(T, <<Acc/binary, H>>);
clockwork_filtermap(<<H, T/binary>>, Acc) when H >= 97, H =< 122 ->
    clockwork_filtermap(T, <<Acc/binary, (H - 32)>>).

%% --------------------------------------------------------------------
%% zbase
decode_zbase(<<>>, Acc) -> Acc;
decode_zbase(<<A, T/bits>>, Acc) ->
    <<_:3, A1:5>> = <<(element(A, ?B32Z_DECODE)):8>>,
    decode_zbase(T, <<Acc/bits, A1:5>>).

%% --------------------------------------------------------------------
%% B45
decode_b45(<<>>, Acc) -> lists:reverse(Acc);
decode_b45(<<C, D>>, Acc) ->
    decode_b45(<<>>, [d_b45(C) + 45 * d_b45(D) | Acc]);
decode_b45(<<C, D, E, T/binary>>, Acc) ->
    N = d_b45(C) + 45 * (d_b45(D) + 45 * d_b45(E)),
    <<A, B>> = <<N:16>>,
    decode_b45(T, [[A, B] | Acc]).

d_b45(C) -> element(C - 31, ?B45_DECODE).

%% --------------------------------------------------------------------
%% B85
decode_b85(<<>>, Acc) -> lists:reverse(Acc);
decode_b85(<<$z, T/binary>>, Acc) ->
    decode_b85(T, [<<0:32>> | T]);
decode_b85(<<A, B, C, D, E, T/binary>>, Acc) ->
    V = lists:foldl(fun d_b85/2, 0, [A, B, C, D, E]),
    decode_b85(T, [[(V div Div) rem 256 || Div <- ?Z85_256] | Acc]).

d_b85(V, Pre) -> (Pre * 85) + V - 33.

remove_whitespace(B) -> B.

%% --------------------------------------------------------------------
%% Z85
decode_z85(<<>>, Acc) -> lists:reverse(Acc);
decode_z85(<<A, B, C, D, E, T/binary>>, Acc) ->
    V = lists:foldl(fun d_z85/2, 0, [A, B, C, D, E]),
    decode_z85(T, [[(V div Div) rem 256 || Div <- ?Z85_256] | Acc]).

d_z85(V, Pre) -> (Pre * 85) + element(V - 31, ?Z85_DECODE).

%% --------------------------------------------------------------------
