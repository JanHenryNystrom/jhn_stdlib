%%==============================================================================
%% Copyright 2021-2026 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A CBOR library based on:
%%%
%%%  Concise Binary Object Representation (CBOR)                       (rfc8949)
%%%
%%%  IANA Considerations and IETF Protocol and
%%%  Documentation Usage for IEEE 802 Parameters                       (rfc9542)
%%%
%%%  Supported tags:
%%%
%%%  rfc8949: 0, 1, 2, 3, 4, 5, 24, 32, 33, 34, 55799
%%%
%%%  rfc9542: 48
%%%
%%%  Tags unlikely to ever be supported: 21, 22, 23
%%%
%%%  CBOR cbor() values are represented as follows:
%%%
%%%  Simple values:
%%%
%%%  False(20)     : false
%%%
%%%  True(21)      : true
%%%
%%%  Null(22)      : null
%%%
%%%  Undefined(23) : undefined
%%%
%%%  (Integer)     : {simple, integer()}
%%%
%%%  Numbers:
%%%
%%%  Integer : integer()
%%%
%%%  Float   : float() | {float16, float()} |
%%%            {float32, float()} | {float64, float()} |
%%%            inf | neg_inf | nan.
%%%
%%%  Bistrings:
%%%
%%%  String                   : {string, binary()}
%%%
%%%  indefinite length String : {strings, [{string, binary()}]}
%%%
%%%  Text                     : binary()
%%%
%%%  indefinite length Texts  : {texts, [binary()]}
%%%
%%%  Collections:
%%%
%%%  Map                   : #{cbor() => cbor()}
%%%
%%%  indefinite length Map : {map, [#{cbor() => cbor()}]}
%%%
%%%  Array                 : [cbor()]
%%%
%%%  Arrays                : {arrays, [cbor()]}
%%%
%%%  Tags:
%%%
%%%  timestamp(0)        : {tag, timestamp, integer() | binary()}
%%%
%%%  posix(1)            : {tag, posix, integer() | float() | null | undefined}
%%%
%%%  bignum(2)           : {tag, bignum, non_neg_integer()}
%%%
%%%  bignum(3)           : {tag, bignum, neg_integer()}
%%%
%%%  decimal_fraction(4) : {tag, decimal_fraction, {integer(), integer()}}
%%%
%%%  bigfloat(5)         : {tag, bigfloat, {integer(), integer()}
%%%
%%%  embedded(24)        : {tag, embedded, binary()}
%%%
%%%  uri(32)             : {tag, uri, #uri{} | binary()}
%%%
%%%  base64url(33)       : {tag, base64url, binary()}
%%%
%%%  base64(34)          : {tag, based, binary()}
%%%
%%%  mac(48)             : {tag, mac, binary()}
%%%
%%%  cbor(55799)         : {tag, cbor, cbor()}
%%%
%%%  (Integer)           : {tag, integer(), _}
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2021-2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_cbor).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2
        ]).


%% Include
-include_lib("jhn_stdlib/include/uri.hrl").

%% Exported types
-export_type([cbor/0,
              simple/0, cfloat/0,
              cstring/0, ctext/0,
              array/0, cmap/0,
              tag/0]).

%% Types
-type cbor()        :: simple() |
                       integer() | float() | cfloat() |
                       cstring() | ctext() |
                       cmap() | array() | tag().

-type simple()      :: true | false | null | undefined | {simple, integer()}.
-type cfloat()      :: {float16, float()} |
                       {float32, float()} |
                       {float64, float()}|
                       inf | neg_inf | nan.
-type cstring()     :: {string, binary()} | {strings, [{string, binary()}]}.
-type ctext()       :: binary() | {texts, [binary()]}.
-type cmap()        :: #{cbor() => cbor()} | {maps, [#{cbor() => cbor()}]}.
-type array()       :: [cbor()] | {arrays, [[cbor()]]}.
-type tag()         :: {tag, integer(), tag_content()} |
                       {tag, tag_alias(), tag_content()}.
-type tag_content() :: _.
-type tag_alias()   ::
        timestamp | %% 0 value integer (posix timestamp) or binary
        posix | %% 1 value integer, float, binary, null, or undefined
        bignum | %% 2, 3 value integer or negative integer
        decimal_fraction | %% 4 value {mantissa, scaling factor}
        bigfloat | %% 5 value {mantissa, scaling factor}
        embedded | %% 24 value CBOR
        uri |  %% 32 value jhn_uri:#uri{} or binary
        base64url | %% 33 value binary
        base64 | %% 34 value binary
        mac | %% 48 value binary, example <<"01-23-45-67-89-AB">> or
              %% <<"01-23-45-67-89-AB-FE-03">>
        cbor. %% 55799

-type opts()     :: [opt()].
-type opt()      :: binary | iolist | continue |
                    {tag_callbacks, [{integer(), atom()}]}.

%% Defines
-define(MAJOR0, 0).
-define(MAJOR1, 1).
-define(MAJOR2, 2).
-define(MAJOR3, 3).
-define(MAJOR4, 4).
-define(MAJOR5, 5).
-define(MAJOR6, 6).
-define(MAJOR7, 7).

-define(BREAK, ?MAJOR7:3, 31:5).

-define(MAX_16BYTE,16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
-define(MAX_8BYTE, 16#FFFFFFFFFFFFFFFF).
-define(MAX_4BYTE, 16#FFFFFFFF).
-define(MAX_2BYTE, 16#FFFF).
-define(MAX_1BYTE, 16#FF).

%% Records
-record(opts, {tag_callbacks = []     :: [{integer(), atom()}],
               return_type   = iolist :: iolist | binary,
               continue      = false  :: boolean()
              }).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Term) -> CBOR.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent of encode(Term, []) -> CBOR.
%% @end
%%--------------------------------------------------------------------
-spec encode(cbor()) -> iodata().
%%--------------------------------------------------------------------
encode(CBOR) -> encode(CBOR, []).

%%--------------------------------------------------------------------
%% Function: encode(Term, [Option]) -> CBOR.
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     iolist -> an iolist is returned (default)
%%     {tag_callbacks, [{integer(), nodule()}]} specifies
%%     a module with a cbor_encode_tag callback function to decode tags
%%     not supporrted in this module.
%% @end
%%--------------------------------------------------------------------
-spec encode(cbor(), opts()) -> binary().
%%--------------------------------------------------------------------
encode(T, Opts) ->
    case parse_opts(Opts) of
        OptsRec = #opts{return_type = iolist} -> do_encode(T, OptsRec);
        OptsRec = #opts{return_type = binary} ->
            iolist_to_binary(do_encode(T, OptsRec))
    end.

%%--------------------------------------------------------------------
%% Function: decode(CBOR) -> {Term, Binary}
%% @doc
%%   Decodes the binary into a tuple of structured Erlang term.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> {cbor(), binary()}.
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, []).

%%--------------------------------------------------------------------
%% Function: decode(CBOR) -> {Term, Binary}
%% @doc
%%   Decodes the binary into a structured Erlang term or a tuple of a
%%   structured Erlang term and the rest of the binary if the continue
%%   option is given.
%%   A {tag_callbacks, [{integer(), nodule()}]} can supplied to specify
%%   a module with a cbor_decode_tag callback function to decode tags
%%   not supporrted in this module.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), opts()) -> {cbor(), binary()}.
%%--------------------------------------------------------------------
decode(Binary, Opts) ->
    case parse_opts(Opts) of
        OptsRec = #opts{continue = true} ->
            do_decode(Binary, OptsRec);
        OptsRec ->
            {CBOR, _} = do_decode(Binary, OptsRec),
            CBOR
    end.

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

%% Defined Simple values
do_encode(false, _) -> <<?MAJOR7:3, 20:5>>;
do_encode(true, _) -> <<?MAJOR7:3, 21:5>>;
do_encode(null, _) -> <<?MAJOR7:3, 22:5>>;
do_encode(undefined, _) -> <<?MAJOR7:3, 23:5>>;
do_encode({simple, I}, _) when is_integer(I), I =< 20 -> <<?MAJOR7:3, I:5>>;
do_encode({simple, I}, _) when is_integer(I), I >= 32, I =< 255 ->
    <<?MAJOR7:3, 24:5, I:8>>;
%% Integers
do_encode(I, Opts) when is_integer(I), I > ?MAX_8BYTE  ->
    do_encode({tag, bignum, I}, Opts);
do_encode(I, _) when is_integer(I), I > ?MAX_4BYTE  ->
    <<?MAJOR0:3, 27:5, I:64>>;
do_encode(I, _) when is_integer(I), I > ?MAX_2BYTE  ->
    <<?MAJOR0:3, 26:5, I:32>>;
do_encode(I, _) when is_integer(I), I > ?MAX_1BYTE  ->
    <<?MAJOR0:3, 25:5, I:16>>;
do_encode(I, _) when is_integer(I), I > 23  ->
    <<?MAJOR0:3, 24:5, I:8>>;
do_encode(I, _) when is_integer(I), I >= 0 ->
    <<?MAJOR0:3, I:5>>;
do_encode(I, _) when is_integer(I), I > -25 ->
    <<?MAJOR1:3, (-1-I):5>>;
do_encode(I, _) when is_integer(I), I > -?MAX_1BYTE ->
 <<?MAJOR1:3, 24:5, (-1-I):8>>;
do_encode(I, _) when is_integer(I), I > -?MAX_2BYTE ->
    <<?MAJOR1:3, 25:5, (-1-I):16>>;
do_encode(I, _) when is_integer(I), I > -?MAX_4BYTE ->
    <<?MAJOR1:3, 26:5, (-1-I):32>>;
do_encode(I, _) when is_integer(I), I >= -?MAX_8BYTE ->
    <<?MAJOR1:3, 27:5, (-1-I):64>>;
do_encode(I, Opts) when is_integer(I) ->
    do_encode({tag, bignum, I}, Opts);
%% Floats
do_encode(inf, _) -> <<?MAJOR7:3, 25:5, 0:1, 31:5, 0:10>>;
do_encode(neg_inf, _) -> <<?MAJOR7:3, 25:5, 1:1, 31:5, 0:10>>;
do_encode(nan, _) ->  <<?MAJOR7:3, 25:5, 0:1, 31:5, 1:10>>;
do_encode({float16, F}, _) when is_float(F) -> <<?MAJOR7:3, 25:5, F:16/float>>;
do_encode({float16, inf}, _) -> <<?MAJOR7:3, 25:5, 0:1, 31:5, 0:10>>;
do_encode({float16, neg_inf}, _) -> <<?MAJOR7:3, 25:5, 1:1, 31:5, 0:10>>;
do_encode({float16, nan}, _) ->  <<?MAJOR7:3, 25:5, 0:1, 31:5, 1:10>>;
do_encode({float32, F}, _) when is_float(F) -> <<?MAJOR7:3, 26:5, F:32/float>>;
do_encode({float32, inf}, _) -> <<?MAJOR7:3, 26:5, 0:1, 255:8, 0:23>>;
do_encode({float32, neg_inf}, _) -> <<?MAJOR7:3, 26:5, 1:1, 255:8, 0:23>>;
do_encode({float32, nan}, _) ->  <<?MAJOR7:3, 26:5, 0:1, 255:8, 1:23>>;
do_encode({float64, F}, _) when is_float(F) -> <<?MAJOR7:3, 27:5, F:64/float>>;
do_encode({float64, inf}, _) -> <<?MAJOR7:3, 27:5, 0:1, 2047:11, 0:52>>;
do_encode({float64, neg_inf}, _) -> <<?MAJOR7:3, 27:5, 1:1, 2047:11, 0:52>>;
do_encode({float64, nan}, _) ->  <<?MAJOR7:3, 27:5, 0:1, 2047:11, 1:52>>;
do_encode(F, _) when is_float(F) -> <<?MAJOR7:3, 27:5, F:64/float>>;
%% Byte String
do_encode({string, S}, _) when is_binary(S) ->
    case byte_size(S) of
        Size when Size < 24 -> <<?MAJOR2:3, Size:5, S/binary>>;
        Size when Size =< ?MAX_1BYTE -> <<?MAJOR2:3, 24:5, Size:8, S/binary>>;
        Size when Size =< ?MAX_2BYTE -> <<?MAJOR2:3, 25:5, Size:16, S/binary>>;
        Size when Size =< ?MAX_4BYTE -> <<?MAJOR2:3, 26:5, Size:32, S/binary>>;
        Size when Size =< ?MAX_8BYTE -> <<?MAJOR2:3, 27:5, Size:54, S/binary>>
    end;
%% Byte Strings indefinite length
do_encode({strings, Strings}, Opts) ->
    [<<?MAJOR2:3, 31:5>>,
     [do_encode(String, Opts) || String <- Strings],
     <<?BREAK>>];
%% Text String
do_encode(T, _) when is_binary(T) ->
    case byte_size(T) of
        Size when Size < 24 -> <<?MAJOR3:3, Size:5, T/binary>>;
        Size when Size =< ?MAX_1BYTE -> <<?MAJOR3:3, 24:5, Size:8, T/binary>>;
        Size when Size =< ?MAX_2BYTE -> <<?MAJOR3:3, 25:5, Size:16, T/binary>>;
        Size when Size =< ?MAX_4BYTE -> <<?MAJOR3:3, 26:5, Size:32, T/binary>>;
        Size when Size =< ?MAX_8BYTE -> <<?MAJOR3:3, 27:5, Size:54, T/binary>>
    end;
%% Text Strings indefinite length
do_encode({texts, Texts}, Opts) ->
    [<<?MAJOR3:3, 31:5>>, [do_encode(Text, Opts) || Text <- Texts], <<?BREAK>>];
%% Array
do_encode(Array, Opts) when is_list(Array) ->
    Head = case length(Array) of
               Size when Size < 24 -> <<?MAJOR4:3, Size:5>>;
               Size when Size =< ?MAX_1BYTE -> <<?MAJOR4:3, 24:5, Size:8>>;
               Size when Size =< ?MAX_2BYTE -> <<?MAJOR4:3, 25:5, Size:16>>;
               Size when Size =< ?MAX_4BYTE -> <<?MAJOR4:3, 26:5, Size:32>>;
               Size when Size =< ?MAX_8BYTE -> <<?MAJOR4:3, 27:5, Size:54>>
           end,
    [Head | [do_encode(E, Opts) || E <- Array]];
%% Array indefinite length
do_encode({arrays, Arrays}, Opts) ->
    [<<?MAJOR4:3, 31:5>>,
     [do_encode(Array, Opts) || Array <- Arrays],
     <<?BREAK>>];
%% Map
do_encode(Map, Opts) when is_map(Map) ->
    Head = case maps:size(Map) of
               Size when Size < 24 -> <<?MAJOR5:3, Size:5>>;
               Size when Size =< ?MAX_1BYTE -> <<?MAJOR5:3, 24:5, Size:8>>;
               Size when Size =< ?MAX_2BYTE -> <<?MAJOR5:3, 25:5, Size:16>>;
               Size when Size =< ?MAX_4BYTE -> <<?MAJOR5:3, 26:5, Size:32>>;
               Size when Size =< ?MAX_8BYTE -> <<?MAJOR5:3, 27:5, Size:54>>
           end,
    [Head | maps:fold(fun(K, V, A) ->
                              [[do_encode(K, Opts), do_encode(V, Opts)] | A]
                      end,
                      [],
                      Map)];
%% Map indefinite length
do_encode({maps, Maps}, Opts) ->
    [<<?MAJOR5:3, 31:5>>, [do_encode(Map, Opts) || Map <- Maps], <<?BREAK>>];
%% Tags
do_encode(Tag = {tag, _, _}, Opts) ->
    encode_tag(Tag, Opts).

%% Tags Named
encode_tag({tag, bignum, BN}, Opts) when BN >= 0 -> encode_tag({tag,2,BN},Opts);
encode_tag({tag, bignum, BN}, Opts) -> encode_tag({tag, 3, BN}, Opts);
encode_tag({tag, Name, V}, Opts) when is_atom(Name) ->
    encode_tag({tag, tag_no(Name), V}, Opts);
%% Tags
encode_tag({tag, 0, TS}, Opts) when is_binary(TS) ->
    [<<?MAJOR6:3, 0:5>>, do_encode(TS, Opts)];
encode_tag({tag, 0, TS}, Opts) when is_integer(TS) ->
    [<<?MAJOR6:3, 0:5>>, do_encode(jhn_timestamp:encode(TS, [binary]), Opts)];
encode_tag({tag, 1, null}, Opts) ->
    [<<?MAJOR6:3, 1:5>>, do_encode(null, Opts)];
encode_tag({tag, 1, undefined}, Opts) ->
    [<<?MAJOR6:3, 1:5>>, do_encode(undefined, Opts)];
encode_tag({tag, 1, Posix}, Opts) when is_integer(Posix) ->
    [<<?MAJOR6:3, 1:5>>, do_encode(Posix, Opts)];
encode_tag({tag, 1, Posix}, Opts) when is_float(Posix) ->
    [<<?MAJOR6:3, 1:5>>, do_encode(Posix, Opts)];
encode_tag({tag, 1, TS}, Opts) when is_binary(TS) ->
    Posix = jhn_timestamp:encode(jhn_timestamp:decode(TS), [posix]),
    [<<?MAJOR6:3, 1:5>>, do_encode(Posix, Opts)];
encode_tag({tag, 2, Bignum}, Opts) ->
    [<<?MAJOR6:3, 2:5>>, do_encode({string, encode_bignum(Bignum)}, Opts)];
encode_tag({tag, 3, Bignum}, Opts) ->
    [<<?MAJOR6:3, 3:5>>, do_encode({string, encode_bignum(-1 -Bignum)}, Opts)];
encode_tag({tag, 4, {Mantissa, ScalingFactor}}, Opts) ->
    [<<?MAJOR6:3, 4:5>>, do_encode([Mantissa, ScalingFactor], Opts)];
encode_tag({tag, 5, {Mantissa, ScalingFactor}}, Opts) ->
    [<<?MAJOR6:3, 5:5>>, do_encode([Mantissa, ScalingFactor], Opts)];
encode_tag({tag, 24, CBOR}, Opts) ->
    [<<?MAJOR6:3, 24:5, 24:8>>,
     do_encode({string, encode(CBOR, [binary])}, Opts)];
encode_tag({tag, 32, URI = #uri{}}, Opts) ->
    [<<?MAJOR6:3, 24:5, 32:8>>, do_encode(jhn_uri:encode(URI, [binary]), Opts)];
encode_tag({tag, 32, URI}, Opts) when is_binary(URI) ->
    [<<?MAJOR6:3, 24:5, 32:8>>, do_encode(URI, Opts)];
encode_tag({tag, 33, Text}, Opts) when is_binary(Text) ->
    [<<?MAJOR6:3, 24:5, 33:8>>,
     do_encode(base64:encode(Text, #{mode => urlsafe}), Opts)];
encode_tag({tag, 34, Text}, Opts) when is_binary(Text) ->
    [<<?MAJOR6:3, 24:5, 34:8>>,
     do_encode(base64:encode(Text), Opts)];
encode_tag({tag, 48, Binary = <<_:17/binary>>}, Opts) ->
    <<M1:16, $-, M2:16, $-, M3:16, $-, M4:16, $-, M5:16, $-, M6:16>> = Binary,
    String = <<M1:16, M2:16, M3:16, M4:16, M5:16, M6:16>>,
    [<<?MAJOR6:3, 24:5, 48:8>>, do_encode({string, String}, Opts)];
encode_tag({tag, 48, Binary = <<_:23/binary>>}, Opts) ->
    <<M1:16, $-, M2:16, $-, M3:16, $-, M4:16, $-, M5:16, $-, M6:16,
      $-, M7:16, $-, M8:16>> = Binary,
    String = <<M1:16, M2:16, M3:16, M4:16, M5:16, M6:16, M7:16, M8:16>>,
    [<<?MAJOR6:3, 24:5, 48:8>>, do_encode({string, String}, Opts)];
encode_tag({tag, 55799, CBOR}, Opts) ->
    [<<?MAJOR6:3, 25:5, 55799:16>>, do_encode(CBOR, Opts)];
%% Tag general case
encode_tag({tag, I, Val}, Opts) when I > ?MAX_4BYTE  ->
    [<<?MAJOR6:3, 27:5, I:64>>, encode_content(I, Val, Opts)];
encode_tag({tag, I, Val}, Opts) when I > ?MAX_2BYTE  ->
    [<<?MAJOR6:3, 26:5, I:32>>, encode_content(I, Val, Opts)];
encode_tag({tag, I, Val}, Opts) when I > ?MAX_1BYTE  ->
    [<<?MAJOR6:3, 25:5, I:16>>, encode_content(I, Val, Opts)];
encode_tag({tag, I, Val}, Opts) when I > 23  ->
    [<<?MAJOR6:3, 24:5, I:8>>, encode_content(I, Val, Opts)];
encode_tag({tag, I, Val}, Opts) when I >= 0 ->
    [<<?MAJOR6:3, I:5>>, encode_content(I, Val, Opts)].

encode_content(N, Val, Opts = #opts{tag_callbacks = TCBMs}) ->
    case jhn_plist:find(N, TCBMs) of
        undefined -> do_encode(Val, Opts);
        CBM ->
            CBM:cbor_encode_tag(N, Val, Opts)
    end.

encode_bignum(0) -> <<0>>;
encode_bignum(N) -> <<N:(8 * (bytes(N, 0)))>>.

bytes(0, N) -> N;
bytes(X, N) -> bytes(X bsr 8, N + 1).

tag_no(timestamp) -> 0;
tag_no(posix) -> 1;
tag_no(decimal_fraction) -> 4;
tag_no(bigfloat) -> 5;
tag_no(embedded) -> 24;
tag_no(uri) -> 32;
tag_no(base64url) -> 33;
tag_no(base64) -> 34;
tag_no(mac) -> 48;
tag_no(cbor) -> 55799.

%% ===================================================================
%% Decoding
%% ===================================================================

%% Positive number
do_decode(<<?MAJOR0:3, N:5, T/binary>>, _) when N < 24 -> {N, T};
do_decode(<<?MAJOR0:3, 24:5, I:8, T/binary>>, _) -> {I, T};
do_decode(<<?MAJOR0:3, 25:5, I:16, T/binary>>, _) -> {I, T};
do_decode(<<?MAJOR0:3, 26:5, I:32, T/binary>>, _) -> {I, T};
do_decode(<<?MAJOR0:3, 27:5, I:64, T/binary>>, _) -> {I, T};
%% Negative number
do_decode(<<?MAJOR1:3, I:5, T/binary>>, _) when I < 24 -> {-1 - I, T};
do_decode(<<?MAJOR1:3, 24:5, I:8, T/binary>>, _) -> {-1 - I, T};
do_decode(<<?MAJOR1:3, 25:5, I:16, T/binary>>, _) -> {-1 - I, T};
do_decode(<<?MAJOR1:3, 26:5, I:32, T/binary>>, _) -> {-1 - I, T};
do_decode(<<?MAJOR1:3, 27:5, I:64, T/binary>>, _) -> {-1 - I, T};
%% byte string
do_decode(<<?MAJOR2:3, I:5, S:I/binary, T/binary>>, _) when I < 24 ->
    {{string, S},T};
do_decode(<<?MAJOR2:3, 24:5, I:8, S:I/binary, T/binary>>, _) ->
    {{string, S}, T};
do_decode(<<?MAJOR2:3, 25:5, I:16, S:I/binary, T/binary>>, _) ->
    {{string, S}, T};
do_decode(<<?MAJOR2:3, 26:5, I:32, S:I/binary, T/binary>>, _) ->
    {{string, S}, T};
do_decode(<<?MAJOR2:3, 27:5, I:64, S:I/binary, T/binary>>, _) ->
    {{string, S}, T};
do_decode(<<?MAJOR2:3, 31:5, T/binary>>, Opts) ->
    decode_string(T, Opts, []);
%% text string
do_decode(<<?MAJOR3:3, I:5, S:I/binary, T/binary>>, _) when I < 24 -> {S, T};
do_decode(<<?MAJOR3:3, 24:5, I:8, S:I/binary, T/binary>>, _) -> {S, T};
do_decode(<<?MAJOR3:3, 25:5, I:16, S:I/binary, T/binary>>, _) -> {S, T};
do_decode(<<?MAJOR3:3, 26:5, I:32, S:I/binary, T/binary>>, _) -> {S, T};
do_decode(<<?MAJOR3:3, 27:5, I:64, S:I/binary, T/binary>>, _) -> {S, T};
do_decode(<<?MAJOR3:3, 31:5, T/binary>>, Opts) -> decode_text(T, Opts, []);
%% array
do_decode(<<?MAJOR4:3, I:5, T/binary>>, Opts) when I < 24 ->
    decode_array(I, T, Opts, []);
do_decode(<<?MAJOR4:3, 24:5, I:8, T/binary>>, Opts) ->
    decode_array(I, T, Opts, []);
do_decode(<<?MAJOR4:3, 25:5, I:16, T/binary>>, Opts) ->
    decode_array(I, T, Opts, []);
do_decode(<<?MAJOR4:3, 26:5, I:32, T/binary>>, Opts) ->
    decode_array(I, T, Opts, []);
do_decode(<<?MAJOR4:3, 27:5, I:64, T/binary>>, Opts) ->
    decode_array(I, T, Opts, []);
do_decode(<<?MAJOR4:3, 31:5, T/binary>>, Opts) ->
    decode_array(T, Opts, []);
%% map
do_decode(<<?MAJOR5:3, I:5, T/binary>>, Opts) when I < 24 ->
    decode_map(I, T, Opts, #{});
do_decode(<<?MAJOR5:3, 24:5, I:8, T/binary>>, Opts) ->
    decode_map(I, T, Opts, #{});
do_decode(<<?MAJOR5:3, 25:5, I:16, T/binary>>, Opts) ->
    decode_map(I, T, Opts, #{});
do_decode(<<?MAJOR5:3, 26:5, I:32, T/binary>>, Opts) ->
    decode_map(I, T, Opts, #{});
do_decode(<<?MAJOR5:3, 27:5, I:64, T/binary>>, Opts) ->
    decode_map(I, T, Opts, #{});
do_decode(<<?MAJOR5:3, 31:5, T/binary>>, Opts) ->
    decode_map(T, Opts, []);
%% tagged item
do_decode(<<?MAJOR6:3, I:5,T/binary>>,Opts) when I < 24 -> decode_tag(I,T,Opts);
do_decode(<<?MAJOR6:3, 24:5, I:8, T/binary>>, Opts) -> decode_tag(I, T, Opts);
do_decode(<<?MAJOR6:3, 25:5, I:16, T/binary>>, Opts) -> decode_tag(I, T, Opts);
do_decode(<<?MAJOR6:3, 26:5, I:32, T/binary>>, Opts) -> decode_tag(I, T, Opts);
do_decode(<<?MAJOR6:3, 27:5, I:64, T/binary>>, Opts) -> decode_tag(I, T, Opts);
%% Simple Value
do_decode(<<?MAJOR7:3, I:5, T/binary>>, _) when I <  20 -> {{simple, I}, T};
do_decode(<<?MAJOR7:3, 20:5, T/binary>>, _) -> {false, T};
do_decode(<<?MAJOR7:3, 21:5, T/binary>>, _) -> {true, T};
do_decode(<<?MAJOR7:3, 22:5, T/binary>>, _) -> {null, T};
do_decode(<<?MAJOR7:3, 23:5, T/binary>>, _) -> {undefined, T};
do_decode(<<?MAJOR7:3, 24:5, I:8, T/binary>>, _) when I >= 32 -> {{simple,I},T};
%% Float
%% 16 bit
do_decode(<<?MAJOR7:3, 25:5, 0:1, 31:5, 0:10, T/binary>>, _) -> {inf, T};
do_decode(<<?MAJOR7:3, 25:5, 1:1, 31:5, 0:10, T/binary>>, _) -> {neg_inf, T};
do_decode(<<?MAJOR7:3, 25:5, _:1, 31:5, _:10, T/binary>>, _) -> {nan, T};
do_decode(<<?MAJOR7:3, 25:5, _:1, 0:5, 0:10, T/binary>>, _) -> {0.0, T};
do_decode(<<?MAJOR7:3, 25:5, 0:1, E:5, M:10, T/binary>>, _) ->
    {decode_f16(E, M), T};
do_decode(<<?MAJOR7:3, 25:5, 1:1, E:5, M:10, T/binary>>, _) ->
    {-decode_f16(E, M), T};
%% 32 bit
do_decode(<<?MAJOR7:3, 26:5, 0:1, 255:8, 0:23, T/binary>>, _) -> {inf, T};
do_decode(<<?MAJOR7:3, 26:5, 1:1, 255:8, 0:23, T/binary>>, _) -> {neg_inf,T};
do_decode(<<?MAJOR7:3, 26:5, _:1, 255:8, _:23, T/binary>>, _) -> {nan, T};
do_decode(<<?MAJOR7:3, 26:5, F:32/float, T/binary>>, _) -> {F, T};
%% 64 bit
do_decode(<<?MAJOR7:3, 27:5, 0:1, 2047:11, 0:52, T/binary>>, _) -> {inf, T};
do_decode(<<?MAJOR7:3, 27:5, 1:1, 2047:11, 0:52, T/binary>>, _) -> {neg_inf, T};
do_decode(<<?MAJOR7:3, 27:5, _:1, 2047:11, _:52, T/binary>>, _) -> {nan, T};
do_decode(<<?MAJOR7:3, 27:5, F:64/float, T/binary>>, _) -> {F, T}.

decode_string(<<?BREAK, T/binary>>, _, Acc) ->
    {{string, iolist_to_binary(lists:reverse(Acc))}, T};
decode_string(B = <<?MAJOR2:3, N:5, _/binary>>, Opts, Acc) when N < 28 ->
    {{string, H}, T} = do_decode(B, Opts),
    decode_string(T, Opts, [H | Acc]).

decode_text(<<?BREAK, T/binary>>, _, Acc) ->
    {{text, iolist_to_binary(lists:reverse(Acc))}, T};
decode_text(B = <<?MAJOR3:3, N:5, _/binary>>, Opts, Acc) when N < 28 ->
    {H, T} = do_decode(B, Opts),
    decode_text(T, Opts, [H | Acc]).

decode_array(0, T, _, Acc) -> {lists:reverse(Acc), T};
decode_array(N, T, Opts, Acc) ->
    {H, T1} = do_decode(T, Opts),
    decode_array(N - 1, T1, Opts, [H | Acc]).

decode_array(<<?BREAK, T/binary>>, _, Acc) -> {{arrays, lists:reverse(Acc)}, T};
decode_array(T, Opts, Acc) ->
    {H, T1} = do_decode(T, Opts),
    decode_array(T1, Opts, [H | Acc]).

decode_map(0, T, _, Acc) -> {Acc, T};
decode_map(N, T, Opts, Acc) ->
    {Key, T1} = do_decode(T, Opts),
    {Val, T2} = do_decode(T1, Opts),
    decode_map(N - 1, T2, Opts, Acc#{Key => Val}).

decode_map(<<?BREAK, T/binary>>, _, Acc) -> {{maps, lists:reverse(Acc)}, T};
decode_map(T, Opts, Acc) ->
    {Map, T1} = do_decode(T, Opts),
    decode_map(T1, Opts, [Map | Acc]).

%% Timestamp
decode_tag(0, T, Opts) ->
    {Text, T1} = do_decode(T, Opts),
    case jhn_timestamp:decode(Text) of
        _ -> {{tag, timestamp, Text}, T1}
    end;
%% Posix
decode_tag(1, T, Opts) ->
    case do_decode(T, Opts) of
        {null, T1} -> {{tag, posix, null}, T1};
        {undefined, T1} -> {{tag, posix, undefined}, T1};
        {I, T1} when is_integer(I) ->
            {{tag, posix, jhn_timestamp:encode(I, [binary])}, T1};
        {F, T1} when is_float(F) ->
            {{tag, posix, decode_float_ts(F)}, T1}
    end;
%% Pos bignum
decode_tag(2, T, Opts) ->
    {{string, String}, T1} = do_decode(T, Opts),
    <<N:(byte_size(String) * 8)/integer>> = String,
    {{tag, bignum, N}, T1};
%% Neg Bignum
decode_tag(3, T, Opts) ->
    {{string, String}, T1} = do_decode(T, Opts),
    <<N:(byte_size(String) * 8)/integer>> = String,
    {{tag, bignum, - 1 - N}, T1};
%% Decimal fraction
decode_tag(4, T, Opts) ->
    {[Mantissa, ScalingFactor], T1} = do_decode(T, Opts),
    {{tag, decimal_fraction, {Mantissa, ScalingFactor}}, T1};
%% Bigfloat
decode_tag(5, T, Opts) ->
    {[Mantissa, ScalingFactor], T1} = do_decode(T, Opts),
    {{tag, bigfloat, {Mantissa, ScalingFactor}}, T1};
%%  Embedded CBOR
decode_tag(24, T, Opts) ->
    {{string, String}, T1} = do_decode(T, Opts),
    {{tag, embedded, String}, T1};
%% URI
decode_tag(32, T, Opts) ->
    {URI, T1} = do_decode(T, Opts),
    {{tag, uri, jhn_uri:decode(URI)}, T1};
%% base64url
decode_tag(33, T, Opts) ->
    {Base, T1} = do_decode(T, Opts),
    {{tag, base64url, base64:decode(Base, #{mode => urlsafe})}, T1};
%% base64
decode_tag(34, T, Opts) ->
    {Base, T1} = do_decode(T, Opts),
    {{tag, base64, base64:decode(Base)}, T1};
%% MAC
decode_tag(48, T, Opts) ->
    {{string, MAC}, T1} = do_decode(T, Opts),
    {{tag, mac, jhn_bstring:join([<<N:16>> || <<N:16>> <= MAC], <<$->>)}, T1};
%% Marked as CBOR
decode_tag(55799, T, Opts) ->
    {CBOR, T1} = do_decode(T, Opts),
    {{tag, cbor, CBOR}, T1};
%%
decode_tag(N, T, Opts = #opts{tag_callbacks = TCBMs}) ->
    {Val, T1} = do_decode(T, Opts),
    case jhn_plist:find(N, TCBMs) of
        undefined -> {{tag, N, Val}, T1};
        CBM ->
            {{tag, N, CBM:cbor_decode_tag(N, Val, Opts)}, T1}
    end.

decode_f16(0, Mant) -> Mant / (1 bsl 24);
decode_f16(Exp, Mant) when Exp < 25 -> (1024 + Mant)/(1 bsl (25 - Exp));
decode_f16(25, Mant) -> 1024 + Mant;
decode_f16(Exp, Mant) -> (1024 + Mant) * (1 bsl (Exp - 25)).

decode_float_ts(F) ->
    I = trunc(F),
    TS = jhn_timestamp:encode(I, [binary]),
    case string:tokens(float_to_list(F, [short]), [$.]) of
        [_, "0"] -> TS;
        [_, Frac] ->
            TSM = jhn_timestamp:decode(TS),
            TSM1 = TSM#{fraction => list_to_integer(Frac)},
            Precision =
                case length(Frac) of
                    L when L < 4 -> milli;
                    L when L < 7 -> micro;
                    _ -> nano
                end,
            jhn_timestamp:encode(TSM1, [binary, Precision])
    end.

%% ===================================================================
%% Common
%% ===================================================================

parse_opts(Opts) -> lists:foldl(fun parse_opt/2, #opts{}, Opts).

parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(continue, Opts) -> Opts#opts{continue = true};
parse_opt({tag_callbacks, CBs}, Opts) -> Opts#opts{tag_callbacks = CBs}.
