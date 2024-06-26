%%==============================================================================
%% Copyright 2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_cbor).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1
        ]).


%% Exported types
-export_type([cbor/0]).

%% Types
-type cbor()      :: true | false | null |
                     number() | cstring() |
                     object() | array().

-type cstring()   :: binary().
-type object()    :: #{integer() => cbor()}.
-type array()     :: [cbor()].

-type opt()       :: binary | iolist.

%% Defines
-define(MAJOR0, 0).
-define(MAJOR1, 1).
-define(MAJOR2, 2).
-define(MAJOR3, 3).
-define(MAJOR4, 4).
-define(MAJOR5, 5).
-define(MAJOR6, 6).
-define(MAJOR7, 7).


-define(MAX_8BYTE, 16#FFFFFFFFFFFFFFFF).
-define(MAX_4BYTE, 16#FFFFFFFF).
-define(MAX_2BYTE, 16#FFFF).
-define(MAX_1BYTE, 16#FF).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Term) -> CBOR.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent of encode(Term, iolist) -> CBOR.
%% @end
%%--------------------------------------------------------------------
-spec encode(cbor()) -> iodata().
%%--------------------------------------------------------------------
%% Defined Simple values
encode(false) -> <<?MAJOR7:3, 20:5>>;
encode(true) -> <<?MAJOR7:3, 21:5>>;
encode(null) -> <<?MAJOR7:3, 22:5>>;
encode(undefined) -> <<?MAJOR7:3, 23:5>>;
%% Integers
encode(I) when is_integer(I), I > ?MAX_4BYTE  -> <<?MAJOR0:3, 27:5, I:64>>;
encode(I) when is_integer(I), I > ?MAX_2BYTE  -> <<?MAJOR0:3, 26:5, I:32>>;
encode(I) when is_integer(I), I > ?MAX_1BYTE  -> <<?MAJOR0:3, 25:5, I:16>>;
encode(I) when is_integer(I), I > 23  -> <<?MAJOR0:3, 24:5, I:8>>;
encode(I) when is_integer(I), I >= 0 -> <<?MAJOR0:3, I:5>>;
encode(I) when is_integer(I), I > -25 -> <<?MAJOR1:3, (-1-I):5>>;
encode(I) when is_integer(I), I > -?MAX_1BYTE -> <<?MAJOR1:3, 24:5, (-1-I):8>>;
encode(I) when is_integer(I), I > -?MAX_2BYTE -> <<?MAJOR1:5, 25:5, (-1-I):16>>;
encode(I) when is_integer(I), I > -?MAX_4BYTE -> <<?MAJOR1:3, 26:5, (-1-I):32>>;
encode(I) when is_integer(I), I > -?MAX_8BYTE -> <<?MAJOR1:3, 27:5, (-1-I):32>>;
%%
encode({string, S}) ->
    case byte_size(S) of
        Size when Size < 24 -> <<?MAJOR2:3, Size:5, S/binary>>;
        Size when Size =< ?MAX_1BYTE -> <<?MAJOR2:3, 24:5, Size:8, S/binary>>;
        Size when Size =< ?MAX_2BYTE -> <<?MAJOR2:3, 25:5, Size:16, S/binary>>;
        Size when Size =< ?MAX_4BYTE -> <<?MAJOR2:3, 26:5, Size:32, S/binary>>;
        Size when Size =< ?MAX_8BYTE -> <<?MAJOR2:3, 27:5, Size:54, S/binary>>
    end;
encode({text, T}) ->
    case byte_size(T) of
        Size when Size < 24 -> <<?MAJOR3:3, Size:5, T/binary>>;
        Size when Size =< ?MAX_1BYTE -> <<?MAJOR3:3, 24:5, Size:8, T/binary>>;
        Size when Size =< ?MAX_2BYTE -> <<?MAJOR3:3, 25:5, Size:16, T/binary>>;
        Size when Size =< ?MAX_4BYTE -> <<?MAJOR3:3, 26:5, Size:32, T/binary>>;
        Size when Size =< ?MAX_8BYTE -> <<?MAJOR3:3, 27:5, Size:54, T/binary>>
    end;
encode(Array) when is_list(Array) ->
    Head = case length(Array) of
               Size when Size < 24 -> <<?MAJOR4:3, Size:5>>;
               Size when Size =< ?MAX_1BYTE -> <<?MAJOR4:3, 24:5, Size:8>>;
               Size when Size =< ?MAX_2BYTE -> <<?MAJOR4:3, 25:5, Size:16>>;
               Size when Size =< ?MAX_4BYTE -> <<?MAJOR4:3, 26:5, Size:32>>;
               Size when Size =< ?MAX_8BYTE -> <<?MAJOR4:3, 27:5, Size:54>>
           end,
    [Head | [encode(E) || E <- Array]];
encode(Map) when is_map(Map) ->
    Head = case maps:size(Map) of
               Size when Size < 24 -> <<?MAJOR5:3, Size:5>>;
               Size when Size =< ?MAX_1BYTE -> <<?MAJOR5:3, 24:5, Size:8>>;
               Size when Size =< ?MAX_2BYTE -> <<?MAJOR5:3, 25:5, Size:16>>;
               Size when Size =< ?MAX_4BYTE -> <<?MAJOR5:3, 26:5, Size:32>>;
               Size when Size =< ?MAX_8BYTE -> <<?MAJOR5:3, 27:5, Size:54>>
           end,
    [Head|maps:fold(fun(K, V, A) -> [[encode(K), encode(V)] | A] end, [], Map)];
encode({tag, I, Val}) when I > ?MAX_4BYTE  ->
    [<<?MAJOR6:3, 27:5, I:64>>, encode(Val)];
encode({tag, I, Val}) when I > ?MAX_2BYTE  ->
    [<<?MAJOR6:3, 26:5, I:32>>, encode(Val)];
encode({tag, I, Val}) when I > ?MAX_1BYTE  ->
    [<<?MAJOR6:3, 25:5, I:16>>, encode(Val)];
encode({tag, I, Val}) when I > 23  ->
    [<<?MAJOR6:3, 24:5, I:8>>, encode(Val)];
encode({tag, I, Val}) when I >= 0 ->
    [<<?MAJOR6:3, I:5>>, encode(Val)].

%%--------------------------------------------------------------------
%% Function: encode(Term, [Option]) -> CBOR.
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     iolist -> an iolist is returned (default)
%% @end
%%--------------------------------------------------------------------
-spec encode(cbor(), [opt()]) -> binary().
%%--------------------------------------------------------------------
encode(T, [binary]) ->  iolist_to_binary(encode(T));
encode(T, _) -> encode(T).

%%--------------------------------------------------------------------
%% Function: decode(CBOR) -> {Term, Binary}
%% @doc
%%   Decodes the binary into a tuple of structured Erlang term and the
%%   remaining binary or a continuation if the binary did not contain
%%   a complete CBOR value. The continuation can be used by decode/2 with
%%   a binary containing the rest of the CBOR value to decode.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> {cbor(), binary()}.
%%--------------------------------------------------------------------
%% Positive number
decode(<<?MAJOR0:3, N:5, T/binary>>) when N < 24 -> {N, T};
decode(<<?MAJOR0:3, 24:5, I:8, T/binary>>) -> {I, T};
decode(<<?MAJOR0:3, 25:5, I:16, T/binary>>) -> {I, T};
decode(<<?MAJOR0:3, 26:5, I:32, T/binary>>) -> {I, T};
decode(<<?MAJOR0:3, 27:5, I:64, T/binary>>) -> {I, T};
%% Negative number
decode(<<?MAJOR1:3, I:5, T/binary>>) when I < 24 -> {-1 - I, T};
decode(<<?MAJOR1:3, 24:5, I:8, T/binary>>) -> {-1 - I, T};
decode(<<?MAJOR1:3, 25:5, I:16, T/binary>>) -> {-1 - I, T};
decode(<<?MAJOR1:3, 26:5, I:32, T/binary>>) -> {-1 - I, T};
decode(<<?MAJOR1:3, 27:5, I:64, T/binary>>) -> {-1 - I, T};
%% byte string
decode(<<?MAJOR2:3, I:5, S:I/binary, T/binary>>) when I < 24 -> {{string, S},T};
decode(<<?MAJOR2:3, 24:5, I:8, S:I/binary, T/binary>>) -> {{string, S}, T};
decode(<<?MAJOR2:3, 25:5, I:16, S:I/binary, T/binary>>) -> {{string, S}, T};
decode(<<?MAJOR2:3, 26:5, I:32, S:I/binary, T/binary>>) -> {{string, S}, T};
decode(<<?MAJOR2:3, 27:5, I:64, S:I/binary, T/binary>>) -> {{string, S}, T};
decode(<<?MAJOR2:3, 31:5, T/binary>>) -> decode_string(T, []);
%% text string
decode(<<?MAJOR3:3, I:5, S:I/binary, T/binary>>) when I < 24 -> {{text, S}, T};
decode(<<?MAJOR3:3, 24:5, I:8, S:I/binary, T/binary>>) -> {{text, S}, T};
decode(<<?MAJOR3:3, 25:5, I:16, S:I/binary, T/binary>>) -> {{text, S}, T};
decode(<<?MAJOR3:3, 26:5, I:32, S:I/binary, T/binary>>) -> {{text, S}, T};
decode(<<?MAJOR3:3, 27:5, I:64, S:I/binary, T/binary>>) -> {{text, S}, T};
decode(<<?MAJOR3:3, 31:5, T/binary>>) -> decode_text(T, []);
%% array
decode(<<?MAJOR4:3, I:5, T/binary>>) when I < 24 -> decode_array(I, T, []);
decode(<<?MAJOR4:3, 24:5, I:8, T/binary>>) -> decode_array(I, T, []);
decode(<<?MAJOR4:3, 25:5, I:16, T/binary>>) -> decode_array(I, T, []);
decode(<<?MAJOR4:3, 26:5, I:32, T/binary>>) -> decode_array(I, T, []);
decode(<<?MAJOR4:3, 27:5, I:64, T/binary>>) -> decode_array(I, T, []);
decode(<<?MAJOR4:3, 31:5, T/binary>>) -> decode_array(T, []);
%% map
decode(<<?MAJOR5:3, I:5, T/binary>>) when I < 24 -> decode_map(I, T, #{});
decode(<<?MAJOR5:3, 24:5, I:8, T/binary>>) -> decode_map(I, T, #{});
decode(<<?MAJOR5:3, 25:5, I:16, T/binary>>) -> decode_map(I, T, #{});
decode(<<?MAJOR5:3, 26:5, I:32, T/binary>>) -> decode_map(I, T, #{});
decode(<<?MAJOR5:3, 27:5, I:64, T/binary>>) -> decode_map(I, T, #{});
decode(<<?MAJOR5:3, 31:5, T/binary>>) -> decode_map(T, #{});
%% tagged item
decode(<<?MAJOR6:3, I:5, T/binary>>) when I < 24 -> decode_tag(I, T);
decode(<<?MAJOR6:3, 24:5, I:8, T/binary>>) -> decode_tag(I, T);
decode(<<?MAJOR6:3, 25:5, I:16, T/binary>>) -> decode_tag(I, T);
decode(<<?MAJOR6:3, 26:5, I:32, T/binary>>) -> decode_tag(I, T);
decode(<<?MAJOR6:3, 27:5, I:64, T/binary>>) -> decode_tag(I, T);
%% Simple Value
decode(<<?MAJOR7:3, I:5, T/binary>>) when I <  20 -> {{simple, I}, T};
decode(<<?MAJOR7:3, 20:5, T/binary>>) -> {false, T};
decode(<<?MAJOR7:3, 21:5, T/binary>>) -> {true, T};
decode(<<?MAJOR7:3, 22:5, T/binary>>) -> {null, T};
decode(<<?MAJOR7:3, 23:5, T/binary>>) -> {undefined, T};
decode(<<?MAJOR7:3, 24:5, I:8, T/binary>>) when I >= 32 -> {{simple, I}, T};
%% Float
%% 16 bit
decode(<<?MAJOR7:3, 25:5, 0:1, 31:5, 0:10, T/binary>>) -> {inf, T};
decode(<<?MAJOR7:3, 25:5, 1:1, 31:5, 0:10, T/binary>>) -> {neg_inf, T};
decode(<<?MAJOR7:3, 25:5, _:1, 31:5, _:10, T/binary>>) -> {nan, T};
decode(<<?MAJOR7:3, 25:5, _:1, 0:5, 0:10, T/binary>>) -> {0.0, T};
decode(<<?MAJOR7:3, 25:5, 0:1, E:5, M:10, T/binary>>) -> {decode_f16(E, M), T};
decode(<<?MAJOR7:3, 25:5, 1:1, E:5, M:10, T/binary>>) -> {-decode_f16(E, M), T};
%% 32 bit
decode(<<?MAJOR7:3, 26:5, 0:1, 255:8, 0:23, T/binary>>) -> {inf, T};
decode(<<?MAJOR7:3, 26:5, 1:1, 255:8, 0:23, T/binary>>) -> {neg_inf, T};
decode(<<?MAJOR7:3, 26:5, _:1, 255:8, _:23, T/binary>>) -> {nan, T};
decode(<<?MAJOR7:3, 26:5, F:32/float, T/binary>>) -> {F, T};
%% 64 bit
decode(<<?MAJOR7:3, 27:5, 0:1, 2047:11, 0:52, T/binary>>) -> {inf, T};
decode(<<?MAJOR7:3, 27:5, 1:1, 2047:11, 0:52, T/binary>>) -> {neg_inf, T};
decode(<<?MAJOR7:3, 27:5, _:1, 2047:11, _:52, T/binary>>) -> {nan, T};
decode(<<?MAJOR7:3, 27:5, F:64/float, T/binary>>) -> {F, T}.

%% ===================================================================
%% Encoding
%% ===================================================================

%% ===================================================================
%% Decoding
%% ===================================================================

decode_string(<<?MAJOR7:3, 31:5, T/binary>>, Acc) ->
    {{string, iolist_to_binary(lists:reverse(Acc))}, T};
decode_string(B = <<?MAJOR2:3, N:5, _/binary>>, Acc) when N < 28 ->
    {H, T} = decode(B),
    decode_string(T, [H | Acc]).

decode_text(<<?MAJOR7:3, 31:5, T/binary>>, Acc) ->
    {{text, iolist_to_binary(lists:reverse(Acc))}, T};
decode_text(B = <<?MAJOR3:3, N:5, _/binary>>, Acc) when N < 28 ->
    {H, T} = decode(B),
    decode_text(T, [H | Acc]).

decode_array(0, T, Acc) -> {lists:reverse(Acc), T};
decode_array(N, T, Acc) ->
    {H, T1} = decode(T),
    decode_array(N - 1, T1, [H | Acc]).

decode_array(<<?MAJOR7:3, 31:5, T/binary>>, Acc) -> {lists:reverse(Acc), T};
decode_array(T, Acc) ->
    {H, T1} = decode(T),
    decode_array(T1, [H | Acc]).

decode_map(0, T, Acc) -> {Acc, T};
decode_map(N, T, Acc) ->
    {Key, T1} = decode(T),
    {Val, T2} = decode(T1),
    decode_map(N - 1, T2, Acc#{Key => Val}).

decode_map(<<?MAJOR7:3, 31:5, T/binary>>, Acc) -> {Acc, T};
decode_map(T, Acc) ->
    {Key, T1} = decode(T),
    {Val, T2} = decode(T1),
    decode_map(T2, Acc#{Key => Val}).

decode_tag(N, T) ->
    {Val, T1} = decode(T),
    {{tag, 0, Val}, T1}.

decode_f16(0, Mant) -> Mant / (1 bsl 24);
decode_f16(Exp, Mant) when Exp < 25 -> (1024 + Mant)/(1 bsl (25 - Exp));
decode_f16(25, Mant) -> 1024 + Mant;
decode_f16(Exp, Mant) -> (1024 + Mant) * (1 bsl (Exp - 25)).

%% f64_f16(F) when is_float(F) -> f32_f16(<<F:32/float>>).

%% f64_f32(F64) when is_float(F64) -> <<F32:32/float>> = <<F64:32/float>>, F32.

%% f32_f16(<<S:1, 0:8, _:23>>) -> <<S:1, 0:5, 0:10>>;
%% f32_f16(<<S:1, 255:8, 0:23>>) -> <<S:1, 31:5, 0:10>>;
%% f32_f16(<<S:1, 255:8, _:23>>) -> <<S:1, 31:5, 512:10>>;
%% f32_f16(<<S:1, E:8, _:23>>) when E > 143 -> <<S:1, 31:5, 0:10>>;
%% f32_f16(<<S:1, E:8, M:23>>) when E > 112 -> <<S:1, (E - 112):5, (M bsr 13):10>>;
%% f32_f16(<<S:1, E:8, M:23>>) when E > 102 -> <<S:1, 0:5, (M bor 16#800000):10>>;
%% f32_f16(<<S:1, _:8, _:23>>) -> <<S:1, 0:5, 0:10>>.
