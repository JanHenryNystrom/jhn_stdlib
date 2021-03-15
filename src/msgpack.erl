%%==============================================================================
%% Copyright 2013-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A MessagePack to and from erlang terms library based on
%%%  http://wiki.msgpack.org/display/MSGPACK/Format+specification.
%%%
%%%  MessagePack is represented as follows:
%%%
%%%  msgpack  : map | array | integers | floats | boolean | raw
%%%  map      : {[{msgpack, msgpack}*]}
%%%  array    : [msgpack*]
%%%  integers : integer | {pos_fixnum, integer} | {neg_fixnum, integer} |
%%%             {uint8, integer} | {uint16, integer} | {uint32, integer} |
%%%             {uint64, integer} | {int8, integer} | {int16, integer} |
%%%             {int632, integer} | {int64, integer}
%%%  floats   : float | {'float', float} | {'double', float}
%%%  nil      : nil
%%%  boolean  : 'true' | 'false'
%%%  raw      : binary
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(msgpack).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2
        ]).

%% Types
-type opt() :: {atom_strings, boolean()} | binary | iolist.

-type msgpack()       :: msgpack_map() | msgpack_array() |
                         integers() | floats() | boolean() | raw().
-type msgpack_map()   :: {[msgpack()]}.
-type msgpack_array() :: [msgpack()].
-type integers()      :: integer() | {integer_type(), integer()}.
-type integer_type()  :: pos_fixnum | neg_fixnum |
                         uint8 | uint16 | uint32 | uint64 |
                         int8 | int16 | int32 | int64.
-type floats()        :: float() | {float_type(), float()}.
-type float_type()    :: float | double.
-type raw()           :: binary().

%% Records
-record(opts, {return_type = iolist :: iolist | binary,
               number_types = false :: boolean()
              }).

%% Defines

-define(POS_FIXNUM_MIN, 0).
-define(POS_FIXNUM_MAX, 127).
-define(NEG_FIXNUM_MIN, -32).
-define(NEG_FIXNUM_MAX, -1).
-define(UINT8_MAX, 255).
-define(UINT16_MAX, 65535).
-define(UINT32_MAX, 4294967295).
-define(UINT64_MAX, 18446744073709551615).
-define(INT8_MIN, -128).
-define(INT16_MIN, -32768).
-define(INT32_MIN, -2147483648).
-define(INT64_MIN, -9223372036854775808).
-define(INT8_MAX, 127).
-define(INT16_MAX, 32767).
-define(INT32_MAX, 2147483647).
-define(INT64_MAX, 9223372036854775807).


-define(POS_FIXNUM, 0:1).
-define(NEG_FIXNUM, 7:3).
-define(NIL, 16#c0).
-define(FALSE, 16#c2).
-define(TRUE, 16#c3).
-define(FLOAT, 16#ca).
-define(DOUBLE, 16#cb).
-define(UINT8, 16#cc).
-define(UINT16, 16#cd).
-define(UINT32, 16#ce).
-define(UINT64, 16#cf).
-define(INT8, 16#d0).
-define(INT16, 16#d1).
-define(INT32, 16#d2).
-define(INT64, 16#d3).
-define(FIX_RAW, 5:3).
-define(RAW16, 16#da).
-define(RAW32, 16#db).
-define(FIX_ARRAY, 9:4).
-define(ARRAY16, 16#dc).
-define(ARRAY32, 16#dd).
-define(FIX_MAP, 8:4).
-define(MAP16, 16#de).
-define(MAP32, 16#df).

-define(FLOAT_TYPES, [float,double]).
-define(INTEGER_TYPES, [pos_fixnum, neg_fixnum,
                        uint8, uint16, uint32, uint64,
                        int8, int16, int32, int64
                       ]).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Term) -> MSGPACK.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent of encode(Term, []) -> MSGPACK.
%% @end
%%--------------------------------------------------------------------
-spec encode(msgpack()) -> iolist().
%%--------------------------------------------------------------------
encode(Term) -> encode(Term, #opts{}).

%%--------------------------------------------------------------------
%% Function: encode(Term, Options) -> MSGPACK.
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     iolist -> a iolist is returned
%% @end
%%--------------------------------------------------------------------
-spec encode(msgpack(), [opt()] | #opts{}) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Term, Opts = #opts{}) ->
    encode_msgpack(Term, Opts);
encode(Term, Opts) ->
    #opts{return_type = Return} = ParsedOpts = parse_opts(Opts, #opts{}),
    case Return of
        iolist -> encode_msgpack(Term, ParsedOpts);
        binary -> iolist_to_binary(encode_msgpack(Term, ParsedOpts))
    end.

%%--------------------------------------------------------------------
%% Function: decode(MSGPACK) -> Term.
%% @doc
%%   Decodes the binary into a structured Erlang term.
%%   Equivalent of decode(MSGPACK, []) -> Term.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> msgpack().
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, #opts{}).

%%--------------------------------------------------------------------
%% Function: decode(MSGPACK, Options) -> Term.
%% @doc
%%   Decodes the binary into a structured Erlang.
%%   Decode will give an exception if the binary is not well formed MSGPACK.
%%   Options are:
%%     number_types -> decode into {Type, Value} for number types.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), [opt()] | #opts{}) -> msgpack().
%%--------------------------------------------------------------------
decode(Binary, Opts = #opts{}) ->
    {Decoded, _Rest} = decode_msgpack(Binary, Opts),
    Decoded;
decode(Binary, Opts) ->
    OptsRec = parse_opts(Opts, #opts{}),
    {Decoded, _Rest} = decode_msgpack(Binary, OptsRec),
    Decoded.

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

encode_msgpack({Map}, Opts) when is_list(Map) -> encode_map(Map, Opts);
encode_msgpack(Array, Opts) when is_list(Array) -> encode_array(Array, Opts);
encode_msgpack(Integer, Opts) when is_integer(Integer) ->
    encode_integer(Integer, Opts);
encode_msgpack(Float, Opts) when is_float(Float)-> encode_float(Float, Opts);
encode_msgpack(nil, _) -> <<?NIL>>;
encode_msgpack(true, _) -> <<?TRUE>>;
encode_msgpack(false, _) -> <<?FALSE>>;
encode_msgpack(Raw, _) when is_binary(Raw) -> encode_raw(Raw);
encode_msgpack(X = {Type, _}, Opts) when is_atom(Type) ->
    case lists:member(Type, ?FLOAT_TYPES) of
        true -> encode_float(X, Opts);
        false ->
            case lists:member(Type, ?INTEGER_TYPES) of
                true -> encode_integer(X, Opts);
                false -> erlang:error(badarg)
            end
    end;
encode_msgpack(_, _) ->
    erlang:error(badarg).

encode_integer({pos_fixnum, I}, _)
  when I >= ?POS_FIXNUM_MIN, I =< ?POS_FIXNUM_MAX ->
    <<?POS_FIXNUM, I:7>>;
encode_integer({neg_fixnum, I}, _)
  when I >= ?NEG_FIXNUM_MIN , I =< ?NEG_FIXNUM_MAX ->
    <<?NEG_FIXNUM, I:5>>;
encode_integer({uint8, I}, _) when I >= 0, I =< ?UINT8_MAX ->
    <<?UINT8, I>>;
encode_integer({uint16, I}, _) when I >= 0, I =< ?UINT16_MAX ->
    <<?UINT16, I:16>>;
encode_integer({uint32, I}, _) when I >= 0, I =< ?UINT32_MAX ->
    <<?UINT32, I:32>>;
encode_integer({uint64, I}, _) when I >= 0, I =< ?UINT64_MAX ->
    <<?UINT64, I:64>>;
encode_integer({int8, I}, _) when I >= ?INT8_MIN, I =< ?INT8_MAX ->
    <<?INT8, I/signed>>;
encode_integer({int16, I}, _) when I >= ?INT16_MIN, I =< ?INT16_MAX ->
    <<?INT16, I:16/signed>>;
encode_integer({int32, I}, _) when I >= ?INT32_MIN, I =< ?INT32_MAX ->
    <<?INT32, I:32/signed>>;
encode_integer({int64, I}, _) when I >= ?INT64_MIN, I =< ?INT64_MAX ->
    <<?INT64, I:64/signed>>;
encode_integer(I, _) when I >= ?POS_FIXNUM_MIN, I =< ?POS_FIXNUM_MAX ->
    <<?POS_FIXNUM, I:7>>;
encode_integer(I, _) when I >= ?NEG_FIXNUM_MIN , I =< ?NEG_FIXNUM_MAX ->
    <<?NEG_FIXNUM, I:5>>;
encode_integer(I, _) when I > ?POS_FIXNUM_MAX, I =< ?UINT8_MAX ->
    <<?UINT8, I>>;
encode_integer(I, _) when I > ?UINT8_MAX, I =< ?UINT16_MAX ->
    <<?UINT16, I:16>>;
encode_integer(I, _) when I > ?UINT16_MAX, I =< ?UINT32_MAX ->
    <<?UINT32, I:32>>;
encode_integer(I, _) when I > ?UINT32_MAX, I =< ?UINT64_MAX ->
    <<?UINT64, I:64>>;
encode_integer(I, _) when I < ?NEG_FIXNUM_MIN, I >= ?INT8_MIN ->
    <<?INT8, I/signed>>;
encode_integer(I, _) when I < ?INT8_MIN, I >= ?INT16_MIN ->
    <<?INT16, I:16/signed>>;
encode_integer(I, _) when I < ?INT16_MIN, I >= ?INT32_MIN ->
    <<?INT32, I:32/signed>>;
encode_integer(I, _) when I < ?INT32_MIN, I >= ?INT64_MIN ->
    <<?INT64, I:64/signed>>;
encode_integer(_, _) ->
    erlang:error(badarg).

encode_float({float, Float}, _) -> <<?FLOAT, Float:32/float>>;
encode_float({double, Float}, _) -> <<?DOUBLE, Float:64/float>>;
encode_float(Float, _) -> <<?DOUBLE, Float:64/float>>.

encode_raw(Raw) ->
    case byte_size(Raw) of
        Size when Size < 32 -> [<<?FIX_RAW, Size:5>>, Raw];
        Size when Size =< ?UINT16_MAX -> [<<?RAW16, Size:16>>, Raw];
        Size when Size =< ?UINT32_MAX -> [<<?RAW32, Size:32>>, Raw];
        _ -> erlang:error(badarg)
    end.

encode_array(Array, Opts) ->
    Tag = case length(Array) of
              L  when L < 16 -> <<?FIX_ARRAY, L:4>>;
              L when L =< ?UINT16_MAX -> <<?ARRAY16, L:16>>;
              L when L =< ?UINT32_MAX -> <<?ARRAY32, L:32>>;
              _ -> erlang:error(badarg)
          end,
    [Tag | [encode_msgpack(Elt, Opts) || Elt <- Array]].

encode_map(Map, Opts) ->
    Tag = case length(Map) of
              L  when L < 16 -> <<?FIX_MAP, L:4>>;
              L when L =< ?UINT16_MAX -> <<?MAP16, L:16>>;
              L when L =< ?UINT32_MAX -> <<?MAP32, L:32>>;
              _ -> erlang:error(badarg)
          end,
    [Tag |
     [[encode_msgpack(Key, Opts), encode_msgpack(Value, Opts)] ||
         {Key, Value} <- Map]].

%% ===================================================================
%% Decoding
%% ===================================================================

decode_msgpack(<<?POS_FIXNUM, I:7, T/binary>>, #opts{number_types = true}) ->
    {{pos_fixnum, I}, T};
decode_msgpack(<<?POS_FIXNUM, I:7, T/binary>>, _) ->
    {I, T};
decode_msgpack(<<?NEG_FIXNUM, I:5, T/binary>>, #opts{number_types = true}) ->
    {{neg_fixnum, I + ?NEG_FIXNUM_MIN}, T};
decode_msgpack(<<?NEG_FIXNUM, I:5, T/binary>>, _) ->
    {I + ?NEG_FIXNUM_MIN, T};
decode_msgpack(<<?UINT8, I, T/binary>>, #opts{number_types = true}) ->
    {{uint8, I}, T};
decode_msgpack(<<?UINT8, I, T/binary>>, _) ->
    {I, T};
decode_msgpack(<<?UINT16, I:16, T/binary>>, #opts{number_types = true}) ->
    {{uint16, I}, T};
decode_msgpack(<<?UINT16, I:16, T/binary>>, _) ->
    {I, T};
decode_msgpack(<<?UINT32, I:32, T/binary>>, #opts{number_types = true}) ->
    {{uint32, I}, T};
decode_msgpack(<<?UINT32, I:32, T/binary>>, _) ->
    {I, T};
decode_msgpack(<<?UINT64, I:64, T/binary>>, #opts{number_types = true}) ->
    {{uint64, I}, T};
decode_msgpack(<<?UINT64, I:64, T/binary>>, _) ->
    {I, T};
decode_msgpack(<<?INT8, I/signed, T/binary>>, #opts{number_types = true}) ->
    {{int8, I}, T};
decode_msgpack(<<?INT8, I/signed, T/binary>>, _) ->
    {I, T};
decode_msgpack(<<?INT16, I:16/signed, T/binary>>, #opts{number_types = true}) ->
    {{int16, I}, T};
decode_msgpack(<<?INT16, I:16/signed, T/binary>>, _) ->
    {I, T};
decode_msgpack(<<?INT32, I:32/signed, T/binary>>, #opts{number_types = true}) ->
    {{int32, I}, T};
decode_msgpack(<<?INT32, I:32/signed, T/binary>>, _) ->
    {I, T};
decode_msgpack(<<?INT64, I:64/signed, T/binary>>, #opts{number_types = true}) ->
    {{int64, I}, T};
decode_msgpack(<<?INT64, I:64/signed, T/binary>>, _) ->
    {I, T};
decode_msgpack(<<?FLOAT, F:32/float, T/binary>>, #opts{number_types = true}) ->
    {{float, F}, T};
decode_msgpack(<<?FLOAT, F:32/float, T/binary>>, _) ->
    {F, T};
decode_msgpack(<<?DOUBLE, F:64/float, T/binary>>, #opts{number_types = true}) ->
    {{double, F}, T};
decode_msgpack(<<?DOUBLE, F:64/float, T/binary>>, _) ->
    {F, T};
decode_msgpack(<<?NIL, T/binary>>, _) -> {nil, T};
decode_msgpack(<<?TRUE, T/binary>>, _) -> {true, T};
decode_msgpack(<<?FALSE, T/binary>>, _) -> {false, T};
decode_msgpack(<<?FIX_RAW, Size:5, Raw:Size/bytes, T/binary>>, _) ->
    {Raw, T};
decode_msgpack(<<?RAW16, Size:16, Raw:Size/bytes, T/binary>>, _) ->
    {Raw, T};
decode_msgpack(<<?RAW32, Size:32, Raw:Size/bytes, T/binary>>, _) ->
    {Raw, T};
decode_msgpack(<<?FIX_ARRAY, L:4, T/binary>>, Opts) ->
    decode_array(L, T, [], Opts);
decode_msgpack(<<?ARRAY16, L:16, T/binary>>, Opts) ->
    decode_array(L, T, [], Opts);
decode_msgpack(<<?ARRAY32, L:32, T/binary>>, Opts) ->
    decode_array(L, T, [], Opts);
decode_msgpack(<<?FIX_MAP, L:4, T/binary>>, Opts) ->
    decode_map(L, T, [], Opts);
decode_msgpack(<<?MAP16, L:16, T/binary>>, Opts) ->
    decode_map(L, T, [], Opts);
decode_msgpack(<<?MAP32, L:32, T/binary>>, Opts) ->
    decode_map(L, T, [], Opts);
decode_msgpack(_, _) ->
    erlang:error(badarg).


decode_array(0, T, Acc, _) -> {lists:reverse(Acc), T};
decode_array(N, Binary, Acc, Opts) ->
    {H, T} = decode_msgpack(Binary, Opts),
    decode_array(N - 1, T, [H | Acc], Opts).

decode_map(0, T, Acc, _) -> {{lists:reverse(Acc)}, T};
decode_map(N, Binary, Acc, Opts) ->
    {Key, T} = decode_msgpack(Binary, Opts),
    {Value, T1} = decode_msgpack(T, Opts),
    decode_map(N - 1, T1, [{Key, Value} | Acc], Opts).


%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(number_types, Opts) -> Opts#opts{number_types = true};
parse_opt(_, _) -> erlang:error(badarg).
