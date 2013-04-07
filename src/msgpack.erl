%%==============================================================================
%% Copyright 2013 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  msgpack  : object | array
%%%  map      : {[{msgpack, msgpack}*]}
%%%  array    : [msgpack*]
%%%  integers : integer | {pos_fixnum, integer} | {neg_fixnum, integer} |
%%%             {uint8, integer} | {uint16, integer} | {uint32, integer} |
%%%             {uint64, integer} | {int8, integer} | {int16, integer} |
%%%             {int632, integer} | {int64, integer}
%%%  float    : float | {'float', float} | {'double', float}
%%%  nil      : []
%%%  boolean  : 'true' | 'false'
%%%  Raw bytes: binary
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(msgpack).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2
        ]).

%% Types
-type opt()          :: {atom_strings, boolean()} | binary | iolist.


-type json()        :: json_text().
-type json_text()   :: json_object() | json_array().
-type json_object() :: {[{json_string(), json_value()}]}.
-type json_array()  :: [json_value()].
-type json_value()  :: false | true | null |
                       number() | json_string() |
                       json_object() | json_array().
-type json_string() :: atom() | string().

%% Records
-record(opts, {return_type = iolist :: iolist | binary,
               orig_call
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
-define(RAW16, 16#da).
-define(RAW32, 16#db).
-define(ARRAY16, 16#dc).
-define(ARRAY32, 16#dd).
-define(MAP16, 16#de).
-define(MAP32, 16#df).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec encode(json()) -> iolist().
%%--------------------------------------------------------------------
encode(Term) -> encode(Term, #opts{orig_call = {encode, [Term], ?LINE}}).

%%--------------------------------------------------------------------
%% Function: encode(, ) -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec encode(json(), [opt()]) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Term, Opts = #opts{}) ->
    encode_msgpack(Term, Opts);
encode(Term, Opts) -> Line = ?LINE,
    #opts{return_type = Return} = ParsedOpts =
        parse_opts(Opts, #opts{orig_call = {encode, [Term, Opts], Line}}),
    case Return of
        iolist -> encode_msgpack(Term, ParsedOpts);
        binary -> iolist_to_binary(encode_msgpack(Term, ParsedOpts))
    end.

%%--------------------------------------------------------------------
%% Function: decode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> json().
%%--------------------------------------------------------------------
decode(Binary) -> Line = ?LINE,
    decode(Binary, #opts{orig_call = {decode, [Binary], Line}}).

%%--------------------------------------------------------------------
%% Function: decode(, ) -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), [opt()]) -> json().
%%--------------------------------------------------------------------
decode(Binary, Opts = #opts{}) -> decode_msgpack(Binary, Opts);
decode(Binary, Opts) -> Line = ?LINE,
    OptsRec = parse_opts(Opts,
                         #opts{orig_call = {decode, [Binary, Opts], Line}}),
    decode_msgpack(Binary, OptsRec).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

encode_msgpack({[]}, _) -> nyi;
encode_msgpack([], _) -> nyi;
encode_msgpack(Integer, Opts) when is_integer(Integer) ->
    encode_integer(Integer, Opts);
encode_msgpack(Float, _) when is_float(Float)-> nyi;
encode_msgpack(nil, _) -> nyi;
encode_msgpack(Bool, _) when is_boolean(Bool) -> nyi;
encode_msgpack(Raw, _) when is_binary(Raw) -> nyi.

encode_integer(I, _) when I >= ?POS_FIXNUM_MIN, I =< ?POS_FIXNUM_MAX ->
    <<?POS_FIXNUM, I:7>>;
encode_integer(I, _) when I >= ?NEG_FIXNUM_MIN , I =< ?NEG_FIXNUM_MAX ->
    <<?NEG_FIXNUM, I:5>>;
encode_integer(I, _) when I > ?POS_FIXNUM_MAX, I =< ?UINT8_MAX ->
    <<?UINT8, I>>;
encode_integer(I, _) when I > ?UINT8_MAX, I =< ?UINT16_MAX ->
    <<?UINT16, I>>;
encode_integer(I, _) when I > ?UINT16_MAX, I =< ?UINT32_MAX ->
    <<?UINT32, I>>;
encode_integer(I, _) when I > ?UINT32_MAX, I =< ?UINT64_MAX ->
    <<?UINT64, I>>;
encode_integer(I, _) when I < ?NEG_FIXNUM_MIN, I >= ?INT8_MIN ->
    <<?INT8, I/signed>>;
encode_integer(I, _) when I < ?INT8_MIN, I >= ?INT16_MIN ->
    <<?INT16, I/signed>>;
encode_integer(I, _) when I < ?INT16_MIN, I >= ?INT32_MIN ->
    <<?INT32, I/signed>>;
encode_integer(I, _) when I < ?INT32_MIN, I >= ?INT64_MIN ->
    <<?INT64, I/signed>>.

%% ===================================================================
%% Decoding
%% ===================================================================

decode_msgpack(_, _) -> nyi.


%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(_, Rec) -> badarg(Rec).

badarg(#opts{orig_call = {Funcion, Args, Line}}) ->
    Trace = [{?MODULE, Funcion, Args, [{file, ?FILE}, {line, Line}]} |
             lists:dropwhile(fun(T) -> element(1, T) == ?MODULE end,
                             erlang:get_stacktrace())],
    exit({badarg, Trace}).
