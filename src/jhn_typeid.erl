%%==============================================================================
%% Copyright 2024-2026 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A UUID library based on:
%%%    TypeID Specification (Version 0.3.0)
%%%    https://github.com/jetify-com/typeid/tree/main/spec
%%%
%%%  A library for generating typeids.
%%%
%%%  Functions for encoding/decoding UUIDs are provided for completness and
%%%  testing purposes and the select few occations they are actually needed.
%%%
%%%  A decoded typeid is represented as a map:
%%%
%%%   * human (default for decode): UUID format with human readbable timestamp
%%%
%%%   `#{prefix => <<"oid">>,
%%%     uuid => #{timestamp => <<"2024-09-30T08:15:06.266Z">>,
%%%               version => 7,
%%%               random => 486766668076239088766}}'
%%%
%%%   * uuid: Standard encoded UUIDv7 format
%%%
%%%   `#{prefix => <<"oid">>,
%%%      uuid => <<"019241ff-581a-7069-a33e-999d70b4ec7e">>}'
%%%
%%%   * hex: Non-standard encoded UUIDv7 hex format without dashes
%%%
%%%   `#{prefix => <<"oid">>, uuid => <<"019241ff581a7069a33e999d70b4ec7e">>}'
%%%
%%% @end
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024-2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_typeid).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([gen/0, gen/1, gen/2,
         encode/1, encode/2,
         decode/1, decode/2,
         is_valid_prefix/1
        ]).

%% Exported types
-export_type([typeid/0]).

%% Records
-record(opts, {return_type = iolist :: return_type(),
               format      = human  :: format_opt()
              }).

%% Types
-type typeid()      :: #{prefix => atom() | list() | binary(),
                         uuid => binary()}.
-type return_type() :: iolist | binary | list | typeid.
-type format_opt()  :: hex | uuid | human.
-type prefix()      :: atom() | list() | binary().

%% Defines
-define(ALPHA(C), C >= $0, C=< $9; C >= $a, C =< $h; C == j;
                  C == k; C == m; C == n; C == p; C == q).

%% ===================================================================
%% Library functions.
%% ===================================================================


%%--------------------------------------------------------------------
%% Function: gen() -> TypeId.
%% @doc
%%   Generates the an TypeId as a binary.
%%
%%   Equivalent of `gen(<<>>)'.
%% @end
%%--------------------------------------------------------------------
-spec gen() -> binary().
%%--------------------------------------------------------------------
gen() -> gen(<<>>).

%%--------------------------------------------------------------------
%% Function: gen(Prefix) -> TypeId.
%% @doc
%%   Generates the an TypeId as an iolist.
%%
%%   Equivalent of gen(Prefix, []).
%% @end
%%--------------------------------------------------------------------
-spec gen(prefix()) -> binary().
%%--------------------------------------------------------------------
gen(Prefix) -> gen(Prefix, []).

%%--------------------------------------------------------------------
%% Function: gen(Prefix, Options) -> TypeId.
%% @doc
%%   Generates the an TypeId with the format determined by the Opts.
%%
%%   Options: iolist -> an iolist is returned
%%            list -> a list is returned
%%            typeid -> a typeid map is returned
%%
%% @end
%%--------------------------------------------------------------------
-spec gen(prefix(), [return_type()]) -> iolist() | list() | binary() | map().
%%--------------------------------------------------------------------
gen(Prefix, Opts) ->
    encode(#{prefix => Prefix, uuid => jhn_uuid:gen(v7, [binary, hex])}, Opts).

%%--------------------------------------------------------------------
%% Function: encode(TypeIdMap) -> TypeId.
%% @doc
%%   Encodes the an TypeId as an iolist.
%%
%%   Equivalent of encode(TypeIdMap).
%% @end
%%--------------------------------------------------------------------
-spec encode(map()) -> iolist().
%%--------------------------------------------------------------------
encode(Id) -> encode(Id, []).

%%--------------------------------------------------------------------
%% Function: encode(TypeIdMap, Opts) -> TypeId.
%% @doc
%%   Encodes the an TypeId with the format determined by the Opts.
%%
%%   Options: iolist -> an iolist is returned
%%            list -> a list is returned
%%            typeid -> a typeid map is returned
%% @end
%%--------------------------------------------------------------------
-spec encode(map(), [return_type()]) -> iolist() | list() | binary() | map().
%%--------------------------------------------------------------------
encode(Id = #{prefix := Prefix}, Opts) when is_atom(Prefix) ->
    encode(Id#{prefix => atom_to_binary(Prefix)}, Opts);
encode(Id = #{prefix := Prefix}, Opts) when is_list(Prefix) ->
    encode(Id#{prefix => unicode:characters_to_binary(Prefix)}, Opts);
encode(#{prefix := <<>>, uuid := UUID}, Opts) ->
    case parse_opts(Opts) of
        #opts{return_type = list} -> binary_to_list(base32_encode(UUID));
        #opts{return_type = typeid} -> #{prefix => <<>>, uuid => UUID};
        _ -> base32_encode(UUID)
    end;
encode(#{prefix := Prefix, uuid := UUID}, Opts) ->
    Id = [Prefix, $_, base32_encode(UUID)],
    case parse_opts(Opts) of
        #opts{return_type = iolist} -> Id;
        #opts{return_type = binary} -> iolist_to_binary(Id);
        #opts{return_type = list} -> binary_to_list(iolist_to_binary(Id));
        #opts{return_type = typeid} -> #{prefix => Prefix, uuid => UUID}
    end.

%%--------------------------------------------------------------------
%% Function: decode(Binary) -> TypeIdMap.
%% @doc
%%   Generates the an TypeId as an iolist.
%%
%%   Equivalent of decode(TypeIdMap, []).
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> map().
%%--------------------------------------------------------------------
decode(Id) -> decode(Id, []).

%%--------------------------------------------------------------------
%% Function: decode(Binary) -> TypeIdMap.
%% @doc
%%   Generates the an TypeId as an iolist.
%%
%%   Equivalent of decode(TypeIdMap, []).
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), [format_opt()]) -> map().
%%--------------------------------------------------------------------
decode(Id = <<$_, _/binary>>, _) -> erlang:error(badarg, [Id]);
decode(Id, Opts) -> do_decode(Id, parse_opts(Opts), 0, <<>>, <<>>).

%%--------------------------------------------------------------------
%% Function: decode(Prefix) -> Boolean.
%% @doc
%%   Verifies that a binary is a valid prefix for a typeid
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_prefix(binary()) -> boolean().
%%--------------------------------------------------------------------
is_valid_prefix(P) when byte_size(P) > 63 -> false;
is_valid_prefix(<<$_, _/binary>>) -> false;
is_valid_prefix(P) ->  valid_prefix(P).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

base32_encode(UUID = <<_:36/binary>>) ->
    HEX = << <<C>> || <<C>> <= UUID, C /= 45>>,
    Bits = <<(binary_to_integer(HEX, 16)):128>>,
    << <<(alpha(X))>> || <<X:5>> <= <<0:2, Bits/binary>> >>;
base32_encode(UUID = <<_:32/binary>>) ->
    Bits = <<(binary_to_integer(UUID, 16)):128>>,
    << <<(alpha(X))>> || <<X:5>> <= <<0:2, Bits/binary>> >>;
base32_encode(UUID = #{}) ->
    Hex = jhn_uuid:encode(UUID, [binary, hex]),
    Bits = <<(binary_to_integer(Hex, 16)):128>>,
    << <<(alpha(X))>> || <<X:5>> <= <<0:2, Bits/binary>> >>.

alpha(0) -> $0;
alpha(1) -> $1;
alpha(2) -> $2;
alpha(3) -> $3;
alpha(4) -> $4;
alpha(5) -> $5;
alpha(6) -> $6;
alpha(7) -> $7;
alpha(8) -> $8;
alpha(9) -> $9;
alpha(10) -> $a;
alpha(11) -> $b;
alpha(12) -> $c;
alpha(13) -> $d;
alpha(14) -> $e;
alpha(15) -> $f;
alpha(16) -> $g;
alpha(17) -> $h;
alpha(18) -> $j;
alpha(19) -> $k;
alpha(20) -> $m;
alpha(21) -> $n;
alpha(22) -> $p;
alpha(23) -> $q;
alpha(24) -> $r;
alpha(25) -> $s;
alpha(26) -> $t;
alpha(27) -> $v;
alpha(28) -> $w;
alpha(29) -> $x;
alpha(30) -> $y;
alpha (31) -> $z.

%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(<<$_, $_, _/binary>>, _, _, _, _) -> erlang:error(badarg);
do_decode(<<>>, Opts, _, Prefix, Acc = <<_:26/binary>>) ->
    decode_uuid(strip_underscore(Prefix), Acc, Opts);
do_decode(<<$_, T/binary>>, Opts, N, Prefix, Acc) ->
    do_decode(T, Opts, N + 1, <<Prefix/binary, Acc/binary, $_>>, <<>>);
do_decode(<<H, T/binary>>, O, N, P, Acc) when N < 91, H >= $a, H =< $z ->
    do_decode(T, O, N + 1, P, <<Acc/binary, H>>);
do_decode(T = <<H, _/binary>>, O, N, P, Acc) when N < 91, ?ALPHA(H) ->
    Left = 26 - byte_size(Acc),
    <<L:Left/binary>> = T,
    decode_uuid(strip_underscore(P), <<Acc/binary, L/binary>>, O).

strip_underscore(<<>>) -> <<>>;
strip_underscore(Prefix) -> jhn_blist:droplast(Prefix).

decode_uuid(Prefix, Base32, #opts{format=Format}) when byte_size(Prefix) < 64 ->
    <<0:2, Integer:128>> = base32_decode(Base32),
    UUID = case Format of
               human -> jhn_uuid:decode(Integer, [binary, human]);
               hex ->
                   jhn_uuid:encode(jhn_uuid:decode(Integer), [hex, binary]);
               uuid ->
                   jhn_uuid:encode(jhn_uuid:decode(Integer), [binary])
           end,
    #{prefix => Prefix, uuid => UUID}.

base32_decode(Bin) ->
    << <<(de_alpha(X)):5>> || <<X>> <= Bin >>.

de_alpha($0) -> 0;
de_alpha($1) -> 1;
de_alpha($2) -> 2;
de_alpha($3) -> 3;
de_alpha($4) -> 4;
de_alpha($5) -> 5;
de_alpha($6) -> 6;
de_alpha($7) -> 7;
de_alpha($8) -> 8;
de_alpha($9) -> 9;
de_alpha($a) -> 10;
de_alpha($b) -> 11;
de_alpha($c) -> 12;
de_alpha($d) -> 13;
de_alpha($e) -> 14;
de_alpha($f) -> 15;
de_alpha($g) -> 16;
de_alpha($h) -> 17;
de_alpha($j) -> 18;
de_alpha($k) -> 19;
de_alpha($m) -> 20;
de_alpha($n) -> 21;
de_alpha($p) -> 22;
de_alpha($q) -> 23;
de_alpha($r) -> 24;
de_alpha($s) -> 25;
de_alpha($t) -> 26;
de_alpha($v) -> 27;
de_alpha($w) -> 28;
de_alpha($x) -> 29;
de_alpha($y) -> 30;
de_alpha($z) -> 31.

%% ===================================================================
%% Validation
%% ===================================================================

valid_prefix(<<>>) -> true;
valid_prefix(<<$_>>) -> false;
valid_prefix(<<$_, T/binary>>) -> valid_prefix(T);
valid_prefix(<<C, T/binary>>) when C >= $a, C =< $z -> valid_prefix(T);
valid_prefix(_) -> false.

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts(Opts) ->
    lists:foldl(fun(Elt, Acc) -> parse_opt(Elt, Acc) end, #opts{}, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(list, Opts) -> Opts#opts{return_type = list};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(typeid, Opts) -> Opts#opts{return_type = typeid};
parse_opt(hex, Opts) -> Opts#opts{format = hex};
parse_opt(uuid, Opts) -> Opts#opts{format = uuid};
parse_opt(human, Opts) -> Opts#opts{format = human};
parse_opt(Opt, _) -> erlang:error(badarg, [Opt]).
