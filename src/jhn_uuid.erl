%%==============================================================================
%% Copyright 2020-2026 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%    Universally Unique IDentifiers (UUIDs)                         (rfc9562)
%%%    Uniform Resource Names (URNs)                                  (rfc8141)
%%%
%%%  A library for generating UUIDs of all versions defined in rfc9562.
%%%
%%%  Functions for encoding/decoding UUIDs are provided for completness and
%%%  testing purposes and the select few occations they are actually needed.
%%%
%%%  N.B. Does not support Version 2, no support currently planned.
%%%
%%%  Support for non-standard UUIDs with no dashes are provided using hex option
%%%
%%% @end
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2020-2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_uuid).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([gen/1, gen/2,
         encode/1, encode/2,
         decode/1, decode/2]).

%% Exported types
-export_type([type/0, uuid/0]).

%% Records
-record(opts,
        {%% v1/v6
         node           = random :: node_type(),
         clock_sequence          :: non_neg_integer() | undefined,
         %% v3/v5
         name                    :: iodata() | undefined,
         name_space              :: atom() | iodata() | undefined,
         %% v8
         custom                  :: non_neg_integer() | bitstring() | undefined,
         %% format
         hex            = false  :: boolean(),
         human          = false  :: boolean(),
         urn            = false  :: boolean(),
         return_type    = iolist :: iolist | binary | list | hex | uuid}).

%% Types
-type uuid() :: #{version => 1,
                  node => node_type(),
                  clock_sequence => non_neg_integer(),
                  %% This is a posix timestamp, nanoseconds precision.
                  timestamp => non_neg_integer() | binary()} |
                #{version => 3,
                  name_space => atom() | iodata(), name => iodata(),
                  hash => binary()} |
                #{version => 4, random => non_neg_integer()} |
                #{version => 5,
                  name_space => atom() | iodata(), name => iodata(),
                  hash => binary()} |
                #{version => 6,
                  node => node_type(),
                  clock_sequence => non_neg_integer(),
                  %% This is a posix timestamp, nanoseconds precision.
                  timestamp => non_neg_integer() | binary()} |
                #{version => 7,
                  random => non_neg_integer(),
                  %% This is a posix timestamp, milliseconds precision.
                  timestamp => posix() | binary()} |
                #{version => 8,
                  custom => non_neg_integer() | binary()} |
                nil | max.

-type type()       :: v1 | v3 | v4 | v5 | v6 | v7 | v8.

-type format_opt() :: iolist | binary | list | hex | uuid | urn | human.
-type v1_opt()     :: {node, node_type()} | {clock_sequence, non_neg_integer()}.
-type name_opt()   :: {name_space, iodata()} | {name, iodata()}.
-type custom_opt() :: {custom, non_neg_integer() | binary()}.
-type posix()      :: non_neg_integer().

-type opt()        :: v1_opt() | name_opt() | format_opt() | custom_opt().
-type opts()       :: [opt()].

%% IEEE 802 MAC address format XX-XX-XX-XX-XX-XX
-type mac_address() :: binary().
-type node_type()   :: undefined | random | non_neg_integer() | mac_address().

%% Defines
-define(POSIX_TO_GREGORIAN, 122192928000000000).
-define(DNS, <<"6ba7b810-9dad-11d1-80b4-00c04fd430c8">>).
-define(URL, <<"6ba7b811-9dad-11d1-80b4-00c04fd430c8">>).
-define(OID, <<"6ba7b812-9dad-11d1-80b4-00c04fd430c8">>).
-define(X500, <<"6ba7b814-9dad-11d1-80b4-00c04fd430c8">>).

-define(NIL, <<"00000000-0000-0000-0000-000000000000">>).
-define(MAX, <<"FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF">>).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: gen(Type) -> UUID.
%% @doc
%%   Generates the an UUID as an iolist of Type (type v3, v5 and v8 not
%%   applicable)
%%
%%   Equivalent of gen(Type, []) -> UUID for applicable types.
%% @end
%%--------------------------------------------------------------------
-spec gen(v1 | v4 | v6 | v7 | nil | max) -> iolist() | nil | max.
%%--------------------------------------------------------------------
gen(v1) -> gen(v1, #opts{});
gen(v4) -> gen(v4, #opts{});
gen(v6) -> gen(v6, #opts{});
gen(v7) -> gen(v7, #opts{});
gen(nil) -> gen(nil, #opts{});
gen(max) -> gen(max, #opts{});
gen(X) -> erlang:error(badarg, [X]).

%%--------------------------------------------------------------------
%% Function: gen(Type, Options) -> UUID.
%% @doc
%%   Generates the an UUID or URN of Type
%%
%%   This is normally the only function used. To quote rfc9562 "As general
%%   guidance, avoiding parsing UUID values unnecessarily is recommended;
%%   instead, treat UUIDs as opaquely as possible".
%%
%%   Types are: v1, v3, v4, v5, v6, v7, v8, nil, max
%%
%%   Options are:
%%
%%     General format:
%%
%%     iolist (default) -> an iolist is returned
%%
%%     binary -> a binary is returned
%%
%%     list -> a flat list is returned
%%
%%     hex -> the raw hex format without the dashes (this is outside the spec).
%%
%%     uuid -> a map representing the UUID is generated that can be passed to
%%             encode but can differ from the one generated from decode since
%%             some information may have been lost.
%%
%%     human -> when combined with uuid provides human readable timstamps for
%%              v1, v6 and v7
%%
%%     urn -> the URN is returned (can be combined with  binary, iolist, list)
%%
%%    Type specific options. They are all optional if not marked as required:
%%
%%    v1:
%%
%%    {node, Node} -> binary MAC, integer or the atoms random and undefined
%%
%%    {clock_sequence, CSeq} -> integer
%%
%%    v3:
%%
%%    {name, Name} (required) -> iodata representing the name
%%
%%    {name_space, NameSpace} -> iodata representing namespace or an atom
%%                               representing one of the predefined namespaces:
%%                               dns, url, oid, x500, nil and max
%%
%%    v5:
%%
%%    {name, Name} (required) -> same as for v3
%%
%%    {name_space, NameSpace} -> same as for v3
%%
%%    v6:
%%
%%    {node, Node} -> same as for v1
%%
%%    {clock_sequence, CSeq} -> same as for v1
%%
%%    v8:
%%
%%    {custom, Custom} (required) -> an integer or 122 bit binary
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec gen(type() | nil | max, opts() | #opts{}) ->
          uuid() | binary() | list() |  iolist() | nil | max.
%%--------------------------------------------------------------------
gen(Type, Opts = #opts{return_type = uuid}) -> do_gen(Type, Opts);
gen(Type, Opts = #opts{}) -> encode(do_gen(Type, Opts), Opts);
gen(Type, Opts) -> gen(Type, parse_opts(Type, Opts, #opts{})).

%%--------------------------------------------------------------------
%% Function: encode(UUID) -> IOList.
%% @doc
%%   Encodes the map represention of an UUID as an iolist.
%%
%%   Equivalent of encode(UUID, [])
%% @end
%%--------------------------------------------------------------------
-spec encode(uuid()) -> iolist().
%%--------------------------------------------------------------------
encode(UUID) -> encode(UUID, #opts{}).

%%--------------------------------------------------------------------
%% Function: encode(UUID, Options) -> Encoded
%% @doc
%%   Encodes the map represention of an UUID or URN as required the options.
%%
%%   Encode will give an exception if the UUID is not well formed.
%%
%%   Options are:
%%
%%     iolist (default) -> an iolist is returned
%%
%%     binary -> a binary is returned
%%
%%     list -> a flat list is returned
%%
%%     hex -> the raw hex format without the dashes (this is outside the spec).
%%
%%     urn -> the URN is returned  (can be combined with  binary, iolist, list)
%% @end
%%--------------------------------------------------------------------
-spec encode(uuid(), opts() | #opts{}) -> iolist() | binary() | list().
%%--------------------------------------------------------------------
encode(UUID, #opts{return_type = Type, urn = URN, hex = Hex}) ->
    case {Type, URN, Hex} of
        {iolist, false, true} -> hex(do_encode(UUID));
        {iolist, false, _} -> do_encode(UUID);
        {iolist, true, _} -> [<<"urn:uuid:">>, do_encode(UUID)];
        {binary, false, true} -> iolist_to_binary(hex(do_encode(UUID)));
        {binary, false, _} -> iolist_to_binary(do_encode(UUID));
        {binary, true,_} -> iolist_to_binary([<<"urn:uuid:">>,do_encode(UUID)]);
        {list, false, true} ->
            binary_to_list(iolist_to_binary(hex(do_encode(UUID))));
        {list, false, _} -> binary_to_list(iolist_to_binary(do_encode(UUID)));
        {list, true, _} ->
          binary_to_list(iolist_to_binary([<<"urn:uuid:">>, do_encode(UUID)]));
         _->
            erlang:error(badarg, [UUID, Type])
    end;
encode(UUID = #{version := V}, Opts) ->
    encode(UUID, parse_opts(version_to_type(V), Opts, #opts{}));
encode(Special, Opts) when is_atom(special) ->
    encode(Special, parse_opts(special, Opts,  #opts{})).


%%--------------------------------------------------------------------
%% Function: decode(Binary) -> UUID.
%% @doc
%%   Decodes the binary or integer into a map representing an UUID.
%%
%%   Decode will raise an exception if the binary is not well formed UUID.
%%
%%   Equivalent of decode(Binary, []) -> UUID.
%%
%% @end
%%--------------------------------------------------------------------
-spec decode(binary() | integer()) -> uuid().
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, #opts{}).

%%--------------------------------------------------------------------
%% Function: decode(Binary, Options) -> UUID.
%% @doc
%%   Decodes the binary or integer into a map representing an UUID.
%%
%%   Decode will raise an exception if the binary is not well formed UUID.
%%
%%   Options are:
%%
%%     human -> a human readable timestamp is generated for types v1, v6 and v7
%% @end
%%--------------------------------------------------------------------
-spec decode(binary() | integer(), opts() | #opts{}) -> uuid().
%%--------------------------------------------------------------------
decode(Binary, #opts{human = Human}) -> do_decode(Binary, Human);
decode(Binary, Opts) -> decode(Binary, parse_opts(format, Opts, #opts{})).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Generate
%% ===================================================================
do_gen(nil, _) -> nil;
do_gen(max, _) -> max;
do_gen(v1, Opts = #opts{clock_sequence = CSeq0, node = Node0}) ->
    CSeq = gen_cseq(CSeq0),
    Node = gen_node(Node0),
    TS = case Opts of
             #opts{human = true, return_type = uuid} ->
                 jhn_timestamp:gen([binary, nano]);
             _ ->
                 erlang:system_time(nano_seconds)
         end,
    #{version => 1, timestamp => TS, clock_sequence => CSeq, node => Node};
do_gen(v3, Opts = #opts{name_space = undefined}) ->
    do_gen(v3, Opts#opts{name_space = gen(v1)});
do_gen(v3, #opts{name_space = Namespace, name = Name}) ->
    #{version => 3, name_space => Namespace, name => Name};
do_gen(v4, _) ->
    <<Random:122, _:6>> = crypto:strong_rand_bytes(16),
    #{version => 4, random => Random};
do_gen(v5, Opts = #opts{name_space = undefined}) ->
    do_gen(v5, Opts#opts{name_space = gen(v1)});
do_gen(v5, #opts{name_space = Namespace, name = Name}) ->
    #{version => 5, name_space => Namespace, name => Name};
do_gen(v6, Opts) ->
    (do_gen(v1, Opts))#{version => 6};
do_gen(v7, Opts) ->
    TS = case Opts of
             #opts{human = true, return_type = uuid} ->
                 jhn_timestamp:gen([binary, milli]);
             _ ->
                 erlang:system_time(milli_seconds)
         end,
    <<Random:74, _:6>> = crypto:strong_rand_bytes(10),
    #{version => 7, timestamp => TS, random => Random};
do_gen(v8, #opts{custom = Custom}) when is_integer(Custom) ->
    #{version => 8, custom => Custom};
do_gen(v8, #opts{custom = Custom = <<_:122>>}) ->
    #{version => 8, custom => Custom}.

gen_cseq(CSeq) when is_integer(CSeq) -> CSeq;
gen_cseq(undefined) ->
    <<_:2, CSeq:14>> = crypto:strong_rand_bytes(2),
    CSeq.

gen_node(Node) when is_integer(Node) -> Node;
gen_node(random) -> random_node();
%% MAC
gen_node(<<M1:16, $-, M2:16, $-, M3:16, $-, M4:16, $-, M5:16, $-, M6:16>>) ->
    binary_to_integer(<<M1:16, M2:16, M3:16,M4:16,M5:16,M6:16>>, 16);
gen_node(undefined) ->
    case erlang:get(?MODULE) of
        undefined ->
            {ok, IFs} = inet:getifaddrs(),
            HWAddrs =
                [jhn_plist:find(hwaddr, PList) ||
                    {_,  PList} <- IFs,
                    jhn_plist:member(hwaddr, PList),
                    lists:member(multicast,
                                 jhn_plist:find(flags, PList))],
            case HWAddrs of
                [] ->
                    N = random_node(),
                    erlang:put(?MODULE, N),
                    N;
                [[M1, M2, M3, M4, M5, M6] | _] ->
                    <<N1, N2, N3, N4, N5, N6, _/binary>> =
                        crypto:hash(md5,
                                    atom_to_binary(node(),utf8)),
                    %% Setting the multicast bit
                    <<N:48>> = <<((M1 bxor N1) bor 1), (M2 bxor N2),
                                 (M3 bxor N3), (M4 bxor N4),
                                 (M5 bxor N5), (M6 bxor N6)>>,
                    erlang:put(?MODULE, N),
                    N
            end;
        N ->
            N
    end.

random_node() ->
    <<N1:7, _:1, N2:40>> = crypto:strong_rand_bytes(6),
    %% Setting the multicast bit
    <<N:48>> = <<N1:7, 1:1, N2:40>>,
    N.

%% ===================================================================
%% Encoding
%% ===================================================================
do_encode(nil) -> ?NIL;
do_encode(max) -> ?MAX;
do_encode(UUID = #{version := 1}) ->
    #{timestamp := TS, clock_sequence := CSeq, node := Mac} = UUID,
    <<THigh:12, TMid:16, TLow:32>> = encode_nano_ts(TS),
    Node = encode_node(Mac),
    encode_binary(<<TLow:32, TMid:16,1:4,THigh:12, 2:2, CSeq:14, Node:48>>);
do_encode(#{version := 3, hash := <<A:48, B:12, C:62>>}) ->
    encode_binary(<<A:48, 3:4, B:12, 2:2, C:62>>);
do_encode(#{version := 3, name_space := NameSpace, name := Name}) ->
    <<A:48, _:4, B:12, _:2, C:62>> =
        crypto:hash(md5, [name_space(NameSpace), Name]),
    encode_binary(<<A:48, 3:4, B:12, 2:2, C:62>>);
do_encode(#{version := 4, random := Random}) ->
    <<A:48, B:12, C:62>> = <<Random:122>>,
    encode_binary(<<A:48, 4:4, B:12, 2:2, C:62>>);
do_encode(#{version := 5, hash := <<A:48, B:12, C:62>>}) ->
    encode_binary(<<A:48, 5:4, B:12, 2:2, C:62>>);
do_encode(#{version := 5, name_space := NameSpace, name := Name}) ->
    <<A:48, _:4, B:12, _:2, C:62, _:32>> =
        crypto:hash(sha, [name_space(NameSpace), Name]),
    encode_binary(<<A:48, 5:4, B:12, 2:2, C:62>>);
do_encode(UUID = #{version := 6}) ->
    #{timestamp := TS, clock_sequence := CSeq, node := Mac} = UUID,
    <<THighMid:48, TLow:12>> = encode_nano_ts(TS),
    Node = encode_node(Mac),
    encode_binary(<<THighMid:48, 6:4, TLow:12, 2:2, CSeq:14, Node:48>>);
do_encode(#{version := 7, timestamp := TS, random := Random}) ->
    TS1 = case TS of
              _ when is_integer(TS) -> TS;
              _ when is_binary(TS) ->
                  jhn_timestamp:encode(jhn_timestamp:decode(TS, [milli]),
                                       [posix, milli])
          end,
    <<A:12, B:62>> = <<Random:74>>,
    encode_binary(<<TS1:48, 7:4, A:12, 2:2, B:62>>);
do_encode(#{version := 8, custom := Custom}) when is_integer(Custom) ->
    <<High:48, Mid:12, Low:62>> = <<Custom:122>>,
    encode_binary(<<High:48, 8:4, Mid:12, 2:2, Low:62>>);
do_encode(#{version := 8, custom := <<High:48, Mid:12, Low:62>>}) ->
    encode_binary(<<High:48, 8:4, Mid:12, 2:2, Low:62>>).

encode_binary(<<N:128>>) ->
    Lower = jhn_bstring:to_lower(integer_to_binary(N, 16)),
    Padding = jhn_blist:duplicate(32 - byte_size(Lower), $0),
    <<A:64, B:32, C:32, D:32, E:96>> = <<Padding/binary, Lower/binary>>,
    [<<A:64>>, $-, <<B:32>>, $-, <<C:32>>, $-, <<D:32>>, $-, <<E:96>>].

name_space(dns) -> ?DNS;
name_space(url) -> ?URL;
name_space(oid) -> ?OID;
name_space(x500) -> ?X500;
name_space(nil) ->  encode(nil);
name_space(max) ->  encode(max);
name_space(UUID) -> UUID.

encode_nano_ts(TS) when is_integer(TS) -> <<(to_timestamp(TS)):60>>;
encode_nano_ts(TS) when is_binary(TS) ->
    TS1 = jhn_timestamp:encode(jhn_timestamp:decode(TS, [nano]),
                               [posix, nano]),
    <<(to_timestamp(TS1)):60>>.

encode_node(<<M1:16, $-, M2:16, $-, M3:16, $-, M4:16, $-, M5:16,$-,M6:16>>) ->
    binary_to_integer(<<M1:16,M2:16,M3:16,M4:16,M5:16,M6:16>>, 16);
encode_node(Mac) when is_integer(Mac) ->
    Mac.

hex(L) -> [E || E <- L, E /= 45].

%% ===================================================================
%% Decoding
%% ===================================================================
do_decode(<<A:64, B:32, C:32, D:32, E:96>>, H) ->
    Bits = <<(binary_to_integer(<<A:64, B:32, C:32, D:32, E:96>>, 16)):128>>,
    decode_bits(Bits, H);
do_decode(<<"urn:uuid:", A:64, $-, B:32, $-, C:32, $-, D:32, $-, E:96>>, H) ->
    Bits = <<(binary_to_integer(<<A:64, B:32, C:32, D:32, E:96>>, 16)):128>>,
    decode_bits(Bits, H);
do_decode(<<A:64, $-, B:32, $-, C:32, $-, D:32, $-, E:96>>, H) ->
    Bits = <<(binary_to_integer(<<A:64, B:32, C:32, D:32, E:96>>, 16)):128>>,
    decode_bits(Bits, H);
do_decode(Integer, H) when is_integer(Integer) ->
    decode_bits(<<Integer:128>>, H).

decode_bits(<<0:128>>, _) -> nil;
decode_bits(<<340282366920938463463374607431768211455:128>>, _) -> max;
decode_bits(<<TLow:32, TMid:16, 1:4, THigh:12, 2:2, CSeq:14, Node:48>>, H) ->
    <<TS0:60>> = <<THigh:12, TMid:16, TLow:32>>,
    TS = decode_ts(TS0, H),
    Mac = jhn_bstring:join([pad(N) || <<N:8>> <= <<Node:48>>], <<$->>),
    #{version => 1, timestamp => TS, clock_sequence => CSeq, node => Mac};
decode_bits(<<A:48, 3:4, B:12, 2:2, C:62>>, _) ->
    #{version => 3, hash => <<A:48, B:12, C:62>>};
decode_bits(<<A:48, 4:4, B:12, 2:2, C:62>>, _) ->
    <<N:122>> = <<A:48, B:12, C:62>>,
    #{version => 4, random => N};
decode_bits(<<A:48, 5:4, B:12, 2:2, C:62>>, _) ->
    #{version => 5, hash => <<A:48, B:12, C:62>>};
decode_bits(<<THighMid:48, 6:4, TLow:12, 2:2, CSeq:14, Node:48>>, H) ->
    <<TS0:60>> = <<THighMid:48, TLow:12>>,
    TS = decode_ts(TS0, H),
    Mac = jhn_bstring:join([pad(N) || <<N:8>> <= <<Node:48>>], <<$->>),
    #{version => 6, timestamp => TS, clock_sequence => CSeq, node => Mac};
decode_bits(<<TS:48, 7:4, A:12, 2:2, B:62>>, H) ->
    <<Random:74>> = <<A:12, B:62>>,
    TS1 = case H of
              true -> jhn_timestamp:encode(TS, [binary, milli]);
              false -> TS
          end,
    #{version => 7, timestamp => TS1, random => Random};
decode_bits(<<High:48, 8:4, Mid:12, 2:2, Low:62>>, _) ->
    #{version => 8, custom => <<High:48, Mid:12, Low:62>>}.

pad(I) ->
    case integer_to_binary(I, 16) of
        <<N:8>> -> <<$0, N>>;
        B -> B
    end.

decode_ts(TS, false) -> from_timestamp(TS);
decode_ts(TS, true) -> jhn_timestamp:encode(from_timestamp(TS), [binary, nano]).


%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts(Type, Opts, Rec) ->
    lists:foldl(fun(Elt, Acc) -> parse_opt(Type, Elt, Acc) end, Rec, Opts).

parse_opt(v2, Opt, _) -> erlang:error(badarg, [v2, Opt]);
parse_opt(_, binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(_, list, Opts) -> Opts#opts{return_type = list};
parse_opt(_, iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(_, uuid, Opts) -> Opts#opts{return_type = uuid};
parse_opt(_, hex, Opts) -> Opts#opts{hex = true};
parse_opt(_, urn, Opts) -> Opts#opts{urn = true};
parse_opt(format, human, Opts) -> Opts#opts{human = true};
parse_opt(v1, human, Opts) -> Opts#opts{human = true};
parse_opt(v1, {node, undefined}, Opts) -> Opts#opts{node = undefined};
parse_opt(v1, {node, random}, Opts) -> Opts#opts{node = random};
parse_opt(v1, {node, I}, Opts) when is_integer(I) -> Opts#opts{node = I};
parse_opt(v1, {node, Mac}, Opts) when is_binary(Mac) -> Opts#opts{node = Mac};
parse_opt(v1, {clock_sequence, CSeq}, Opts) when is_integer(CSeq) ->
    Opts#opts{clock_sequence = CSeq};
parse_opt(v3, {name, Name}, Opts) -> Opts#opts{name = Name};
parse_opt(v3, {name_space, NameSpace}, Opts) -> Opts#opts{name_space=NameSpace};
parse_opt(v5, {name, Name}, Opts) -> Opts#opts{name = Name};
parse_opt(v5, {name_space, NameSpace}, Opts) -> Opts#opts{name_space=NameSpace};
parse_opt(v6, Opt, Opts) -> parse_opt(v1, Opt, Opts);
parse_opt(v7, human, Opts) -> Opts#opts{human = true};
parse_opt(v8, {custom, C}, Opts) when is_integer(C) -> Opts#opts{custom = C};
parse_opt(v8, {custom, C = <<_:122>>}, Opts) -> Opts#opts{custom = C};
parse_opt(Type, Opt, _) -> erlang:error(badarg, [Type, Opt]).

to_timestamp(PosixNano) -> (PosixNano div 100) + ?POSIX_TO_GREGORIAN.

from_timestamp(TS) -> (TS - ?POSIX_TO_GREGORIAN) * 100.

version_to_type(1) -> v1;
version_to_type(3) -> v3;
version_to_type(4) -> v4;
version_to_type(5) -> v5;
version_to_type(6) -> v6;
version_to_type(7) -> v7;
version_to_type(8) -> v8.
