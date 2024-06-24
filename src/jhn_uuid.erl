%%==============================================================================
%% Copyright 2020 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%    A Universally Unique IDentifier (UUID) URN Namespace            (rfc4122)
%%%
%%%
%%% N.B. Does not support Version 2, no support planned in the near future.
%%%
%%% @end
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2020, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_uuid).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([gen/1, gen/2, gen/3,
         encode/1, encode/2,
         decode/1, decode/2]).

%% Exported types
-export_type([type/0, uuid/0]).

%% Records
-record(opts, {human = false :: boolean(),
               urn = false :: boolean(),
               return_type = iolist :: iolist | binary | list | uuid}).

%% Types
-type uuid() :: #{} | nil.

-type type() :: v1 | v3 | v4 | v5.

-type opt() :: iolist | binary | list | uuid | urn.

%% Defines
-define(POSIX_TO_GREGORIAN_NANO, 122192928000000000).
-define(DNS, <<"6ba7b810-9dad-11d1-80b4-00c04fd430c8">>).
-define(URL, <<"6ba7b811-9dad-11d1-80b4-00c04fd430c8">>).
-define(OID, <<"6ba7b812-9dad-11d1-80b4-00c04fd430c8">>).
-define(X500, <<"6ba7b814-9dad-11d1-80b4-00c04fd430c8">>).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: gen(Type) -> UUID.
%% @doc
%%   Generates the an UUID as an iolist of Type (type 3 and 5 not applicable)
%%   Equivalent of gen(Type, #{}, []) -> URI.
%% @end
%%--------------------------------------------------------------------
-spec gen(type()) -> iolist().
%%--------------------------------------------------------------------
gen(Type) -> gen(Type, #{}, #opts{}).

%%--------------------------------------------------------------------
%% Function: gen(Type, Map | Options) -> UUID.
%% @doc
%%   Generates the an UUID as an iolist of Type
%%   The map contains information needed to generate the UUID such as the name
%%   used for version 3
%%   Options are:
%%     uuid -> a map representing the UUID is generated
%%     binary -> a binary is returned
%%     list -> a flat list is returned
%%     iolist (default) -> an iolist is returned
%%     urn -> the URN is returned
%%   Equivalent of gen(Type, Map, []) or gen(Type, #{}, Options)
%% @end
%%--------------------------------------------------------------------
-spec gen(type(), map() | [opt()]) -> iolist() | uuid() | binary() | list().
%%--------------------------------------------------------------------
gen(Type, Map = #{}) -> gen(Type, Map, #opts{});
gen(Type, []) -> gen(Type, #{}, #opts{});
gen(Type, Opts = [_|_]) -> gen(Type, #{}, Opts).

%%--------------------------------------------------------------------
%% Function: gen(Type, Map, Options) -> UUID
%% @doc
%%   Generates an UUID or encode as an iolist, list, or binary.
%%   The map contains information needed to generate the UUID such as the name
%%   used for version 3.
%%   Options are:
%%     uuid -> a map representing the UUID is generated
%%     binary -> a binary is returned
%%     list -> a flat list is returned
%%     iolist (default) -> an iolist is returned
%%     urn -> the URN is returned
%% @end
%%--------------------------------------------------------------------
-spec gen(type(), map(), [opt()] | #opts{}) -> uuid() | list() |
                                               iolist()| binary().
%%--------------------------------------------------------------------
gen(Type, Map, Opts = #opts{return_type = uuid}) -> do_gen(Type, Map, Opts);
gen(Type, Map, Opts = #opts{}) -> encode(do_gen(Type, Map, Opts), Opts);
gen(Type, Map, Opts) -> gen(Type, Map, parse_opts(Opts, #opts{})).


%%--------------------------------------------------------------------
%% Function: encode(UUID) -> IOList.
%% @doc
%%   Encodes the map as an iolist.
%%   Equivalent of encode(UUID, [])
%% @end
%%--------------------------------------------------------------------
-spec encode(uuid()) -> iolist().
%%--------------------------------------------------------------------
encode(UUID) -> encode(UUID, #opts{}).

%%--------------------------------------------------------------------
%% Function: encode(UUID, Options) -> Encoded
%% @doc
%%   Encodes the UUID as an iolist or binary.
%%   Encode will give an exception if the UUID is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     list -> a flat list is returned
%%     iolist -> an iolist is returned
%%     urn -> the URN is returned
%% @end
%%--------------------------------------------------------------------
-spec encode(uuid(), [opt()] | #opts{}) -> iolist() | binary() | list().
%%--------------------------------------------------------------------
encode(UUID, Opts = #opts{return_type = Type, urn = URN}) ->
    case {Type, URN} of
        {iolist, false} -> do_encode(UUID, Opts);
        {iolist, true} -> [<<"urn:uuid:">>, do_encode(UUID, Opts)];
        {binary, false} -> iolist_to_binary(do_encode(UUID, Opts));
        {binary, true} ->
            iolist_to_binary([<<"urn:uuid:">>, do_encode(UUID, Opts)]);
        {list, false} ->
            binary_to_list(iolist_to_binary(do_encode(UUID, Opts)));
        {list, true} ->
            binary_to_list(
              iolist_to_binary([<<"urn:uuid:">>, do_encode(UUID, Opts)]))
    end;
encode(UUID, Opts) ->
    encode(UUID, parse_opts(Opts, #opts{})).

%%--------------------------------------------------------------------
%% Function: decode(Binary) -> UUID.
%% @doc
%%   Decodes the binary into a map representing an UUID.
%%   Equivalent of decode(Binary, []) -> UUID.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary() | integer()) -> uuid().
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, #opts{}).

%%--------------------------------------------------------------------
%% Function: decode(Binary, Options) -> UUID.
%% @doc
%%   Decodes the binary into a map representing an UUID.
%%   Decode will give an exception if the binary is not well formed UUID.
%%   Options are:
%% @end
%%--------------------------------------------------------------------
-spec decode(binary() | integer(), [opt()] | #opts{}) -> uuid().
%%--------------------------------------------------------------------
decode(Binary, Opts = #opts{}) -> do_decode(Binary, Opts);
decode(Binary, Opts) -> do_decode(Binary, parse_opts(Opts, #opts{})).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Generate
%% ===================================================================

do_gen(v1, Map = #{}, _) ->
    CSeq = case maps:get(clock_sequence, Map, undefined) of
               CSeq0 when is_integer(CSeq0) -> CSeq0;
               undefined ->
                   <<_:2, CSeq0:14>> = crypto:strong_rand_bytes(2),
                   CSeq0
           end,
    Node =
        case maps:get(node, Map, undefined) of
            Node0 when is_integer(Node0) -> Node0;
            <<M1:16, $-, M2:16, $-, M3:16, $-, M4:16, $-, M5:16, $-, M6:16>> ->
                    binary_to_integer(<<M1:16, M2:16, M3:16,M4:16,M5:16,M6:16>>,
                                      16);
            undefined ->
                case erlang:get(?MODULE) of
                    undefined ->
                        {ok, IFs} = inet:getifaddrs(),
                        HWAddrs =
                            [plist:find(hwaddr, PList) ||
                                {_,  PList} <- IFs,
                                plist:member(hwaddr, PList),
                                lists:member(multicast,
                                             plist:find(flags, PList))],
                        case HWAddrs of
                            [] ->
                                <<N1:7, _:1, N2:40>> =
                                    crypto:strong_rand_bytes(6),
                                %% Setting the multicast bit
                                <<N:48>> = <<N1:7, 1:1, N2:40>>,
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
                end
        end,
    TS = (erlang:system_time(nano_seconds) + ?POSIX_TO_GREGORIAN_NANO) div 100,
    #{version => 1, timestamp => TS, clock_sequence => CSeq, node => Node};
do_gen(v3, #{name_space := Namespace, name := Name}, _) ->
    #{version => 3, name_space => Namespace, name => Name};
do_gen(v3, #{name := Name}, _) ->
    #{version => 3, name_space => gen(v1), name => Name};
do_gen(v4, _, _) ->
    <<Random:122, _:6>> = crypto:strong_rand_bytes(16),
    #{version => 4, random => Random};
do_gen(v5, #{name_space := Namespace, name := Name}, _) ->
    #{version => 5, name_space => Namespace, name => Name};
do_gen(v5, #{name := Name}, _) ->
    #{version => 5, name_space => gen(v1), name => Name}.

%% ===================================================================
%% Encoding
%% ===================================================================

do_encode(nil, _) -> <<"00000000-0000-0000-0000-000000000000">>;
do_encode(UUID = #{version := 1}, _) ->
    #{timestamp := TS, clock_sequence := CSeq, node := Mac} = UUID,
    <<THigh:12, TMid:16, TLow:32>> = <<TS:60>>,
    <<CHigh:6, CLow:8>> = <<CSeq:14>>,
    Node = case Mac of
               <<M1:16, $-, M2:16, $-, M3:16, $-, M4:16, $-, M5:16,$-,M6:16>> ->
                   binary_to_integer(<<M1:16,M2:16,M3:16,M4:16,M5:16,M6:16>>,
                                     16);
               _ when is_integer(Mac) ->
                   Mac
           end,
    encode_binary(<<TLow:32, TMid:16,1:4,THigh:12,2:2,CHigh:6,CLow:8,Node:48>>);
do_encode(#{version := 3, hash := <<A:48, B:12, C:62>>}, _) ->
    encode_binary(<<A:48, 3:4, B:12, 2:2, C:62>>);
do_encode(#{version := 3, name_space := NameSpace, name := Name}, _) ->
    <<A:48, _:4, B:12, _:2, C:62>> =
        crypto:hash(md5, [name_space(NameSpace), Name]),
    encode_binary(<<A:48, 3:4, B:12, 2:2, C:62>>);
do_encode(#{version := 4, random := <<A:48, B:12, C:62>>}, _) ->
    encode_binary(<<A:48, 4:4, B:12, 2:2, C:62>>);
do_encode(#{version := 4, random := Random}, _) ->
    <<A:48, B:12, C:62>> = <<Random:122>>,
    encode_binary(<<A:48, 4:4, B:12, 2:2, C:62>>);
do_encode(#{version := 5, hash := <<A:48, B:12, C:62>>}, _) ->
    encode_binary(<<A:48, 5:4, B:12, 2:2, C:62>>);
do_encode(#{version := 5, name_space := NameSpace, name := Name}, _) ->
    <<A:48, _:4, B:12, _:2, C:62, _:32>> =
        crypto:hash(sha, [name_space(NameSpace), Name]),
    encode_binary(<<A:48, 5:4, B:12, 2:2, C:62>>).

encode_binary(<<N:128>>) ->
    Lower = bstring:to_lower(integer_to_binary(N, 16)),
    Padding = blist:duplicate(32 - byte_size(Lower), $0),
    <<A:64, B:32, C:32, D:32, E:96>> = <<Padding/binary, Lower/binary>>,
    [<<A:64>>, $-, <<B:32>>, $-, <<C:32>>, $-, <<D:32>>, $-, <<E:96>>].

name_space(dns) -> ?DNS;
name_space(url) -> ?URL;
name_space(oid) -> ?OID;
name_space(x500) -> ?X500;
name_space(nil) ->  encode(nil);
name_space(UUID) -> UUID.

%% ===================================================================
%% Decoding
%% ===================================================================

%% <<"f412e400-c445-1131-bdc6-03f9e757eb34">>
do_decode(<<"urn:uuid:", A:64, $-, B:32, $-, C:32, $-, D:32, $-, E:96>>, _) ->
    Bits = <<(binary_to_integer(<<A:64, B:32, C:32, D:32, E:96>>, 16)):128>>,
    decode_bits(Bits);
do_decode(<<A:64, $-, B:32, $-, C:32, $-, D:32, $-, E:96>>, _) ->
    Bits = <<(binary_to_integer(<<A:64, B:32, C:32, D:32, E:96>>, 16)):128>>,
    decode_bits(Bits).

decode_bits(<<0:128>>) -> nil;
decode_bits(<<TLow:32, TMid:16, 1:4, THigh:12, 2:2, CHigh:6,CLow:8,Node:48>>) ->
    <<TS0:60>> = <<THigh:12, TMid:16, TLow:32>>,
    Human = true,
    TS = case Human of
             %% false -> TS0;
             true ->
                 timestamp:encode((TS0 * 100) - ?POSIX_TO_GREGORIAN_NANO,
                                  [binary, nano])
         end,
    <<CSeq:14>> = <<CHigh:6, CLow:8>>,

    %% Micro = (TS0 - 122192928000000000) div 100,
    %% io:format("Foo: ~p~n", [timestamp:encode(Micro, [binary, micro])]),

    Mac = bstring:join([pad(N) || <<N:8>> <= <<Node:48>>], <<$->>),
    #{version => 1, timestamp => TS, clock_sequence => CSeq, node => Mac};
decode_bits(<<TLowMid:48, 3:4, THigh:12, 2:2, CSeq:14, Node:48>>) ->
    Hash  = <<TLowMid:60, THigh:12, CSeq:14, Node:48>>,
    #{version => 3, hash => Hash};
decode_bits(<<A:48, 4:4, B:12, 2:2, C:62>>) ->
    <<N:122>> = <<A:48, B:12, C:62>>,
    #{version => 4, random => N};
decode_bits(<<TLowMid:48, 5:4, THigh:12, 2:2, CSeq:14, Node:48>>) ->
    Hash  = <<TLowMid:60, THigh:12, CSeq:14, Node:48>>,
    #{version => 5, hash => Hash}.

pad(I) ->
    case integer_to_binary(I, 16) of
        <<N:8>> -> <<$0, N>>;
        B -> B
    end.

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(list, Opts) -> Opts#opts{return_type = list};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(uuid, Opts) -> Opts#opts{return_type = uuid};
parse_opt(urn, Opts) -> Opts#opts{urn = true};
parse_opt(human, Opts) -> Opts#opts{human = true};
parse_opt(_, _) -> erlang:error(badarg).
