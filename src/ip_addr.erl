%%==============================================================================
%% Copyright 2016 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A IP library based on:
%%%    IP Version 6 Addressing Architecture                            (rfc4291)
%%%    A Recommendation for IPv6 Address Text Representation           (rfc5952)
%%%    Classless Inter-domain Routing (CIDR):                          (rfc4632)
%%%          The Internet Address Assignment and Aggregation Plan
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(ip_addr).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2,
         bounds/1, bounds/2
        ]).

%% Records
-record(opts, {format = ipv4 :: ipv4 | ipv6,
               ipv6ipv4 = false :: boolean(),
               continue = false :: boolean(),
               range = false :: boolean(),
               compact = false :: boolean(),
               return_type = iolist :: iolist | list | binary}).

%% Types
-type opt() :: _.
-type ip() :: ipv4() | ipv6().
-type ipv4() :: integer() | {integer(), integer(), integer(), integer()}.
-type ipv6() :: integer() |
                {integer(), integer(), integer(), integer(),
                 integer(), integer(), integer(), integer()} |
                {integer(), integer(), integer(), integer(),
                 integer(), integer(),
                 {integer(), integer(), integer(), integer()}}.
-type range() :: integer().


%% Defines
-define(UINT32_MAX, 4294967295).

%% Decode macros
-define(IS_INT(C), C>=$0, C=<$9).

-define(IS_HEX(C),
            C >= $a, C =< $f; C >= $A, C =< $F;
            C >= $0, C =< $9; C == $.).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Term) -> IP.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent of encode(Term, []) -> IP.
%% @end
%%--------------------------------------------------------------------
-spec encode(ip()) -> iolist().
%%--------------------------------------------------------------------
encode(Term) -> encode(Term, #opts{}).

%%--------------------------------------------------------------------
%% Function: encode(Term, Options) -> IP
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     list -> a flat list is returned
%%     iolist -> an iolist is returned (Default)
%%     ipv4 -> an ipv4 address is encoded when the Term is an integer
%%     ipv6 -> an ipv6 address is encoded when the Term is an integer
%%     ipv6ipv4 -> encoded IPv6 host address has the two least sigificant
%%                 segments repesented in IPv4 address format
%%     compact -> the most compact encoding of IPv6 used (collapsed zeros)
%% @end
%%--------------------------------------------------------------------
-spec encode(ip(), [opt()] | #opts{}) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Term, Opts = #opts{}) -> do_encode(Term, Opts);
encode(Term, Opts) ->
    ParsedOpts = parse_opts(Opts, #opts{}),
    case ParsedOpts#opts.return_type of
        binary -> iolist_to_binary(do_encode(Term, ParsedOpts));
        iolist -> do_encode(Term, ParsedOpts);
        list -> binary_to_list(iolist_to_binary(do_encode(Term, ParsedOpts)))
    end.

%%--------------------------------------------------------------------
%% Function: decode(IOData) -> Integer.
%% @doc
%%   Decodes the binary into an Integer.
%%   Equivalent of decode(IOData, []) -> Integer.
%% @end
%%--------------------------------------------------------------------
-spec decode(iodata()) -> ip() | {ip(), iodata()}.
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, #opts{}).

%%--------------------------------------------------------------------
%% Function: decode(Binary, Options) -> IP.
%% @doc
%%   Decodes the binary into a structured Erlang.
%%   Decode will give an exception if the binary is not well formed IP.
%%   Options are:
%%     integer -> an integer is returned (Default)
%%     tuple -> a tuple of integers is returned
%%     ipv6ipv4 -> with tuple the two last parts are returned as an IPv4 tuple
%%     range -> a IP range is being decoded
%%     continue -> all remaining indata is returned
%% @end
%%--------------------------------------------------------------------
-spec decode(iodata(), [opt()] | #opts{}) -> ip() |
                                             {ip(), range()} |
                                             {ip(), iodata()} |
                                             {ip(), range(), iodata()}.
%%--------------------------------------------------------------------
decode(Binary, Opts = #opts{}) -> do_decode(Binary, Opts);
decode(Binary, Opts) -> do_decode(Binary, parse_opts(Opts, #opts{})).

%%--------------------------------------------------------------------
%% Function: bounds(Range) -> {LowerIP, UpperIP}.
%% @doc
%%   Determines the IP bounds for a range
%%   Equivalent of bounds(IOData, []).
%% @end
%%--------------------------------------------------------------------
-spec bounds(iodata() | {ip(), range()}) -> {integer(), integer()}.
%%--------------------------------------------------------------------
bounds(Range) -> bounds(Range, #opts{}).

%%--------------------------------------------------------------------
%% Function: bounds(Range, Options) -> {LowerIP, UpperIP}.
%% @doc
%%   Determines the IP bounds for a range
%%   Bounds will give an exception if the binary is not well formed IP.
%%   Options are:
%%     integer -> an integer is returned (Default)
%%     tuple -> a tuple of integers is returned
%%     ipv6ipv4 -> with tuple the two last parts are returned as an IPv4 tuple
%% @end
%%--------------------------------------------------------------------
-spec bounds(iodata() | {ip(), range()}, [opt()] | #opts{}) -> {ip(), ip()}.
%%--------------------------------------------------------------------
bounds(Binary, Opts = #opts{}) -> do_bounds(Binary, Opts);
bounds(Binary, Opts) -> do_bounds(Binary, parse_opts(Opts, #opts{})).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

do_encode({IP, Range}, Opts) ->
    [do_encode(IP, Opts), $/, integer_to_binary(Range)];
do_encode(IPv4 = {_, _, _, _}, _) ->
    join([integer_to_binary(I) || I <- tuple_to_list(IPv4)], $.);
do_encode({A, B, C, D, E, F, IP = {_, _, _, _}}, Opts) ->
    IPv4 = do_encode(IP, Opts),
    [compact([A, B, C, D, E, F], Opts), $:, IPv4];
do_encode({A, B, C, D, E, F, G, H}, Opts = #opts{ipv6ipv4 = true}) ->
    <<A1, B1, C1, D1>> = <<G:16, H:16>>,
    IPv4 = do_encode({A1, B1, C1, D1}, Opts),
    [compact([A, B, C, D, E, F], Opts), $:, IPv4];
do_encode(IPv6 = {_, _, _, _, _, _, _, _}, Opts) ->
    compact(tuple_to_list(IPv6), Opts);
do_encode(I, Opts = #opts{ipv6ipv4 = true}) when I > ?UINT32_MAX ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, A1, B1, C1, D1>> = <<I:128>>,
    do_encode({A, B, C, D, E, F, {A1, B1, C1, D1}}, Opts);
do_encode(I, Opts) when I > ?UINT32_MAX ->
    do_encode(list_to_tuple([X || <<X:16>> <= <<I:128>>]), Opts);
do_encode(I, Opts = #opts{format = ipv4}) ->
    do_encode(list_to_tuple([X || <<X:8>> <= <<I:32>>]), Opts);
do_encode(I, Opts = #opts{format = ipv6, ipv6ipv4 = true}) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, A1, B1, C1, D1>> = <<I:128>>,
    do_encode({A, B, C, D, E, F, {A1, B1, C1, D1}}, Opts);
do_encode(I, Opts = #opts{format = ipv6}) ->
    do_encode(list_to_tuple([X || <<X:16>> <= <<I:128>>]), Opts).

compact(IPv6, #opts{compact = false}) -> join([hex(I) || I <- IPv6], $:);
compact(IPv6, #opts{ipv6ipv4 = IPv6IPv4}) ->
    case longest_zeros(IPv6, 0, 0, 0, start, 0) of
        {_, 0} -> join([hex(I) || I <- IPv6], $:);
        {0, _} -> [$:, drop_zeros(IPv6, IPv6IPv4)];
        {Start, _} -> drop_zeros(IPv6, 0, Start, IPv6IPv4)
    end.

longest_zeros([], _, Start, Length, _, _) -> {Start, Length};
longest_zeros([0 | T], N, _, Length, Start1, Length1) when Length1 > Length->
    longest_zeros(T, N + 1, Start1, Length1 + 1, Start1, Length1 + 1);
longest_zeros([0 | T], N, Start, Length, start, _) ->
    longest_zeros(T, N + 1, Start, Length, N, 1);
longest_zeros([0 | T], N, Start, Length, Start1, Length1) ->
    longest_zeros(T, N + 1, Start, Length, Start1, Length1 + 1);
longest_zeros([_ | T], N, Start, Length, _, _) ->
    longest_zeros(T, N + 1, Start, Length, start, 0).

drop_zeros([_ | T], Start, Start, IPv6IPv4) -> drop_zeros(T, IPv6IPv4);
drop_zeros([H | T], N, Start, IPv6IPv4) ->
    [hex(H), $: | drop_zeros(T, N + 1, Start, IPv6IPv4)].

drop_zeros([], true) -> [];
drop_zeros([], false) -> [$:];
drop_zeros([0 | T], IPv6IPv4) -> drop_zeros(T, IPv6IPv4);
drop_zeros(T, _) -> [$: | join([hex(I) || I <- T], $:)].

hex(I) -> bstring:to_lower(integer_to_binary(I, 16)).

join([], _) -> [];
join([H | T], Sep) -> [H | [[Sep, E] || E <- T]].

%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(IP, Opts) -> decode_ip(IP, [], [], Opts).

decode_ip(I, Acc, Parts, Opts) ->
    case next(I) of
        {$:, T} -> decode_ipv6(T, [], [to_binary(Acc) | Parts], Opts);
        {$., T} -> decode_ipv4(T, [], [to_binary(Acc) | Parts], Opts);
        {H, T} when ?IS_INT(H) -> decode_ip(T, [H | Acc], Parts, Opts);
        {H, T} when ?IS_HEX(H) -> decode_ipv6(T, [H | Acc], Parts, Opts);
        {H, T} when ?IS_HEX(H) -> decode_ipv6(T, [H | Acc], Parts, Opts)
    end.

decode_ipv4(I, Acc, Parts, Opts) ->
    case next(I) of
        eos -> decode_ipv4_host([to_binary(Acc) | Parts], Opts);
        {$., T} -> decode_ipv4(T, [], [to_binary(Acc) | Parts], Opts);
        {H, T} when ?IS_INT(H) -> decode_ipv4(T, [H | Acc], Parts, Opts);
        {$/, T} when Opts#opts.range ->
            decode_ipv4_range(T, [], [to_binary(Acc) | Parts],Opts);
        _ when Opts#opts.continue ->
            {decode_ipv4_host([to_binary(Acc) | Parts], Opts), I};
        _ ->
            decode_ipv4_host([to_binary(Acc) | Parts], Opts)
    end.

decode_ipv4_host([D, C, B, A], #opts{return_type = tuple}) ->
    {binary_to_integer(A),
     binary_to_integer(B),
     binary_to_integer(C),
     binary_to_integer(D)};
decode_ipv4_host([D, C, B, A], _) ->
    <<I:32>> = <<(binary_to_integer(A)),
                 (binary_to_integer(B)),
                 (binary_to_integer(C)),
                 (binary_to_integer(D))>>,
    I.

decode_ipv4_range(I, Acc = [_, _], IP, Opts = #opts{continue = true}) ->
    {decode_ipv4_host(IP, Opts), binary_to_integer(to_binary(Acc)), I};
decode_ipv4_range(_, Acc = [_, _], IP, Opts) ->
    {decode_ipv4_host(IP, Opts), binary_to_integer(to_binary(Acc))};
decode_ipv4_range(I, Acc, IP, Opts) ->
    case next(I) of
        eos -> {decode_ipv4_host(IP, Opts),binary_to_integer(to_binary(Acc))};
        {H, T} when ?IS_INT(H) -> decode_ipv4_range(T, [H | Acc], IP, Opts);
        _ when Opts#opts.continue ->
            {decode_ipv4_host(IP,Opts),binary_to_integer(to_binary(Acc))};
        _ ->
            {decode_ipv4_host(IP,Opts),binary_to_integer(to_binary(Acc))}
    end.

decode_ipv6(I, Acc, Parts, Opts) ->
    case next(I) of
        eos -> decode_ipv6_host([to_binary(Acc) | Parts], Opts);
        {$:, T} -> decode_ipv6(T, [], [to_binary(Acc) | Parts], Opts);
        {H, T} when ?IS_HEX(H) -> decode_ipv6(T, [H | Acc], Parts, Opts);
        {$/, T} when Opts#opts.range ->
            decode_ipv6_range(T, [], [to_binary(Acc) | Parts],Opts);
        _ when Opts#opts.continue ->
            {decode_ipv6_host([to_binary(Acc) | Parts], Opts), I};
        _ ->
            decode_ipv6_host([to_binary(Acc) | Parts], Opts)
    end.

decode_ipv6_host([H | T], Opts)  when byte_size(H) > 4 ->
    {A, B , C, D} = decode_ipv6ipv4(H, [], []),
    <<H7:16/unsigned-integer, H8:16/unsigned-integer>> =
        <<A:8/unsigned-integer,
          B:8/unsigned-integer,
          C:8/unsigned-integer,
          D:8/unsigned-integer>>,
    case [decode_hex(E) || E <- lists:reverse(T)] of
        Decoded when length(Decoded) == 6 ->
            format_ipv6(ensure_non_empty(Decoded ++ [H7, H8]), Opts);
        [empty, empty | Decoded] ->
            Pad = lists:duplicate(6 - length(Decoded), 0),
            format_ipv6(ensure_non_empty(Pad ++ Decoded ++ [H7, H8]), Opts);
        Decoded ->
            format_ipv6(ipv6_fill(Decoded ++ [H7, H8],6 - length(Decoded)),Opts)
    end;
decode_ipv6_host(L, Opts) ->
    case [decode_hex(E) || E <- lists:reverse(L)] of
        [empty, empty, empty] -> {0, 0, 0, 0, 0, 0, 0, 0};
        [empty, empty | Decoded] ->
            Pad = lists:duplicate(8 - length(Decoded), 0),
            format_ipv6(ensure_non_empty(Pad ++ Decoded), Opts);
        Decoded ->
            format_ipv6(ipv6_fill(Decoded, 8 - length(Decoded)), Opts)
    end.

decode_ipv6_range(I, Acc = [_, _, _], IP, Opts = #opts{continue = true}) ->
    {decode_ipv6_host(IP, Opts), binary_to_integer(to_binary(Acc)), I};
decode_ipv6_range(_, Acc = [_, _, _], IP, Opts) ->
    {decode_ipv6_host(IP, Opts), binary_to_integer(to_binary(Acc))};
decode_ipv6_range(I, Acc, IP, Opts) ->
    case next(I) of
        eos -> {decode_ipv6_host(IP, Opts),binary_to_integer(to_binary(Acc))};
        {H, T} when ?IS_INT(H) -> decode_ipv6_range(T, [H | Acc], IP, Opts);
        _ when Opts#opts.continue ->
            {decode_ipv6_host(IP,Opts),binary_to_integer(to_binary(Acc)), I};
        _ ->
            {decode_ipv6_host(IP,Opts),binary_to_integer(to_binary(Acc))}
    end.

ipv6_fill(L, 0) -> ensure_non_empty(L);
ipv6_fill([empty, empty], N) -> ensure_non_empty(lists:duplicate(N + 2, 0));
ipv6_fill([empty | T], N) -> ensure_non_empty(lists:duplicate(N + 1, 0) ++ T);
ipv6_fill([H | T], N) when H /= empty -> [H | ipv6_fill(T, N)].

ensure_non_empty(E) ->
    case lists:any(fun(empty) -> true; (_) -> false end, E) of
        true -> erlang:error(badarg);
        false -> E
    end.

decode_hex(<<>>) -> empty;
decode_hex(Hex) ->
    <<Value:16/unsigned-integer>> =
        case [unhex(C) || <<C>> <= Hex] of
            [D] -> <<0:12, D:4/unsigned-integer>>;
            [C, D] -> <<0:8, C:4/unsigned-integer, D:4/unsigned-integer>>;
            [B, C, D] ->
                <<0:4/unsigned-integer,
                  B:4/unsigned-integer,
                  C:4/unsigned-integer,
                  D:4/unsigned-integer>>;
            [A, B, C, D] ->
                <<A:4/unsigned-integer,
                  B:4/unsigned-integer,
                  C:4/unsigned-integer,
                  D:4/unsigned-integer>>
    end,
    Value.

decode_ipv6ipv4(<<>>, Acc, Parts) ->
    list_to_tuple([binary_to_integer(I) ||
                      I <- lists:reverse([to_binary(Acc) | Parts])]);
decode_ipv6ipv4(<<$., T/binary>>, Acc, Parts) ->
    decode_ipv6ipv4(T, [], [to_binary(Acc) | Parts]);
decode_ipv6ipv4(<<H, T/binary>>, Acc, Parts) when ?IS_INT(H) ->
    decode_ipv6ipv4(T, [H | Acc], Parts);
decode_ipv6ipv4(_, _, _) ->
    false.

format_ipv6([A, B, C, D, E, F, G, H],#opts{return_type=tuple, ipv6ipv4=true}) ->
    <<A1, B1, C1, D1>> = <<G:16, H:16>>,
    {A, B, C, D, E, F, {A1, B1, C1, D1}};
format_ipv6(L, #opts{return_type = tuple}) -> list_to_tuple(L);
format_ipv6(L, _) ->
    <<I:128>> = << <<X:16>> || X <- L>>,
    I.

next(<<>>) -> eos;
next(<<H, T/binary>>) -> {H, T};
next([]) -> eos;
next([H | T]) when is_integer(H) -> {H, T};
next([L | T]) when is_list(L) -> next({L, [T]});
next([B | T]) when is_binary(B) -> next({B, [T]});
next({[], []}) -> eos;
next({<<>>, []}) -> eos;
next({[], [H | T]}) -> next({H, T});
next({<<>>, [H | T]}) -> next({H, T});
next({[H | T], Stack}) when is_integer(H) -> {H, {T, Stack}};
next({<<H, T/binary>>, Stack}) -> {H, {T, Stack}};
next({[L | T], Stack}) when is_list(L) -> next({L, [T | Stack]});
next({[B | T], Stack}) when is_binary(B) -> next({B, [T | Stack]}).

to_binary(Acc) -> list_to_binary(lists:reverse(Acc)).

unhex($0) ->  0;
unhex($1) ->  1;
unhex($2) ->  2;
unhex($3) ->  3;
unhex($4) ->  4;
unhex($5) ->  5;
unhex($6) ->  6;
unhex($7) ->  7;
unhex($8) ->  8;
unhex($9) ->  9;
unhex($A) -> 10;
unhex($B) -> 11;
unhex($C) -> 12;
unhex($D) -> 13;
unhex($E) -> 14;
unhex($F) -> 15;
unhex($a) -> 10;
unhex($b) -> 11;
unhex($c) -> 12;
unhex($d) -> 13;
unhex($e) -> 14;
unhex($f) -> 15.

%% ===================================================================
%% Encoding
%% ===================================================================

do_bounds({IP, Range}, Opts) when is_integer(IP) ->
    calculate_bounds(IP, Range, Opts);
do_bounds({IP, Range}, Opts) ->
    calculate_bounds(decode(encode(IP)), Range, Opts);
do_bounds(IOData, Opts) ->
    do_bounds(decode(IOData, [range]), Opts).

calculate_bounds(IP, Range, Opts=#opts{format = ipv4}) when IP < ?UINT32_MAX ->
    calculate_bounds_ipv4(IP, Range, Opts);
calculate_bounds(IP, Range, Opts) ->
    calculate_bounds_ipv6(IP, Range, Opts).

calculate_bounds_ipv4(IP, Range, #opts{return_type = tuple}) ->
    Mask = mask_ipv4(Range),
    <<L3, L2, L1, L0>> = <<(IP band Mask):32>>,
    <<H3, H2, H1, H0>> = <<(IP bor bnot Mask):32>>,
    {{L3, L2, L1, L0}, {H3, H2, H1, H0}};
calculate_bounds_ipv4(IP, Range, _) ->
    Mask = mask_ipv4(Range),
    <<H:32>> = <<(IP bor bnot Mask):32>>,
    {IP band Mask, H}.

mask_ipv4(N) when N =< 32 -> 16#FFFFFFFF bsl (32 - N).

calculate_bounds_ipv6(IP, Range, #opts{return_type = tuple, ipv6ipv4 = true}) ->
    Mask = mask_ipv6(Range),
    <<L0:12/binary, L43, L42, L41, L40>> = <<(IP band Mask):128>>,
    L = list_to_tuple([X || <<X:16>> <= L0] ++  [{L43, L42, L41, L40}]),
    <<R0:12/binary, R43, R42, R41, R40>> = <<(IP bor bnot Mask):128>>,
    R = list_to_tuple([X || <<X:16>> <= R0] ++  [{R43, R42, R41, R40}]),
    {L, R};
calculate_bounds_ipv6(IP, Range, #opts{return_type = tuple}) ->
    Mask = mask_ipv6(Range),
    L = list_to_tuple([X || <<X:16>> <= <<(IP band Mask):128>>]),
    R = list_to_tuple([X || <<X:16>> <= <<(IP bor bnot Mask):128>>]),
    {L, R};
calculate_bounds_ipv6(IP, Range, _) ->
    Mask = mask_ipv6(Range),
    <<H:128>> = <<(IP bor bnot Mask):128>>,
    {IP band Mask, H}.

mask_ipv6(N) when N =< 128 -> 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF bsl (128 - N).

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(list, Opts) -> Opts#opts{return_type = list};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(integer, Opts) -> Opts#opts{return_type = integer};
parse_opt(tuple, Opts) -> Opts#opts{return_type = tuple};
parse_opt(ipv4, Opts) -> Opts#opts{format = ipv4};
parse_opt(ipv6, Opts) -> Opts#opts{format = ipv6};
parse_opt(ipv6ipv4, Opts) -> Opts#opts{ipv6ipv4 = true};
parse_opt(continue, Opts) -> Opts#opts{continue = true};
parse_opt(range, Opts) -> Opts#opts{range = true};
parse_opt(compact, Opts) -> Opts#opts{compact = true};
parse_opt(_, _) -> erlang:error(badarg).
