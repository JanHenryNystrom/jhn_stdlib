%%==============================================================================
%% Copyright 2015 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A URI library based on:
%%%    Uniform Resource Identifier (URI): Generic Syntax               (rfc3986)
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2015, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(uri).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2
        ]).

%% Exported types
-export_type([uri/0]).

%% Includes
-include_lib("jhn_stdlib/include/uri.hrl").

%% Records
-record(opts, {ipv4 = false :: boolean(),
               return_type = iolist :: iolist | binary,
               orig_call}).

%% Types
-type uri() :: #uri{}.

-type opt() :: none.


%% Defines

%% Char macros
-define(IS_WS(WS), WS == ?HT; WS == ?LF; WS == ?CR; WS == ?SPC).

%% Decode macros
-define(IS_INT(C), C>=$0, C=<$9).
-define(IS_ALPHA(C), C>=$a, C=<$z; C>=$A, C=<$Z).
-define(IS_SCHEME(C), C>=$a, C=<$z; C>=$A, C=<$Z; C == $+; C == $-; C == $.).

-define(IS_UNRESERVED_DELIM(C),
            C >= $a, C =< $z; C >=$A, C =< $Z;
            C >=$0, C =< $9; C >= $&, C =< $.;
            C == $!; C == $$; C == $;; C == $=;
            C == $_; C == $~).

-define(IS_PCHAR(C),
            C >= $a, C =< $z; C >=$@, C =< $Z;
            C >=$0, C =< $9; C >= $&, C =< $.;
            C == $!; C == $$; C == $:; C == $;;
            C == $=; C == $_; C == $~).

-define(IS_NC(C),
            C >= $a, C =< $z; C >=$@, C =< $Z;
            C >=$0, C =< $9; C >= $&, C =< $.;
            C == $!; C == $$; C == $;;
            C == $=; C == $_; C == $~).

-define(IS_HEX(C),
            C >= $a, C =< $f; C >= $A, C =< $F;
            C >= $0, C =< $9; C == $.).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Term) -> URI.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent of encode(Term, []) -> URI.
%% @end
%%--------------------------------------------------------------------
-spec encode(uri()) -> iolist().
%%--------------------------------------------------------------------
encode(Term) ->
    encode(Term, #opts{orig_call = {encode, [Term], ?LINE}}).

%%--------------------------------------------------------------------
%% Function: encode(Term, Options) -> URI
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     iolist -> a iolist is returned
%%     ipv4 -> encoded IPv6 host address has the two least sigificant
%%             segments repesented in IPv4 address format
%% @end
%%--------------------------------------------------------------------
-spec encode(uri(), [opt()] | #opts{}) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Term, Opts = #opts{}) -> do_encode(Term, Opts);
encode(Term, Opts) ->
    Line = ?LINE,
    ParsedOpts =
        parse_opts(Opts, #opts{orig_call = {encode, [Term, Opts], Line}}),
    case ParsedOpts#opts.return_type of
        iolist-> do_encode(Term, ParsedOpts);
        binary -> iolist_to_binary(do_encode(Term, ParsedOpts))
    end.

%%--------------------------------------------------------------------
%% Function: decode(Binary) -> URI.
%% @doc
%%   Decodes the binary into a structured Erlang term.
%%   Equivalent of decode(Binary, []) -> URI.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> uri().
%%--------------------------------------------------------------------
decode(Binary) ->
    Line = ?LINE,
    decode(Binary, #opts{orig_call = {decode, [Binary], Line}}).

%%--------------------------------------------------------------------
%% Function: decode(Binary, Options) -> URI.
%% @doc
%%   Decodes the binary into a structured Erlang.
%%   Decode will give an exception if the binary is not well formed URI.
%%   Options are:
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), [opt()] | #opts{}) -> uri().
%%--------------------------------------------------------------------
decode(Binary, Opts = #opts{}) -> do_decode(Binary, Opts);
decode(Binary, Opts) ->
    Line = ?LINE,
    ParsedOpts =
        parse_opts(Opts, #opts{orig_call = {decode, [Binary, Opts], Line}}),
    do_decode(Binary, ParsedOpts).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

do_encode(URI = #uri{host = undefined}, _) ->
    #uri{scheme = Scheme,
         path = Path,
         query = Query,
         fragment = Fragment} = URI,
    S = case Scheme of
            undefined -> [];
            _ -> [atom_to_binary(Scheme, utf8), $:]
        end,
    P = join(Path, $/),
    Q = case Query of
            <<>> -> [];
            _ -> [$?, Query]
        end,
    F = case Fragment of
            <<>> -> [];
            _ -> [$#, Fragment]
        end,
    [S, P, Q, F];
do_encode(URI = #uri{}, Opts) ->
    #uri{scheme = Scheme,
         userinfo = UserInfo,
         host = Host,
         port = Port,
         path = Path,
         query = Query,
         fragment = Fragment} = URI,
    S = case Scheme of
            undefined -> [];
            _ -> [atom_to_binary(Scheme, utf8), "://"]
        end,
    I  = case UserInfo of
             [] -> [];
             _ -> [join(UserInfo, $:), $@]
         end,
    Po = case Port of
             undefined -> [];
             _ -> [$:, integer_to_binary(Port)]
         end,
    Pa = case Path of
             [] -> [];
             _ -> [$/, join(Path, $/)]
         end,
    Q = case Query of
            <<>> -> [];
            _ -> [$?, Query]
        end,
    F = case Fragment of
            <<>> -> [];
            _ -> [$#, Fragment]
        end,
    [S, I, encode_host(Host, Opts), Po, Pa, Q, F].

encode_host(Bin, _) when is_binary(Bin) -> Bin;
encode_host(IPv4 = {_, _, _, _}, _) ->
    join([integer_to_binary(I) || I <- tuple_to_list(IPv4)], $.);
encode_host({A, B, C, D, E, F, G, H}, Opts = #opts{ipv4 = true}) ->
    <<A1:8/unsigned-integer,
      B1:8/unsigned-integer,
      C1:8/unsigned-integer,
      D1:8/unsigned-integer>> = <<G:16/unsigned-integer, H:16/unsigned-integer>>,
    IPv4 = encode_host({A1, B1, C1, D1}, Opts),
    [join([integer_to_binary(I) || I <- [A, B, C, D, E, F]], $:), $:, IPv4];
encode_host(IPv6 = {_, _, _, _, _, _, _, _}, _) ->
    join([integer_to_binary(I) || I <- tuple_to_list(IPv6)], $:).

join([], _) -> [];
join([H | T], Sep) -> [H | [[Sep, E] || E <- T]].

%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(IOData, Opts) ->
    case next(IOData) of
        {$/, _} -> decode_authority_or_path(IOData, [], #uri{}, Opts);
        {$?, T} -> decode_query(T, [], #uri{}, Opts);
        {$#, T} -> decode_fragment(T, [], #uri{}, Opts);
        {$%, T} ->
            {H, T1} = decode_escaped(T),
            decode_nc(T1, [H], Opts);
        {C, T} when ?IS_ALPHA(C) ->
            decode_scheme(T, [C], Opts);
        {C, T} when ?IS_NC(C) ->
            decode_nc(T, [C], Opts);
        _ -> badarg(Opts)
    end.

decode_scheme(I, Acc, Opts) ->
    case next(I) of
        eos -> #uri{path = [to_binary(Acc)]};
        {$:, T} ->
            URI = #uri{scheme = to_scheme(Acc, [])},
            decode_authority_or_path(T, [], URI, Opts);
        {$%, T} ->
            {H, T1} = decode_escaped(T),
            decode_nc(T1, [H | Acc], Opts);
        {$/, T} ->
            decode_path(T, [], [to_binary(Acc)], #uri{}, Opts);
        {C, T} when ?IS_SCHEME(C) ->
            decode_scheme(T, [C | Acc], Opts);
        {C, T} when ?IS_NC(C) ->
            decode_nc(T, [C| Acc], Opts);
        _ ->
            badarg(Opts)
    end.

decode_nc(I, Acc, Opts) ->
    case next(I) of
        eos -> #uri{path = [to_binary(Acc)]};
        {$/, T} -> decode_path(T, [], [to_binary(Acc)], #uri{}, Opts);
        {$?, T} -> decode_query(T, [], #uri{path = [to_binary(Acc)]}, Opts);
        {$#, T} -> decode_fragment(T, [], #uri{path = [to_binary(Acc)]}, Opts);
        {$%, T} ->
            {H, T1} = decode_escaped(T),
            decode_nc(T1, [H | Acc], Opts);
        {H, T} when ?IS_NC(H) ->
            decode_nc(T, [H | Acc], Opts);
        _ ->
            badarg(Opts)
    end.
            
decode_authority_or_path(I, Acc, URI, Opts) ->
    case {next(I), Acc} of
        {eos, _} -> URI#uri{path = to_binary(Acc)};
        {{$/, T}, []} -> decode_authority_or_path(T, [$/], URI, Opts);
        {{$/, T}, [$/]} -> decode_authority(T, [], URI, Opts);
        {{$%, T}, _} ->
            {H, T1} = decode_escaped(T),
            decode_path(T1, [H], [], URI, Opts);
        {{H, T}, _} when ?IS_PCHAR(H) ->
            decode_path(T, [H], [], URI, Opts);
        _ ->
            badarg(Opts)
    end.

decode_authority(I, Acc, URI, Opts) ->
    case next(I) of
        eos -> URI#uri{host = to_binary(Acc)};
        {$/, T} ->
            URI1 = URI#uri{host = to_binary(Acc)},
            decode_path(T, [], [], URI1, Opts);
        {$?, T} ->
            URI1 = URI#uri{host = to_binary(Acc)},
            decode_query(T, [], URI1, Opts);
        {$#, T} ->
            URI1 = URI#uri{host = to_binary(Acc)},
            decode_fragment(T, [], URI1, Opts);
        {$:, T} ->
            Component = to_binary(Acc),
            decode_userinfo(T, [], [Component], URI, Opts);
        {$@, T} ->
            URI1 = URI#uri{userinfo = [to_binary(Acc)]},
            decode_host(T, [], URI1, Opts);
        {$[, T} ->
            decode_ipv6(T, [], [], URI, Opts);
        {$%, T} ->
            {H, T1} = decode_escaped(T),
            decode_authority(T1, [H | Acc], URI, Opts);
        {H, T} when ?IS_UNRESERVED_DELIM(H) ->
            decode_authority(T, [H | Acc], URI, Opts);
        _ ->
            badarg(Opts)
    end.

decode_userinfo(I, Acc, Components, URI, Opts) ->
    case next(I) of
        eos ->
            [Host] = Components,
            URI#uri{host = Host, port = to_integer(Acc)};
        {$/, T} ->
            [Host] = Components,
            URI1 = URI#uri{host = Host,
                           port = to_integer(Acc)},
            decode_path(T, [], [], URI1, Opts);
        {$?, T} ->
            [Host] = Components,
            URI1 = URI#uri{host = Host,
                           port = to_integer(Acc)},
            decode_query(T, [], URI1, Opts);
        {$#, T} ->
            [Host] = Components,
            URI1 = URI#uri{host = Host,
                           port = to_integer(Acc)},
            decode_fragment(T, [], URI1, Opts);
        {$@, T} ->
            URI1 = URI#uri{userinfo =
                               lists:reverse([to_binary(Acc) | Components])},
            decode_host(T, [], URI1, Opts);
        {$:, T} ->
            Component = list_to_binary(lists:reverse(Acc)),
            decode_userinfo(T, [], [Component | Components], URI, Opts);
        {$%, T} ->
            {H, T1} = decode_escaped(T),
            decode_userinfo(T1, [H | Acc], Components, URI, Opts);
        {H, T} when ?IS_UNRESERVED_DELIM(H) ->
            decode_userinfo(T, [H | Acc], Components, URI, Opts);
        _ ->
            badarg(Opts)
    end.

decode_host(I, Acc, URI, Opts) ->
    case next(I) of
        eos -> URI#uri{host = to_binary(Acc)};
        {$/, T} -> decode_path(T, [], [], URI#uri{host = to_binary(Acc)},Opts);
        {$?, T} -> decode_query(T, [], URI#uri{host = to_binary(Acc)}, Opts);
        {$#, T} -> decode_fragment(T, [], URI#uri{host = to_binary(Acc)},Opts);
        {$:, T} -> decode_port(T, [], URI#uri{host = to_binary(Acc)}, Opts);
        {$[, T} when Acc == [] -> decode_ipv6(T, [], [], URI, Opts);
        {$%, T} ->
            {H, T1} = decode_escaped(T),
            decode_host(T1, [H | Acc], URI, Opts);
        {H, T} when ?IS_UNRESERVED_DELIM(H) ->
            decode_host(T, [H | Acc], URI, Opts);
        _ ->
            badarg(Opts)
    end.

decode_port(I, Acc, URI, Opts) ->
    case next(I) of
        eos -> URI#uri{port = to_integer(Acc)};
        {$/, T} -> decode_path(T, [], [], URI#uri{port =to_integer(Acc)},Opts);
        {$?, T} -> decode_query(T, [], URI#uri{port = to_integer(Acc)}, Opts);
        {$#, T} -> decode_fragment(T, [], URI#uri{port =to_integer(Acc)},Opts);
        {H, T} when ?IS_INT(H) -> decode_port(T, [H | Acc], URI, Opts);
        _ ->
            badarg(Opts)
    end.

decode_ipv6(I, Acc, Components, URI, Opts) ->
    case next(I) of
        {$:, T} ->
            decode_ipv6(T, [], [to_binary(Acc) | Components], URI, Opts);
        {$], T} ->
            IPv6 =
                decode_ipv6_host([to_binary(Acc) | Components], Opts),
            URI1 = URI#uri{host = IPv6},
            case next(T) of
                eos -> URI1;
                {$/, T1} -> decode_path(T1, [], [], URI1, Opts);
                {$?, T1} -> decode_query(T1, [], URI1, Opts);
                {$#, T1} -> decode_fragment(T1, [], URI1,Opts);
                {$:, T1} -> decode_port(T1, [], URI1, Opts);
                _ -> badarg(Opts)
            end;
        {H, T} when ?IS_HEX(H) ->
            decode_ipv6(T, [H | Acc], Components, URI, Opts);
        _ ->
            badarg(Opts)
    end.


decode_ipv6_host([H | T], Opts)  when byte_size(H) > 4 ->
    {A, B , C, D} = decode_ipv4(H, [], []),
    <<H7:16/unsigned-integer, H8:16/unsigned-integer>> =
        <<A:8/unsigned-integer,
          B:8/unsigned-integer,
          C:8/unsigned-integer,
          D:8/unsigned-integer>>,
    case [decode_hex(E) || E <- lists:reverse(T)] of
        Decoded when length(Decoded) == 6 ->
            list_to_tuple(ensure_non_empty(Decoded ++ [H7, H8], Opts));
        [empty, empty | Decoded] ->
            Pad = lists:duplicate(6 - length(Decoded), 0),
            list_to_tuple(ensure_non_empty(Pad ++ Decoded ++ [H7, H8], Opts));
        Decoded ->
            list_to_tuple(ipv6_fill(Decoded ++ [H7, H8], 6 - length(Decoded),
                                    Opts))
    end;
decode_ipv6_host(L, Opts) ->
    case [decode_hex(E) || E <- lists:reverse(L)] of
        [empty, empty, empty] -> {0, 0, 0, 0, 0, 0, 0, 0};
        [empty, empty | Decoded] ->
            Pad = lists:duplicate(8 - length(Decoded), 0),
            list_to_tuple(ensure_non_empty(Pad ++ Decoded, Opts));
        Decoded ->
            list_to_tuple(ipv6_fill(Decoded, 8 - length(Decoded), Opts))
    end.

ipv6_fill(L, 0, Opts) -> ensure_non_empty(L, Opts);
ipv6_fill([empty, empty], N, Opts) ->
    ensure_non_empty(lists:duplicate(N + 2, 0), Opts);
ipv6_fill([empty | T], N, Opts) ->
    ensure_non_empty(lists:duplicate(N + 1, 0) ++ T, Opts);
ipv6_fill([H | T], N, Opts) when H /= empty ->
    [H | ipv6_fill(T, N, Opts)].

ensure_non_empty(E, Opts) ->
    case lists:any(fun(empty) -> true; (_) -> false end, E) of
        true -> badarg(Opts);
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

decode_ipv4(<<>>, Acc, Components) ->
    list_to_tuple([binary_to_integer(I) ||
                      I <- lists:reverse([to_binary(Acc) | Components])]);
decode_ipv4(<<$., T/binary>>, Acc, Components) ->
    decode_ipv4(T, [], [to_binary(Acc) | Components]);
decode_ipv4(<<H, T/binary>>, Acc, Components) when ?IS_INT(H) ->
    decode_ipv4(T, [H | Acc], Components);
decode_ipv4(_, _, _) ->
    false.


decode_path(I, Acc, Components, URI, Opts) ->
    case next(I) of
        eos -> URI#uri{path = lists:reverse([to_binary(Acc) | Components])};
        {$?, T} ->
            URI1 = URI#uri{path = lists:reverse([to_binary(Acc) | Components])},
            decode_query(T, [], URI1, Opts);
        {$#, T} ->
            URI1 = URI#uri{path = lists:reverse([to_binary(Acc) | Components])},
            decode_fragment(T, [], URI1, Opts);
        {$/, T} ->
            decode_path(T, [], [to_binary(Acc) | Components], URI, Opts);
        {$%, T} ->
            {H, T1} = decode_escaped(T),
            decode_path(T1, [H | Acc], Components, URI, Opts);
        {H, T} when ?IS_PCHAR(H) ->
            decode_path(T, [H | Acc], Components, URI, Opts);
        _ ->
            badarg(Opts)
    end.

decode_query(I, Acc, URI, Opts) ->
    case next(I) of
        eos -> URI#uri{query = to_binary(Acc)};
        {$#, T} -> decode_fragment(T, [], URI#uri{query=to_binary(Acc)}, Opts);
        {$%, T} ->
            {H, T1} = decode_escaped(T),
            decode_query(T1, [H | Acc], URI, Opts);
        {H, T} when ?IS_PCHAR(H); H == $/; H == $? ->
            decode_query(T, [H | Acc], URI, Opts)
    end.

decode_fragment(I, Acc, URI, Opts) ->
    case next(I) of
        eos -> URI#uri{fragment = to_binary(Acc)};
        {$%, T} ->
            {H, T1} = decode_escaped(T),
            decode_fragment(T1, [H | Acc], URI, Opts);
        {H, T} when ?IS_PCHAR(H); H == $/; $? ->
            decode_fragment(T, [H | Acc], URI, Opts)
    end.


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

to_scheme([], Acc) -> list_to_atom(Acc);
to_scheme([C | T], Acc) when C >= $A, C =< $Z ->  to_scheme(T, [C + 32 | Acc]);
to_scheme([C | T], Acc) -> to_scheme(T, [C | Acc]).

to_binary(Acc) -> list_to_binary(lists:reverse(Acc)).

to_integer(Acc) -> list_to_integer(lists:reverse(Acc)).

decode_escaped(I) ->
    {Hi, T} = next(I),
    {Lo, T1} = next(T),
    {unhex(Hi, Lo), T1}.

unhex(High, Low) -> unhex(High) bsl 4 bor unhex(Low).

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
%% Common parts
%% ===================================================================

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(ipv4, Opts) -> Opts#opts{ipv4 = true};
parse_opt(_, Opts) -> badarg(Opts).

%%--------------------------------------------------------------------
-spec badarg(#opts{}) -> no_return().
%%--------------------------------------------------------------------
badarg(#opts{orig_call = {Funcion, Args, Line}}) ->
    Trace = [{?MODULE, Funcion, Args, [{file, ?FILE}, {line, Line - 1}]} |
             lists:dropwhile(fun(T) -> element(1, T) == ?MODULE end,
                             erlang:get_stacktrace())],
    exit({badarg, Trace}).
