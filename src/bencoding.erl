%%==============================================================================
%% Copyright 2018-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A Bencoding library based on:
%%%    The BitTorrent specification
%%%
%%%  Bencoding is represented as follows:
%%%
%%%  Byte string : binary (octets)
%%%  Integer     : integer
%%%  List        : list
%%%  Dictionary  : map
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2018-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(bencoding).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2
        ]).

%% Exported types
-export_type([bencoding/0]).

%% Types
-type bencoding() :: binary() | integer() | list() | map().
-type opt()       :: binary | iolist | continue.

%% Records
-record(opts, {return_type = iolist :: binary() | iolist(),
               continue    = false :: boolean()
              }).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Term) -> Bencoding.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent of encode(Term, []) -> Bencoding.
%% @end
%%--------------------------------------------------------------------
-spec encode(bencoding()) -> iolist().
%%--------------------------------------------------------------------
encode(Term) -> encode(Term, []).

%%--------------------------------------------------------------------
%% Function: encode(Term, Options) -> Bencoding
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     iolist -> an iolist is returned (default)
%% @end
%%--------------------------------------------------------------------
-spec encode(bencoding(), [opt()]) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Term, Opts) ->
    Encoded = do_encode(Term),
    case (parse_opts(Opts, #opts{}))#opts.return_type of
        iolist -> Encoded;
        binary -> iolist_to_binary(Encoded)
    end.

%%--------------------------------------------------------------------
%% Function: decode(Binary) -> Term.
%% @doc
%%   Decodes the iodata into a structured Erlang term.
%%   Equivalent of decode(JSON, []) -> Term.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> bencoding().
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, []).

%%--------------------------------------------------------------------
%% Function: decode(Binary) -> Term | {Term, Rest}.
%% @doc
%%   Decodes the iodata into a structured Erlang term.
%%   Options are:
%%     continue -> return the tuple of the decoded Bencoding and the
%%                 remaining binary
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), [opt()]) -> bencoding() | {bencoding(), binary()}.
%%--------------------------------------------------------------------
decode(Binary, Opts) ->
    case (parse_opts(Opts, #opts{}))#opts.continue of
        true -> do_decode(Binary);
        false -> element(1, do_decode(Binary))
    end.

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================
do_encode(<<>>) -> "0:";
do_encode(String = <<_/binary>>) ->
    [integer_to_binary(byte_size(String)), ":", String];
do_encode([]) ->
    "le";
do_encode(List = [_|_]) ->
    [$l, [do_encode(E) || E <- List], $e];
do_encode(Map = #{}) ->
    case maps:to_list(Map) of
        [] -> "de";
        List ->
            [$d,
             [[encode_string(K), do_encode(V)] ||
                 {K, V} <- lists:keysort(1, List)],
             $e]
    end;
do_encode(Integer) ->
    [$i, integer_to_binary(Integer), $e].

encode_string(String = <<_/binary>>) ->
    [integer_to_binary(byte_size(String)), ":", String].

%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(<<$i, I/binary>>) -> decode_integer(I, []);
do_decode(<<$l, L/binary>>) -> decode_list(L, []);
do_decode(<<$d, D/binary>>) -> decode_dictionary(D, []);
do_decode(B) -> decode_string(B, []).
    
decode_integer(<<$e, T/binary>>, Acc) ->
    {list_to_integer(lists:reverse(Acc)), T};
decode_integer(<<H, T/binary>>, Acc) ->
    decode_integer(T, [H | Acc]).

decode_list(<<$e, T/binary>>, Acc) -> {lists:reverse(Acc), T};
decode_list(Bin, Acc) ->
    {E, T} = do_decode(Bin),
    decode_list(T, [E | Acc]).

decode_dictionary(<<$e, T/binary>>, Acc) -> {maps:from_list(Acc), T};
decode_dictionary(Bin, Acc) ->
    {K, T} = decode_string(Bin, []),
    {V, T1} = do_decode(T),
    decode_dictionary(T1, [{K, V} | Acc]).

decode_string(<<$:, T/binary>>, Acc) ->
    Len = list_to_integer(lists:reverse(Acc)),
    <<String:Len/binary, T1/binary>> = T,
    {String, T1};
decode_string(<<H, T/binary>>, Acc) ->
    decode_string(T, [H | Acc]).

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(binary, Rec) -> Rec#opts{return_type = binary};
parse_opt(iolist, Rec) -> Rec#opts{return_type = iolist};
parse_opt(continue, Rec) -> Rec#opts{continue = true};
parse_opt(_, _) -> erlang:error(badarg).
