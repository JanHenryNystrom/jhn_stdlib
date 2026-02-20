-module(jhn_hpack).

-export([decode/1, decode/2,
         encode/1, encode/2]).

-define(MAX_SIZE, 4096).

-record(context,
        {table          = []     :: [header()],
         without        = []     :: [header_name()],
         never          = []     :: [header_name()],
         max                     :: non_neg_integer(),
         limit                   :: non_neg_integer(),
         length         = 0      :: non_neg_integer(),
         size           = 0      :: non_neg_integer(),
         %% encode
         huffman_encode = false  :: boolean(),
         return_type    = binary :: binary | iolist
        }).

-record(opts,
        {limit          = 4096   :: non_neg_integer(),
         max            = 4096   :: non_neg_integer(),
         return_type    = binary :: binary | iolist,
         huffman_encode = false  :: boolean(),
         without        = []     :: [header_name()],
         never          = []     :: [header_name()]
        }).

-type header_name()  :: binary().
-type header_value() :: binary().
-type header()       :: {header_name(), header_value()}.
-type size_update()  :: {size_update, non_neg_integer()}.
-type field()        :: header() | size_update().

-type context() :: #context{}.

-type opts() :: #{limit => non_neg_integer(),
                  return_type => binary | iolist,
                  huffman_encode => boolean()
                 }.


-export_type([header/0]).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
-spec decode(binary()) -> {ok, [field()], context()}.
%%--------------------------------------------------------------------
decode(Bin) -> decode(Bin, #{}).

%%--------------------------------------------------------------------
-spec decode(binary(), context() | opts()) -> {ok, [field()], context()}.
%%--------------------------------------------------------------------
decode(Bin, Ctx = #context{}) -> decode(Bin, Ctx, []);
decode(Bin, Opts = #{}) ->
    #opts{limit = L, max = M} = maps:fold(fun opt/3, #opts{}, Opts),
    decode(Bin, #context{limit = L, max = M}).

%%--------------------------------------------------------------------
-spec encode([field()]) -> {ok, binary(), context()}.
%%--------------------------------------------------------------------
encode(Headers) -> encode(Headers, #{}).

%%--------------------------------------------------------------------
-spec encode([field()], context() | opts()) -> {ok, iodata(), context()}.
%%--------------------------------------------------------------------
encode(Headers, Ctx = #context{}) -> encode(Headers, Ctx, []);
encode(Headers, Opts = #{}) ->
    #opts{return_type = R, limit = L, max = M, huffman_encode = H,
          without = W, never = N} = maps:fold(fun opt/3, #opts{}, Opts),
    Ctx = #context{limit = L, max = M, huffman_encode = H, return_type = R,
                   without = W, never = N},
    encode(Headers, Ctx).

%% ===================================================================
%% Library functions.
%% ===================================================================

%% -------------------------------------------------------------------
%% Opts
%% -------------------------------------------------------------------

opt(return_type, binary, Opts) -> Opts#opts{return_type = binary};
opt(return_type, iolist, Opts) -> Opts#opts{return_type = iolist};
opt(huffman_encode, true, Opts) -> Opts#opts{huffman_encode = true};
opt(huffman_encode, false, Opts) -> Opts#opts{huffman_encode = false};
opt(without, W, Opts) -> Opts#opts{without = lists:sort(W)};
opt(never, N, Opts) -> Opts#opts{never = lists:sort(N)};
opt(limit, L, Opts = #opts{max = M}) when is_integer(L), M =< L ->
    Opts#opts{limit = L};
opt(max, M, Opts = #opts{limit = L}) when is_integer(M), M =< L ->
    Opts#opts{max = M}.


%% -------------------------------------------------------------------
%% Decode
%% -------------------------------------------------------------------

decode(<<>>, Ctx = #context{never = N}, Acc) ->
    {ok, lists:reverse(Acc), Ctx#context{never = lists:usort(N)}};
%% Indexed header
decode(<<2#1:1, B/bits>>, Ctx, Acc) ->
    {Index, B1} = decode_integer(B, 7),
    decode(B1, Ctx, [lookup(Index, Ctx) | Acc]);
%% Literal header with indexing
decode(<<2#01:2, 2#000000:6, B/bits>>, Ctx, Acc) ->
    {Name, Value, B1} = indexed_none(B),
    decode(B1, add(Name, Value, Ctx), [{Name, Value} | Acc]);
decode(<<2#01:2, B/bits>>, Ctx, Acc) ->
    {Name, Value, B1} = indexed_name(B, Ctx, 6),
    decode(B1, add(Name, Value, Ctx), [{Name, Value} | Acc]);
%% Dynamic table size update
decode(<<2#001:3, B/bits>>, Ctx = #context{limit = L}, []) ->
    case decode_integer(B, 5) of
        {Size, B1} when L >= Size -> decode(B1, resize(Size, Ctx));
        _ -> {error, compression_error}
    end;
%% literal header without indexing
decode(<<2#0000:4, 2#0000:4, B/bits>>, Ctx, Acc) ->
    {Name, Value, B1} = indexed_none(B),
    decode(B1, Ctx, [{Name, Value} | Acc]);
decode(<<2#0000:4, B/bits>>, Ctx, Acc) ->
    {Name, Value, B1} = indexed_name(B, Ctx, 4),
    decode(B1, Ctx, [{Name, Value} | Acc]);
%% literal header never indexed
decode(<<2#0001:4, 2#0000:4, B/bits>>, Ctx, Acc) ->
    {Name, Value, B1} = indexed_none(B),
    decode(B1, never(Name, Ctx), [{Name, Value} | Acc]);
decode(<<2#0001:4, B/bits>>, Ctx, Acc) ->
    {Name, Value, B1} = indexed_name(B, Ctx, 4),
    decode(B1, never(Name, Ctx), [{Name, Value} | Acc]).

indexed_name(B, Ctx, NoBits) ->
    {Index, B1} = decode_integer(B, NoBits),
    {Name, _} = lookup(Index, Ctx),
    {Value, B2} = decode_string(B1),
    {Name, Value, B2}.

indexed_none(B) ->
    {Name, B1} = decode_string(B),
    {Value, B2} = decode_string(B1),
    {Name, Value, B2}.

%% -------------------------------------------------------------------
%% Encode
%% -------------------------------------------------------------------

encode([], Ctx = #context{return_type = R}, Acc) ->
    Decoded = case R of
                 binary -> iolist_to_binary(lists:reverse(Acc));
                 iolist -> lists:reverse(Acc)
             end,
    {ok, Decoded, Ctx};
encode([{size_update, U} | T], Ctx, Acc) when is_integer(U) ->
    Ctx1 = case Ctx of
               #context{limit = L} when U =< L ->
                   resize(U, Ctx#context{max = U});
               _ ->
                   exit(encoding_error)
           end,
    encode(T, Ctx1, [[<<2#001:3, (encode_integer(U, 5))/bits>>] | Acc]);
encode([{Name, Val} | T], Ctx = #context{huffman_encode = H}, Acc) ->
    {Encoded, Ctx1} =
        case match({Name, Val}, Ctx) of
            {indexed, I} when I < 63 -> {<<2#1:1, I:7>>, Ctx};
            {indexed, I} -> {<<2#1:1, (encode_integer(I, 7))/bits>>, Ctx};
            {indexed_name, I} ->
                case index(Name, Ctx) of
                    yes ->
                        {<<2#01:2,
                           (encode_integer(I, 6))/bits,
                           (encode_string(H, Val))/binary>>,
                         add(Name, Val, Ctx)};
                    no ->
                        {<<2#0000:4,
                           (encode_integer(I, 4))/bits,
                           (encode_string(H, Val))/binary>>,
                         Ctx};
                    never ->
                        {<<2#0001:4,
                           (encode_integer(I, 4))/bits,
                           (encode_string(H, Val))/binary>>,
                         Ctx}
                end;
            wo_indexing ->
                case index(Name, Ctx) of
                    yes ->
                        {<<2#01000000,
                           (encode_string(H, Name))/binary,
                           (encode_string(H, Val))/binary>>,
                         add(Name, Val, Ctx)};
                    no ->
                        {<<2#0000:4, 2#0000:4,
                           (encode_string(H, Name))/binary,
                           (encode_string(H, Val))/binary>>,
                         Ctx};
                    never ->
                        {<<2#0001:4, 2#0000:4,
                           (encode_string(H, Name))/binary,
                           (encode_string(H, Val))/binary>>,
                         Ctx}
                end
        end,
    encode(T, Ctx1, [Encoded | Acc]).

%% -------------------------------------------------------------------
%% Index
%% -------------------------------------------------------------------

add(Name, Value, Ctx) -> add1({Name, Value}, entry_size({Name, Value}), Ctx).

never(Name, Ctx = #context{never = N}) -> Ctx#context{never = [Name | N]}.

index(Name, #context{without = W, never = N}) ->
    case lists:member(Name, N) of
        true -> never;
        false -> case lists:member(Name, W) of
                     true -> no;
                     false -> yes
                 end
    end.

lookup(1, _) -> {~":authority", ~""};
lookup(2, _) -> {~":method", ~"GET"};
lookup(3, _) -> {~":method", ~"POST"};
lookup(4, _) -> {~":path", ~"/"};
lookup(5, _) -> {~":path", ~"/index.html"};
lookup(6, _) -> {~":scheme", ~"http"};
lookup(7, _) -> {~":scheme", ~"https"};
lookup(8, _) -> {~":status", ~"200"};
lookup(9, _) -> {~":status", ~"204"};
lookup(10, _) -> {~":status", ~"206"};
lookup(11, _) -> {~":status", ~"304"};
lookup(12, _) -> {~":status", ~"400"};
lookup(13, _) -> {~":status", ~"404"};
lookup(14, _) -> {~":status", ~"500"};
lookup(15, _) -> {~"accept-charset", ~""};
lookup(16, _) -> {~"accept-encoding", ~"gzip, deflate"};
lookup(17, _) -> {~"accept-language", ~""};
lookup(18, _) -> {~"accept-ranges", ~""};
lookup(19, _) -> {~"accept", ~""};
lookup(20, _) -> {~"access-control-allow-origin", ~""};
lookup(21, _) -> {~"age", ~""};
lookup(22, _) -> {~"allow", ~""};
lookup(23, _) -> {~"authorization", ~""};
lookup(24, _) -> {~"cache-control", ~""};
lookup(25, _) -> {~"content-disposition", ~""};
lookup(26, _) -> {~"content-encoding", ~""};
lookup(27, _) -> {~"content-language", ~""};
lookup(28, _) -> {~"content-length", ~""};
lookup(29, _) -> {~"content-location", ~""};
lookup(30, _) -> {~"content-range", ~""};
lookup(31, _) -> {~"content-type", ~""};
lookup(32, _) -> {~"cookie", ~""};
lookup(33, _) -> {~"date", ~""};
lookup(34, _) -> {~"etag", ~""};
lookup(35, _) -> {~"expect", ~""};
lookup(36, _) -> {~"expires", ~""};
lookup(37, _) -> {~"from", ~""};
lookup(38, _) -> {~"host", ~""};
lookup(39, _) -> {~"if-match", ~""};
lookup(40, _) -> {~"if-modified-since", ~""};
lookup(41, _) -> {~"if-none-match", ~""};
lookup(42, _) -> {~"if-range", ~""};
lookup(43, _) -> {~"if-unmodified-since", ~""};
lookup(44, _) -> {~"last-modified", ~""};
lookup(45, _) -> {~"link", ~""};
lookup(46, _) -> {~"location", ~""};
lookup(47, _) -> {~"max-forwards", ~""};
lookup(48, _) -> {~"proxy-authenticate", ~""};
lookup(49, _) -> {~"proxy-authorization", ~""};
lookup(50, _) -> {~"range", ~""};
lookup(51, _) -> {~"referer", ~""};
lookup(52, _) -> {~"refresh", ~""};
lookup(53, _) -> {~"retry-after", ~""};
lookup(54, _) -> {~"server", ~""};
lookup(55, _) -> {~"set-cookie", ~""};
lookup(56, _) -> {~"strict-transport-security", ~""};
lookup(57, _) -> {~"transfer-encoding", ~""};
lookup(58, _) -> {~"user-agent", ~""};
lookup(59, _) -> {~"vary", ~""};
lookup(60, _) -> {~"via", ~""};
lookup(61, _) -> {~"www-authenticate", ~""};
lookup(Index, #context{table = Table}) -> lists:nth(Index - 61, Table).

match({~":authority", ~""}, _) -> {indexed, 1};
match({~":method", ~"GET"}, _) -> {indexed, 2};
match({~":method", ~"POST"}, _) -> {indexed, 3};
match({~":path", ~"/"}, _) -> {indexed, 4};
match({~":path", ~"/index.html"}, _) -> {indexed, 5};
match({~":scheme", ~"http"}, _) -> {indexed, 6};
match({~":scheme", ~"https"}, _) -> {indexed, 7};
match({~":status", ~"200"}, _) -> {indexed, 8};
match({~":status", ~"204"}, _) -> {indexed, 9};
match({~":status", ~"206"}, _) -> {indexed, 10};
match({~":status", ~"304"}, _) -> {indexed, 11};
match({~":status", ~"400"}, _) -> {indexed, 12};
match({~":status", ~"404"}, _) -> {indexed, 13};
match({~":status", ~"500"}, _) -> {indexed, 14};
match({~"accept-charset", ~""}, _) -> {indexed, 15};
match({~"accept-encoding", ~"gzip, deflate"}, _) -> {indexed, 16};
match({~"accept-language", ~""}, _) -> {indexed, 17};
match({~"accept-ranges", ~""}, _) -> {indexed, 18};
match({~"accept", ~""}, _) -> {indexed, 19};
match({~"access-control-allow-origin", ~""}, _) -> {indexed, 20};
match({~"age", ~""}, _) -> {indexed, 21};
match({~"allow", ~""}, _) -> {indexed, 22};
match({~"authorization", ~""}, _) -> {indexed, 23};
match({~"cache-control", ~""}, _) -> {indexed, 24};
match({~"content-disposition", ~""}, _) -> {indexed, 25};
match({~"content-encoding", ~""}, _) -> {indexed, 26};
match({~"content-language", ~""}, _) -> {indexed, 27};
match({~"content-length", ~""}, _) -> {indexed, 28};
match({~"content-location", ~""}, _) -> {indexed, 29};
match({~"content-range", ~""}, _) -> {indexed, 30};
match({~"content-type", ~""}, _) -> {indexed, 31};
match({~"cookie", ~""}, _) -> {indexed, 32};
match({~"date", ~""}, _) -> {indexed, 33};
match({~"etag", ~""}, _) -> {indexed, 34};
match({~"expect", ~""}, _) -> {indexed, 35};
match({~"expires", ~""}, _) -> {indexed, 36};
match({~"from", ~""}, _) -> {indexed, 37};
match({~"host", ~""}, _) -> {indexed, 38};
match({~"if-match", ~""}, _) -> {indexed, 39};
match({~"if-modified-since", ~""}, _) -> {indexed, 40};
match({~"if-none-match", ~""}, _) -> {indexed, 41};
match({~"if-range", ~""}, _) -> {indexed, 42};
match({~"if-unmodified-since", ~""}, _) -> {indexed, 43};
match({~"last-modified", ~""}, _) -> {indexed, 44};
match({~"link", ~""}, _) -> {indexed, 45};
match({~"location", ~""}, _) -> {indexed, 46};
match({~"max-forwards", ~""}, _) -> {indexed, 47};
match({~"proxy-authenticate", ~""}, _) -> {indexed, 48};
match({~"proxy-authorization", ~""}, _) -> {indexed, 49};
match({~"range", ~""}, _) -> {indexed, 50};
match({~"referer", ~""}, _) -> {indexed, 51};
match({~"refresh", ~""}, _) -> {indexed, 52};
match({~"retry-after", ~""}, _) -> {indexed, 53};
match({~"server", ~""}, _) -> {indexed, 54};
match({~"set-cookie", ~""}, _) -> {indexed, 55};
match({~"strict-transport-security", ~""}, _) -> {indexed, 56};
match({~"transfer-encoding", ~""}, _) -> {indexed, 57};
match({~"user-agent", ~""}, _) -> {indexed, 58};
match({~"vary", ~""}, _) -> {indexed, 59};
match({~"via", ~""}, _) -> {indexed, 60};
match({~"www-authenticate", ~""}, _) -> {indexed, 61};
match(Entry = {Name, _}, #context{table = Tab}) ->
    match(62, Tab, Entry, static_match(Name)).

match(_, [], _, undefined) -> wo_indexing;
match(_, [], _, I) -> {indexed_name, I};
match(I, [Entry | _], Entry, _) -> {indexed, I};
match(I, [E = {N, ~""} | T], {N, _}, undefined) -> match(I + 1, T, E, I);
match(I, [_ | T], Entry, Found) -> match(I + 1, T, Entry, Found).

static_match(~":authority") -> 1;
static_match(~":method") -> 2;
static_match(~":path") -> 4;
static_match(~":scheme") -> 6;
static_match(~":status") -> 8;
static_match(~"accept-charset") -> 15;
static_match(~"accept-encoding") -> 16;
static_match(~"accept-language") -> 17;
static_match(~"accept-ranges") -> 18;
static_match(~"accept") -> 19;
static_match(~"access-control-allow-origin") -> 20;
static_match(~"age") -> 21;
static_match(~"allow") -> 22;
static_match(~"authorization") -> 23;
static_match(~"cache-control") -> 24;
static_match(~"content-disposition") -> 25;
static_match(~"content-encoding") -> 26;
static_match(~"content-language") -> 27;
static_match(~"content-length") -> 28;
static_match(~"content-location") -> 29;
static_match(~"content-range") -> 30;
static_match(~"content-type") -> 31;
static_match(~"cookie") -> 32;
static_match(~"date") -> 33;
static_match(~"etag") -> 34;
static_match(~"expect") -> 35;
static_match(~"expires") -> 36;
static_match(~"from") -> 37;
static_match(~"host") -> 38;
static_match(~"if-match") -> 39;
static_match(~"if-modified-since") -> 40;
static_match(~"if-none-match") -> 41;
static_match(~"if-range") -> 42;
static_match(~"if-unmodified-since") -> 43;
static_match(~"last-modified") -> 44;
static_match(~"link") -> 45;
static_match(~"location") -> 46;
static_match(~"max-forwards") -> 47;
static_match(~"proxy-authenticate") -> 48;
static_match(~"proxy-authorization") -> 49;
static_match(~"range") -> 50;
static_match(~"referer") -> 51;
static_match(~"refresh") -> 52;
static_match(~"retry-after") -> 53;
static_match(~"server") -> 54;
static_match(~"set-cookie") -> 55;
static_match(~"strict-transport-security") -> 56;
static_match(~"transfer-encoding") -> 57;
static_match(~"user-agent") -> 58;
static_match(~"vary") -> 59;
static_match(~"via") -> 60;
static_match(~"www-authenticate") -> 61;
static_match(_) -> undefined.

resize(Max, Ctx = #context{size = S}) when S =< Max -> Ctx#context{max = Max};
resize(Max, Ctx) -> resize(Max, drop(Ctx)).

add1(E, ES, Ctx = #context{size = S, length = Len, max = M, table = Tab}) ->
    case S + ES of
        NS when NS > M -> add1(E, ES, drop(Ctx));
        NS -> Ctx#context{size = NS, length = Len + 1, table = [E | Tab]}
    end.

drop(Ctx = #context{size = S, length = Len, table = Tab}) ->
    {Tab1, [E]} = lists:split(Len - 1, Tab),
    Ctx#context{size = S - entry_size(E), length = Len - 1, table = Tab1}.

entry_size(E = <<_/binary>>) -> 32 + byte_size(E);
entry_size({Name, Value}) -> 32 + byte_size(Name) + byte_size(Value).


%% -------------------------------------------------------------------
%% integer
%% -------------------------------------------------------------------

%% Decode

decode_integer(<<1:1, B/bits>>, 1) -> plus(1, decode_integer(B, 0, 0));
decode_integer(<<3:2, B/bits>>, 2) -> plus(3, decode_integer(B, 0, 0));
decode_integer(<<7:3, B/bits>>, 3) -> plus(7, decode_integer(B, 0, 0));
decode_integer(<<15:4, B/bits>>, 4) -> plus(15, decode_integer(B, 0, 0));
decode_integer(<<31:5, B/bits>>, 5) -> plus(31, decode_integer(B, 0, 0));
decode_integer(<<63:6, B/bits>>, 6) -> plus(63, decode_integer(B, 0, 0));
decode_integer(<<127:7, B/bits>>, 7) -> plus(127, decode_integer(B, 0, 0));
decode_integer(<<255:8, B/bits>>, 8) -> plus(255, decode_integer(B, 0, 0));
decode_integer(B, Prefix) ->
    <<Value:Prefix, T/bits>> = B,
    {Value, T}.

decode_integer(<<0:1, Int:7, T/binary>>, M, I) -> {I + Int * pow(2, M), T};
decode_integer(<<1:1, Int:7, T/binary>>, M, I) ->
    decode_integer(T, M + 7, I + Int * pow(2, M)).

plus(Prefix, {I, B}) -> {Prefix + I, B}.

pow(2, 0) -> 1;
pow(2, 7) -> 128;
pow(2, 14) -> 16384;
pow(2, 21) -> 2097152;
pow(2, 28) -> 268435456;
pow(2, 35) -> 34359738368;
pow(2, 42) -> 4398046511104.

%% Encode

encode_integer(1, 1) -> <<1:1, 0:8>>;
encode_integer(3, 2) -> <<3:2, 0:8>>;
encode_integer(7, 3) -> <<7:3, 0:8>>;
encode_integer(15, 4) -> <<15:4, 0:8>>;
encode_integer(31, 5) -> <<31:5, 0:8>>;
encode_integer(63, 6) -> <<63:6, 0:8>>;
encode_integer(127, 7) -> <<127:7, 0:8>>;
encode_integer(255, 8) -> <<255, 0>>;
encode_integer(Int, N) when Int < (1 bsl N - 1) -> <<Int:N>>;
encode_integer(Int, N) ->
    Prefix = 1 bsl N - 1,
    <<Prefix:N, (encode_integer1(Int - Prefix, <<>>))/binary>>.

encode_integer1(I, Acc) ->
    LeastSig = (I rem 128),
    case I bsr 7 of
        0 -> <<Acc/binary, LeastSig>>;
        T -> encode_integer1(T, <<Acc/binary, (128 + LeastSig)>>)
    end.

%% -------------------------------------------------------------------
%% String
%% -------------------------------------------------------------------

%% Decode

decode_string(<<>>) -> {<<>>, <<>>};
decode_string(<<Huff:1, B/bits>>) ->
    {Length, B1} = decode_integer(B, 7),
    <<Raw:Length/binary, B2/bits>> = B1,
    String = case Huff of
                 1 -> decode_huffman(Raw);
                 0 -> Raw
             end,
    {String, B2}.

%% Encode

encode_string(true, S) ->
    H = encode_huffman(S),
    <<2#1:1, (encode_integer(byte_size(H), 7))/bits, H/binary>>;
encode_string(false, S) ->
    <<2#0:1, (encode_integer(byte_size(S), 7))/bits, S/binary>>.

%% -------------------------------------------------------------------
%% String
%% -------------------------------------------------------------------

%% Decode

decode_huffman(Bin) -> decode_h(Bin, <<>>).

decode_h(<<16#1FF8:13, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 0>>);
decode_h(<<16#7FFFD8:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 1>>);
decode_h(<<16#FFFFFE2:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 2>>);
decode_h(<<16#FFFFFE3:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 3>>);
decode_h(<<16#FFFFFE4:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 4>>);
decode_h(<<16#FFFFFE5:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 5>>);
decode_h(<<16#FFFFFE6:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 6>>);
decode_h(<<16#FFFFFE7:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 7>>);
decode_h(<<16#FFFFFE8:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 8>>);
decode_h(<<16#FFFFEA:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 9>>);
decode_h(<<16#3FFFFFFC:30, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 10>>);
decode_h(<<16#FFFFFE9:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 11>>);
decode_h(<<16#FFFFFEA:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 12>>);
decode_h(<<16#3FFFFFFD:30, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 13>>);
decode_h(<<16#FFFFFEB:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 14>>);
decode_h(<<16#FFFFFEC:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 15>>);
decode_h(<<16#fffffed:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 16>>);
decode_h(<<16#fffffee:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 17>>);
decode_h(<<16#fffffef:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 18>>);
decode_h(<<16#ffffff0:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 19>>);
decode_h(<<16#ffffff1:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 20>>);
decode_h(<<16#ffffff2:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 21>>);
decode_h(<<16#3ffffffe:30, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 22>>);
decode_h(<<16#ffffff3:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 23>>);
decode_h(<<16#ffffff4:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 24>>);
decode_h(<<16#ffffff5:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 25>>);
decode_h(<<16#ffffff6:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 26>>);
decode_h(<<16#ffffff7:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 27>>);
decode_h(<<16#ffffff8:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 28>>);
decode_h(<<16#ffffff9:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 29>>);
decode_h(<<16#ffffffa:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 30>>);
decode_h(<<16#ffffffb:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 31>>);
decode_h(<<16#14:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 32>>);
decode_h(<<16#3f8:10, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 33>>);
decode_h(<<16#3f9:10, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 34>>);
decode_h(<<16#ffa:12, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 35>>);
decode_h(<<16#1ff9:13, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 36>>);
decode_h(<<16#15:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 37>>);
decode_h(<<16#f8:8, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 38>>);
decode_h(<<16#7fa:11, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 39>>);
decode_h(<<16#3fa:10, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 40>>);
decode_h(<<16#3fb:10, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 41>>);
decode_h(<<16#f9:8, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 42>>);
decode_h(<<16#7fb:11, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 43>>);
decode_h(<<16#fa:8, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 44>>);
decode_h(<<16#16:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 45>>);
decode_h(<<16#17:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 46>>);
decode_h(<<16#18:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 47>>);
decode_h(<<16#0:5, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 48>>);
decode_h(<<16#1:5, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 49>>);
decode_h(<<16#2:5, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 50>>);
decode_h(<<16#19:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 51>>);
decode_h(<<16#1a:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 52>>);
decode_h(<<16#1b:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 53>>);
decode_h(<<16#1c:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 54>>);
decode_h(<<16#1d:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 55>>);
decode_h(<<16#1e:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 56>>);
decode_h(<<16#1f:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 57>>);
decode_h(<<16#5c:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 58>>);
decode_h(<<16#fb:8, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 59>>);
decode_h(<<16#7ffc:15, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 60>>);
decode_h(<<16#20:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 61>>);
decode_h(<<16#ffb:12, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 62>>);
decode_h(<<16#3fc:10, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 63>>);
decode_h(<<16#1ffa:13, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 64>>);
decode_h(<<16#21:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 65>>);
decode_h(<<16#5d:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 66>>);
decode_h(<<16#5e:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 67>>);
decode_h(<<16#5f:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 68>>);
decode_h(<<16#60:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 69>>);
decode_h(<<16#61:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 70>>);
decode_h(<<16#62:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 71>>);
decode_h(<<16#63:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 72>>);
decode_h(<<16#64:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 73>>);
decode_h(<<16#65:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 74>>);
decode_h(<<16#66:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 75>>);
decode_h(<<16#67:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 76>>);
decode_h(<<16#68:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 77>>);
decode_h(<<16#69:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 78>>);
decode_h(<<16#6a:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 79>>);
decode_h(<<16#6b:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 80>>);
decode_h(<<16#6c:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 81>>);
decode_h(<<16#6d:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 82>>);
decode_h(<<16#6e:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 83>>);
decode_h(<<16#6f:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 84>>);
decode_h(<<16#70:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 85>>);
decode_h(<<16#71:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 86>>);
decode_h(<<16#72:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 87>>);
decode_h(<<16#fc:8, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 88>>);
decode_h(<<16#73:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 89>>);
decode_h(<<16#fd:8, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 90>>);
decode_h(<<16#1ffb:13, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 91>>);
decode_h(<<16#7fff0:19, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 92>>);
decode_h(<<16#1ffc:13, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 93>>);
decode_h(<<16#3ffc:14, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 94>>);
decode_h(<<16#22:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 95>>);
decode_h(<<16#7ffd:15, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 96>>);
decode_h(<<16#3:5, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 97>>);
decode_h(<<16#23:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 98>>);
decode_h(<<16#4:5, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 99>>);
decode_h(<<16#24:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 100>>);
decode_h(<<16#5:5, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 101>>);
decode_h(<<16#25:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 102>>);
decode_h(<<16#26:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 103>>);
decode_h(<<16#27:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 104>>);
decode_h(<<16#6:5, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 105>>);
decode_h(<<16#74:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 106>>);
decode_h(<<16#75:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 107>>);
decode_h(<<16#28:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 108>>);
decode_h(<<16#29:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 109>>);
decode_h(<<16#2a:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 110>>);
decode_h(<<16#7:5, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 111>>);
decode_h(<<16#2b:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 112>>);
decode_h(<<16#76:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 113>>);
decode_h(<<16#2c:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 114>>);
decode_h(<<16#8:5, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 115>>);
decode_h(<<16#9:5, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 116>>);
decode_h(<<16#2d:6, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 117>>);
decode_h(<<16#77:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 118>>);
decode_h(<<16#78:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 119>>);
decode_h(<<16#79:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 120>>);
decode_h(<<16#7a:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 121>>);
decode_h(<<16#7b:7, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 122>>);
decode_h(<<16#7ffe:15, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 123>>);
decode_h(<<16#7fc:11, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 124>>);
decode_h(<<16#3ffd:14, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 125>>);
decode_h(<<16#1ffd:13, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 126>>);
decode_h(<<16#ffffffc:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 127>>);
decode_h(<<16#fffe6:20, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 128>>);
decode_h(<<16#3fffd2:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 129>>);
decode_h(<<16#fffe7:20, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 130>>);
decode_h(<<16#fffe8:20, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 131>>);
decode_h(<<16#3fffd3:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 132>>);
decode_h(<<16#3fffd4:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 133>>);
decode_h(<<16#3fffd5:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 134>>);
decode_h(<<16#7fffd9:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 135>>);
decode_h(<<16#3fffd6:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 136>>);
decode_h(<<16#7fffda:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 137>>);
decode_h(<<16#7fffdb:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 138>>);
decode_h(<<16#7fffdc:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 139>>);
decode_h(<<16#7fffdd:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 140>>);
decode_h(<<16#7fffde:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 141>>);
decode_h(<<16#ffffeb:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 142>>);
decode_h(<<16#7fffdf:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 143>>);
decode_h(<<16#ffffec:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 144>>);
decode_h(<<16#ffffed:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 145>>);
decode_h(<<16#3fffd7:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 146>>);
decode_h(<<16#7fffe0:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 147>>);
decode_h(<<16#ffffee:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 148>>);
decode_h(<<16#7fffe1:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 149>>);
decode_h(<<16#7fffe2:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 150>>);
decode_h(<<16#7fffe3:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 151>>);
decode_h(<<16#7fffe4:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 152>>);
decode_h(<<16#1fffdc:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 153>>);
decode_h(<<16#3fffd8:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 154>>);
decode_h(<<16#7fffe5:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 155>>);
decode_h(<<16#3fffd9:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 156>>);
decode_h(<<16#7fffe6:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 157>>);
decode_h(<<16#7fffe7:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 158>>);
decode_h(<<16#ffffef:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 159>>);
decode_h(<<16#3fffda:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 160>>);
decode_h(<<16#1fffdd:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 161>>);
decode_h(<<16#fffe9:20, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 162>>);
decode_h(<<16#3fffdb:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 163>>);
decode_h(<<16#3fffdc:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 164>>);
decode_h(<<16#7fffe8:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 165>>);
decode_h(<<16#7fffe9:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 166>>);
decode_h(<<16#1fffde:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 167>>);
decode_h(<<16#7fffea:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 168>>);
decode_h(<<16#3fffdd:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 169>>);
decode_h(<<16#3fffde:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 170>>);
decode_h(<<16#fffff0:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 171>>);
decode_h(<<16#1fffdf:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 172>>);
decode_h(<<16#3fffdf:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 173>>);
decode_h(<<16#7fffeb:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 174>>);
decode_h(<<16#7fffec:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 175>>);
decode_h(<<16#1fffe0:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 176>>);
decode_h(<<16#1fffe1:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 177>>);
decode_h(<<16#3fffe0:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 178>>);
decode_h(<<16#1fffe2:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 179>>);
decode_h(<<16#7fffed:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 180>>);
decode_h(<<16#3fffe1:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 181>>);
decode_h(<<16#7fffee:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 182>>);
decode_h(<<16#7fffef:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 183>>);
decode_h(<<16#fffea:20, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 184>>);
decode_h(<<16#3fffe2:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 185>>);
decode_h(<<16#3fffe3:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 186>>);
decode_h(<<16#3fffe4:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 187>>);
decode_h(<<16#7ffff0:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 188>>);
decode_h(<<16#3fffe5:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 189>>);
decode_h(<<16#3fffe6:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 190>>);
decode_h(<<16#7ffff1:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 191>>);
decode_h(<<16#3ffffe0:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 192>>);
decode_h(<<16#3ffffe1:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 193>>);
decode_h(<<16#fffeb:20, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 194>>);
decode_h(<<16#7fff1:19, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 195>>);
decode_h(<<16#3fffe7:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 196>>);
decode_h(<<16#7ffff2:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 197>>);
decode_h(<<16#3fffe8:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 198>>);
decode_h(<<16#1ffffec:25, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 199>>);
decode_h(<<16#3ffffe2:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 200>>);
decode_h(<<16#3ffffe3:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 201>>);
decode_h(<<16#3ffffe4:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 202>>);
decode_h(<<16#7ffffde:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 203>>);
decode_h(<<16#7ffffdf:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 204>>);
decode_h(<<16#3ffffe5:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 205>>);
decode_h(<<16#fffff1:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 206>>);
decode_h(<<16#1ffffed:25, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 207>>);
decode_h(<<16#7fff2:19, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 208>>);
decode_h(<<16#1fffe3:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 209>>);
decode_h(<<16#3ffffe6:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 210>>);
decode_h(<<16#7ffffe0:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 211>>);
decode_h(<<16#7ffffe1:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 212>>);
decode_h(<<16#3ffffe7:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 213>>);
decode_h(<<16#7ffffe2:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 214>>);
decode_h(<<16#fffff2:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 215>>);
decode_h(<<16#1fffe4:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 216>>);
decode_h(<<16#1fffe5:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 217>>);
decode_h(<<16#3ffffe8:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 218>>);
decode_h(<<16#3ffffe9:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 219>>);
decode_h(<<16#ffffffd:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 220>>);
decode_h(<<16#7ffffe3:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 221>>);
decode_h(<<16#7ffffe4:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 222>>);
decode_h(<<16#7ffffe5:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 223>>);
decode_h(<<16#fffec:20, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 224>>);
decode_h(<<16#fffff3:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 225>>);
decode_h(<<16#fffed:20, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 226>>);
decode_h(<<16#1fffe6:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 227>>);
decode_h(<<16#3fffe9:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 228>>);
decode_h(<<16#1fffe7:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 229>>);
decode_h(<<16#1fffe8:21, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 230>>);
decode_h(<<16#7ffff3:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 231>>);
decode_h(<<16#3fffea:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 232>>);
decode_h(<<16#3fffeb:22, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 233>>);
decode_h(<<16#1ffffee:25, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 234>>);
decode_h(<<16#1ffffef:25, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 235>>);
decode_h(<<16#fffff4:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 236>>);
decode_h(<<16#fffff5:24, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 237>>);
decode_h(<<16#3ffffea:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 238>>);
decode_h(<<16#7ffff4:23, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 239>>);
decode_h(<<16#3ffffeb:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 240>>);
decode_h(<<16#7ffffe6:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 241>>);
decode_h(<<16#3ffffec:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 242>>);
decode_h(<<16#3ffffed:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 243>>);
decode_h(<<16#7ffffe7:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 244>>);
decode_h(<<16#7ffffe8:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 245>>);
decode_h(<<16#7ffffe9:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 246>>);
decode_h(<<16#7ffffea:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 247>>);
decode_h(<<16#7ffffeb:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 248>>);
decode_h(<<16#ffffffe:28, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 249>>);
decode_h(<<16#7ffffec:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 250>>);
decode_h(<<16#7ffffed:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 251>>);
decode_h(<<16#7ffffee:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 252>>);
decode_h(<<16#7ffffef:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 253>>);
decode_h(<<16#7fffff0:27, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 254>>);
decode_h(<<16#3ffffee:26, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 255>>);
decode_h(<<16#3fffffff:30, B/bits>>, Acc) -> decode_h(B, <<Acc/binary, 256>>);
decode_h(_, Acc) -> Acc.

%% Encode

encode_huffman(Bin) -> encode_h(Bin, <<>>).

encode_h(<<0:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1ff8:13>>);
encode_h(<<1:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffd8:23>>);
encode_h(<<2:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffe2:28>>);
encode_h(<<3:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffe3:28>>);
encode_h(<<4:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffe4:28>>);
encode_h(<<5:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffe5:28>>);
encode_h(<<6:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffe6:28>>);
encode_h(<<7:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffe7:28>>);
encode_h(<<8:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffe8:28>>);
encode_h(<<9:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffea:24>>);
encode_h(<<10:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffffc:30>>);
encode_h(<<11:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffe9:28>>);
encode_h(<<12:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffea:28>>);
encode_h(<<13:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffffd:30>>);
encode_h(<<14:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffeb:28>>);
encode_h(<<15:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffec:28>>);
encode_h(<<16:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffed:28>>);
encode_h(<<17:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffee:28>>);
encode_h(<<18:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffffef:28>>);
encode_h(<<19:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffff0:28>>);
encode_h(<<20:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffff1:28>>);
encode_h(<<21:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffff2:28>>);
encode_h(<<22:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffffe:30>>);
encode_h(<<23:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffff3:28>>);
encode_h(<<24:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffff4:28>>);
encode_h(<<25:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffff5:28>>);
encode_h(<<26:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffff6:28>>);
encode_h(<<27:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffff7:28>>);
encode_h(<<28:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffff8:28>>);
encode_h(<<29:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffff9:28>>);
encode_h(<<30:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffffa:28>>);
encode_h(<<31:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffffb:28>>);
encode_h(<<32:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#14:6>>);
encode_h(<<33:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3f8:10>>);
encode_h(<<34:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3f9:10>>);
encode_h(<<35:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffa:12>>);
encode_h(<<36:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1ff9:13>>);
encode_h(<<37:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#15:6>>);
encode_h(<<38:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#f8:8>>);
encode_h(<<39:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fa:11>>);
encode_h(<<40:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fa:10>>);
encode_h(<<41:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fb:10>>);
encode_h(<<42:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#f9:8>>);
encode_h(<<43:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fb:11>>);
encode_h(<<44:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fa:8>>);
encode_h(<<45:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#16:6>>);
encode_h(<<46:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#17:6>>);
encode_h(<<47:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#18:6>>);
encode_h(<<48:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#0:5>>);
encode_h(<<49:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1:5>>);
encode_h(<<50:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#2:5>>);
encode_h(<<51:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#19:6>>);
encode_h(<<52:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1a:6>>);
encode_h(<<53:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1b:6>>);
encode_h(<<54:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1c:6>>);
encode_h(<<55:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1d:6>>);
encode_h(<<56:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1e:6>>);
encode_h(<<57:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1f:6>>);
encode_h(<<58:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#5c:7>>);
encode_h(<<59:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fb:8>>);
encode_h(<<60:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffc:15>>);
encode_h(<<61:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#20:6>>);
encode_h(<<62:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffb:12>>);
encode_h(<<63:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fc:10>>);
encode_h(<<64:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1ffa:13>>);
encode_h(<<65:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#21:6>>);
encode_h(<<66:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#5d:7>>);
encode_h(<<67:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#5e:7>>);
encode_h(<<68:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#5f:7>>);
encode_h(<<69:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#60:7>>);
encode_h(<<70:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#61:7>>);
encode_h(<<71:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#62:7>>);
encode_h(<<72:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#63:7>>);
encode_h(<<73:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#64:7>>);
encode_h(<<74:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#65:7>>);
encode_h(<<75:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#66:7>>);
encode_h(<<76:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#67:7>>);
encode_h(<<77:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#68:7>>);
encode_h(<<78:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#69:7>>);
encode_h(<<79:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#6a:7>>);
encode_h(<<80:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#6b:7>>);
encode_h(<<81:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#6c:7>>);
encode_h(<<82:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#6d:7>>);
encode_h(<<83:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#6e:7>>);
encode_h(<<84:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#6f:7>>);
encode_h(<<85:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#70:7>>);
encode_h(<<86:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#71:7>>);
encode_h(<<87:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#72:7>>);
encode_h(<<88:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fc:8>>);
encode_h(<<89:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#73:7>>);
encode_h(<<90:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fd:8>>);
encode_h(<<91:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1ffb:13>>);
encode_h(<<92:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fff0:19>>);
encode_h(<<93:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1ffc:13>>);
encode_h(<<94:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffc:14>>);
encode_h(<<95:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#22:6>>);
encode_h(<<96:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffd:15>>);
encode_h(<<97:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3:5>>);
encode_h(<<98:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#23:6>>);
encode_h(<<99:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#4:5>>);
encode_h(<<100:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#24:6>>);
encode_h(<<101:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#5:5>>);
encode_h(<<102:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#25:6>>);
encode_h(<<103:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#26:6>>);
encode_h(<<104:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#27:6>>);
encode_h(<<105:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#6:5>>);
encode_h(<<106:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#74:7>>);
encode_h(<<107:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#75:7>>);
encode_h(<<108:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#28:6>>);
encode_h(<<109:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#29:6>>);
encode_h(<<110:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#2a:6>>);
encode_h(<<111:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7:5>>);
encode_h(<<112:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#2b:6>>);
encode_h(<<113:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#76:7>>);
encode_h(<<114:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#2c:6>>);
encode_h(<<115:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#8:5>>);
encode_h(<<116:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#9:5>>);
encode_h(<<117:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#2d:6>>);
encode_h(<<118:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#77:7>>);
encode_h(<<119:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#78:7>>);
encode_h(<<120:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#79:7>>);
encode_h(<<121:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7a:7>>);
encode_h(<<122:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7b:7>>);
encode_h(<<123:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffe:15>>);
encode_h(<<124:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fc:11>>);
encode_h(<<125:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffd:14>>);
encode_h(<<126:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1ffd:13>>);
encode_h(<<127:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffffc:28>>);
encode_h(<<128:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffe6:20>>);
encode_h(<<129:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffd2:22>>);
encode_h(<<130:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffe7:20>>);
encode_h(<<131:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffe8:20>>);
encode_h(<<132:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffd3:22>>);
encode_h(<<133:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffd4:22>>);
encode_h(<<134:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffd5:22>>);
encode_h(<<135:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffd9:23>>);
encode_h(<<136:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffd6:22>>);
encode_h(<<137:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffda:23>>);
encode_h(<<138:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffdb:23>>);
encode_h(<<139:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffdc:23>>);
encode_h(<<140:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffdd:23>>);
encode_h(<<141:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffde:23>>);
encode_h(<<142:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffeb:24>>);
encode_h(<<143:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffdf:23>>);
encode_h(<<144:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffec:24>>);
encode_h(<<145:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffed:24>>);
encode_h(<<146:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffd7:22>>);
encode_h(<<147:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffe0:23>>);
encode_h(<<148:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffee:24>>);
encode_h(<<149:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffe1:23>>);
encode_h(<<150:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffe2:23>>);
encode_h(<<151:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffe3:23>>);
encode_h(<<152:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffe4:23>>);
encode_h(<<153:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffdc:21>>);
encode_h(<<154:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffd8:22>>);
encode_h(<<155:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffe5:23>>);
encode_h(<<156:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffd9:22>>);
encode_h(<<157:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffe6:23>>);
encode_h(<<158:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffe7:23>>);
encode_h(<<159:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffef:24>>);
encode_h(<<160:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffda:22>>);
encode_h(<<161:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffdd:21>>);
encode_h(<<162:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffe9:20>>);
encode_h(<<163:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffdb:22>>);
encode_h(<<164:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffdc:22>>);
encode_h(<<165:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffe8:23>>);
encode_h(<<166:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffe9:23>>);
encode_h(<<167:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffde:21>>);
encode_h(<<168:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffea:23>>);
encode_h(<<169:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffdd:22>>);
encode_h(<<170:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffde:22>>);
encode_h(<<171:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffff0:24>>);
encode_h(<<172:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffdf:21>>);
encode_h(<<173:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffdf:22>>);
encode_h(<<174:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffeb:23>>);
encode_h(<<175:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffec:23>>);
encode_h(<<176:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffe0:21>>);
encode_h(<<177:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffe1:21>>);
encode_h(<<178:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffe0:22>>);
encode_h(<<179:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffe2:21>>);
encode_h(<<180:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffed:23>>);
encode_h(<<181:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffe1:22>>);
encode_h(<<182:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffee:23>>);
encode_h(<<183:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffef:23>>);
encode_h(<<184:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffea:20>>);
encode_h(<<185:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffe2:22>>);
encode_h(<<186:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffe3:22>>);
encode_h(<<187:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffe4:22>>);
encode_h(<<188:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffff0:23>>);
encode_h(<<189:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffe5:22>>);
encode_h(<<190:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffe6:22>>);
encode_h(<<191:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffff1:23>>);
encode_h(<<192:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffe0:26>>);
encode_h(<<193:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffe1:26>>);
encode_h(<<194:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffeb:20>>);
encode_h(<<195:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fff1:19>>);
encode_h(<<196:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffe7:22>>);
encode_h(<<197:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffff2:23>>);
encode_h(<<198:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffe8:22>>);
encode_h(<<199:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1ffffec:25>>);
encode_h(<<200:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffe2:26>>);
encode_h(<<201:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffe3:26>>);
encode_h(<<202:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffe4:26>>);
encode_h(<<203:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffde:27>>);
encode_h(<<204:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffdf:27>>);
encode_h(<<205:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffe5:26>>);
encode_h(<<206:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffff1:24>>);
encode_h(<<207:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1ffffed:25>>);
encode_h(<<208:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fff2:19>>);
encode_h(<<209:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffe3:21>>);
encode_h(<<210:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffe6:26>>);
encode_h(<<211:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffe0:27>>);
encode_h(<<212:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffe1:27>>);
encode_h(<<213:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffe7:26>>);
encode_h(<<214:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffe2:27>>);
encode_h(<<215:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffff2:24>>);
encode_h(<<216:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffe4:21>>);
encode_h(<<217:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffe5:21>>);
encode_h(<<218:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffe8:26>>);
encode_h(<<219:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffe9:26>>);
encode_h(<<220:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffffd:28>>);
encode_h(<<221:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffe3:27>>);
encode_h(<<222:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffe4:27>>);
encode_h(<<223:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffe5:27>>);
encode_h(<<224:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffec:20>>);
encode_h(<<225:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffff3:24>>);
encode_h(<<226:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffed:20>>);
encode_h(<<227:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffe6:21>>);
encode_h(<<228:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffe9:22>>);
encode_h(<<229:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffe7:21>>);
encode_h(<<230:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1fffe8:21>>);
encode_h(<<231:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffff3:23>>);
encode_h(<<232:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffea:22>>);
encode_h(<<233:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3fffeb:22>>);
encode_h(<<234:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1ffffee:25>>);
encode_h(<<235:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#1ffffef:25>>);
encode_h(<<236:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffff4:24>>);
encode_h(<<237:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#fffff5:24>>);
encode_h(<<238:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffea:26>>);
encode_h(<<239:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffff4:23>>);
encode_h(<<240:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffeb:26>>);
encode_h(<<241:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffe6:27>>);
encode_h(<<242:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffec:26>>);
encode_h(<<243:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3ffffed:26>>);
encode_h(<<244:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffe7:27>>);
encode_h(<<245:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffe8:27>>);
encode_h(<<246:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffe9:27>>);
encode_h(<<247:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffea:27>>);
encode_h(<<248:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffeb:27>>);
encode_h(<<249:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#ffffffe:28>>);
encode_h(<<250:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffec:27>>);
encode_h(<<251:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffed:27>>);
encode_h(<<252:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffee:27>>);
encode_h(<<253:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7ffffef:27>>);
encode_h(<<254:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#7fffff0:27>>);
encode_h(<<255:8, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3FFFFEE:26>>);
encode_h(<<256:9, T/binary>>, Acc) -> encode_h(T, <<Acc/bits, 16#3FFFFFFF:30>>);
encode_h(<<>>, Acc) ->
    case bit_size(Acc) rem 8 of
        0 -> Acc;
        1 -> <<Acc/bits, 127:7>>;
        2 -> <<Acc/bits, 63:6>>;
        3 -> <<Acc/bits, 31:5>>;
        4 -> <<Acc/bits, 15:4>>;
        5 -> <<Acc/bits, 7:3>>;
        6 -> <<Acc/bits, 3:2>>;
        7 -> <<Acc/bits, 1:1>>
    end.
