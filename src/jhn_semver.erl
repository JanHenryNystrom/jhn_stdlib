-module(jhn_semver).
-export([decode/1, decode/2, encode/1,
         compare/2, between/3, check/3,
         bump/2
        ]).

-define(DIGIT(C), C >= 48, C =< 57).
-define(POS_DIGIT(C), C >= 49, C =< 57).
-define(CHAR(C), C == 45; %% -
                 C >= 48, C =< 57;
                 C >= 65, C =< 90;
                 C >= 97, C =< 122).

-record(opts,
        {strict = false :: boolean(),
         continue = false :: boolean(),
         return_type = map :: version()
        }).

-type version() :: version_map() | version_plist() | version_tuple().

-type version_map()   :: #{major       := major(),
                           minor       := minor(),
                           patch       := patch(),
                           pre_release => pre_release(),
                           build       => build()}.
-type version_plist() :: jhn_plist:plist().
-type version_tuple() :: {major(), minor(), patch()} |
                         {major(), minor(), patch(), pre_release(), undefined} |
                         {major(), minor(), patch(), undefined, build()} |
                         {major(), minor(), patch(), pre_release(), build()}.

-type major()       :: non_neg_integer().
-type minor()       :: non_neg_integer().
-type patch()       :: non_neg_integer().
-type pre_release() :: [binary() | integer()].
-type build()       :: binary().

-type comparison() :: lt | gt | equal.

-type opt() :: strict | map | plist | tuple | continue |
               {strict, boolean()} | {continue, boolean()} |
               {return_type, map | plist | tuple}.

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
-spec decode(binary()) -> version_map().
%%--------------------------------------------------------------------
decode(B) -> decode(B, []).

%%--------------------------------------------------------------------
-spec decode(binary(), [opt()]) -> version() | {version(), binary()}.
%%--------------------------------------------------------------------
decode(B, Opts) ->
    #opts{strict = Strict,
          continue = Continue,
          return_type = Return} = lists:foldr(fun parse_opt/2, #opts{}, Opts),
    {Version, B1} = case Strict of
                        true -> decode_strict(B);
                        false -> decode_relaxed(B)
                    end,
    case Continue of
        true -> {return(Version, Return), B1};
        false -> return(Version, Return)
    end.

%%--------------------------------------------------------------------
-spec encode(version()) -> binary().
%%--------------------------------------------------------------------
encode(Plist = [_ | _]) -> encode(maps:from_list(Plist));
encode(M = #{major := Major, minor := Minor, patch := Patch}) ->
    Pre = maps:get(pre_release, M, undefined),
    Build = maps:get(build, M, undefined),
    encode({Major, Minor, Patch, Pre, Build});
encode({Major, Minor, Patch}) ->
    iolist_to_binary([integer_to_binary(Major), $.,
                      integer_to_binary(Minor), $.,
                      integer_to_binary(Patch)]);
encode({Major, Minor, Patch, Pre, Build}) ->
    SV = [integer_to_binary(Major), $.,
          integer_to_binary(Minor), $.,
          integer_to_binary(Patch)],
    PB = case {Pre, Build} of
             {undefined, undefined} -> [];
             {Pre, undefined} -> [$-, encode_pre(Pre, [], [])];
             {undefined, Build} -> [$+, Build];
             {Pre, Build} -> [$-, encode_pre(Pre, [], []), $+, Build]
         end,
    iolist_to_binary([SV | PB]).

%%--------------------------------------------------------------------
-spec compare(version() | binary(), version() | binary()) -> comparison().
%%--------------------------------------------------------------------
compare(V1, V2) -> do_compare(normalize(V1), normalize(V2)).

%%--------------------------------------------------------------------
-spec between(version() | binary(),
              version() | binary(),
              version() | binary()) -> boolean().
%%--------------------------------------------------------------------
between(V1, V2, V3) ->
    V1N = normalize(V1),
    V2N = normalize(V2),
    case do_compare(V2N, V1N) of
        gt -> false;
        _ ->
            V3N = normalize(V3),
            case do_compare(V1N, V3N) of
                gt -> false;
                _ -> true
                end
    end.

%%--------------------------------------------------------------------
-spec check(version() | binary(), version() | binary(), atom()) -> version().
%%--------------------------------------------------------------------
check(_V1, _V2, _Check) -> tbd.

%%--------------------------------------------------------------------
-spec bump(atom(), version() | binary()) -> version().
%%--------------------------------------------------------------------
bump(major, _V) -> tbd.

%% ===================================================================
%% Internal functions.
%% ===================================================================

parse_opt(strict, Opts) -> Opts#opts{strict = true};
parse_opt(continue, Opts) -> Opts#opts{continue = true};
parse_opt({strict, B}, Opts) when is_boolean(B) -> Opts#opts{strict = B};
parse_opt({continue, B}, Opts) when is_boolean(B) -> Opts#opts{continue = B};
parse_opt(map, Opts) -> Opts#opts{return_type = map};
parse_opt(tuple ,Opts) -> Opts#opts{return_type = tuple};
parse_opt(plist, Opts) -> Opts#opts{return_type = plist};
parse_opt({return_type, map}, Opts) -> Opts#opts{return_type = map};
parse_opt({return_type, tuple} ,Opts) -> Opts#opts{return_type = tuple};
parse_opt({return_type, plist}, Opts) -> Opts#opts{return_type = plist}.

return(M, map) ->  M;
return(M, plist) -> map:to_list(M);
return(M = #{major := Major, minor := Minor, patch := Patch}, tuple) ->
    case {maps:get(pre_release, M, undefined), maps:get(build, M, undefined)} of
        {undefined, undefined} -> {Major, Minor, Patch};
        {Pre, Build} -> {Major, Minor, Patch, Pre, Build}
    end.

%% ===================================================================
%% Encoding
%% ===================================================================

encode_pre([], _, Acc) ->  lists:reverse(Acc);
encode_pre([H | T], Sep, Acc) when is_integer(H) ->
    encode_pre(T, $\., [integer_to_binary(H), Sep | Acc]);
encode_pre([H | T], Sep, Acc) ->
    encode_pre(T, $\., [H, Sep | Acc]).

%% ===================================================================
%% Decoding
%% ===================================================================

decode_strict(B) ->
    {Major, <<$., B1/binary>>} = digits(B),
    {Minor, <<$., B2/binary>>} = digits(B1),
    case digits(B2) of
        {Patch, <<$-, B3/binary>>} ->
            case pre_release(B3) of
                {Pre, B4} ->
                    {#{major => Major, minor => Minor, patch => Patch,
                       pre_release => Pre},
                     B4};
                {Pre, Build, B4} ->
                    {#{major => Major, minor => Minor, patch => Patch,
                       pre_release => Pre, build => Build},
                     B4}
            end;
        {Patch, <<$+, B3/binary>>} ->
            {Build, B4} = build(B3, <<>>),
            {#{major => Major, minor => Minor, patch => Patch,
               build => Build},
             B4};
        {Patch, B4} ->
            {#{major => Major, minor => Minor, patch => Patch}, B4}
    end.

decode_relaxed(B) ->
    case digits(drop_v(B)) of
        {Major, <<>>} -> {#{major => Major, minor => 0, patch => 0}, <<>>};
        {Major, <<$., B1/binary>>} ->
            case digits(B1) of
                {Minor, <<>>} ->
                    {#{major => Major, minor => Minor, patch => 0}, <<>>};
                {Minor, <<$., B2/binary>>} ->
                    case digits(B2) of
                        {Patch, <<>>} ->
                            {#{major => Major, minor => Minor, patch => Patch},
                             <<>>};
                        {Patch, <<$-, B3/binary>>} ->
                            case pre_release(B3) of
                                {Pre, B4} ->
                                    {#{major => Major,
                                       minor => Minor,
                                       patch => Patch,
                                       pre_release => Pre},
                                     B4};
                                {Pre, Build, B4} ->
                                    {#{major => Major,
                                       minor => Minor,
                                       patch => Patch,
                                       pre_release => Pre,
                                       build => Build},
                                     B4}
                            end;
                        {Patch, <<$+, B3/binary>>} ->
                            {Build, B4} = build(B3, <<>>),
                            {#{major => Major, minor => Minor, patch => Patch,
                               build => Build},
                             B4};
                        {Patch, B3} ->
                            {#{major => Major, minor => Minor, patch => Patch},
                             B3}
                    end;
                {Minor, <<$-, B2/binary>>} ->
                    case pre_release(B2) of
                        {Pre, B3} ->
                            {#{major => Major, minor => Minor, patch => 0,
                               pre_release => Pre},
                             B3};
                        {Pre, Build, B3} ->
                            {#{major => Major, minor => Minor, patch => 0,
                               pre_release => Pre, build => Build},
                             B3}
                    end;
                {Minor, <<$+, B2/binary>>} ->
                    {Build, B3} = build(B2, <<>>),
                    {#{major => Major, minor => Minor, patch => 0,
                       build => Build},
                     B3};
                {Minor, B2} ->
                    {#{major => Major, minor => Minor, patch => 0}, B2}
            end;
        {Major, <<$-, B1/binary>>} ->
            case pre_release(B1) of
                {Pre, B2} ->
                    {#{major => Major, minor => 0, patch => 0,
                       pre_release => Pre},
                     B2};
                {Pre, Build, B2} ->
                    {#{major => Major, minor => 0, patch => 0,
                       pre_release => Pre, build => Build},
                     B2}
            end;
        {Major, <<$+, B1/binary>>} ->
            {Build, B2} = build(B1, <<>>),
            {#{major => Major, minor => 0, patch => 0, build => Build}, B2};
        {Major, B1} ->
            {#{major => Major, minor => 0, patch => 0}, B1}
    end.

digits(<<$0, T/binary>>) -> {0, T};
digits(B) -> digits(B, []).

digits(<<>>, Acc) -> {list_to_integer(lists:reverse(Acc)), <<>>};
digits(<<H, T/binary>>, Acc) when ?DIGIT(H) -> digits(T, [H | Acc]);
digits(T, Acc) -> {list_to_integer(lists:reverse(Acc)), T}.

pre_release(B) -> pre_release(B, [], true, <<>>).



pre_release(<<>>, P, I, Acc = <<_, _/binary>>) ->
    {lists:reverse([numeric(Acc, I) | P]), <<>>};
pre_release(<<$\., H, T/binary>>, P, I, Acc) when ?DIGIT(H) ->
    pre_release(T, [numeric(Acc, I) | P], true, <<H>>);
pre_release(<<H, T/binary>>, P, true, Acc) when ?DIGIT(H) ->
    pre_release(T, P, true, <<Acc/binary, H>>);
pre_release(<<$\., H, T/binary>>, P, I, Acc) when ?CHAR(H) ->
    pre_release(T, [numeric(Acc, I) | P], true, <<H>>);
pre_release(<<H, T/binary>>, P, _, Acc) when ?CHAR(H) ->
    pre_release(T, P, false, <<Acc/binary, H>>);
pre_release(<<$+, T/binary>>, P, I, Acc) ->
    {Build, T1} = build(T, <<>>),
    {lists:reverse([numeric(Acc, I) | P]), Build, T1};
pre_release(T, P, I, Acc) ->
    {lists:reverse([numeric(Acc, I) | P]), T}.

numeric(B, true) -> binary_to_integer(B);
numeric(B, false) -> B.

build(<<>>, Acc) -> {Acc, <<>>};
build(<<$\., H, T/binary>>, Acc) when ?CHAR(H) ->
    build(<<T/binary>>, <<Acc/binary, $\., H>>);
build(<<H, T/binary>>, Acc) when ?CHAR(H) ->
    build(T, <<Acc/binary, H>>);
build(T, Acc) ->
    {Acc, T}.

drop_v(<<$v, T/binary>>) -> T;
drop_v(T) -> T.

%% ===================================================================
%% Comparison
%% ===================================================================

do_compare(V, V) -> equal;
do_compare({V, P1}, {V, P2}) ->
    case {P1, P2} of
        {_, undefined} -> lt;
        {undefined, _} -> gt;
        _ when P1 < P2 -> lt;
        _ -> gt
    end;
do_compare({V1, _}, {V2, _}) when V1 < V2 -> lt;
do_compare(_, _) -> gt.

normalize(B = <<_/binary>>) -> normalize(decode(B));
normalize({Major, Minor, Patch}) -> {{Major, Minor, Patch}, undefined};
normalize({Major, Minor, P, undefined, _}) -> {{Major, Minor, P}, undefined};
normalize({Major, Minor, Patch, Pre, _}) -> {{Major, Minor, Patch}, Pre};
normalize(Map = #{major := Major, minor := Minor, patch := Patch}) ->
    case maps:get(pre_release, Map, undefined) of
        undefined ->{{Major, Minor, Patch}, undefined};
        Pre -> {{Major, Minor, Patch}, Pre}
    end;
normalize(PList = [_|_]) -> normalize(maps:from_list(PList)).
