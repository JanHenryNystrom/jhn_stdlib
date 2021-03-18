%%==============================================================================
%% Copyright 2016-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%   Implements Bloom filters
%%   Based on: Scalable Bloom Filters by
%%   Paulo Sérgio Almeida, Carlos Baquero, Nuno Preguiça, David Hutchison
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(bloom).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([filter/0, filter/1,
         is_filter/1, type/1,
         member/2, add/2,
         capacity/1
        ]).

%% Records
-record(filter,
        {type         = fixed :: fixed | scalable,
         size                 :: integer(),
         capacity             :: integer(),
         error_prob   = 0.001 :: float(),
         error_ratio          :: float(),
         growth_ratio = 1     :: integer(),
         slice_size           :: integer(),
         slices               :: [array:array(integer())] | [filter()]
        }).

%% Types
-type   opt() :: fixed | scalable |
                 {size, integer()} |
                 {error_prob, float()} |
                 {error_ratio, float()} |
                 {growth_ratio, integer()}.
-opaque filter() :: #filter{}.

%% Exported Types
-export_type([filter/0]).

%% Defines
-define(MAX_32, 4294967296).
-define(MAX_16, 65536).

-define(Width, 27).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%   Returns a bloom filter
%% @end
%%--------------------------------------------------------------------
-spec filter() -> filter().
%%--------------------------------------------------------------------
filter() -> filter(check_size(#filter{})).

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%  Returns a bloom filter
%%   Options are:
%%     fixed -> standard partitioned bloom filter, default type
%%     scalable -> scalable bloom filter
%%     size -> the maximum size for fixed and initial size for scalable filter
%%             the defaults are 4000 and 32000 respectively
%%     error_prob -> error probability, default 0.001
%%   Options relevant to scalable filters only
%%     growth_ratio -> log 2 of growth ratio, one of 1, 2, 3 default 1
%%     error_ratio -> error probability ratio, default 0.85
%% @end
%%--------------------------------------------------------------------
-spec filter([opt()]) -> filter().
%%--------------------------------------------------------------------
filter(Opts) when is_list(Opts) -> filter(parse_opts(Opts));
filter(F = #filter{type = fixed, size = N, error_prob = E}) ->
    K = 1 + trunc(log2(1 / E)),
    Size =
        1 bsl (1 + trunc(- log2(1 - math:pow(1 - math:pow(E, 1 / K), 1 / N)))),
    Capacity = trunc(math:log(1 - math:pow(E, 1 / K)) / math:log(1 - 1 / Size)),
    Slice = array:new((Size - 1) div ?Width + 1, {default, 0}),
    F#filter{size = 0,
             slice_size = Size,
             capacity = Capacity,
             slices = lists:duplicate(K, Slice)};
filter(F = #filter{type = scalable, size = S, error_prob = E, error_ratio=R}) ->
    F#filter{slices = [filter([{size, S}, {error_prob, E * (1 - R)}])]}.

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%   Returns true if the argument is a Bloom filter
%% @end
%%--------------------------------------------------------------------
-spec is_filter(_) -> boolean().
%%--------------------------------------------------------------------
is_filter(#filter{}) -> true;
is_filter(_) -> false.

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%  Returns the type of Bloom filter
%% @end
%%--------------------------------------------------------------------
-spec type(_) -> fixed | scalable.
%%--------------------------------------------------------------------
type(#filter{type = Type}) -> Type.

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%  Returns if the term is a member of the Bloom filter
%% @end
%%--------------------------------------------------------------------
-spec member(_, filter()) -> boolean().
%%--------------------------------------------------------------------
member(Term, #filter{type = fixed, slice_size = Size, slices = Slices}) ->
    Mask = Size - 1,
    {I0, I1} = indices(Mask, Term),
    all_set(Mask, I1, I0, Slices);
member(Term, #filter{type = scalable, slices = Filters}) ->
    {H0, H1} = hashes(Term),
    member(H0, H1, Filters).

member(_, _, []) -> false;
member(H0, H1, [#filter{slice_size = Size, slices = Slices} | T]) ->
    Mask = Size - 1,
    {I0, I1} = indices(Mask, H0, H1),
    case all_set(Mask, I1, I0, Slices) of
        true -> true;
        false -> member(H0, H1, T)
    end.

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%  Adds a term to the Bloom filter
%% @end
%%--------------------------------------------------------------------
-spec add(_, filter())-> filter().
%%--------------------------------------------------------------------
add(Term, F = #filter{type = fixed, size = S, slice_size=SS, slices=Slices}) ->
    Mask = SS -1,
    {I0, I1} = indices(Mask, Term),
    case all_set(Mask, I1, I0, Slices) of
        true -> F;
        false -> F#filter{size = S + 1,
                          slices = set_bits(Mask, I1, I0, Slices, [])}
    end;
add(Term, F = #filter{type=scalable, slices=Fs,growth_ratio=G,error_ratio=R}) ->
    {H0, H1} = hashes(Term),
    case member(H0, H1, Fs) of
        true -> F;
        false ->
            case Fs of
                [H = #filter{capacity = C, size = S}| T] when S < C ->
                    F#filter{slices = [add(H0, H1, H) | T]};
                [#filter{slice_size = SS, error_prob = E}| _] ->
                    Opts = [{size, SS bsl G}, {error_prob, E * R}],
                    F#filter{slices = [add(H0, H1, filter(Opts)) | Fs]}
            end
    end.

add(H0, H1, F = #filter{size = S, slice_size=SS, slices=Slices}) ->
    Mask = SS - 1,
    {I0, I1} = indices(Mask, H0, H1),
    F#filter{size = S + 1, slices = set_bits(Mask, I1, I0, Slices, [])}.

%%--------------------------------------------------------------------
%% Function:
%% @doc
%%   Returns the capacity of the Bloom filter
%% @end
%%--------------------------------------------------------------------
-spec capacity(filter()) -> integer().
%%--------------------------------------------------------------------
capacity(#filter{type = scalable}) -> infinity;
capacity(#filter{type = fixed, capacity = Capacity}) -> Capacity.

%% ===================================================================
%% Internal functions.
%% ===================================================================

log2(X) -> math:log(X) / math:log(2).

indices(Mask, Term) when Mask < ?MAX_16 ->
    H = erlang:phash2({Term},?MAX_32),
    {(H bsr 16) band Mask, H band Mask};
indices(Mask, Term) ->
    {erlang:phash2({Term}, ?MAX_32) band Mask,
     erlang:phash2([Term], ?MAX_32) band Mask}.

indices(Mask, H0, _) when Mask<?MAX_16 -> {(H0 bsr 16) band Mask, H0 band Mask};
indices(Mask, H0, H1) -> {H0 band Mask, H1 band Mask}.

hashes(Term) -> {erlang:phash2({Term},?MAX_32), erlang:phash2([Term],?MAX_32)}.

all_set(_, _, _, []) -> true;
all_set(Mask, I1, I, [H | T]) ->
    case array:get(I div ?Width, H) band (1 bsl (I rem ?Width)) of
        0 -> false;
        _ -> all_set(Mask, I1, (I + I1) band Mask, T)
    end.

set_bits(_, _, _, [], Acc) -> lists:reverse(Acc);
set_bits(Mask, I1, I, [H | T], Acc) ->
    AI = I div ?Width,
    H1 = array:set(AI, array:get(AI, H) bor (1 bsl (I rem ?Width)), H),
    set_bits(Mask, I1, (I + I1) band Mask, T, [H1 | Acc]).


parse_opts(Opts) -> check_size(lists:foldl(fun parse_opt/2, #filter{}, Opts)).

parse_opt(fixed, Filter) -> Filter#filter{type = fixed};
parse_opt(scalable, Filter) -> Filter#filter{type = scalable};
parse_opt({size, S}, Filter) when is_integer(S), S > 0 ->
    Filter#filter{size = S};
parse_opt({error_prob, E}, Filter) when is_float(E), E > 0, E < 1 ->
    Filter#filter{error_prob = E};
parse_opt({error_ratio, R}, Filter) when is_float(R), R > 0, R < 1 ->
    Filter#filter{error_ratio = R};
parse_opt({growth_ratio, R}, Filter) when is_integer(R), R > 0, R < 4 ->
    Filter#filter{growth_ratio = R};
parse_opt(_, _) -> erlang:error(badarg).

%% rule of thumb due to double hashing
check_size(F = #filter{type = fixed, size = undefined}) ->
    check_size(F#filter{size = 4000});
check_size(F = #filter{type = fixed, size = S, error_prob = E}) when S >= 4/E ->
    F;
check_size(F = #filter{type=scalable, size = undefined}) ->
    check_size(F#filter{size = 32000});
check_size(F = #filter{type=scalable, growth_ratio=1, error_ratio=undefined}) ->
    check_size(F#filter{error_ratio = 0.85});
check_size(F = #filter{type=scalable, growth_ratio=2, error_ratio=undefined}) ->
    check_size(F#filter{error_ratio = 0.75});
check_size(F = #filter{type=scalable, growth_ratio=3, error_ratio=undefined}) ->
    check_size(F#filter{error_ratio = 0.65});
check_size(F = #filter{type = scalable, size = S, error_prob=E, error_ratio=R})
           when S >= 4/(E * (1 - R)) ->
    F.
