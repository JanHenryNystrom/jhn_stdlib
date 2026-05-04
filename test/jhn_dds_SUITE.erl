-module(jhn_dds_SUITE).
-compile([export_all, nowarn_export_all]).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-include("../src/ddskerl.hrl").

%% Exported functions
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test suite configuration
-spec all() -> [ct_suite:ct_test_def()].
all() ->
    [
        {group, ddskerl_std}
    ].

-spec groups() -> [ct_suite:ct_group_def()].
groups() ->
    [
        {ddskerl_std, tests()}
    ].

-spec tests_ets_tuples() -> [atom()].
tests_ets_tuples() ->
    [merge_ets_tuples, total_ets_tuple, sum_ets_tuple, quantile_ets_tuple].

-spec tests() -> [atom()].
tests() ->
    [
        get_quantile_test,
        zero_test,
        zero_insert_test,
        sum_test,
        merge_test,
        prop_quantile_error_test
    ].

-spec init_per_suite(ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_suite(Config) ->
    Config.

-spec end_per_suite(ct_suite:ct_config()) -> term().
end_per_suite(_Config) ->
    ok.

-spec init_per_group(ct_suite:ct_groupname(), ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_group(GroupModule, Config) ->
    [{module, GroupModule} | Config].

-spec end_per_group(ct_suite:ct_groupname(), ct_suite:ct_config()) -> term().
end_per_group(_Group, _Config) ->
    ok.

-spec init_per_testcase(ct_suite:ct_testcase(), ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_testcase(TestCase, Config) ->
    ets:new(TestCase, [public, named_table]),
    Config.

-spec end_per_testcase(ct_suite:ct_testcase(), ct_suite:ct_config()) -> term().
end_per_testcase(_TestCase, _Config) ->
    ok.

reset_counters(_Config) ->
    Sample = [4, 8],
    RelativeError = 0.01,
    Bound = bucket_size(10, RelativeError),
    Sketch = estimate_sketch(ddskerl_counters, Sample, ?FUNCTION_NAME, RelativeError, Bound),
    ddskerl_counters:reset(Sketch),
    ?assertEqual(0, ddskerl_counters:total(Sketch)),
    ddskerl_counters:insert(Sketch, 2),
    [
        ?assert(abs(2 - ddskerl_counters:quantile(Sketch, Q)) =< RelativeError, Q)
     || Q <- quantiles()
    ].

reset_ets(_Config) ->
    Sample = [4, 8],
    RelativeError = 0.01,
    Bound = bucket_size(10, RelativeError),
    Sketch = estimate_sketch(ddskerl_ets, Sample, ?FUNCTION_NAME, RelativeError, Bound),
    ddskerl_ets:reset(Sketch),
    ?assertEqual(0, ddskerl_ets:total(Sketch)),
    ddskerl_ets:insert(Sketch, 2),
    [
        ?assert(abs(2 - ddskerl_ets:quantile(Sketch, Q)) =< RelativeError, Q)
     || Q <- quantiles()
    ].

%% Test cases
get_quantile_test(Config) ->
    Module = proplists:get_value(module, Config),
    Sample = [4, 8],
    RelativeError = 0.02,
    Bound = bucket_size(10, RelativeError),
    Sketch = estimate_sketch(Module, Sample, ?FUNCTION_NAME, RelativeError, Bound),
    Quantile = Module:quantile(Sketch, 0.5),
    assert_quantile(4.0, Quantile, RelativeError, Quantile, Sketch).

assert_quantile(Expected, Quantile, RelativeError, Quantile, Sketch) ->
    ?assert(
        abs(Quantile - Expected) =< RelativeError * 4.0,
        #{quantile => Quantile, sketch => Sketch}
    ).

prepare_basic_ets_sketch(FName, RelativeError) ->
    Bound = bucket_size(10, RelativeError),
    estimate_sketch(ddskerl_ets, [2, 4, 6, 8, 10], FName, RelativeError, Bound).

total_ets_tuple(_) ->
    RelativeError = 0.02,
    #ddskerl_ets{name = Name} = prepare_basic_ets_sketch(?FUNCTION_NAME, RelativeError),
    [Tuple] = ets:lookup(?FUNCTION_NAME, Name),
    ?assertEqual(5, ddskerl_ets:total_tuple(Tuple)).

sum_ets_tuple(_) ->
    RelativeError = 0.02,
    #ddskerl_ets{name = Name} = prepare_basic_ets_sketch(?FUNCTION_NAME, RelativeError),
    [Tuple] = ets:lookup(?FUNCTION_NAME, Name),
    ?assertEqual(30, ddskerl_ets:sum_tuple(Tuple)).

quantile_ets_tuple(_) ->
    RelativeError = 0.02,
    Sketch = #ddskerl_ets{name = Name} = prepare_basic_ets_sketch(?FUNCTION_NAME, RelativeError),
    [Tuple] = ets:lookup(?FUNCTION_NAME, Name),
    Quantile = ddskerl_ets:quantile_tuple(Tuple, 0.50),
    assert_quantile(6, Quantile, RelativeError, Quantile, Sketch).

merge_ets_tuples(_) ->
    RelativeError = 0.02,
    Bound = bucket_size(10, RelativeError),
    Sketch1 = estimate_sketch(ddskerl_ets, [4], ?FUNCTION_NAME, RelativeError, Bound),
    Sketch2 = estimate_sketch(ddskerl_ets, [8], ?FUNCTION_NAME, RelativeError, Bound),
    MergedSketch = ddskerl_ets:merge_out(Sketch1, Sketch2),
    ets:insert(?FUNCTION_NAME, MergedSketch),
    Quantile = ddskerl_ets:quantile(Sketch1, 0.5),
    assert_merge(Quantile, RelativeError, Sketch1, Sketch2).

merge_test(Config) ->
    Module = proplists:get_value(module, Config),
    RelativeError = 0.02,
    Bound = bucket_size(10, RelativeError),
    Sketch1 = estimate_sketch(Module, [4], ?FUNCTION_NAME, RelativeError, Bound),
    Sketch2 = estimate_sketch(Module, [8], ?FUNCTION_NAME, RelativeError, Bound),
    MergedSketch = Module:merge(Sketch1, Sketch2),
    Quantile = Module:quantile(MergedSketch, 0.5),
    assert_merge(Quantile, RelativeError, Sketch1, Sketch2).

assert_merge(Quantile, RelativeError, Sketch1, Sketch2) ->
    ?assert(
        abs(Quantile - 4.0) =< RelativeError * 4.0,
        #{sketch_one => Sketch1, sketch_two => Sketch2}
    ).

zero_test(Config) ->
    Module = proplists:get_value(module, Config),
    RelativeError = 0.01,
    Bound = bucket_size(10, RelativeError),
    Sketch = estimate_sketch(Module, [], ?FUNCTION_NAME, RelativeError, Bound),
    Q = Module:quantile(Sketch, 0.5),
    ?assertEqual(0.0, Q, #{sketch => Sketch, q => Q}).

zero_insert_test(Config) ->
    Module = proplists:get_value(module, Config),
    zero_insert_test(Config, Module).

zero_insert_test(_, Module) when ddskerl_ets =:= Module; ddskerl_counters =:= Module ->
    {skip, aa};
zero_insert_test(_, Module) ->
    RelativeError = 0.01,
    Bound = bucket_size(10, RelativeError),
    Opts = #{
        ets_table => ?FUNCTION_NAME, name => make_ref(), error => RelativeError, bound => Bound
    },
    Sketch0 = Module:new(Opts),
    %% Test inserting 0.0 (float) - this should not throw an error
    %% 0 represents negative infinity (log2(0) = -infinity)
    Sketch1 = Module:insert(Sketch0, 0.0),
    ?assertEqual(1, Module:total(Sketch1)),
    ?assertEqual(0.0, float(Module:sum(Sketch1))),
    Q = float(Module:quantile(Sketch1, 0.5)),
    ?assertEqual(0.0, Q, #{sketch => Sketch1, q => Q}),
    %% Test inserting integer 0 - this should also not throw an error
    Sketch2 = Module:insert(Sketch1, 0),
    ?assertEqual(2, Module:total(Sketch2)),
    ?assertEqual(0.0, float(Module:sum(Sketch2))),
    %% Test inserting multiple 0.0 values
    Sketch3 = Module:insert(Sketch2, 0.0),
    ?assertEqual(3, Module:total(Sketch3)),
    ?assertEqual(0.0, float(Module:sum(Sketch3))),
    %% Test inserting 0.0 along with other values
    Sketch4 = lists:foldl(fun(V, S) -> Module:insert(S, V) end, Sketch0, [0.0, 1.0, 2.0, 0.0]),
    ?assertEqual(4, Module:total(Sketch4)),
    ?assertEqual(3.0, Module:sum(Sketch4)),
    %% Test that quantile 0.0 returns the minimum (which should be the neg_infinity value)
    MinQ = Module:quantile(Sketch4, 0.0),
    ?assertEqual(0.0, MinQ, #{sketch => Sketch4, q => MinQ}),
    %% Test that quantile 1.0 returns the maximum (which should be 2.0)
    MaxQ = Module:quantile(Sketch4, 1.0),
    ?assertEqual(2.0, MaxQ, #{sketch => Sketch4, q => MaxQ}).

sum_test(Config) ->
    Module = proplists:get_value(module, Config),
    RelativeError = 0.02,
    Bound = bucket_size(10, RelativeError),
    Sketch = estimate_sketch(Module, lists:seq(1, 100), ?FUNCTION_NAME, RelativeError, Bound),
    Sum = Module:sum(Sketch),
    ?assertEqual(5050, Sum, #{sketch => Sketch, sum => Sum}).

%% Run the property-based test
prop_quantile_error_test(Config) ->
    Module = proplists:get_value(module, Config),
    Opts = [
        quiet,
        noshrink,
        long_result,
        {start_size, 5},
        {max_size, 1_000_000},
        {numtests, 100},
        {numworkers, erlang:system_info(schedulers_online)}
    ],
    case proper:counterexample(prop_quantile(Module, ?FUNCTION_NAME), Opts) of
        true ->
            ct:comment("Property check for ~s passed", [Module]);
        [{RelativeError, Sample} | _] ->
            Bound = bucket_size(1 + lists:max(Sample), RelativeError),
            Sketch = estimate_sketch(Module, Sample, ?FUNCTION_NAME, RelativeError, Bound),
            Summary = calculate_summary(Sample),
            Properties = lists:map(
                fun(Q) ->
                    SketchQuantile = Module:quantile(Sketch, Q),
                    ActualQuantile = ddskerl_exact:quantile(Summary, Q),
                    Property =
                        abs(SketchQuantile / ActualQuantile - 1) =< RelativeError,
                    {Q, SketchQuantile, ActualQuantile, Property}
                end,
                quantiles()
            ),
            Format = "-properties: ~w~n-sketch: ~p~n-sample: ~p~n",
            Values = [Properties, Sketch, Sample],
            ct:pal(Format, Values),
            ct:fail(
                "Failed with relative error ~p and sample size ~B, see logs for more details",
                [RelativeError, length(Sample)]
            )
    end.

%% Property-based test for quantile error
prop_quantile(Module, T) ->
    ?FORALL(
        {RelativeError, Sample},
        {relative_error(), sample(Module)},
        prop(Module, T, RelativeError, Sample)
    ).

prop(Module, T, RelativeError, Sample) ->
    SampleSize = length(Sample),
    Bound = bucket_size(1 + lists:max(Sample), RelativeError),
    Sketch = estimate_sketch(Module, Sample, T, RelativeError, Bound),
    Summary = calculate_summary(Sample),
    lists:all(
        fun(Q) ->
            check_quantile(Sketch, Summary, Module, RelativeError, SampleSize, Q) andalso
                check_totals(Sketch, Summary, Module) andalso
                check_sums(Sketch, Summary, Module, RelativeError)
        end,
        quantiles()
    ).

check_totals(Sketch, Summary, Module) ->
    Module:total(Sketch) =:= ddskerl_exact:total(Summary).

check_sums(Sketch, Summary, Module, RelativeError) ->
    ExactSum = ddskerl_exact:sum(Summary),
    EstimatedSum = Module:sum(Sketch),
    abs(EstimatedSum - ExactSum) =< RelativeError * ExactSum.

check_quantile(Sketch, Summary, Module, RelativeError, SampleSize, Q) ->
    SketchQuantile = Module:quantile(Sketch, Q),
    ActualQuantile = ddskerl_exact:quantile(Summary, Q),
    abs(SketchQuantile - ActualQuantile) =< RelativeError * ActualQuantile andalso
        SampleSize =:= Module:total(Sketch).

estimate_sketch(Module, Sample, T, RelativeError, Bound) ->
    Opts = #{ets_table => T, name => make_ref(), error => RelativeError, bound => Bound},
    Sketch = Module:new(Opts),
    lists:foldl(fun(V, S) -> Module:insert(S, V) end, Sketch, Sample).

calculate_summary(Sample) ->
    Sketch = ddskerl_exact:new(),
    lists:foldl(fun(V, S) -> ddskerl_exact:insert(S, V) end, Sketch, Sample).

pos_real() ->
    union([pos_integer(), float(0.0000001, inf)]).

relative_error() ->
    float(0.001, 0.999999).

bucket_size(Max, Err) ->
    ceil(math:log2(Max) * (1.0 / math:log2((1 + Err) / (1 - Err)))).

%% Non emty list of arbitrarily big positive integers and floats
sample(Module) when ddskerl_std =:= Module; ddskerl_bound =:= Module ->
    non_empty(list(pos_real()));
sample(Module) when ddskerl_counters =:= Module; ddskerl_ets =:= Module ->
    non_empty(list(pos_integer())).

quantiles() ->
    [0.0, 0.5, 0.60, 0.75, 0.85, 0.90, 0.95, 0.96, 0.97, 0.98, 0.99, 0.999, 1.0].
