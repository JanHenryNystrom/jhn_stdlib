-module(jhn_semver_tests).

-include_lib("eunit/include/eunit.hrl").



encode_1_test_() ->
    [?_test(?assertEqual(jhn_semver:encode(B), Version)) ||
        {Version, B} <-
            [{~"0.0.0", #{major => 0, minor => 0, patch => 0}},
             {~"1.0.0", #{major => 1, minor => 0, patch => 0}},
             {~"1.0.0-alpha.13.beta",
              #{major => 1, minor => 0, patch => 0,
                pre_release => [~"alpha", 13, ~"beta"]}}
            ]
    ].

decode_1_test_() ->
    [?_test(?assertEqual(jhn_semver:decode(B), Version)) ||
        {B, Version} <-
            [{~"0.0.0", #{major => 0, minor => 0, patch => 0}},
             {~"1.0.0", #{major => 1, minor => 0, patch => 0}},
             {~"1.0", #{major => 1, minor => 0, patch => 0}},
             {~"1", #{major => 1, minor => 0, patch => 0}},
             {~"1.0.0-alpha",
              #{major => 1, minor => 0, patch => 0, pre_release => [~"alpha"]}},
             {~"1.0.0-alpha.beta",
              #{major => 1, minor => 0, patch => 0,
                pre_release => [~"alpha", ~"beta"]}},
             {~"1.0.0-alpha.13.beta",
              #{major => 1, minor => 0, patch => 0,
                pre_release => [~"alpha", 13, ~"beta"]}},
             {~"1.0-alpha",
              #{major => 1, minor => 0, patch => 0, pre_release => [~"alpha"]}},
             {~"1-alpha",
              #{major => 1, minor => 0, patch => 0, pre_release => [~"alpha"]}},
             {~"1.0.0-alpha+123",
              #{major => 1, minor => 0, patch => 0,
                pre_release => [~"alpha"], build => ~"123"}},
             {~"1.0-alpha+123",
              #{major => 1, minor => 0, patch => 0,
                pre_release => [~"alpha"], build => ~"123"}},
             {~"1-alpha+123",
              #{major => 1, minor => 0, patch => 0,
                pre_release => [~"alpha"], build => ~"123"}},
             {~"1.0.0+1.2.3",
              #{major => 1, minor => 0, patch => 0, build => ~"1.2.3"}},
             {~"1.0+123",
              #{major => 1, minor => 0, patch => 0, build => ~"123"}},
             {~"1+123",
              #{major => 1, minor => 0, patch => 0, build => ~"123"}}

            ]
    ].

decode_2_test_() ->
    [?_test(?assertEqual(jhn_semver:decode(B, [strict]), Version)) ||
        {B, Version} <-
            [{~"0.0.0", #{major => 0, minor => 0, patch => 0}},
             {~"1.0.0", #{major => 1, minor => 0, patch => 0}}
            ]
    ].

compare_2_test_() ->
    [?_test(?assertEqual(jhn_semver:compare(A, B), Result)) ||
        {A, B, Result} <-
            [{~"0.0.0", #{major => 0, minor => 0, patch => 0}, equal},
             {~"1.0.0", #{major => 1, minor => 0, patch => 0}, equal}
            ]
    ].

between_3_test_() ->
    [?_test(?assertEqual(jhn_semver:between(X, L, U), Result)) ||
        {X, L, U, Result} <-
            [{~"0.0.0", #{major => 0, minor => 0, patch => 0}, ~"0.0.0", true},
             {~"1.0.0", #{major => 1, minor => 0, patch => 0}, ~"1.0.1", true}
            ]
    ].

