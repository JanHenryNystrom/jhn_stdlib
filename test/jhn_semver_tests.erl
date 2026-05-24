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
             {~"1.0.0-0.a",
              #{major => 1, minor => 0, patch => 0, pre_release => [0, ~"a"]}},
             {~"1.0.0-10.a",
              #{major => 1, minor => 0, patch => 0, pre_release => [10, ~"a"]}},
             {~"1.0.0-10a",
              #{major => 1, minor => 0, patch => 0, pre_release => [~"10a"]}},
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
    [?_test(?assertEqual(jhn_semver:decode(B, Flags), Version)) ||
        {B, Version, Flags} <-
            [{~"0.0.0", #{major => 0, minor => 0, patch => 0}, [strict]},
             {~"1.0.0", #{major => 1, minor => 0, patch => 0}, [strict]},
             {~"1.0.0", {1, 0, 0}, [strict, tuple]},
             {~"0.0.0", #{major => 0, minor => 0, patch => 0}, [map, strict]},
             {~"1.0.0", [{major, 1}, {minor, 0}, {patch, 0}], [strict, plist]},
             {~"1.0.0", {{1, 0, 0}, ~""}, [continue, tuple]},
             {~"1.0.0fnu", {{1, 0, 0}, ~"fnu"}, [continue, tuple]},
             {~"1.0fnu", {{1, 0, 0}, ~"fnu"}, [continue, tuple]},
             {~"1fnu", {{1, 0, 0}, ~"fnu"}, [continue, tuple]},
             {~"1-beta apa", {{1, 0, 0, [~"beta"], undefined}, ~" apa"},
              [continue, tuple]},
             {~"1+001gnu apa", {{1, 0, 0, undefined, ~"001gnu"}, ~" apa"},
              [continue, tuple]},
             {~"1-beta+001gnu apa", {{1, 0, 0, [~"beta"], ~"001gnu"}, ~" apa"},
              [continue, tuple]},
             {~"3.2.1-beta+001gnu apa",{{3,2, 1, [~"beta"], ~"001gnu"},~" apa"},
              [strict, continue, tuple]}
            ]
    ].

compare_2_test_() ->
    [?_test(?assertEqual(jhn_semver:compare(A, B), Result)) ||
        {A, B, Result} <-
            [{~"0.0.0", #{major => 0, minor => 0, patch => 0}, eq},
             {~"1.0.0", #{major => 1, minor => 0, patch => 0}, eq}
            ]
    ].

between_3_test_() ->
    [?_test(?assertEqual(jhn_semver:between(X, L, U), Result)) ||
        {X, L, U, Result} <-
            [{~"0.0.0", #{major => 0, minor => 0, patch => 0}, ~"0.0.0", true},
             {~"1.0.0", #{major => 1, minor => 0, patch => 0}, ~"1.0.1", true}
            ]
    ].

check_1_test_() ->
    [?_test(?assertEqual(jhn_semver:check(E), Result)) ||
        {E, Result} <-
            [{{'=', ~"0.0.0", #{major => 0, minor => 0, patch => 0}}, true},
             {{'<', ~"1.0.0", #{major => 1, minor => 0, patch => 0}}, false}
            ]
    ].

check_3_test_() ->
    [?_test(?assertEqual(jhn_semver:check(C, A, B), Result)) ||
        {C, A, B, Result} <-
            [{'=', ~"0.0.0", #{major => 0, minor => 0, patch => 0}, true},
             {'<', ~"1.0.0", #{major => 1, minor => 0, patch => 0}, false}
            ]
    ].

bump_2_test_() ->
    [?_test(?assertEqual(jhn_semver:bump(A, B), Result)) ||
        {A, B, Result} <-
            [{major, ~"0.0.0", #{major => 1, minor => 0, patch => 0}},
             {minor, ~"0.0.0", #{major => 0, minor => 1, patch => 0}},
             {patch, ~"0.0.0", #{major => 0, minor => 0, patch => 1}}
            ]
    ].

