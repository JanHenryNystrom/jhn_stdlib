-module(jhn_base_tests).

-include_lib("eunit/include/eunit.hrl").

encode_2_test_() ->
    [?_test(?assertEqual(iolist_to_binary(jhn_base:encode(E, B)), Result)) ||
        {E, Result, B} <-
            [{b32, ~"", ~""},
             {b32, ~"MY======", ~"f"},
             {b32, ~"MZXQ====", ~"fo"},
             {b32, ~"MZXW6===", ~"foo"},
             {b32, ~"MZXW6YQ=", ~"foob"},
             {b32, ~"MZXW6YTB", ~"fooba"},
             {b32, ~"MZXW6YTBOI======", ~"foobar"},
             {b45, ~"BB8", ~"AB"},
             {b45, ~"%69 VD92EX0", ~"Hello!!"},
             {b45, ~"A2", <<100>>},
             {z85,
              ~"HelloWorld",
              <<16#86, 16#4F, 16#D2, 16#6F, 16#B5, 16#59, 16#F7, 16#5B>>}
            ]
    ].


encode_3_test_() ->
    [?_test(?assertEqual(jhn_base:encode(E, B, O), Result)) ||
        {E, O, Result, B} <-
            [{b32, [{alfabet, hex}, binary], ~"", ~""},
             {b32, [{alfabet, hex}, binary], ~"CO======", ~"f"},
             {b32, [{alfabet, hex}, binary], ~"CPNG====", ~"fo"},
             {b32, [{alfabet, hex}, binary], ~"CPNMU===", ~"foo"},
             {b32, [{alfabet, hex}, binary], ~"CPNMUOG=", ~"foob"},
             {b32, [{alfabet, hex}, binary], ~"CPNMUOJ1", ~"fooba"},
             {b32, [{alfabet, hex}, binary], ~"CPNMUOJ1E8======", ~"foobar"}
            ]
    ].

decode_2_test_() ->
    [?_test(?assertEqual(iolist_to_binary(jhn_base:decode(E, B)), Result)) ||
        {E, Result, B} <-
            [{b32, ~"f", ~"MY======"},
             {b32, ~"fo", ~"MZXQ===="},
             {b32, ~"foo", ~"MZXW6==="},
             {b32, ~"foob", ~"MZXW6YQ="},
             {b32, ~"fooba", ~"MZXW6YTB"},
             {b32, ~"foobar", ~"MZXW6YTBOI======"},
             {b45, ~"AB", ~"BB8"},
             {b45, <<100>>, ~"A2"},
             {b45, ~"Hello!!", ~"%69 VD92EX0"},
             {z85,
              <<16#86, 16#4F, 16#D2, 16#6F, 16#B5, 16#59, 16#F7, 16#5B>>,
              ~"HelloWorld"}
            ]
    ].

decode_3_test_() ->
    [?_test(?assertEqual(jhn_base:decode(E, B, O), Result)) ||
        {E, O, Result, B} <-
            [{b32, [{alfabet, hex}, binary], ~"", ~""},
             {b32, [{alfabet, hex}, binary], ~"f", ~"CO======"},
             {b32, [{alfabet, hex}, binary], ~"fo", ~"CPNG===="},
             {b32, [{alfabet, hex}, binary], ~"foo", ~"CPNMU==="},
             {b32, [{alfabet, hex}, binary], ~"foob", ~"CPNMUOG="},
             {b32, [{alfabet, hex}, binary], ~"fooba", ~"CPNMUOJ1"},
             {b32, [{alfabet, hex}, binary], ~"foobar", ~"CPNMUOJ1E8======"}
            ]
    ].

encode_3_decode_3_test_() ->
    [?_test(?assertEqual(jhn_base:decode(E, jhn_base:encode(E, B, O), O), B)) ||
        {E, O, B} <-
            [{b32, [{alfabet, hex}, binary], ~""},
             {b32, [{alfabet, hex}, binary], ~"f"},
             {b32, [{alfabet, hex}, binary], ~"fo"},
             {b32, [{alfabet, hex}, binary], ~"foo"},
             {b32, [{alfabet, hex}, binary], ~"foob"},
             {b32, [{alfabet, hex}, binary], ~"fooba"},
             {b32, [{alfabet, hex}, binary], ~"foobar"}
            ]
    ].
