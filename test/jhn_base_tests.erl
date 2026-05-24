-module(jhn_base_tests).

-include_lib("eunit/include/eunit.hrl").

encode_2_test_() ->
    [?_test(?assertEqual(iolist_to_binary(jhn_base:encode(E, B)), Result)) ||
        {E, Result, B} <-
            [{32, ~"", ~""},
             {32, ~"MY======", ~"f"},
             {32, ~"MZXQ====", ~"fo"},
             {32, ~"MZXW6===", ~"foo"},
             {32, ~"MZXW6YQ=", ~"foob"},
             {32, ~"MZXW6YTB", ~"fooba"},
             {32, ~"MZXW6YTBOI======", ~"foobar"},
             {45, ~"BB8", ~"AB"},
             {45, ~"%69 VD92EX0", ~"Hello!!"},
             {45, ~"A2", <<100>>}
            ]
    ].


encode_3_test_() ->
    [?_test(?assertEqual(jhn_base:encode(E, B, O), Result)) ||
        {E, O, Result, B} <-
            [{32, [{alfabet, hex}, binary], ~"", ~""},
             {32, [{alfabet, hex}, binary], ~"CO======", ~"f"},
             {32, [{alfabet, hex}, binary], ~"CPNG====", ~"fo"},
             {32, [{alfabet, hex}, binary], ~"CPNMU===", ~"foo"},
             {32, [{alfabet, hex}, binary], ~"CPNMUOG=", ~"foob"},
             {32, [{alfabet, hex}, binary], ~"CPNMUOJ1", ~"fooba"},
             {32, [{alfabet, hex}, binary], ~"CPNMUOJ1E8======", ~"foobar"},
             {85,
              [{algo, z85}, binary],
              ~"HelloWorld",
              <<16#86, 16#4F, 16#D2, 16#6F, 16#B5, 16#59, 16#F7, 16#5B>>}
            ]
    ].

decode_2_test_() ->
    [?_test(?assertEqual(iolist_to_binary(jhn_base:decode(E, B)), Result)) ||
        {E, Result, B} <-
            [{32, ~"f", ~"MY======"},
             {32, ~"fo", ~"MZXQ===="},
             {32, ~"foo", ~"MZXW6==="},
             {32, ~"foob", ~"MZXW6YQ="},
             {32, ~"fooba", ~"MZXW6YTB"},
             {32, ~"foobar", ~"MZXW6YTBOI======"},
             {45, ~"AB", ~"BB8"},
             {45, <<100>>, ~"A2"},
             {45, ~"Hello!!", ~"%69 VD92EX0"}
            ]
    ].

decode_3_test_() ->
    [?_test(?assertEqual(jhn_base:decode(E, B, O), Result)) ||
        {E, O, Result, B} <-
            [{32, [{alfabet, hex}, binary], ~"", ~""},
             {32, [{alfabet, hex}, binary], ~"f", ~"CO======"},
             {32, [{alfabet, hex}, binary], ~"fo", ~"CPNG===="},
             {32, [{alfabet, hex}, binary], ~"foo", ~"CPNMU==="},
             {32, [{alfabet, hex}, binary], ~"foob", ~"CPNMUOG="},
             {32, [{alfabet, hex}, binary], ~"fooba", ~"CPNMUOJ1"},
             {32, [{alfabet, hex}, binary], ~"foobar", ~"CPNMUOJ1E8======"},
             {85,
              [{algo, z85}, binary],
              <<16#86, 16#4F, 16#D2, 16#6F, 16#B5, 16#59, 16#F7, 16#5B>>,
              ~"HelloWorld"}
            ]
    ].

encode_3_decode_3_test_() ->
    [?_test(?assertEqual(jhn_base:decode(E, jhn_base:encode(E, B, O), O), B)) ||
        {E, O, B} <-
            [{32, [{alfabet, hex}, binary], ~""},
             {32, [{alfabet, hex}, binary], ~"f"},
             {32, [{alfabet, hex}, binary], ~"fo"},
             {32, [{alfabet, hex}, binary], ~"foo"},
             {32, [{alfabet, hex}, binary], ~"foob"},
             {32, [{alfabet, hex}, binary], ~"fooba"},
             {32, [{alfabet, hex}, binary], ~"foobar"}
            ]
    ].
