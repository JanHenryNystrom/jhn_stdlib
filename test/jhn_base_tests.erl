%%==============================================================================
%% Copyright 2026 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the jhn_base library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_base_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

-define(HOBBE,
        <<"Man is distinguished, not only by his reason, but by this"
          " singular passion from other animals, which is a lust of the"
          " mind, that by a perseverance of delight in the continued and"
          " indefatigable generation of knowledge, exceeds the short"
          " vehemence of any carnal pleasure.">>).

-define(HOBBE85,
        <<"9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)"
          "/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee."
          "Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U"
          "=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlo"
          "lDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JM"
          "K@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/c">>).


%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

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
             {45, ~"A2", <<100>>},
             {58, ~"2NEpo7TZRRrLZSi2U", ~"Hello World!"},
             {58,
              ~"USm3fpXnKG5EUBx2ndxBDMPVciP5hGey2Jh4NDv6gmeo1LkMeiKrLJUUBk6Z",
              ~"The quick brown fox jumps over the lazy dog."},
             {58, ~"11233QC4", binary:decode_hex(~"0000287fb4cd")},
             {85, ?HOBBE85, ?HOBBE},
             {85, ~"87cURDc^jt", ~"HelloWor"},
             {85, ~"z", <<0:32>>}
            ]
    ].


encode_3_test_() ->
    [?_test(?assertEqual(jhn_base:encode(E, B, O), Result)) ||
        {E, O, Result, B} <-
            [{32, [{alphabet, hex}, binary], ~"", ~""},
             {32, [{alphabet, hex}, binary], ~"CO======", ~"f"},
             {32, [{alphabet, hex}, binary], ~"CPNG====", ~"fo"},
             {32, [{alphabet, hex}, binary], ~"CPNMU===", ~"foo"},
             {32, [{alphabet, hex}, binary], ~"CPNMUOG=", ~"foob"},
             {32, [{alphabet, hex}, binary], ~"CPNMUOJ1", ~"fooba"},
             {32, [{alphabet, hex}, binary], ~"CPNMUOJ1E8======", ~"foobar"},
             {32, [{algo, crockford}, binary], ~"CR", ~"f"},
             {32, [{algo, crockford}, binary], ~"CSQG", ~"fo"},
             {32, [{algo, crockford}, binary], ~"CSQPY", ~"foo"},
             {32, [{algo, crockford}, binary], ~"CSQPYRG", ~"foob"},
             {32, [{algo, crockford}, binary], ~"CSQPYRK1", ~"fooba"},
             {32, [{algo, crockford}, binary], ~"CSQPYRK1E8", ~"foobar"},
             {32, [{algo, crockford}, binary], ~"EHJQ6X0", ~"test"},
             {32, [{algo, crockford}, binary], ~"0XDWT58", <<123456789:32>>},
             {32, [{algo, clockwork}, binary], ~"CR", ~"f"},
             {32, [{algo, clockwork}, binary], ~"CSQPYRK1E8", ~"foobar"},
             {32,
              [{algo, clockwork}, binary],
              ~"91JPRV3F5GG7EVVJDHJ22",
              ~"Hello, world!"},
             {32,
              [{algo, clockwork}, binary],
              <<"AHM6A83HENMP6TS0C9S6YXVE41K6YY10D9TPTW3K41QQCS"
                "BJ41T6GS90DHGQMY90CHQPEBG">>,
              ~"The quick brown fox jumps over the lazy dog."},
             {32, [{algo, zbase}, binary], ~"y", <<0:1>>},
             {32, [{algo, zbase}, binary], ~"o", <<1:1>>},
             {32, [{algo, zbase}, binary], ~"e", <<1:2>>},
             {32, [{algo, zbase}, binary], ~"a", <<3:2>>},
             {32, [{algo, zbase}, binary], ~"yy", <<0:10>>},
             {32, [{algo, zbase}, binary], ~"on", <<514:10>>},
             {32, [{algo, zbase}, binary], ~"tqre", <<571528:20>>},
             {32, [{algo, zbase}, binary], ~"6n9hq", <<15777735:24>>},
             {32, [{algo, zbase}, binary], ~"4t7ye", <<13924868:24>>},
             {32,
              [{algo, zbase}, binary],
              ~"6im54d",
              <<1029041987:30>>},
             {58,
              [{alphabet, flickr}, binary],
              ~"brXijP",
              <<16#01, 16#98, 16#B9, 16#A1, 16#0F>>},
             {58,
              [{alphabet, ripple}, binary],
              ~"p4NFofTZRRiLZS5p7",
              ~"Hello World!"},
             {85,
              [{algo, z85}, binary],
              ~"HelloWorld",
              <<16#86, 16#4F, 16#D2, 16#6F, 16#B5, 16#59, 16#F7, 16#5B>>},
             {85,
              [{algo, ipv6}, binary],
              ~"4)+k&C#VzJ4br>0wv%Yp",
              ~"1080:0:0:0:8:800:200C:417A"}
            ]
    ].

%% ===================================================================
%% Decoding
%% ===================================================================

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
             {45, ~"Hello!!", ~"%69 VD92EX0"},
             {85, ~"HelloWor", ~"87cURDc^jt"},
             {85, ?HOBBE, ?HOBBE85}
            ]
    ].

decode_3_test_() ->
    [?_test(?assertEqual(jhn_base:decode(E, B, O), Result)) ||
        {E, O, Result, B} <-
            [{32, [{alphabet, hex}, binary], ~"", ~""},
             {32, [{alphabet, hex}, binary], ~"f", ~"CO======"},
             {32, [{alphabet, hex}, binary], ~"fo", ~"CPNG===="},
             {32, [{alphabet, hex}, binary], ~"foo", ~"CPNMU==="},
             {32, [{alphabet, hex}, binary], ~"foob", ~"CPNMUOG="},
             {32, [{alphabet, hex}, binary], ~"fooba", ~"CPNMUOJ1"},
             {32, [{alphabet, hex}, binary], ~"foobar", ~"CPNMUOJ1E8======"},
             {32, [{algo, crockford}, binary], ~"f", ~"CR"},
             {32, [{algo, crockford}, binary], ~"fo", ~"CSQG"},
             {32, [{algo, crockford}, binary], ~"foo", ~"CSQPY"},
             {32, [{algo, crockford}, binary], ~"foob", ~"CSQPYRG"},
             {32, [{algo, crockford}, binary], ~"fooba", ~"CSQPYRK1"},
             {32, [{algo, crockford}, binary], ~"foobar", ~"CSQPYRK1E8"},
             {32, [{algo, crockford}, binary], ~"test", ~"EHJQ6X0"},
             {32, [{algo, crockford}, binary], ~"test", ~"ehjq6xo"},
             {32, [{algo, crockford}, binary], <<123456789:32>>, ~"0XDWT58"},
             {32, [{algo, crockford}, binary], ~"foobar", ~"CSQPY-RK1E8"},
             {32, [{algo, crockford}, binary], ~"foobar", ~"csqpyrkle8"},
             {32, [{algo, crockford}, binary], ~"foobar", ~"csqpyrkie8"},
             {32, [{algo, clockwork}, binary], ~"f", ~"CR"},
             {32, [{algo, clockwork}, binary], ~"f", ~"CR0"},
             {32, [{algo, clockwork}, binary], ~"foobar", ~"CSQPYRK1E8"},
             {32,
              [{algo, clockwork}, binary],
              ~"Hello, world!",
              ~"91JPRV3F5GG7EVVJDHJ22"},
             {32,
              [{algo, clockwork}, binary],
              ~"The quick brown fox jumps over the lazy dog.",
              <<"AHM6A83HENMP6TS0C9S6YXVE41K6YY10D9TPTW3K41QQCS"
                "BJ41T6GS90DHGQMY90CHQPEBG">>},
             {32, [{algo, zbase}], <<0:5>>, ~"y"},
             {32, [{algo, zbase}], <<1:1, 0:4>>, ~"o"},
             {32, [{algo, zbase}], <<1:2, 0:3>>, ~"e"},
             {32, [{algo, zbase}], <<3:2, 0:3>>, ~"a"},
             {32, [{algo, zbase}], <<0:5, 0:5>>, ~"yy"},
             {32, [{algo, zbase}], <<128, 2:2>>, ~"on"},
             {32, [{algo, zbase}], <<17:5, 14:5, 4:5, 8:5>>, ~"tqre"},
             {32, [{algo, zbase}], <<30:5, 2:5, 31:5, 28:5, 14:5>>, ~"6n9hq"},
             {32, [{algo, zbase}], <<26:5, 17:5, 29:5, 0:5, 8:5>>, ~"4t7ye"},
             {32, [{algo, zbase}], <<30:5,21:5,11:5,27:5,26:5,3:5>>, ~"6im54d"},
             {58,
              [{alphabet, flickr}, binary],
              <<16#01, 16#98, 16#B9, 16#A1, 16#0F>>,
              ~"brXijP"},
             {58,
              [{alphabet, ripple}, binary],
              ~"Hello World!",
              ~"p4NFofTZRRiLZS5p7"},
             {85,
              [{algo, z85}, binary],
              <<16#86, 16#4F, 16#D2, 16#6F, 16#B5, 16#59, 16#F7, 16#5B>>,
              ~"HelloWorld"},
             {85,
              [{algo, ipv6}, binary],
              ~"1080:0:0:0:8:800:200c:417a",
              ~"4)+k&C#VzJ4br>0wv%Yp"}
            ]
    ].

%% ===================================================================
%% Encoding/Decoding
%% ===================================================================

encode_3_decode_3_test_() ->
    [?_test(?assertEqual(jhn_base:decode(E, jhn_base:encode(E, B, O), O), B)) ||
        {E, O, B} <-
            [{32, [{alphabet, hex}, binary], ~""},
             {32, [{alphabet, hex}, binary], ~"f"},
             {32, [{alphabet, hex}, binary], ~"fo"},
             {32, [{alphabet, hex}, binary], ~"foo"},
             {32, [{alphabet, hex}, binary], ~"foob"},
             {32, [{alphabet, hex}, binary], ~"fooba"},
             {32, [{alphabet, hex}, binary], ~"foobar"}
            ]
    ].
