-module(jhn_hpack_tests).

-record(context,
        {table, without, never, max, limit, length, size,
         %% encode
         huffman_encode, return_type
        }).

-include_lib("eunit/include/eunit.hrl").

-define(HS1,
        [{~":method", ~"GET"},
         {~":path", ~"/"},
         {~":scheme", ~"http"},
         {~":authority", ~"localhost:8080"},
         {~"accept", ~"*/*"},
         {~"accept-encoding", ~"gzip, deflate"},
         {~"user-agent", ~"foo/1.0.0"}]).

-define(HS2,
        [{~":method", ~"GET"},
         {~":path", ~"/"},
         {~":scheme", ~"http"},
         {~":authority", ~"localhost:8081"},
         {~"accept", ~"*/*"},
         {~"accept-encoding", ~"gzip, deflate"},
         {~"user-agent", ~"nghttp2/0.7.11"},
         {~"x-tyktorp", ~"B"},
         {~"x-tyktorp", ~"O"},
         {~"x-meow", ~"p"},
         {~"x-meow", ~"t"},
         {~"x-tyktorp", ~"t"}]).

encode_1_decode_1_test_() ->
    [?_test(encode_1_decode_1(Hs)) || Hs <- [?HS1, ?HS2]].

encode_1_decode_1(Hs) ->
    {ok, Bin, C} = jhn_hpack:encode(Hs),
    ?assertEqual({ok, Hs, C}, jhn_hpack:decode(Bin)).

encode_2_decode_1_test_() ->
    [?_test(encode_2_decode_1(Hs)) || Hs <- [?HS1, ?HS2]].

encode_2_decode_1(Hs) ->
    {ok, Bin, _} = jhn_hpack:encode(Hs, #{huffman_encode => true}),
    ?assertMatch({ok, Hs, _}, jhn_hpack:decode(Bin)).

%%--------------------------------------------------------------------
%% rfc7541 C.2.1 Literal Header Field with Indexing
%%--------------------------------------------------------------------
%% 400a 6375 7374 6f6d 2d6b 6579 0d63 7573 | @.custom-key.cus
%% 746f 6d2d 6865 6164 6572                | tom-header

-define(C_2_1_STR, ~"@\ncustom-key\rcustom-header").
-define(C_2_1_BIN,
        <<16#40,16#0A,16#63,16#75,16#73,16#74,16#6F,16#6D,16#2D,16#6B,16#65,
          16#79,16#0D,16#63,16#75,16#73,16#74,16#6F,16#6D,16#2D,16#68,16#65,
          16#61,16#64,16#65,16#72>>).
-define(C_2_1_H, [{~"custom-key", ~"custom-header"}]).

rfc7541_c_2_1_test() ->
    ?assertEqual(?C_2_1_STR, ?C_2_1_BIN),
    ?assertMatch({ok, ?C_2_1_BIN, _}, jhn_hpack:encode(?C_2_1_H)),
    ?assertMatch({ok, ?C_2_1_H, _}, jhn_hpack:decode(?C_2_1_BIN)),
    {ok, Bin, _} = jhn_hpack:encode(?C_2_1_H, #{huffman_encode => true}),
    ?assertNotMatch(?C_2_1_BIN, Bin),
    ?assertMatch({ok, ?C_2_1_H, _}, jhn_hpack:decode(Bin)).

%%--------------------------------------------------------------------
%% rfc7541 C.2.2 Literal Header Field without Indexing
%%--------------------------------------------------------------------
%% 040c 2f73 616d 706c 652f 7061 7468 | ../sample/path

-define(C_2_2_STR,~"\4\f/sample/path").
-define(C_2_2_BIN,
        <<16#04,16#0C,16#2F,16#73,16#61,16#6D,16#70,16#6C,16#65,
          16#2F,16#70,16#61,16#74,16#68>>).
-define(C_2_2_H, [{~":path", ~"/sample/path"}]).

rfc7541_c_2_2_test() ->
    Opts = #{without => [~":path"]},
    HOpts = #{without => [~":path"], huffman_encode => true},
    ?assertEqual(?C_2_2_STR, ?C_2_2_BIN),
    ?assertMatch({ok, ?C_2_2_BIN, _}, jhn_hpack:encode(?C_2_2_H, Opts)),
    ?assertMatch({ok, ?C_2_2_H, _}, jhn_hpack:decode(?C_2_2_BIN)),
    {ok, Bin, _} = jhn_hpack:encode(?C_2_2_H, HOpts),
    ?assertNotMatch(?C_2_2_BIN, Bin),
    ?assertMatch({ok, ?C_2_2_H, _}, jhn_hpack:decode(Bin)).

%%--------------------------------------------------------------------
%% rfc7541 C.2.3 Literal Header Field Never Indexed
%%--------------------------------------------------------------------
%% 1008 7061 7373 776f 7264 0673 6563 7265 74 | ..password.secret

-define(C_2_3_STR, ~"\20\bpassword\6secret").
-define(C_2_3_BIN,
        <<16#10, 16#08, 16#70, 16#61, 16#73, 16#73, 16#77, 16#6f,
          16#72, 16#64, 16#06, 16#73, 16#65, 16#63, 16#72, 16#65, 16#74>>).
-define(C_2_3_H, [{~"password", ~"secret"}]).

rfc7541_c_2_3_test() ->
    Never = [~"password"],
    Opts = #{never => Never},
    HOpts = #{never => Never, huffman_encode => true},
    ?assertMatch(?C_2_3_STR, ?C_2_3_BIN),
    ?assertMatch({ok, ?C_2_3_BIN, _}, jhn_hpack:encode(?C_2_3_H, Opts)),
    ?assertMatch({ok, ?C_2_3_H, #context{never = Never}},
                 jhn_hpack:decode(?C_2_3_BIN)),
    {ok, Bin, _} = jhn_hpack:encode(?C_2_3_H, HOpts),
    ?assertNotMatch(?C_2_3_BIN, Bin),
    ?assertMatch({ok, ?C_2_3_H, #context{never = Never}},
                 jhn_hpack:decode(Bin)).

%%--------------------------------------------------------------------
%% rfc7541 C.2.4 Indexed Header Field
%%--------------------------------------------------------------------
%% 82 | . (:method: GET)

-define(C_2_4_BIN, <<16#82>>).
-define(C_2_4_H, [{~":method", ~"GET"}]).

rfc7541_c_2_4_test() ->
    Opts = #{never => [~"password"]},
    ?assertMatch({ok, ?C_2_4_BIN, _}, jhn_hpack:encode(?C_2_4_H, Opts)),
    ?assertMatch({ok, ?C_2_4_H, _}, jhn_hpack:decode(?C_2_4_BIN)).

%%--------------------------------------------------------------------
%% rfc7541 C.3.1 - C.3.3 Request Examples without Huffman Coding
%%--------------------------------------------------------------------

-define(C_3_1_BIN,
        <<16#82, 16#86, 16#84, 16#41, 16#0F, 16#77, 16#77, 16#77,
          16#2E, 16#65, 16#78, 16#61, 16#6D, 16#70, 16#6C, 16#65,
          16#2E, 16#63, 16#6F, 16#6D>>).
-define(C_3_1_H,
        [{~":method", ~"GET"},
         {~":scheme", ~"http"},
         {~":path", ~"/"},
         {~":authority", ~"www.example.com"}]).
-define(C_3_1_T, [{~":authority", ~"www.example.com"}]).
-define(C_3_1_S, 57).

-define(C_3_2_BIN,
        <<16#82, 16#86, 16#84, 16#BE, 16#58, 16#08, 16#6E, 16#6F,
          16#2D, 16#63, 16#61, 16#63, 16#68, 16#65>>).
-define(C_3_2_H,
        [{~":method", ~"GET"},
         {~":scheme", ~"http"},
         {~":path", ~"/"},
         {~":authority", ~"www.example.com"},
         {~"cache-control", ~"no-cache"}]).
-define(C_3_2_T, [{~"cache-control", ~"no-cache"},
                  {~":authority", ~"www.example.com"}]).
-define(C_3_2_S, 110).

-define(C_3_3_BIN,
        <<16#82, 16#87, 16#85, 16#BF, 16#40, 16#0A, 16#63, 16#75,
          16#73, 16#74, 16#6F, 16#6D, 16#2D, 16#6B, 16#65, 16#79,
          16#0C, 16#63, 16#75, 16#73, 16#74, 16#6F, 16#6D, 16#2D,
          16#76, 16#61, 16#6C, 16#75, 16#65>>).
-define(C_3_3_H,
        [{~":method", ~"GET"},
         {~":scheme", ~"https"},
         {~":path", ~"/index.html"},
         {~":authority", ~"www.example.com"},
         {~"custom-key", ~"custom-value"}]).
-define(C_3_3_T, [{~"custom-key", ~"custom-value"},
                  {~"cache-control", ~"no-cache"},
                  {~":authority", ~"www.example.com"}]).
-define(C_3_3_S, 164).

rfc7541_c_3_test() ->
    {ok, Bin1, EC1} = jhn_hpack:encode(?C_3_1_H),
    ?assertEqual(?C_3_1_BIN, Bin1),
    {ok, ?C_3_1_H, DC1 = #context{table = T1, size = S1}} =
        jhn_hpack:decode(?C_3_1_BIN),
    ?assertMatch(?C_3_1_T, T1),
    ?assertMatch(?C_3_1_S, S1),
    {ok, Bin2, EC2} = jhn_hpack:encode(?C_3_2_H, EC1),
    ?assertEqual(?C_3_2_BIN, Bin2),
    {ok, ?C_3_2_H, DC2 = #context{table = T2, size = S2}} =
        jhn_hpack:decode(?C_3_2_BIN, DC1),
    ?assertMatch(?C_3_2_T, T2),
    ?assertMatch(?C_3_2_S, S2),
    {ok, Bin3, _} = jhn_hpack:encode(?C_3_3_H, EC2),
    ?assertEqual(?C_3_3_BIN, Bin3),
    {ok, ?C_3_3_H, #context{table = T3, size = S3}} =
        jhn_hpack:decode(?C_3_3_BIN, DC2),
    ?assertMatch(?C_3_3_T, T3),
    ?assertMatch(?C_3_3_S, S3).

%%--------------------------------------------------------------------
%% rfc7541 C.4.1 - C.4.3 Request Examples with Huffman Coding
%%--------------------------------------------------------------------

-define(C_4_1_BIN,
        <<16#82, 16#86, 16#84, 16#41, 16#8c, 16#f1, 16#e3, 16#c2,
          16#e5, 16#f2, 16#3a, 16#6b, 16#a0, 16#ab, 16#90, 16#f4,
          16#ff>>).
-define(C_4_1_H,
        [{~":method", ~"GET"},
         {~":scheme", ~"http"},
         {~":path", ~"/"},
         {~":authority", ~"www.example.com"}]).
-define(C_4_1_T, [{~":authority", ~"www.example.com"}]).
-define(C_4_1_S, 57).

-define(C_4_2_BIN,
        <<16#82, 16#86, 16#84, 16#be, 16#58, 16#86,
          16#a8, 16#eb, 16#10, 16#64, 16#9c, 16#bf>>).
-define(C_4_2_H,
        [{~":method", ~"GET"},
         {~":scheme", ~"http"},
         {~":path", ~"/"},
         {~":authority", ~"www.example.com"},
         {~"cache-control", ~"no-cache"}]).
-define(C_4_2_T, [{~"cache-control", ~"no-cache"},
                  {~":authority", ~"www.example.com"}]).
-define(C_4_2_S, 110).

-define(C_4_3_BIN,
        <<16#82, 16#87, 16#85, 16#bf, 16#40, 16#88, 16#25, 16#a8,
          16#49, 16#e9, 16#5b, 16#a9, 16#7d, 16#7f, 16#89, 16#25,
          16#a8, 16#49, 16#e9, 16#5b, 16#b8, 16#e8, 16#b4, 16#bf>>).
-define(C_4_3_H,
        [{~":method", ~"GET"},
         {~":scheme", ~"https"},
         {~":path", ~"/index.html"},
         {~":authority", ~"www.example.com"},
         {~"custom-key", ~"custom-value"}]).
-define(C_4_3_T, [{~"custom-key", ~"custom-value"},
                  {~"cache-control", ~"no-cache"},
                  {~":authority", ~"www.example.com"}]).
-define(C_4_3_S, 164).

rfc7541_c_4_test() ->
    Opts = #{huffman_encode => true},
    {ok, Bin1, EC1} = jhn_hpack:encode(?C_4_1_H, Opts),
    ?assertEqual(?C_4_1_BIN, Bin1),
    {ok, ?C_4_1_H, DC1 = #context{table = T1, size = S1}} =
        jhn_hpack:decode(?C_4_1_BIN),
    ?assertMatch(?C_4_1_T, T1),
    ?assertMatch(?C_4_1_S, S1),
    {ok, Bin2, EC2} = jhn_hpack:encode(?C_4_2_H, EC1),
    ?assertEqual(?C_4_2_BIN, Bin2),
    {ok, ?C_4_2_H, DC2 = #context{table = T2, size = S2}} =
        jhn_hpack:decode(?C_4_2_BIN, DC1),
    ?assertMatch(?C_4_2_T, T2),
    ?assertMatch(?C_4_2_S, S2),
    {ok, Bin3, _} = jhn_hpack:encode(?C_4_3_H, EC2),
    ?assertEqual(?C_4_3_BIN, Bin3),
    {ok, ?C_4_3_H, #context{table = T3, size = S3}} =
        jhn_hpack:decode(?C_4_3_BIN, DC2),
    ?assertMatch(?C_4_3_T, T3),
    ?assertMatch(?C_4_3_S, S3).

%%--------------------------------------------------------------------
%% rfc7541 C.5.1 - C.5.3 Response Examples without Huffman Coding
%%--------------------------------------------------------------------

%% Added a Dynamic Table Size Update to set the max size

-define(C_5_1_BIN,
        <<16#3F, 16#E1, 16#01,
          16#48, 16#03, 16#33, 16#30, 16#32, 16#58, 16#07, 16#70,
          16#72, 16#69, 16#76, 16#61, 16#74, 16#65, 16#61, 16#1D,
          16#4d, 16#6f, 16#6e, 16#2c, 16#20, 16#32, 16#31, 16#20,
          16#4f, 16#63, 16#74, 16#20, 16#32, 16#30, 16#31, 16#33,
          16#20, 16#32, 16#30, 16#3a, 16#31, 16#33, 16#3a, 16#32,
          16#31, 16#20, 16#47, 16#4d, 16#54, 16#6e, 16#17, 16#68,
          16#74, 16#74, 16#70, 16#73, 16#3a, 16#2f, 16#2f, 16#77,
          16#77, 16#77, 16#2e, 16#65, 16#78, 16#61, 16#6d, 16#70,
          16#6c, 16#65, 16#2e, 16#63, 16#6f, 16#6d>>).
-define(C_5_1_H,
        [{~":status", ~"302"},
         {~"cache-control", ~"private"},
         {~"date", ~"Mon, 21 Oct 2013 20:13:21 GMT"},
         {~"location", ~"https://www.example.com"}]).
-define(C_5_1_T,
        [{~"location", ~"https://www.example.com"},
         {~"date", ~"Mon, 21 Oct 2013 20:13:21 GMT"},
         {~"cache-control", ~"private"},
         {~":status", ~"302"}]).
-define(C_5_1_S, 222).

-define(C_5_2_BIN, <<16#48, 16#03, 16#33, 16#30, 16#37, 16#c1, 16#c0, 16#bf>>).
-define(C_5_2_H,
        [{~":status", ~"307"},
         {~"cache-control", ~"private"},
         {~"date",  ~"Mon, 21 Oct 2013 20:13:21 GMT"},
         {~"location", ~"https://www.example.com"}]).
-define(C_5_2_T,
        [{~":status", ~"307"},
         {~"location", ~"https://www.example.com"},
         {~"date", ~"Mon, 21 Oct 2013 20:13:21 GMT"},
         {~"cache-control", ~"private"}]).
-define(C_5_2_S, 222).

-define(C_5_3_BIN,
        <<16#88, 16#c1, 16#61, 16#1d, 16#4d, 16#6f, 16#6e, 16#2c,
          16#20, 16#32, 16#31, 16#20, 16#4f, 16#63, 16#74, 16#20,
          16#32, 16#30, 16#31, 16#33, 16#20, 16#32, 16#30, 16#3a,
          16#31, 16#33, 16#3a, 16#32, 16#32, 16#20, 16#47, 16#4d,
          16#54, 16#c0, 16#5a, 16#04, 16#67, 16#7a, 16#69, 16#70,
          16#77, 16#38, 16#66, 16#6f, 16#6f, 16#3d, 16#41, 16#53,
          16#44, 16#4a, 16#4b, 16#48, 16#51, 16#4b, 16#42, 16#5a,
          16#58, 16#4f, 16#51, 16#57, 16#45, 16#4f, 16#50, 16#49,
          16#55, 16#41, 16#58, 16#51, 16#57, 16#45, 16#4f, 16#49,
          16#55, 16#3b, 16#20, 16#6d, 16#61, 16#78, 16#2d, 16#61,
          16#67, 16#65, 16#3d, 16#33, 16#36, 16#30, 16#30, 16#3b,
          16#20, 16#76, 16#65, 16#72, 16#73, 16#69, 16#6f, 16#6e,
          16#3d, 16#31>>).
-define(C_5_3_H,
        [{~":status", ~"200"},
         {~"cache-control", ~"private"},
         {~"date", ~"Mon, 21 Oct 2013 20:13:22 GMT"},
         {~"location", ~"https://www.example.com"},
         {~"content-encoding", ~"gzip"},
         {~"set-cookie",
          ~"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"}]).
-define(C_5_3_T,
        [{~"set-cookie",
          ~"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"},
         {~"content-encoding", ~"gzip"},
         {~"date", ~"Mon, 21 Oct 2013 20:13:22 GMT"}]).
-define(C_5_3_S, 215).

rfc7541_c_5_test() ->
    {ok, Bin1, EC1} = jhn_hpack:encode([{size_update, 256} | ?C_5_1_H]),
    ?assertEqual(?C_5_1_BIN, Bin1),
    {ok, ?C_5_1_H, DC1 = #context{table = T1, size = S1}} =
        jhn_hpack:decode(?C_5_1_BIN),
    ?assertMatch(?C_5_1_T, T1),
    ?assertMatch(?C_5_1_S, S1),
    {ok, Bin2, EC2} = jhn_hpack:encode(?C_5_2_H, EC1),
    ?assertEqual(?C_5_2_BIN, Bin2),
    {ok, ?C_5_2_H, DC2 = #context{table = T2, size = S2}} =
        jhn_hpack:decode(?C_5_2_BIN, DC1),
    ?assertMatch(?C_5_2_T, T2),
    ?assertMatch(?C_5_2_S, S2),
    {ok, Bin3, _} = jhn_hpack:encode(?C_5_3_H, EC2),
    ?assertEqual(?C_5_3_BIN, Bin3),
    {ok, ?C_5_3_H, #context{table = T3, size = S3}} =
        jhn_hpack:decode(?C_5_3_BIN, DC2),
    ?assertMatch(?C_5_3_T, T3),
    ?assertMatch(?C_5_3_S, S3).

%%--------------------------------------------------------------------
%% rfc7541 C.6.1 - C.6.3 Response Examples with Huffman Coding
%%--------------------------------------------------------------------

-define(C_6_1_BIN,
        <<16#48, 16#82, 16#64, 16#02, 16#58, 16#85, 16#ae, 16#c3,
          16#77, 16#1a, 16#4b, 16#61, 16#96, 16#d0, 16#7a, 16#be,
          16#94, 16#10, 16#54, 16#d4, 16#44, 16#a8, 16#20, 16#05,
          16#95, 16#04, 16#0b, 16#81, 16#66, 16#e0, 16#82, 16#a6,
          16#2d, 16#1b, 16#ff, 16#6e, 16#91, 16#9d, 16#29, 16#ad,
          16#17, 16#18, 16#63, 16#c7, 16#8f, 16#0b, 16#97, 16#c8,
          16#e9, 16#ae, 16#82, 16#ae, 16#43, 16#d3>>).
-define(C_6_1_H,
        [{~":status", ~"302"},
         {~"cache-control", ~"private"},
         {~"date", ~"Mon, 21 Oct 2013 20:13:21 GMT"},
         {~"location", ~"https://www.example.com"}]).
-define(C_6_1_T,
        [{~"location", ~"https://www.example.com"},
         {~"date", ~"Mon, 21 Oct 2013 20:13:21 GMT"},
         {~"cache-control", ~"private"},
         {~":status", ~"302"}]).
-define(C_6_1_S, 222).

-define(C_6_2_BIN, <<16#48, 16#83, 16#64, 16#0e, 16#ff, 16#c1, 16#c0, 16#bf>>).
-define(C_6_2_H,
        [{~":status", ~"307"},
         {~"cache-control", ~"private"},
         {~"date",  ~"Mon, 21 Oct 2013 20:13:21 GMT"},
         {~"location", ~"https://www.example.com"}]).
-define(C_6_2_T,
        [{~":status", ~"307"},
         {~"location", ~"https://www.example.com"},
         {~"date", ~"Mon, 21 Oct 2013 20:13:21 GMT"},
         {~"cache-control", ~"private"}]).
-define(C_6_2_S, 222).

-define(C_6_3_BIN,
        <<16#88, 16#c1, 16#61, 16#96, 16#d0, 16#7a, 16#be, 16#94,
          16#10, 16#54, 16#d4, 16#44, 16#a8, 16#20, 16#05, 16#95,
          16#04, 16#0b, 16#81, 16#66, 16#e0, 16#84, 16#a6, 16#2d,
          16#1b, 16#ff, 16#c0, 16#5a, 16#83, 16#9b, 16#d9, 16#ab,
          16#77, 16#ad, 16#94, 16#e7, 16#82, 16#1d, 16#d7, 16#f2,
          16#e6, 16#c7, 16#b3, 16#35, 16#df, 16#df, 16#cd, 16#5b,
          16#39, 16#60, 16#d5, 16#af, 16#27, 16#08, 16#7f, 16#36,
          16#72, 16#c1, 16#ab, 16#27, 16#0f, 16#b5, 16#29, 16#1f,
          16#95, 16#87, 16#31, 16#60, 16#65, 16#c0, 16#03, 16#ed,
          16#4e, 16#e5, 16#b1, 16#06, 16#3d, 16#50, 16#07>>).
-define(C_6_3_H,
        [{~":status", ~"200"},
         {~"cache-control", ~"private"},
         {~"date", ~"Mon, 21 Oct 2013 20:13:22 GMT"},
         {~"location", ~"https://www.example.com"},
         {~"content-encoding", ~"gzip"},
         {~"set-cookie",
          ~"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"}]).
-define(C_6_3_T,
        [{~"set-cookie",
          ~"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"},
         {~"content-encoding", ~"gzip"},
         {~"date", ~"Mon, 21 Oct 2013 20:13:22 GMT"}]).
-define(C_6_3_S, 215).

rfc7541_c_6_test() ->
    Opts = #{max => 256, huffman_encode => true},
    {ok, Bin1, EC1} = jhn_hpack:encode(?C_6_1_H, Opts),
    ?assertEqual(?C_6_1_BIN, Bin1),
    {ok, ?C_6_1_H, DC1 = #context{table = T1, size = S1}} =
        jhn_hpack:decode(?C_6_1_BIN, Opts),
    ?assertMatch(?C_6_1_T, T1),
    ?assertMatch(?C_6_1_S, S1),
    {ok, Bin2, EC2} = jhn_hpack:encode(?C_6_2_H, EC1),
    ?assertEqual(?C_6_2_BIN, Bin2),
    {ok, ?C_6_2_H, DC2 = #context{table = T2, size = S2}} =
        jhn_hpack:decode(?C_6_2_BIN, DC1),
    ?assertMatch(?C_6_2_T, T2),
    ?assertMatch(?C_6_2_S, S2),
    {ok, Bin3, _} = jhn_hpack:encode(?C_6_3_H, EC2),
    ?assertEqual(?C_6_3_BIN, Bin3),
    {ok, ?C_6_3_H, #context{table = T3, size = S3}} =
        jhn_hpack:decode(?C_6_3_BIN, DC2),
    ?assertMatch(?C_6_3_T, T3),
    ?assertMatch(?C_6_3_S, S3).
