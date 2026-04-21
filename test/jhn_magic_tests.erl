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
%%%   eunit unit tests for the jhn_magic library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_magic_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(APPLE_SINGLE,
        <<0,5,22,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,
          0,0,3,0,0,0,86,0,0,0,14,0,0,0,8,0,0,0,100,0,0,0,16,0,0,
          0,9,0,0,0,116,0,0,0,32,0,0,0,10,0,0,0,148,0,0,0,8,0,0,0,
          1,0,0,0,156,0,0,0,4,97,95,116,101,115,116,102,105,108,
          101,46,116,120,116,49,122,147,218,49,122,147,220,49,122,
          147,218,49,122,147,218,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,102,111,
          111,10>>).

-define(
   PDF,
   <<"%PDF-1.0\n"
     "1 0 obj<</Type/Catalog/Pages 2 0 R>>endobj\n"
     "2 0 obj<</Type/Pages/Kids[3 0 R]/Count 1>>endobj\n"
     "3 0 obj<</Type/Page/Parent "
     "2 0 R/Resources<<>>/MediaBox[0 0 9 9]>>endobj\n"
     "xref\n"
     "0 4\n"
     "0000000000 65535 f \n"
     "0000000009 00000 n \n"
     "0000000052 00000 n \n"
     "0000000101 00000 n \n"
     "trailer<</Root 1 0 R/Size 4>>\n"
     "startxref\n"
     "174\n"
     "%%EOF">>).

-define(PNG,
        <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,
          0,1,0,0,0,1,1,0,0,0,0,55,110,249,36,0,0,0,10,73,68,
          65,84,120,1,99,96,0,0,0,2,0,1,115,117,1,24,0,0,0,0,
          73,69,78,68,174,66,96,130>>).

-define(JPEG,
        <<16#FF, 16#D8, 16#FF, 16#E0, 16#00, 16#10, 16#4A, 16#46, 16#49,
          16#46, 16#00, 16#01, 16#01, 16#01, 16#00, 16#48, 16#00, 16#48,
          16#00, 16#00, 16#FF, 16#DB, 16#00, 16#43, 16#00, 16#FF, 16#FF,
          16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF,
          16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF,
          16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF,
          16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF,
          16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF,
          16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF,
          16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF, 16#FF,
          16#C2, 16#00, 16#0B, 16#08, 16#00, 16#01, 16#00, 16#01, 16#01,
          16#01, 16#11, 16#00, 16#FF, 16#C4, 16#00, 16#14, 16#10, 16#01,
          16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
          16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#FF, 16#DA,
          16#00, 16#08, 16#01, 16#01, 16#00, 16#01, 16#3F, 16#10>>).

-define(RTF,
        ~"{\rtf1\ansi {\fonttbl\f0\fswiss Helvetica;} \f0\fs24 A RTF.}").

-define(BMP,
        <<66, 77, 30, 0, 0, 0, 0, 0, 0, 0, 26, 0, 0, 0, 12, 0,
          0, 0, 1, 0, 1, 0, 1,  0, 24, 0, 0, 0, 255, 0>>).

-define(CAL,
        <<"BEGIN:VCALENDAR\n"
          "VERSION:2.0\n"
          "PRODID:-//hacksw/handcal//NONSGML v1.0//EN\n"
          "BEGIN:VEVENT\n"
          "UID:19970610T172345Z-AF23B2@example.com\n"
          "DTSTAMP:19970610T172345Z\n"
          "DTSTART:19970714T170000Z\n"
          "DTEND:19970715T040000Z\n"
          "SUMMARY:Bastille Day Party\n"
          "END:VEVENT\n"
          "END:VCALENDAR\n">>).

-define(XML, ~"<?xml version=\"1.0\"?><a/>").

-define(ATOM,
        <<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
          "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
          "<title>Example Feed</title>\n"
          "<link href=\"http://example.org/\"/>\n"
          "<updated>2003-12-13T18:30:02Z</updated>\n"
          "<author>\n"
          "<name>John Doe</name>\n"
          "</author>\n"
          "<id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>\n"
          "<entry>\n"
          "<title>Atom-Powered Robots Run Amok</title>\n"
          "<link href=\"http://example.org/2003/12/13/atom03\"/>\n"
          "<id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>\n"
          "<updated>2003-12-13T18:30:02Z</updated>\n"
          "<summary>Some text.</summary>\n"
          "</entry>\n"
          "</feed>\n">>).

-define(MS,
        <<16#D0, 16#CF, 16#11, 16#E0, 16#A1, 16#B1, 16#1A, 16#E1, "Foo Bar">>).

%%
%% zlib:gzip(~"Foo").
%%
-define(GZIP,
        <<31,139,8,0,0,0,0,0,0,19,115,203,207,7,0,193,35,62,180,3, 0,0,0>>).


%%
%% zip:zip("test.zip", [{"test.txt", ~"Foo"}], [memory])
%%
-define(ZIP,
        <<80,75,3,4,10,0,0,0,0,0,150,0,150,92,193,35,62,180,3,0,0,
          0,3,0,0,0,8,0,28,0,116,101,115,116,46,116,120,116,85,84,
          9,0,3,125,244,231,105,125,244,231,105,117,120,11,0,1,4,
          0,0,0,0,4,0,0,0,0,70,111,111,80,75,1,2,61,3,10,0,0,0,0,
          0,150,0,150,92,193,35,62,180,3,0,0,0,3,0,0,0,8,0,24,0,0,
          0,0,0,0,0,0,0,164,1,0,0,0,0,116,101,115,116,46,116,120,
          116,85,84,5,0,1,125,244,231,105,117,120,11,0,1,4,0,0,0,
          0,4,0,0,0,0,80,75,5,6,0,0,0,0,1,0,1,0,78,0,0,0,69,0,0,0,
          0,0>>).

%%
%% zip:zip("test.zip",
%%         [{"[Content_Types].xml", ~"Bar"}, {"test.txt", ~"Foo"}],
%%          [memory])
%%
-define(ZIP_CONTENT_TYPES,
        <<80,75,3,4,10,0,0,0,0,0,54,1,150,92,74,202,178,78,3,0,0,
          0,3,0,0,0,19,0,28,0,91,67,111,110,116,101,110,116,95,84,
          121,112,101,115,93,46,120,109,108,85,84,9,0,3,169,245,
          231,105,169,245,231,105,117,120,11,0,1,4,0,0,0,0,4,0,0,
          0,0,66,97,114,80,75,3,4,10,0,0,0,0,0,54,1,150,92,193,35,
          62,180,3,0,0,0,3,0,0,0,8,0,28,0,116,101,115,116,46,116,
          120,116,85,84,9,0,3,169,245,231,105,169,245,231,105,117,
          120,11,0,1,4,0,0,0,0,4,0,0,0,0,70,111,111,80,75,1,2,61,
          3,10,0,0,0,0,0,54,1,150,92,74,202,178,78,3,0,0,0,3,0,0,
          0,19,0,24,0,0,0,0,0,0,0,0,0,164,1,0,0,0,0,91,67,111,110,
          116,101,110,116,95,84,121,112,101,115,93,46,120,109,108,
          85,84,5,0,1,169,245,231,105,117,120,11,0,1,4,0,0,0,0,4,
          0,0,0,0,80,75,1,2,61,3,10,0,0,0,0,0,54,1,150,92,193,35,
          62,180,3,0,0,0,3,0,0,0,8,0,24,0,0,0,0,0,0,0,0,0,164,1,
          80,0,0,0,116,101,115,116,46,116,120,116,85,84,5,0,1,169,
          245,231,105,117,120,11,0,1,4,0,0,0,0,4,0,0,0,0,80,75,5,
          6,0,0,0,0,2,0,2,0,167,0,0,0,149,0,0,0,0,0>>).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% media_type/1
%%--------------------------------------------------------------------
media_type_1_test_() ->
    [?_test(?assertEqual(jhn_magic:media_type(B), Type)) ||
        {Type, B} <-
            [{~"application/applefile", ?APPLE_SINGLE},
             {~"application/gzip", ?GZIP},
             {~"application/msword", file("file-sample_100kB.doc")},
             {~"application/pdf", ?PDF},
             {~"application/rtf", ?RTF},
             {~"application/vnd.ms-excel", file("file_example_XLS_10.xls")},
             {~"application/vnd.ms-powerpoint",
              file("file_example_PPT_250kB.ppt")},
             {~"application/vnd.visio", file("MVPSession1SimpleTimeline.vsd")},
             {~"application/xml", ?XML},
             {~"application/xml", ?ATOM},
             {~"application/zip", file("file-sample_100kB.docx")},
             {~"application/zip", file("file_example_XLSX_10.xlsx")},
             {~"application/zip", file("Extlst-test.pptx")},
             {~"image/bmp", ?BMP},
             {~"image/jpeg", ?JPEG},
             {~"image/png", ?PNG},
             {~"text/calendar", ?CAL},
             {undefined, ~"Foo"},
             {undefined, ?MS}
            ]
    ].

%%--------------------------------------------------------------------
%% media_type/2
%%--------------------------------------------------------------------
media_type_2_test_() ->
    [?_test(?assertEqual(jhn_magic:media_type(B, [{deep, true}]), Type)) ||
        {Type, B} <-
            [{~"application/atom+xml", ?ATOM},
             {~"application/pdf", ?PDF},
             {~"application/vnd.ms-visio.drawing.main+xml",
              file("computer-network.vsdx")},
             {<<"application/"
                "vnd.openxmlformats-officedocument.wordprocessingml.document">>,
              file("file-sample_100kB.docx")},
             {<<"application/"
                "vnd.openxmlformats-officedocument."
                "presentationml.presentation">>,
              file("Extlst-test.pptx")},
             {<<"application/"
                "vnd.openxmlformats-officedocument.spreadsheetml.sheet">>,
              file("file_example_XLSX_10.xlsx")},
             {~"application/xml", ?XML},
             {~"application/zip", ?ZIP},
             {~"application/zip", ?ZIP_CONTENT_TYPES}
            ]
    ].

%% ===================================================================
%% Internal functions.
%% ===================================================================

file(F) ->
    {ok, B} = file:read_file(filename:join([code:lib_dir(jhn_stdlib),
                                            test,
                                            magic,
                                            F])),
    B.
