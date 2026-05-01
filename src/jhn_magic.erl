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
%%%  A media type determining library based on a subset of the types found in:
%%%
%%%  https://www.iana.org/assignments/media-types/media-types.xhtml
%%%
%%%
%%%  The trivially determined types are:
%%%
%%%    application/applefile
%%%    application/gzip                                                (rfc6713)
%%%    application/msword
%%%    application/pdf                                                 (rfc8118)
%%%    application/postscript                                          (rfc2045)
%%%    application/rtf
%%%    application/sqlite3
%%%    application/vnd.ms-excel
%%%    application/vnd.ms-powerpoint
%%%    application/vnd.visio
%%%    application/zip
%%%    image/bmp                                                       (rfc7903)
%%%    image/jpeg                                                      (rfc2046)
%%%    image/png                                    (https://www.w3.org/TR/png/)
%%%    image/vnd.microsoft.icon
%%%    image/webp                                                      (rfc9649)
%%%    text/calendar                                                   (rfc5545)
%%%
%%%  The following types requires deeper analysis possibly including
%%%  uncompressing the file and are only determined when the {deep, true} option
%%%  is provided, if not the encapsulating type, e.g., application/zip.
%%%
%%%    application/atom+xml                                            (rfc4287)
%%%    application/epub+zip            (https://www.w3.org/TR/epub-overview-33/)
%%%    application/vnd.ms-visio.drawing.main+xml
%%%    application/vnd.openxmlformats-officedocument.wordprocessingml.document
%%%    application/
%%%      vnd.openxmlformats-officedocument.presentationml.presentation
%%%    application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
%%%
%%%    So far only a few types are supported but more will be added.
%%%
%%% @end
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_magic).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([check/2, check/3,
         media_type/1, media_type/2]).

%% Exported types
-export_type([media_type/0]).

%% Includes
-include_lib("stdlib/include/zip.hrl").

%% Records
-record(opts, {deep = false  :: boolean()}).

%% Types
-type media_type() :: binary().

-type opts() :: [opt()].
-type opt() :: {atom(), _}.

%% Defines
-define(APPLEFILE_SINGLE_MAGIC, 16#00, 16#05, 16#16, 16#00).
-define(APPLEFILE_DOUBLE_MAGIC, 16#00, 16#05, 16#16, 16#07).
-define(GZIP_MAGIC, 16#1F, 16#8B).
-define(MS_MAGIC, 16#D0, 16#CF, 16#11, 16#E0, 16#A1, 16#B1, 16#1A, 16#E1).
-define(BMP_MAGIC, 16#42, 16#4D).
-define(GIF_MAGIC, "GIF8").
-define(ICO_MAGIC,  16#00, 16#00, 16#01, 16#00).
-define(JPEG_MAGIC, 16#FF, 16#D8, 16#FF).
-define(PNG_MAGIC, 16#89, 16#50, 16#4E, 16#47, 16#0D, 16#0A, 16#1A, 16#0A).
-define(WEBP_MAGIC, "RIFF", _:4/binary, "WEBPVP8").

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: check(MediaType(s), File) ->  Boolean
%% @doc
%%   Equivalent of calling check(MediaType(s) File, [])
%% @end
%%--------------------------------------------------------------------
-spec check(binary() | [binary()], binary()) -> boolean().
%%--------------------------------------------------------------------
check(Types, File) -> check(Types, File, []).

%%--------------------------------------------------------------------
%% Function: check(MediaType(s), File, Options) -> Boolean
%% @doc
%%   The files media type is checked against the type or types provided and
%%   if they are the same or a member true is returned otherwise false.
%%
%%   Options are:
%%     {deep, boolean()} -> does more expensive analysis default false
%%
%% @end
%%--------------------------------------------------------------------
-spec check(binary() | [binary()], binary(), opts()) -> boolean().
%%--------------------------------------------------------------------
check(Types = [_|_], File, Opts) -> lists:member(media_type(File, Opts), Types);
check(Type = <<_/binary>>, File, Opts) -> Type == media_type(File, Opts).

%%--------------------------------------------------------------------
%% Function: media_type(File) ->  MediaType.
%% @doc
%%   Equivalent of calling media_type(File, [])
%% @end
%%--------------------------------------------------------------------
-spec media_type(binary()) -> media_type() | undefined.
%%--------------------------------------------------------------------
media_type(File) -> media_type(File, []).

%%--------------------------------------------------------------------
%% Function: media_type(File, Options) ->  MediaType or undefined
%% @doc
%%   The files media type is returned.
%%
%%   Options are:
%%     {deep, boolean()} -> does more expensive analysis default false
%%
%% @end
%%--------------------------------------------------------------------
-spec media_type(binary(), opts()) -> media_type() | undefined.
%%--------------------------------------------------------------------
media_type(File, Opts) -> gen(File, lists:foldr(fun opt/2, #opts{}, Opts)).


%% ===================================================================
%% Internal functions.
%% ===================================================================

opt({deep, Bool}, Opts) when is_boolean(Bool) -> Opts#opts{deep = Bool}.

gen(<<?APPLEFILE_SINGLE_MAGIC, _/binary>>, _) -> ~"application/applefile";
gen(<<?APPLEFILE_DOUBLE_MAGIC, _/binary>>, _) -> ~"application/applefile";
gen(<<?GZIP_MAGIC, _/binary>>, _) -> ~"application/gzip";
gen(<<?MS_MAGIC, T/binary>>, _) -> ms(T);
gen(<<"%PDF-", _/binary>>, _) -> ~"application/pdf";
gen(<<"%!PS", _/binary>>, _) -> ~"application/postscript";
gen(<<"{\rtf", _/binary>>, _) -> ~"application/rtf";
gen(<<"SQLite format 3", _/binary>>, _) -> ~"application/sqlite3";
gen(ZIP = <<"PK", 3, 4, _/binary>>, #opts{deep = true}) -> zip(ZIP);
gen(<<"PK", 3, 4, _/binary>>, #opts{deep = false}) -> ~"application/zip";
gen(XML = <<"<?xml", _/binary>>, #opts{deep = true}) -> xml(XML);
gen(<<"<?xml", _/binary>>, #opts{deep = false}) -> ~"application/xml";
gen(<<?BMP_MAGIC, _/binary>>, _) -> ~"image/bmp";
gen(<<?GIF_MAGIC, _/binary>>, _) -> ~"image/gif";
gen(<<?ICO_MAGIC, _/binary>>, _) -> ~"image/vnd.microsoft.icon";
gen(<<?JPEG_MAGIC, _/binary>>, _) -> ~"image/jpeg";
gen(<<?PNG_MAGIC, _/binary>>, _) -> ~"image/png";
gen(<<?WEBP_MAGIC, _/binary>>, _) -> ~"image/webp";
gen(<<"BEGIN:VCALENDAR", _/binary>>, _) -> ~"text/calendar";
gen(_, _) -> undefined.


xml(XML) ->
    case xml_type(XML) of
        none -> ~"application/xml";
        ~"http://www.w3.org/2005/Atom" -> ~"application/atom+xml"
    end.

xml_type(~"") -> none;
xml_type(<<"xmlns", XML/binary>>) -> xmlns(XML, ~"=\"");
xml_type(<<_, T/binary>>) -> xml_type(T).

xmlns(<<T/binary>>, ~"") -> xmlns1(T, ~"");
xmlns(<<H, T/binary>>, <<H, T1/binary>>) -> xmlns(T, T1);
xmlns(<<_, T/binary>>, Left) -> xmlns(T, Left).

xmlns1(<<$\", _/binary>>, Acc) -> Acc;
xmlns1(<<H, T/binary>>, Acc) ->  xmlns1(T, <<Acc/binary, H>>).

ms(~"") -> undefined;
ms(<<"Excel", _/binary>>) -> ~"application/vnd.ms-excel";
ms(<<"PowerPoint", _/binary>>) -> ~"application/vnd.ms-powerpoint";
ms(<<"Visio", _/binary>>) -> ~"application/vnd.visio";
ms(<<"Word", _/binary>>) -> ~"application/msword";
ms(<<_, T/binary>>) ->  ms(T).

zip(Z) ->
    {ok, Files} = zip:list_dir(Z),
    Names = [Name || #zip_file{name = Name} <- Files],
    case zip_category(Names) of
        zip -> ~"application/zip";
        msooxml -> msooxml([hd(string:tokens(N, "/"))  || N <- Names]);
        epub -> epub(Z)
    end.

zip_category([]) -> zip;
zip_category(["[Content_Types].xml" | _]) -> msooxml;
zip_category(["mimetype" | _]) -> epub;
zip_category(["Metadata" | _]) -> apple;
zip_category([_ | T]) -> zip_category(T).

msooxml([]) -> ~"application/zip";
msooxml(["word" | _]) ->
    ~"application/vnd.openxmlformats-officedocument.wordprocessingml.document";
msooxml(["ppt" | _]) ->
    <<"application/"
      "vnd.openxmlformats-officedocument.presentationml.presentation">>;
msooxml(["xl" | _]) ->
    ~"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
msooxml(["visio" | _]) ->
    ~"application/vnd.ms-visio.drawing.main+xml";
msooxml([_ | T]) ->
    msooxml(T).

epub(Z) ->
    {ok, H} = zip:zip_open(Z, [memory]),
    {ok, {_, MimeType}} = zip:zip_get("mimetype", H),
    zip:zip_close(H),
    case MimeType of
        <<"application/epub+zip", _/binary>> -> ~"application/epub+zip";
        _ -> ~"application/zip"
    end.
