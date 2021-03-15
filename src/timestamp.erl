%%==============================================================================
%% Copyright 2017-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A timestamp library based on:
%%%    Date and Time on the Internet: Timestamps                       (rfc3339)
%%%
%%%  The timestamp is represented as follows:
%%%
%%%  posix  : integer()
%%%
%%%  stamp  : #{year => integer(),
%%%             month => integer(),
%%%             day => integer(),
%%%             hour => integer(),
%%%             minute => integer(),
%%%             second => integer(),
%%%             fraction => integer(),
%%%             offset => Offset
%%%            }
%%%  Offset : 'Z' | #{sign => '+' | '-',
%%%                   hours => integer(),
%%%                   minutes => integer()}
%%%
%%%  The fraction and offset parts are optional
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2017-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(timestamp).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([gen/0, gen/1,
         encode/1, encode/2,
         decode/1, decode/2]).

%% Exported types
-export_type([posix/0, stamp/0]).

%% Records
-record(opts, {precision = seconds :: seconds | milli | micro,
               continue = false :: boolean(),
               return_type = iolist :: iolist | binary | list | posix}).

%% Types
-type posix() :: integer().
-type stamp() :: #{}.


-type opt() :: [seconds | milli | micro | iolist | binary | list | posix].

%% Defines
-define(SECONDS_PER_MINUTE, 60).
-define(SECONDS_PER_HOUR, 3600).
-define(SECONDS_PER_DAY, 86400).
-define(DAYS_PER_YEAR, 365).
-define(DAYS_PER_LEAP_YEAR, 366).
-define(EPOCH, 62167219200).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: gen() -> Encoded.
%% @doc
%%   Generates the timestamp as an iolist of precision seconds
%%   Equivalent of gen([])
%% @end
%%--------------------------------------------------------------------
-spec gen() -> iolist().
%%--------------------------------------------------------------------
gen() -> gen([]).

%%--------------------------------------------------------------------
%% Function: gen(Options) -> Encoded
%% @doc
%%   Generates a timestamp and encode as an iolist, list, binary,
%%   or integer posix timestamp.
%%   Options are:
%%     seconds (default) -> second precision
%%     milli -> milli second precision
%%     micro -> micro second precision
%%     binary -> a binary is returned
%%     list -> a flat list is returned
%%     iolist (default) -> an iolist is returned
%%     posix -> a posix integer timestamp is returned
%% @end
%%--------------------------------------------------------------------
-spec gen([opt()] | #opts{}) -> list()| iolist() | binary() | posix().
%%--------------------------------------------------------------------
gen(#opts{precision = Precision, return_type = posix}) ->
    os:system_time(precision(Precision));
gen(Opts = #opts{precision = Precision}) ->
    encode(os:system_time(precision(Precision)), Opts);
gen(Opts) ->
    gen(parse_opts(Opts, #opts{})).

%%--------------------------------------------------------------------
%% Function: encode(Stamp) -> IOList.
%% @doc
%%   Encodes the map as an iolist.
%%   Equivalent of encode(Stamp, [])
%% @end
%%--------------------------------------------------------------------
-spec encode(stamp() | posix()) -> iolist().
%%--------------------------------------------------------------------
encode(Stamp) -> encode(Stamp, #opts{}).

%%--------------------------------------------------------------------
%% Function: encode(Stamp, Options) -> Encoded
%% @doc
%%   Encodes the Stamp as an iolist or binary.
%%   Encode will give an exception if the Stamp is not well formed.
%%   Options are:
%%     seconds (default) -> second precision (no fraction generated)
%%     milli -> milli second precision
%%     micro -> micro second precision
%%     binary -> a binary is returned
%%     list -> a flat list is returned
%%     iolist -> an iolist is returned
%%     posix -> a posix integer timestamp is returned
%% @end
%%--------------------------------------------------------------------
-spec encode(stamp() | posix(), [opt()] | #opts{}) ->
                    iolist() | binary() | list() | posix().
%%--------------------------------------------------------------------
encode(Stamp, ParsedOpts = #opts{}) ->
    case ParsedOpts#opts.return_type of
        iolist -> do_encode(Stamp, ParsedOpts);
        posix -> do_encode(Stamp, ParsedOpts);
        binary -> iolist_to_binary(do_encode(Stamp, ParsedOpts));
        list -> binary_to_list(iolist_to_binary(do_encode(Stamp, ParsedOpts)))
    end;
encode(Stamp, Opts) ->
    encode(Stamp, parse_opts(Opts, #opts{})).

%%--------------------------------------------------------------------
%% Function: decode(Binary | Posix) -> Stamp
%% @doc
%%   Decodes the binary or posix integer timestamp into a map representing
%%   a timestamp.
%%   Equivalent of decode(Binary, [])
%% @end
%%--------------------------------------------------------------------
-spec decode(binary() | posix()) -> stamp().
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, #opts{}).

%%--------------------------------------------------------------------
%% Function: decode(Binary | Posix, Options) -> Stamp
%% @doc
%%   Decodes the binary or posix integer timestamp into a map representing
%%   a timestamp.
%%   Decode will give an exception if the binary is not well formed timestamp.
%%   Options are:
%%     seconds (default) -> second precision
%%     milli -> milli second precision
%%     micro -> micro second precision
%%     continue -> all remaining indata is returned when decoding a binary
%% @end
%%--------------------------------------------------------------------
-spec decode(binary() | posix(), [opt()] | #opts{}) -> stamp().
%%--------------------------------------------------------------------
decode(Binary, Opts = #opts{}) -> do_decode(Binary, Opts);
decode(Binary, Opts) -> do_decode(Binary, parse_opts(Opts, #opts{})).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Generate
%% ===================================================================

precision(seconds) -> seconds;
precision(milli) -> milli_seconds;
precision(micro) -> micro_seconds.

%% ===================================================================
%% Encoding
%% ===================================================================

do_encode(Map, #opts{return_type = posix, precision = P}) ->
    #{year := Year, month := Month, day := Day,
      hour := Hour, minute := Minute, second := Second} = Map,
    'Z' = maps:get(offset, Map, 'Z'),
    Fraction = maps:get(fraction, Map, 0),
    Seconds =
        ?SECONDS_PER_DAY * days(Year, Month, Day) +
        Hour * ?SECONDS_PER_HOUR +
        Minute * ?SECONDS_PER_MINUTE +
        Second - ?EPOCH,
    case P of
        seconds -> Seconds;
        milli -> Seconds * 1000 + Fraction;
        micro -> Seconds * 1000000 + Fraction
    end;
do_encode(Map = #{}, #opts{precision = Precision}) ->
    #{year := Year, month := Month, day := Day,
      hour := Hour, minute := Minute, second := Second} = Map,
    Fraction = case {Precision, maps:get(fraction, Map, 0)} of
                   {seconds, _} -> "";
                   {milli, Fraction0} -> [$., pad(Fraction0, 3)];
                   {micro, Fraction0} -> [$., pad(Fraction0, 6)]
               end,
    Offset = case maps:get(offset, Map, 'Z') of
                 'Z' -> "Z";
                 #{sign := '+', hours := 0, minutes := 0} -> "Z";
                 #{sign := '+', hours := Hours, minutes := Minutes} ->
                     [$+, pad(Hours, 2), $:, pad(Minutes, 2)];
                 #{sign := '-', hours := Hours, minutes := Minutes} ->
                     [$-, pad(Hours, 2), $:, pad(Minutes, 2)]
             end,
    [pad(Year, 4), $-, pad(Month, 2), $-, pad(Day, 2), $T,
     pad(Hour, 2), $:, pad(Minute, 2), $:, pad(Second, 2),
     Fraction, Offset];
do_encode(Seconds, Opts = #opts{precision = seconds}) ->
    do_encode(decode_posix(Seconds, 0), Opts);
do_encode(Milli, Opts = #opts{precision = milli}) ->
    do_encode(decode_posix(Milli div 1000, Milli rem 1000), Opts);
do_encode(Micro, Opts = #opts{precision = micro}) ->
    do_encode(decode_posix(Micro div 1000000, Micro rem 1000000), Opts).

days(Year, Month, Day) ->
    year1(Year) + month_days(Month) + leap(Year, Month) + Day - 1.

month_days(1) -> 0;
month_days(2) -> 31;
month_days(3) -> 59;
month_days(4) -> 90;
month_days(5) -> 120;
month_days(6) -> 151;
month_days(7) -> 181;
month_days(8) -> 212;
month_days(9) -> 243;
month_days(10) -> 273;
month_days(11) -> 304;
month_days(12) -> 334.

leap(_, 1) -> 0;
leap(_, 2) -> 0;
leap(Year, _) when Year rem 4 =:= 0, Year rem 100 > 0; Year rem 400 =:= 0 -> 1;
leap(_, _) -> 0.

pad(Integer, Size) ->
    List = integer_to_list(Integer),
    [lists:duplicate(Size - length(List), $0), List].

%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(B, #opts{continue = Continue}) when is_binary(B) ->
    {Year, B1} = decode_digit(4, B, $-, []),
    {Month, B2} = decode_digit(2, B1, $-, []),
    {Day, B3} = decode_digit(2, B2, [$T, $t], []),
    {Hour, B4} = decode_digit(2, B3, $:, []),
    {Minute, B5} = decode_digit(2, B4, $:, []),
    {Second, B6} = decode_digit(2, B5, [], []),
    {Fraction, Offset, B7} = decode_fraction(B6),
    Stamp = #{year => Year, month => Month, day => Day,
              hour => Hour, minute => Minute, second => Second,
              fraction => Fraction, offset => Offset},
    case Continue of
        true -> {Stamp, B7};
        false -> Stamp
    end;
do_decode(Seconds, #opts{precision = seconds}) ->
    decode_posix(Seconds, 0);
do_decode(Milli, #opts{precision = milli}) ->
    decode_posix(Milli div 1000, Milli rem 1000);
do_decode(Micro, #opts{precision = micro}) ->
    decode_posix(Micro div 1000000, Micro rem 1000000).

decode_digit(0, Binary, Skip, Acc) ->
    {list_to_integer(lists:reverse(Acc)), skip(Skip, Binary)};
decode_digit(N, <<H, T/binary>>, Skip, Acc) ->
    decode_digit(N - 1, T, Skip, [H | Acc]).

skip([], Binary) -> Binary;
skip([H, _], <<H, T/binary>>) -> T;
skip([_, H], <<H, T/binary>>) -> T;
skip(H, <<H, T/binary>>) -> T.

decode_fraction(<<"Z", T/binary>>) -> {0, 'Z', T};
decode_fraction(<<"z", T/binary>>) -> {0, 'Z', T};
decode_fraction(<<"+", T/binary>>) -> decode_offset(T, 0, '+');
decode_fraction(<<"-", T/binary>>) -> decode_offset(T, 0, '-');
decode_fraction(<<".", T/binary>>) -> decode_fraction(T, []).

decode_fraction(<<"Z", T/binary>>, Acc) ->
    {list_to_integer(lists:reverse(Acc)), 'Z', T};
decode_fraction(<<"z", T/binary>>, Acc) ->
    {list_to_integer(lists:reverse(Acc)), 'Z', T};
decode_fraction(<<"+", T/binary>>, Acc) ->
    decode_offset(T, list_to_integer(lists:reverse(Acc)), '+');
decode_fraction(<<"-", T/binary>>, Acc) ->
    decode_offset(T, list_to_integer(lists:reverse(Acc)), '-');
decode_fraction(<<H, T/binary>>, Acc) -> decode_fraction(T, [H | Acc]).

decode_offset(B, Fraction, Sign) ->
    {Hours, B1} = decode_digit(2, B, $:, []),
    {Minutes, B2} = decode_digit(2, B1, [], []),
    case {Sign, Hours, Minutes} of
        {'+', 0, 0} -> {Fraction, 'Z', B2};
        _ ->
            {Fraction, #{sign => Sign, hours => Hours, minutes => Minutes}, B2}
    end.

decode_posix(Seconds, Fraction) ->
    Secs = Seconds + ?EPOCH,
    Days = Secs div ?SECONDS_PER_DAY,
    Y0 = Days div ?DAYS_PER_YEAR,
    {Y, {M, D}} =
        case year(Y0, Days, year1(Y0)) of
            {Y1, D1} when  Y1 rem 4 =:= 0, Y1 rem 100 > 0; Y1 rem 400 =:= 0 ->
                {Y1, leap_date(Days - D1)};
            {Y1, D1} ->
                {Y1, date(Days - D1)}
        end,
    Rest = Secs rem ?SECONDS_PER_DAY,
    H = Rest div ?SECONDS_PER_HOUR,
    Rest1 = Rest rem ?SECONDS_PER_HOUR,
    Mi =  Rest1 div ?SECONDS_PER_MINUTE,
    S =  Rest1 rem ?SECONDS_PER_MINUTE,
    #{year => Y, month => M, day => D,
      hour => H, minute => Mi, second => S,
      fraction => Fraction}.

year(Y, D1, D2) when D1 < D2 -> year(Y - 1, D1, year1(Y - 1));
year(Y, _, D2) -> {Y, D2}.

year1(Y) when Y =< 0 -> 0;
year1(Y) ->
    X = Y - 1,
    X div 4 - X div 100 + X div 400 + X * ?DAYS_PER_YEAR + ?DAYS_PER_LEAP_YEAR.

date(Day) when Day < 31 -> {1, Day + 1};
date(Day) when Day < 59 -> {2, Day - 30};
date(Day) when Day < 90 -> {3, Day - 58};
date(Day) when Day < 120 -> {4, Day - 89};
date(Day) when Day < 151 -> {5, Day - 119};
date(Day) when Day < 181 -> {6, Day - 150};
date(Day) when Day < 212 -> {7, Day - 180};
date(Day) when Day < 243 -> {8, Day - 211};
date(Day) when Day < 273 -> {9, Day - 242};
date(Day) when Day < 304 -> {10,Day - 272};
date(Day) when Day < 334 -> {11,Day - 303};
date(Day) -> {12, Day - 333}.

leap_date(Day) when Day < 31 -> {1, Day + 1};
leap_date(Day) when Day < 60 -> {2, Day - 30};
leap_date(Day) when Day < 91 -> {3, Day - 59};
leap_date(Day) when Day < 121 -> {4, Day - 90};
leap_date(Day) when Day < 152 -> {5, Day - 120};
leap_date(Day) when Day < 182 -> {6, Day - 151};
leap_date(Day) when Day < 213 -> {7, Day - 181};
leap_date(Day) when Day < 244 -> {8, Day - 212};
leap_date(Day) when Day < 274 -> {9, Day - 243};
leap_date(Day) when Day < 305 -> {10,Day - 273};
leap_date(Day) when Day < 335 -> {11,Day - 304};
leap_date(Day) -> {12, Day - 334}.

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(continue, Opts) -> Opts#opts{continue = true};
parse_opt(seconds, Opts) -> Opts#opts{precision = seconds};
parse_opt(milli, Opts) -> Opts#opts{precision = milli};
parse_opt(micro, Opts) -> Opts#opts{precision = micro};
parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(list, Opts) -> Opts#opts{return_type = list};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(posix, Opts) -> Opts#opts{return_type = posix};
parse_opt(_, _) -> erlang:error(badarg).
