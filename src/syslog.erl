%%==============================================================================
%% Copyright 2016 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A Syslog library based on:
%%%    The Syslog Protocol                                             (rfc5424)
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(syslog).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2
        ]).

%% Exported types
-export_type([uri/0]).

%% Includes

%% Records
-record(opts, {return_type = iolist :: iolist | binary}).
-record(offset, {direction = 'Z' :: 'Z' | '-' |'+'}).
-record(time_stamp, {date :: {integer(), integer(), integer()},
                     time :: {integer(), integer(), integer()},
                     fraction = 0 :: integer(),
                     offset = #offset{} :: #offset{}
                    }).
-record(header, {facility :: integer(),
                 severity :: integer(),
                 version = 1 :: integer(),
                 time_stamp = nil :: nil | #time_stamp{},
                 host_name = nil :: nil | binary(),
                 app_name = nil :: nil | binary(),
                 proc_id = nil :: nil | binary(),
                 msg_id = nil :: nil | binary()
                }).

%% Types
-type opt() :: none.


%% Defines

%% Facilities
-define(KERN, 0).
-define(USER, 8)
-define(MAIL, 16).
-define(SYSTEM, 24).
-define(SECURITY1, 32).
-define(INTERNAL, 40).
-define(PRINTER, 48).
-define(NEWS, 56).
-define(UUCP, 64).
-define(CLOCK1, 72).
-define(SECURITY2, 80).
-define(FTP, 88).
-define(NTP, 96).
-define(AUDIT, 104).
-define(ALERT, 112).
-define(CLOCK2, 120).
-define(LOCAL0, 128).
-define(LOCAL1, 136).
-define(LOCAL2, 144).
-define(LOCAL3, 152).
-define(LOCAL4, 160).
-define(LOCAL5, 168).
-define(LOCAL6, 176).
-define(LOCAL7, 184).

%% Severity
-define(Emergency, 0).
-define(ALERT, 1).
-define(CRITICAL, 2).
-define(ERROR, 3).
-define(WARNING, 4).
-define(NOTICE, 5).
-define(INFO, 6).
-define(DEBUG, 7).

%% Time
-define(GREGORIAN_POSIX_DIFF, 62167219200).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode(Term) -> Syslog entry.
%% @doc
%%   Encodes the structured Erlang term as an iolist.
%%   Equivalent of encode(Term, []) -> Syslog encode.
%% @end
%%--------------------------------------------------------------------
-spec encode(_) -> iolist().
%%--------------------------------------------------------------------
encode(Term) -> encode(Term, #opts{}).

%%--------------------------------------------------------------------
%% Function: encode(Term, Options) -> Syslog entry
%% @doc
%%   Encodes the structured Erlang term as an iolist or binary.
%%   Encode will give an exception if the erlang term is not well formed.
%%   Options are:
%%     binary -> a binary is returned
%%     iolist -> a iolist is returned
%% @end
%%--------------------------------------------------------------------
-spec encode(uri(), [opt()] | #opts{}) -> iolist() | binary().
%%--------------------------------------------------------------------
encode(Term, Opts = #opts{}) -> do_encode(Term, Opts);
encode(Term, Opts) ->
    ParsedOpts = parse_opts(Opts),
    case ParsedOpts#opts.return_type of
        iolist-> do_encode(Term, ParsedOpts);
        binary -> iolist_to_binary(do_encode(Term, ParsedOpts))
    end.

%%--------------------------------------------------------------------
%% Function: decode(Binary) -> Term.
%% @doc
%%   Decodes the binary into a structured Erlang term.
%%   Equivalent of decode(Binary, []) -> URI.
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> uri().
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, #opts{}).

%%--------------------------------------------------------------------
%% Function: decode(Binary, Options) -> Term.
%% @doc
%%   Decodes the binary into a structured Erlang.
%%   Decode will give an exception if the binary is not well formed
%%   Syslog entry.
%%   Options are:
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), [opt()] | #opts{}) -> _.
%%--------------------------------------------------------------------
decode(Binary, Opts = #opts{}) -> do_decode(Binary, Opts);
decode(Binary, Opts) -> do_decode(Binary, parse_opts(Opts)).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================


%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(<<$<, T/binary>>, Opts) ->
    Header = decode_header(T, <<>>).

decode_header(<<$>, T/binary>>, Acc) ->
    Priority = binary_to_integer(Acc),
    Facility = Priority div 8,
    Severity = Priority - (Facility * 8),
    decode_version(T, #header{facility = Facility, severity = Severity}, <<>>);
decode_header(<<H, T/binary>>, Acc) ->
    decode_header(T, <<Acc/binary, H>>).

decode_version(<<$\s, T/binary>>, Header, Acc) ->
    decode_timestamp(T, #header{version = binary_to_integer(Acc)});
decode_version(<<H, T/binary>>, Header, Acc) ->
    decode_version(T, Header, <<Acc/binary ,H>>).

decode_timestamp(<<$-, $\s, T\binary>>, Header) ->
    decode_hostname(T, Header#header{time_stamp = nil}, <<>>);
decode_timestamp(<<Y:32, $-, M:2/bytes, $-, D:2/bytes, $T,
                   H:2/bytes, $:, Mi:2/bytes, $:, S:2/bytes,
                   T/binary>>,
                 Header) ->
    Header1 = Header#header{time_stamp =
                                #time_stamp{date = {Y,
                                                    binary_to_integer(M),
                                                    binary_to_integer(D)},
                                            time = {binary_to_integer(H),
                                                    binary_to_integer(Mi),
                                                    binary_to_integer(S)}}},
    decode_timestamp1(T, Header1).

decode_timestamp1(<<$Z, $\s, T/binary>>, Header) -> decode_hostname(T, Header);
decode_timestamp1(<<$., T/binary>>, Header) -> decode_fraction(T, Header, <<>>);
decode_timestamp1(<<$+, T/binary>>, Header = #header{time_stamp = TS}) ->
    decode_offset(T, Header#header{time_stamp = TS#time_stamp{direction='+'}});
decode_timestamp1(<<$-, T/binary>>, Header = #header{time_stamp = TS}) ->
    decode_offset(T, Header#header{time_stamp = TS#time_stamp{direction='-'}}).

decode_fraction(<<$Z, $\s, T/binary>>, Header=#header{time_stamp = TS}) ->
    Header1 =
        Header#header{time_stamp =
                          TS#time_stamp{fraction = normalize_fraction(Acc)}},
    decode_hostname(T, Header1).

decode_offset(<<H:2/bytes, $:, M:2/bytes, $\s, T/binary>>, Header) ->
    #header{time_stamp = TS = #time_stamp{offset = O}} = Header,
    Header1 =
        Header#header{time_stamp = TS#time_stamp{offset = #offset{hour = H,
                                                                  minute = M}}
    decode_hostname(T, Header1).

%% ===================================================================
%% Common parts
%% ===================================================================

parse_opts([], Rec) -> Rec;
parse_opts(Opts, Rec) -> lists:foldl(fun parse_opt/2, Rec, Opts).

parse_opt(binary, Opts) -> Opts#opts{return_type = binary};
parse_opt(iolist, Opts) -> Opts#opts{return_type = iolist};
parse_opt(_, Opts) -> badarg(Opts).
