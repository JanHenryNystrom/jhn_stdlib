%%==============================================================================
%% Copyright 2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the typeid library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_typeid_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines
-define(PREFIXES,
        ['', <<>>, "", a, <<"a">>, "a", a_test, a_test_with_several,
         lists:duplicate(63, $a)]).

-define(INVALID_PREFIXES,
        ["_", "_foo", "foo_", "dfkdf10", lists:duplicate(64, $a)]).

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% Generate
%%--------------------------------------------------------------------

gen_0_test_() ->
    [{"gen()", ?_test(?assertEqual(true, is_typeid(jhn_typeid:gen())))}].

gen_1_test_() ->
    [{"gen/1",
      ?_test(?assertEqual(true,
                          is_typeid(iolist_to_binary(jhn_typeid:gen(P)))))} ||
        P <- ?PREFIXES
    ].

gen_2_test_() ->
    [{"gen/2 iolist",
      ?_test(
         ?assertEqual(true,
                      is_typeid(iolist_to_binary(
                                  jhn_typeid:gen(P,
                                                 [iolist])))))} ||
        P <- ?PREFIXES
    ]  ++
    [{"gen/2 list",
      ?_test(
         ?assertEqual(true,
                      is_typeid(iolist_to_binary(
                                  jhn_typeid:gen(P, [list])))))} ||
        P <- ?PREFIXES
    ]  ++
    [{"gen/2 binary",
      ?_test( ?assertEqual(true, is_typeid(jhn_typeid:gen(P, [binary]))))} ||
        P <- ?PREFIXES
    ] ++
    [{"gen/2 typeid",
      ?_test( ?assertEqual(true, is_typeid(jhn_typeid:gen(P, [typeid]))))} ||
        P <- ?PREFIXES
    ].

%%--------------------------------------------------------------------
%% Encode
%%--------------------------------------------------------------------
encode_1_test_() ->
    [{"encode/1",
      ?_test(?assertEqual(true,
                          is_typeid(iolist_to_binary(
                                      jhn_typeid:encode(
                                        jhn_typeid:gen(P, [typeid]))))))} ||
        P <- ?PREFIXES
    ].

encode_2_test_() ->
    [{"encode/2",
      ?_test(?assertEqual(true, is_typeid(jhn_typeid:encode(T, [typeid]))))} ||
        T <- [#{prefix => <<"a">>, uuid => jhn_uuid:gen(v7, [binary])},
              #{prefix => <<"a">>, uuid => jhn_uuid:gen(v7, [uuid])}]
    ].

spec_encode_1_test_() ->
    [{"spec: " ++ binary_to_list(N),
      ?_test(?assertEqual(T,
                          iolist_to_binary(
                            jhn_typeid:encode(#{prefix => P,
                                                uuid => U}))))} ||
        #{<<"name">> := N,
          <<"typeid">> := T,
          <<"prefix">> := P,
          <<"uuid">> := U} <- spec(valid)
    ].

%%--------------------------------------------------------------------
%% Decode
%%--------------------------------------------------------------------

spec_decode_1_test_() ->
    [{"spec invalid: " ++ binary_to_list(N),
      ?_test(?assertError(_, jhn_typeid:decode(T, [uuid])))} ||
        #{<<"name">> := N, <<"typeid">> := T} <- spec(invalid)
    ].

decode_2_test_() ->
    T = jhn_typeid:gen(a, [binary]),
    [{"decode/2",
      ?_test(?assertEqual(true, is_typeid(jhn_typeid:decode(T, [F]))))} ||
        F <- [hex, uuid, human]
    ].


spec_decode_2_test_() ->
    [{"spec valid: " ++ binary_to_list(N),
      ?_test(?assertEqual(true,
                          match(#{prefix => P, uuid => U},
                                jhn_typeid:decode(T, [uuid]))))} ||
        #{<<"name">> := N,
          <<"typeid">> := T,
          <<"prefix">> := P,
          <<"uuid">> := U} <- spec(valid)
    ].

%%--------------------------------------------------------------------
%% Valid Prefix
%%--------------------------------------------------------------------

spec_is_valid_prefix_1_test_() ->
    [{"is_valid_prefix: true",
      ?_test(?assertEqual(true, jhn_typeid:is_valid_prefix(bin(P))))} ||
        P <- ?PREFIXES
    ] ++ 
    [{"is_valid_prefix: false",
      ?_test(?assertEqual(false, jhn_typeid:is_valid_prefix(bin(P))))} ||
        P <- ?INVALID_PREFIXES
    ].

%%--------------------------------------------------------------------
%% Bad Opts
%%--------------------------------------------------------------------

spec_bad_opts_test_() ->
    UUID = <<"01924bcb-34b1-747f-a63e-18736edd5428">>,
    TypeId = <<"a_01j95wrnjpfn3snq2cy9f3sb47">>,
    [{"Bad decode opt", ?_test(?assertError(_, jhn_typeid:decode(a, [x])))},
     {"Bad encode opt",
      ?_test(
         ?assertError(_,
                      jhn_typeid:encode(#{prefix => a, uuid => UUID}, [x])))},
     {"Bad decode opt",
      ?_test(?assertError(_, jhn_typeid:decode(TypeId, [x])))}
    ].

%%--------------------------------------------------------------------
%% Gen/Encode/Decode/Encode/Decode
%%--------------------------------------------------------------------

%% ===================================================================
%% Internal functions.
%% ===================================================================


is_typeid(#{prefix := P, uuid := U}) ->
    jhn_typeid:is_valid_prefix(P) and is_v7(U);
is_typeid(Id) ->
    is_typeid(jhn_typeid:decode(Id)).

match(#{prefix := P, uuid := U1}, #{prefix := P, uuid := U2}) ->
    jhn_bstring:to_upper(U1) == jhn_bstring:to_upper(U2);
match(_, _) ->
    false.

is_v7(#{version := 7}) -> true;
is_v7(nil) -> true;
is_v7(max) -> true;
is_v7(Binary) ->
    #{version := 7} = jhn_uuid:decode(Binary),
    true.

bin(L) when is_list(L) -> iolist_to_binary(L);
bin(A) when is_atom(A) -> atom_to_binary(A);
bin(B) -> B.

spec(Part) ->
    File = filename:join([code:lib_dir(jhn_stdlib), test, typeid,
                          atom_to_list(Part) ++ ".json"]),
    {ok, Bin} = file:read_file(File),
    jhn_json:decode(Bin).
