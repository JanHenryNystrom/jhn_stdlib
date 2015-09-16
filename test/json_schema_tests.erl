%%==============================================================================
%% Copyright 2015 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the json library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2015, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(json_schema_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% JSON Schema validation
%% ===================================================================

%%--------------------------------------------------------------------
%% schema.json
%%--------------------------------------------------------------------
json_schema_test_() ->
    {ok, Schema} =
        file:read_file(
          filename:join([code:priv_dir(jhn_stdlib), 'draft-04', 'schema.json'])),
    [{"schema.json", ?_test(?assertMatch({true, _}, json:validate(Schema)))}].

all_schema_test_() ->
    Dir = filename:join([code:lib_dir(jhn_stdlib, deps),
                         'JSON-Schema-Test-Suite',
                         'tests',
                         'draft4']),
    {ok, Files} = file:list_dir(Dir),
    Files1 = [File || File <- Files, filename:extension(File) == ".json"],
    [{File,
      [?_test(?assertMatch({true, _}, json:validate(Schema))) ||
          Schema <-
              [plist:find(<<"schema">>, Suite) ||
                  {Suite} <-
                      json:decode(
                        element(2,
                                file:read_file(filename:join(Dir, File))))]]} ||
        File <- Files1].

%%--------------------------------------------------------------------
%% additionalItems
%%--------------------------------------------------------------------

additionalItems_test_() -> suites(additionalItems).

%%--------------------------------------------------------------------
%% additionalProperties
%%--------------------------------------------------------------------

additionalProperties_test_() -> suites(additionalProperties).

%%--------------------------------------------------------------------
%% allOf
%%--------------------------------------------------------------------

allOf_test_() -> suites(allOf).

%%--------------------------------------------------------------------
%% anyOf
%%--------------------------------------------------------------------

anyOf_test_() -> suites(anyOf).

%%--------------------------------------------------------------------
%% default
%%--------------------------------------------------------------------

default_test_() -> suites(default).

%%--------------------------------------------------------------------
%% definitions
%%--------------------------------------------------------------------

definitions_test_() -> suites(definitions).

%%--------------------------------------------------------------------
%% dependencies
%%--------------------------------------------------------------------

dependencies_test_() -> suites(dependencies).

%%--------------------------------------------------------------------
%% enum
%%--------------------------------------------------------------------

enum_test_() -> suites(enum).

%%--------------------------------------------------------------------
%% items
%%--------------------------------------------------------------------

items_test_() -> suites(items).

%%--------------------------------------------------------------------
%% maxItems
%%--------------------------------------------------------------------

maxItems_test_() -> suites(maxItems).

%%--------------------------------------------------------------------
%% maxLength
%%--------------------------------------------------------------------

maxLength_test_() -> suites(maxLength).

%%--------------------------------------------------------------------
%% maxProperties
%%--------------------------------------------------------------------

maxProperties_test_() -> suites(maxProperties).

%%--------------------------------------------------------------------
%% maximum
%%--------------------------------------------------------------------

maximum_test_() -> suites(maximum).

%%--------------------------------------------------------------------
%% minItems
%%--------------------------------------------------------------------

minItems_test_() -> suites(minItems).

%%--------------------------------------------------------------------
%% minLength
%%--------------------------------------------------------------------

minLength_test_() -> suites(minLength).

%%--------------------------------------------------------------------
%% minProperties
%%--------------------------------------------------------------------

minProperties_test_() -> suites(minProperties).

%%--------------------------------------------------------------------
%% minimum
%%--------------------------------------------------------------------

minimum_test_() -> suites(minimum).

%%--------------------------------------------------------------------
%% multipleOf
%%--------------------------------------------------------------------

multipleOf_test_() -> suites(multipleOf).

%%--------------------------------------------------------------------
%% not
%%--------------------------------------------------------------------

not_test_() -> suites('not').

%%--------------------------------------------------------------------
%% oneOf
%%--------------------------------------------------------------------

oneOf_test_() -> suites(oneOf).

%%--------------------------------------------------------------------
%% pattern
%%--------------------------------------------------------------------

pattern_test_() -> suites(pattern).

%%--------------------------------------------------------------------
%% patternProperties
%%--------------------------------------------------------------------

patternProperties_test_() -> suites(patternProperties).

%%--------------------------------------------------------------------
%% properties
%%--------------------------------------------------------------------

properties_test_() -> suites(properties).

%%--------------------------------------------------------------------
%% ref
%%--------------------------------------------------------------------

ref_test_() -> suites(ref).

%%--------------------------------------------------------------------
%% refRemote
%%--------------------------------------------------------------------

refRemote_test_() ->
    Base = filename:join([code:lib_dir(jhn_stdlib, deps),
                          'JSON-Schema-Test-Suite',
                          'remotes']),
    Opts = #{base => Base},
    suites(refRemote, [{resolver, fun json:resolve_local_file/2, Opts}]).

%%--------------------------------------------------------------------
%% required
%%--------------------------------------------------------------------

required_test_() -> suites(required).

%%--------------------------------------------------------------------
%% type
%%--------------------------------------------------------------------

type_test_() -> suites(type).

%%--------------------------------------------------------------------
%% uniqueItems
%%--------------------------------------------------------------------

uniqueItems_test_() -> suites(uniqueItems).
    

%% ===================================================================
%% Internal functions.
%% ===================================================================

suites(Name) -> suites(Name, []).

suites(Name, Extra) ->
    [[gen_suite(Suite, Extra ++ Opts) || Suite <- load(Name, Extra ++ Opts)] ||
        Opts <- [
                 []%% ,
                 %% [atom_keys],
                 %% [{plain_string, utf8}],
                 %% [{plain_string, {utf16, big}}],
                 %% [{plain_string, {utf16, little}}],
                 %% [{plain_string, {utf32, big}}],
                 %% [{plain_string, {utf32, little}}],
                 %% [atom_keys, {plain_string, {utf16, big}}],
                 %% [atom_keys, {plain_string, {utf32, big}}]
                ]
    ].

gen_suite(Suite, Opts) ->
    Description = find(description, Suite, Opts),
    Schema = find(schema, Suite, Opts),
    Tests = find(tests, Suite, Opts),
    {Description, [gen_test(Schema, Test, Opts)|| Test <- Tests]}.

gen_test(Schema, Test, Opts) ->
    TestDescription = find(description, Test, Opts),
    Data = find(data, Test, Opts),
    case find(valid, Test, Opts) of
        true ->
            {TestDescription,
             ?_test(?assertEqual(true,
                                 json:validate(Schema, Data, Opts)))};
        false ->
            {TestDescription,
             ?_test(?assertEqual(false,
                                 json:validate(Schema, Data, Opts)))}
    end.

load(TestSuites, Opts) ->
    {ok, Bin} = file:read_file(file(TestSuites)),
    json:decode(Bin, Opts).

file(TestSuites) ->
    filename:join([code:lib_dir(jhn_stdlib, deps),
                   'JSON-Schema-Test-Suite',
                   'tests',
                   'draft4',
                   atom_to_list(TestSuites) ++ ".json"
                  ]).

find(Key, {Object}, Opts) ->
    Atom = lists:member(atom_keys,Opts) or lists:member(existing_atom_keys,Opts),
    case {Atom, plist:find(plain_string, Opts, utf8)} of
        {true, _} -> plist:find(Key, Object);
        {_, Plain} ->
            Key1 = unicode:characters_to_binary(atom_to_binary(Key, utf8),
                                                utf8,
                                                Plain),
            plist:find(Key1, Object)
    end.
