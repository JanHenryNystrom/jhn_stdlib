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

%% definitions_test_() -> [gen_suite(Suite) || Suite <- load(definitions)].

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

%% ref_test_() ->
%%     [gen_suite(Suite) || Suite <- load(ref)].

%%--------------------------------------------------------------------
%% refRemote
%%--------------------------------------------------------------------

%% refRemote_test_() ->
%%     [gen_suite(Suite) || Suite <- load(refRemote)].

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

suites(Name) ->
    [[gen_suite(Suite, Opts) || Suite <- load(Name, Opts)] ||
        Opts <- [[atom_keys]]].

gen_suite({Suite}, Opts) ->        
    Description = plist:find(description, Suite),
    Schema = plist:find(schema, Suite),
    Tests = plist:find(tests, Suite),
    {Description, [gen_test(Schema, Test, Opts)|| Test <- Tests]}.

gen_test(Schema, {Test}, Opts) ->
    TestDescription = plist:find(description, Test),
    Data = plist:find(data, Test),
    case plist:find(valid, Test) of
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
    
