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

additionalItems_test_() -> [gen_suite(Suite) || Suite <- load(additionalItems)].

%%--------------------------------------------------------------------
%% additionalProperties
%%--------------------------------------------------------------------

additionalProperties_test_() ->
    [gen_suite(Suite) || Suite <- load(additionalProperties)].

%%--------------------------------------------------------------------
%% allOf
%%--------------------------------------------------------------------

allOf_test_() -> [gen_suite(Suite) || Suite <- load(allOf)].

%%--------------------------------------------------------------------
%% anyOf
%%--------------------------------------------------------------------

anyOf_test_() -> [gen_suite(Suite) || Suite <- load(anyOf)].

%%--------------------------------------------------------------------
%% default
%%--------------------------------------------------------------------

default_test_() -> [gen_suite(Suite) || Suite <- load(default)].

%%--------------------------------------------------------------------
%% definitions
%%--------------------------------------------------------------------

%% definitions_test_() ->
%%     [gen_suite(Suite) || Suite <- load(definitions)].

%%--------------------------------------------------------------------
%% dependencies
%%--------------------------------------------------------------------

dependencies_test_() -> [gen_suite(Suite) || Suite <- load(dependencies)].

%%--------------------------------------------------------------------
%% enum
%%--------------------------------------------------------------------

%% enum_test_() ->
%%     [gen_suite(Suite) || Suite <- load(enum)].

%%--------------------------------------------------------------------
%% items
%%--------------------------------------------------------------------

items_test_() -> [gen_suite(Suite) || Suite <- load(items)].

%%--------------------------------------------------------------------
%% maxItems
%%--------------------------------------------------------------------

maxItems_test_() -> [gen_suite(Suite) || Suite <- load(maxItems)].

%%--------------------------------------------------------------------
%% maxLength
%%--------------------------------------------------------------------

maxLength_test_() -> [gen_suite(Suite) || Suite <- load(maxLength)].

%%--------------------------------------------------------------------
%% maxProperties
%%--------------------------------------------------------------------

maxProperties_test_() -> [gen_suite(Suite) || Suite <- load(maxProperties)].

%%--------------------------------------------------------------------
%% maximum
%%--------------------------------------------------------------------

maximum_test_() -> [gen_suite(Suite) || Suite <- load(maximum)].

%%--------------------------------------------------------------------
%% minItems
%%--------------------------------------------------------------------

minItems_test_() -> [gen_suite(Suite) || Suite <- load(minItems)].

%%--------------------------------------------------------------------
%% minLength
%%--------------------------------------------------------------------

minLength_test_() -> [gen_suite(Suite) || Suite <- load(minLength)].

%%--------------------------------------------------------------------
%% minProperties
%%--------------------------------------------------------------------

minProperties_test_() -> [gen_suite(Suite) || Suite <- load(minProperties)].

%%--------------------------------------------------------------------
%% minimum
%%--------------------------------------------------------------------

minimum_test_() -> [gen_suite(Suite) || Suite <- load(minimum)].

%%--------------------------------------------------------------------
%% multipleOf
%%--------------------------------------------------------------------

multipleOf_test_() -> [gen_suite(Suite) || Suite <- load(multipleOf)].

%%--------------------------------------------------------------------
%% not
%%--------------------------------------------------------------------

not_test_() -> [gen_suite(Suite) || Suite <- load('not')].

%%--------------------------------------------------------------------
%% oneOf
%%--------------------------------------------------------------------

%% oneOf_test_() ->
%%     [gen_suite(Suite) || Suite <- load(oneOf)].

%%--------------------------------------------------------------------
%% pattern
%%--------------------------------------------------------------------

pattern_test_() -> [gen_suite(Suite) || Suite <- load(pattern)].

%%--------------------------------------------------------------------
%% patternProperties
%%--------------------------------------------------------------------

patternProperties_test_() ->
    [gen_suite(Suite) || Suite <- load(patternProperties)].

%%--------------------------------------------------------------------
%% properties
%%--------------------------------------------------------------------

properties_test_() -> [gen_suite(Suite) || Suite <- load(properties)].

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

required_test_() -> [gen_suite(Suite) || Suite <- load(required)].

%%--------------------------------------------------------------------
%% type
%%--------------------------------------------------------------------

type_test_() -> [gen_suite(Suite) || Suite <- load(type)].

%%--------------------------------------------------------------------
%% uniqueItems
%%--------------------------------------------------------------------

uniqueItems_test_() -> [gen_suite(Suite) || Suite <- load(uniqueItems)].

%% ===================================================================
%% Internal functions.
%% ===================================================================

gen_suite({Suite}) ->        
    Description = plist:find(description, Suite),
    Schema = plist:find(schema, Suite),
    Tests = plist:find(tests, Suite),
    [gen_test(Description, Schema, Test) || Test <- Tests].

gen_test(Description, Schema, {Test}) ->
    TestDescription = plist:find(description, Test),
    Data = plist:find(data, Test),
    case plist:find(valid, Test) of
        true ->
            {iolist_to_binary([Description, " : ", TestDescription]),
             begin
                 %% ?debugFmt("~n~n~n~njson:validate(~w, ~w)~n~n~n~n",
                 %%           [Schema, Data]),
                 ?_test(?assertEqual(true,
                                     json:validate(Schema, Data, [atom_keys])))
             end};
        false ->
            {iolist_to_binary([Description, " : ", TestDescription]),
             ?_test(?assertEqual(false,
                                 json:validate(Schema, Data, [atom_keys])))}
    end.

load(TestSuites) ->
    {ok, Bin} = file:read_file(file(TestSuites)),
    json:decode(Bin, [atom_keys]).

file(TestSuites) ->
    filename:join([code:lib_dir(jhn_stdlib, deps),
                   'JSON-Schema-Test-Suite',
                   'tests',
                   'draft4',
                   atom_to_list(TestSuites) ++ ".json"
                  ]).
    
