%%==============================================================================
%% Copyright 2017 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   eunit unit tests for the mustache library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2017, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(mustache_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% render/2
%%--------------------------------------------------------------------
render_2_test_() ->
    [?_test(
        ?assertEqual(<<>>,
                     renderf(<<"{{foo}}">>, []))),
     ?_test(
        ?assertEqual(<<"FOO">>,
                     renderf(<<"{{foo}}">>, [{foo, <<"FOO">>}]))),
     ?_test(
        ?assertEqual(<<"FOO ">>,
                     renderf(<<"{{foo}} ">>, [{foo, <<"FOO">>}]))),
     ?_test(
        ?assertEqual(<<" FOO">>,
                     renderf(<<" {{foo}}">>, [{foo, <<"FOO">>}]))),
     ?_test(
        ?assertEqual(<<" FOO ">>,
                     renderf(<<" {{foo}} ">>, [{foo, <<"FOO">>}]))),
     ?_test(
        ?assertEqual(<<"FOO">>,
                     renderf(<<"{{ foo}}">>, [{foo, <<"FOO">>}]))),
     ?_test(
        ?assertEqual(<<"FOO">>,
                     renderf(<<"{{foo }}">>, [{foo, <<"FOO">>}]))),
     ?_test(
        ?assertEqual(<<"FOOBAR">>,
                     renderf(<<"{{foo}}{{! foo}}BAR">>, [{foo, <<"FOO">>}]))),
     ?_test(
        ?assertEqual(<<"&lt;FOO&gt;">>,
                     renderf(<<"{{foo}}">>, [{foo, <<"<FOO>">>}]))),
     ?_test(
        ?assertEqual(<<"<FOO>">>,
                     renderf(<<"{{&foo}}">>, [{foo, <<"<FOO>">>}]))),
     ?_test(
        ?assertEqual(<<"FOO">>,
                     renderf(<<"{{=!! !!=}}!!foo!!">>, [{foo, <<"FOO">>}]))),
     ?_test(
        ?assertEqual(<<"kalle olle ">>,
                     renderf(<<"{{#person}}{{person}} {{/person}}">>,
                             [{person, [<<"kalle">>, <<"olle">>]}]))),
     ?_test(
        ?assertEqual(<<"kalle olle ">>,
                     renderf(<<"{{#person}}{{name}} {{/person}}">>,
                             [{person, [[{name, <<"kalle">>}],
                                        [{name, <<"olle">>}]]}]))),
     ?_test(
        ?assertEqual(<<"kalle">>,
                     renderf(<<"{{#person}}{{person}}{{/person}}">>,
                             [{person, <<"kalle">>}]))),
     ?_test(
        ?assertEqual(<<>>,
                     renderf(<<"{{#person}}{{person}}{{/person}}">>,
                             [{person, false}]))),
     ?_test(
        ?assertEqual(<<>>,
                     renderf(<<"{{#person}}{{person}}{{/person}}">>,
                             [{person, []}]))),
     ?_test(
        ?assertEqual(<<>>,
                     renderf(<<"{{#person}}{{person}}{{/person}}">>,
                             []))),
     ?_test(
        ?assertEqual(<<"That Olle">>,
                     renderf(<<"{{#person}}{{name}}{{/person}}">>,
                             [{person,
                               fun(T, C) ->
                                       ["That ", mustache:render(T, C)]
                               end},
                              {name, <<"Olle">>}]))),
     ?_test(
        ?assertEqual(<<"User:kalle User:olle ">>,
                     renderf(<<"{{#name}}{{>user}} {{/name}}">>,
                             [{user, <<"User:{{name}}">>},
                              {name, [<<"kalle">>, <<"olle">>]}]))),
     ?_test(
        ?assertEqual(<<>>,
                     renderf(<<"{{foo}}">>, #{}))),
     ?_test(
        ?assertEqual(<<"FOO">>,
                     renderf(<<"{{foo}}">>, #{foo => <<"FOO">>}))),
     ?_test(
        ?assertEqual(<<"FOO ">>,
                     renderf(<<"{{foo}} ">>, #{foo => <<"FOO">>}))),
     ?_test(
        ?assertEqual(<<" FOO">>,
                     renderf(<<" {{foo}}">>, #{foo => <<"FOO">>}))),
     ?_test(
        ?assertEqual(<<" FOO ">>,
                     renderf(<<" {{foo}} ">>, #{foo => <<"FOO">>}))),
     ?_test(
        ?assertEqual(<<"FOO">>,
                     renderf(<<"{{ foo}}">>, #{foo => <<"FOO">>}))),
     ?_test(
        ?assertEqual(<<"FOO">>,
                     renderf(<<"{{foo }}">>, #{foo => <<"FOO">>}))),
     ?_test(
        ?assertEqual(<<"FOOBAR">>,
                     renderf(<<"{{foo}}{{! foo}}BAR">>, #{foo => <<"FOO">>}))),
     ?_test(
        ?assertEqual(<<"&lt;FOO&gt;">>,
                     renderf(<<"{{foo}}">>, #{foo => <<"<FOO>">>}))),
     ?_test(
        ?assertEqual(<<"<FOO>">>,
                     renderf(<<"{{&foo}}">>, #{foo => <<"<FOO>">>}))),
     ?_test(
        ?assertEqual(<<"FOO">>,
                     renderf(<<"{{=!! !!=}}!!foo!!">>, #{foo => <<"FOO">>}))),
     ?_test(
        ?assertEqual(<<"kalle olle ">>,
                     renderf(<<"{{#person}}{{person}} {{/person}}">>,
                             #{person => [<<"kalle">>, <<"olle">>]}))),
     ?_test(
        ?assertEqual(<<"kalle olle ">>,
                     renderf(<<"{{#person}}{{name}} {{/person}}">>,
                             #{person =>
                                   [#{name => <<"kalle">>},
                                    #{name => <<"olle">>}]}))),
     ?_test(
        ?assertEqual(<<"kalle">>,
                     renderf(<<"{{#person}}{{person}}{{/person}}">>,
                             #{person => <<"kalle">>}))),
     ?_test(
        ?assertEqual(<<>>,
                     renderf(<<"{{#person}}{{person}}{{/person}}">>,
                             #{person => false}))),
     ?_test(
        ?assertEqual(<<>>,
                     renderf(<<"{{#person}}{{person}}{{/person}}">>,
                             #{person => []}))),
     ?_test(
        ?assertEqual(<<>>,
                     renderf(<<"{{#person}}{{person}}{{/person}}">>,
                             #{}))),
     ?_test(
        ?assertEqual(<<"That Olle">>,
                     renderf(<<"{{#person}}{{name}}{{/person}}">>,
                             #{person =>
                                   fun(T, C) ->
                                           ["That ", mustache:render(T, C)]
                                   end,
                               name => <<"Olle">>}))),
     ?_test(
        ?assertEqual(<<"User:kalle User:olle ">>,
                     renderf(<<"{{#name}}{{>user}} {{/name}}">>,
                             #{user => <<"User:{{name}}">>,
                               name => [<<"kalle">>, <<"olle">>]}))),
     ?_test(
        ?assertEqual(<<"&lt;FOO&gt;">>,
                     mustache:render(<<"{{foo}}">>,
                                     [{foo, <<"<FOO>">>}],
                                     binary))),
     ?_test(
        ?assertEqual(<<"{ FOO">>,
                     renderf(<<"{ {{foo}}">>, #{foo => <<"FOO">>}))),

     ?_test(
        ?assertEqual(<<"FOO">>,
                     renderf(<<"{{foo} }}">>, #{'foo}' => <<"FOO">>}))),
     ?_test(
        ?assertEqual(<<"FOOBAR">>,
                     renderf(<<"{{foo}}{{! f} }}BAR">>, #{foo => <<"FOO">>}))),
     ?_test(
        ?assertEqual(<<"&gt;FOO&gt;">>,
                     mustache:render(<<"{{foo}}">>,
                                     [{foo, <<">FOO>">>}],
                                     binary))),
     ?_test(
        ?assertEqual(<<"&amp;FOO">>,
                     mustache:render(<<"{{foo}}">>,
                                     [{foo, <<"&FOO">>}],
                                     binary))),
     ?_test(
        ?assertEqual(<<"hepp">>,
                     renderf(<<"{{^person}}hepp{{/person}}">>,
                             [{person, false}]))),
     ?_test(
        ?assertEqual(<<"hepp">>,
                     renderf(<<"{{^person}}hepp{{/person}}">>, []))),
     ?_test(
        ?assertEqual(<<"hepp">>,
                     renderf(<<"{{^person}}hepp{{/person}}">>,
                             [{person, []}]))),
     ?_test(
        ?assertEqual(<<"">>,
                     renderf(<<"{{^person} }}{{name}} {{/person} }}">>,
                             [{'person}', [[{name, <<"kalle">>}],
                                        [{name, <<"olle">>}]]}]))),
     ?_test(
        ?assertEqual(<<"User:kalle User:olle ">>,
                     renderf(<<"{{#name}}{{>user} }} {{/name}}">>,
                             #{'user}' => <<"User:{{name}}">>,
                               name => [<<"kalle">>, <<"olle">>]})))

    %% ,
    %%  ?_test(
    %%     ?assertEqual(<<"kalle olle ">>,
    %%                  renderf(<<"{{#person} }}{{person} }} {{/person} }}">>,
    %%                          [{'person}', [<<"kalle">>, <<"olle">>]}])))
    ].

%% ===================================================================
%% Internal functions.
%% ===================================================================

renderf(Template, Context) ->
    iolist_to_binary(mustache:render(Template, Context)).
