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
%%%   eunit unit tests for the jhn_route_tree library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_route_tree_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% Defines

%% ===================================================================
%% Tests.
%% ===================================================================

%%--------------------------------------------------------------------
%% star
%%--------------------------------------------------------------------

star_test_() ->
    Routes = [{~"foo.bar.*", a}, {~"foo.bar", b}, {~"foo.*.*", c},
              {~"a.b", 1}, {~"a.b.*", 2}
             ],
    Tree = jhn_route_tree:build(star, $., Routes),
    [?_test(
        ?assertEqual(a, jhn_route_tree:lookup(~"foo.bar.bat", star, $., Tree))),
     ?_test(
        ?assertEqual(c, jhn_route_tree:lookup(~"foo.bnu.gne", star, $., Tree))),
     ?_test(
        ?assertEqual(undefined,
                     jhn_route_tree:lookup(~"foo.bar", star, $., Tree))),

     ?_test(
        ?assertEqual(1, jhn_route_tree:lookup(~"a.b", star, $., Tree)))

    ].

%%--------------------------------------------------------------------
%% var
%%--------------------------------------------------------------------

var_test_() ->
    Routes = [{~"foo/bar/:a", a}, {~"foo/bar", b}, {~"foo/:a/:b", c}
             ],
    Tree = jhn_route_tree:build(var, $/, Routes),
    [?_test(
        ?assertEqual(a, jhn_route_tree:lookup(~"foo/bar/bat", var, $/, Tree)))
    ].


%% ===================================================================
%% Internal functions.
%% ===================================================================

