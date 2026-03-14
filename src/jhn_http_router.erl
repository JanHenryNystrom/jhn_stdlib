%==============================================================================
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
%%%  A HTTP routing library
%%%
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2026, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_http_router).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% API
-export([compile/1, lookup/4]).

%% Exported types
-export_type([route/0,
              method_return/0, auth_type/0,
             ]).

%% Callbacks
-callback routes() -> [jhn_posse_router:route()].

-callback connect(req()) -> method_return().
-callback delete(req()) -> method_return().
-callback get(req()) -> method_return().
-callback head(req()) -> method_return().
-callback options(req()) -> method_return().
-callback patch(req()) -> method_return().
-callback post(req()) -> method_return().
-callback put(req()) -> method_return().
-callback trace(req()) -> method_return().

-callback auth(auth_type(), req()) -> boolean().

-optional_callbacks([connect/1, delete/1, get/1, head/1, options/1, patch/1,
                     post/1, put/1, trace/1, auth/2]).

%% Types
-type route() :: #{name := atom(),
                   path := path(),
                   module := module(),
                   log => boolean(),
                   extra => _,
                   methods => [method()]
                  }.

-type req() :: #{host := host(),
                 path := path(),
                 headers := [{binary(), binary()}],
                 bindings := [{binary(), binary()}],
                 extra := _,,
                 log := boolean(),
                 body := binary()
                }.

-type host() :: binary().
-type path() :: binary().
-type method() :: connect | delete | get | head | options | patch | post |
                  put | trace.

-type method_return() :: status() |
                         {status(), headers()} |
                         {status(), headers(), body()}.

-type status() ::
        100 | 101 |
        200 | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208 | 226 |
        300 | 301 | 302 | 303 | 304 | 305 | 307 | 308 |
        400 | 401 | 402 | 403 | 404 | 405 | 406 | 407 | 408 | 409 |
        410 | 411 | 412 | 413 | 414 | 415 | 416 | 417 | 418 |
        421 | 422 | 423 | 424 | 428 | 429 | 431 | 451 |
        500 | 501 | 502 | 503 | 504 | 505 | 506 | 507 | 508 | 510 | 511.
-type headers() :: #{header() => value()}.
-type header() ::
        cache_control | connection  | date | pragma | transfer_encoding |
        upgrade | via | accept | accept_charset | accept_encoding |
        accept_language | authorization | from | host | if_modified_since |
        if_match | if_none_match | if_range | if_unmodified_since |
        max_forwards | proxy_authorization | range | referer | user_agent |
        age | location | proxy_authenticate | public | retry_after | server |
        vary | warning | www_authenticate | allow |
        content_base | content_encoding | content_language | content_length |
        content_location | content_md5 | content_range | content_type |
        etag | expires | last_modified | accept_ranges | set_cookie |
        set_cookie2 | x_forwarded_for | cookie | keep_alive | proxy_connection |
        accept_patch | x_correlation_id |
        %% CORS support
        access_control_allow_origin | access_control_allow_methods |
        access_control_max_age | access_control_allow_headers |
        %% All others
        binary().
-type value() :: iodata().
-type body() :: iodata().

-type auth_type() :: anonymous | basic | bearer.

%% Defines
-define(METHODS,
        [connect, delete, get, head, options, patch, post, put, trace]).

%% ===================================================================
%% API
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%%   
%% @end
%%--------------------------------------------------------------------
-spec compile([{binary(), [module()]}]) -> jhn_route_tree:tree().
%%--------------------------------------------------------------------
compile(Specs) ->
    PL = [{Host, compile_routes(Mods)} || {Host, Mods} <- Specs],
    jhn_route_tree:build(star, $\., PL).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%%   
%% @end
%%--------------------------------------------------------------------
-spec lookup(host(), path(), method(), jhn_route_tree:tree()) ->
          route() | {error, no_host | no_path | no_method}.
%%--------------------------------------------------------------------
lookup(Host, Path, Method, Hosts) ->
    case jhn_route_tree:lookup(Host, star, $., Hosts) of
        undefined -> {error, no_host};
        Routes ->
            case jhn_route_tree:lookup(Path, var, $/, Routes) of
                undefined -> {error, no_path};
                {Route = #{methods := Methods}, Bindings}  ->
                    case lists:member(Method, Methods) of
                        true -> Route#{bindings => Bindings};
                        false -> {error, no_method}
                    end
            end
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

compile_routes(Modules) ->
    PL = [key_value(Module, Route) || Module <- Modules,
                                      Route <- Module:routes()],
    jhn_route_tree:build(var, $/, PL).

key_value(Mod, Route = #{path := Path, name := Name}) ->
    Exported =
        only_methods(lists:sort([F || {F, 1} <- Mod:module_info(exports)]),
                     ?METHODS,
                     []),
    Value = #{module => Mod,
              name => Name,
              extra => maps:get(extra, Route, undefined),
              log => maps:get(log, Route, true)},
    case maps:get(methods, Route, undefined) of
        undefined ->
            {Path, Value#{methods => Exported}};
        Methods ->
            case is_subset(lists:sort(Methods), Exported) of
                true -> {Path, Value#{methods => Methods}};
                false -> erlang:error(badarg, {not_exported, Methods})
            end
    end.

only_methods([], _, Acc) -> lists:reverse(Acc);
only_methods([H | T1], [H | T2], Acc) -> only_methods(T1, T2, [H | Acc]);
only_methods(L1 = [H1 | _], [H2 | T2], Acc) when  H1 > H2 ->
    only_methods(L1, T2, Acc);
only_methods([_ | T1], L2, Acc) ->
    only_methods(T1, L2, Acc).

is_subset([], _) -> true;
is_subset([H | T1], [H | T2]) -> is_subset(T1, T2);
is_subset(L = [H1 | _], [H2 | T2]) when H2 < H1 -> is_subset(L, T2);
is_subset(_, _) -> false.
