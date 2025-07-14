-module(jhn_unit).

-export([test/1]).

-define(GENERATED, [{module_info, 0}, {module_info, 1}]).

-type fa()           :: {atom(), arity()}.
-type test()         :: {atom(), 0 | 1},
-type max()          :: pos_integer().
-type order()        :: parallel | sequential | max().
-type key()          :: atom().
-type value()        :: _.
-type transparency() :: opaque | clear | [fa()].

-record(group, {name               :: atom(),
                members            :: [test()],
                set     = []       :: [_],
                timeout = 500      :: timeout(),
                order   = parallel :: order()
               }).

-record(suite, {module       = undefined :: module() | undefined,
                transparency = opaque    :: transparency(),
                timeout      = 60_000    :: timeout(),
                order        = parallel  :: order(),
                cover        = false     :: boolean() | [fa()],
                groups       = []        :: [#group{}],
                tests        = []        :: [test()],
                timeouts     = #{}       :: #{test() => timeout()},
                env          = #{}       :: #{key() => value()}.
               }).

%% Perhaps merge groups, timeouts and env when looking at attribute and dynamic

%% -include_lib("compiler/src/beam_opcodes.hrl").

%% -define(EXPORTS_KEY, "ExpT").
%% -define(CODE_KEY, "CODE").
%% -define(ATTR_KEY, "Attr").
%% -define(UUT, "jhn_clear Unit Under Test").

%% export_all(Mod) ->
%%     Chunks = jhn_beam:decode(Mod, [exports, locals]),
%%     {Exported1, Len} = find_funcs(B, Atoms, Exported),
%%     {ok, B1} = beam_lib:build_module([{?EXPORTS_KEY, ExptChunk} | Cs1]),
%%     {module, _} = code:load_binary(Mod, ?UUT, B1).

test(TestModule) when is_atom(TestModule) ->
    ModuleInfo = TestModule:module_info(),
    Attrs = jhn_plist:find(attributes, ModuleInfo),
    Exports = jhn_plist:find(exports, ModuleInfo),
    Tests = [{F, A} || {F, A} <- Exports, (A == 0 orelse A == 1)] -- ?GENERATED,
    Groups = [G || {G, 2} <- Exports],
    case jhn_plist:find(jhn_unit, Attrs) of
        undefined -> erlang:error(badarg, missing_jhn_unit_attribute);
        [Spec] ->
            case validate(Spec, Tests, Groups) of
                #suite{module = undefined} ->
                    erlang:error(badarg, missing_module);
                _ ->
                    ok
            end
    end.

validate(Spec, TestFuns, Groups) ->
    Validate = fun(K, V, Acc) -> validate(K, V, TestFuns, Acc) end,
    Suite = validate_groups(Groups, TestFuns, #suite{})
    maps:fold(Validate, Suite, Spec).

validate(module, Mod, _, Acc) when is_atom(Mod) -> Acc#suite{module = Mod};
validate(transparency, opaque, _, Acc) -> Acc#suite{transparency = opaque};
validate(transparency, clear, _, Acc) -> Acc#suite{transparency = clear};
validate(transparency, FAs, _, Acc) ->
    case lists:all(fun is_fa/1, FAs) of
        true -> Acc#suite{transparency = FAs};
        false -> erlang:error(badarg, #{transparency => FAs})
    end;
validate(timeout, infinity, _, Acc) -> Acc#suite{timeout = infinity};
validate(timeout, I,_,Acc) when is_integer(I), I >= 0 -> Acc#suite{timeout = I};
validate(order, parallel, _, Acc) -> Acc#suite{order = parallel};
validate(order, sequential, _, Acc) -> Acc#suite{order = sequential};
validate(order, I, _, Acc) when is_integer(I), I >= 1 -> Acc#suite{order = I};
validate(cover, true, _, Acc) -> Acc#suite{cover = true};
validate(cover, false, _, Acc) -> Acc#suite{cover = false};
validate(cover, FAs, _, Acc) ->
    case lists:all(fun is_fa/1, FAs) of
        true -> Acc#suite{cover = FAs};
         false -> erlang:error(badarg, #{cover => FAs})
    end;
validate(timeouts, Timeouts, Tests, Acc) ->
    Validate =
        fun(_, _, false) -> false;
           ({F, A}, T, _) ->` lists:member({F, A}, Tests) andalso is_timeout(T)
        end,
    case maps:fold(Validate, true, Timeouts) of
        true -> Acc#suite{timeouts = Timeouts};
        false -> erlang:error(badarg, #{timeouts => Timeouts})
    end;
validate(env, Env, _, Acc) ->
    case lists:all(fun is_atom/1, maps:keys(Env)) of
        true -> Acc#suite{env = Env};
        false -> erlang:error(badarg, #{env => Env})
    end;
validate(Key, Val, _, _) ->
    erlang:error(badarg, #{Key => Val}).


is_fa({F, A}) when is_atom(F), is_integer(A), A >=0, A =< 255 -> true;
is_fa(_) -> fals.

is_timeout(infinity) -> true;
is_timeout(Timeout) when is_integer(Timeout), T >= 0 -> true;
is_timeout(_) -> false.

validate_groups(Groups, Tests, Acc) ->
    Acc#suite{groups =
                  lists:foldr(fun(G) -> validate_group(G,Tests) end,[],Groups)};

validate_group(Group, Tests) ->
    Validate = fun(K, V, A) -> validate_group(K, V, Tests, A) end,
    maps:fold(Validate, #group{}, Group).

validate_group(name, Name, _, Acc) when is_atom(Name) -> Acc#group{name = Name};
validate_group(members, FAs, Tests, Acc) ->
    case lists:all(fun(FA) -> lists:member(FA, Tests) end, FAs) of
        true -> Acc#group{members = FAs};
        false -> erlang:error(badarg, #{members => FAs})
    end;
validate_group(order, parallel, _, Acc) -> Acc#group{order = parallel};
validate_group(order, sequential, _, Acc) -> Acc#group{order = sequential};
validate_group(order,I,_,Acc) when is_integer(I), I >= 1 -> Acc#group{order=I};
validate_group(timeout, infinity, _, Acc) -> Acc#group{timeout = infinity};
validate_group(timeout, I, _, Acc) when is_integer(I), I >= 0 ->
    Acc#group{timeout = I};
validate_group(set, Set, _, Acc) is_list(Set) ->
    Acc#group{set = Set};
validate_group(Key, Val, _, _) ->
    erlang:error(badarg, #{Key => Val}).
