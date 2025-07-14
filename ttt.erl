-module(ttt).

%% Group
-export([group_t/2, group_dynam/2, set_t1/2]).

%% Tests
-export([t/0, t1/2, p/13]).

-export([a/0]).

-jhn_unit(#{module => xx}).

-jhn_unit(#{module => xx,
            transparency => [x/1, y/1],
            timeout => 100_000,
            order => parallel,
            cover => false,
            timeouts => #{t/1 =>  500},
            env => {host => <<"127.0.0.x1">>}
           }).

%% /2 group/set
group_t(config, Env) ->
    Config = #{members => [t1/1], timeout => 500, order => sequential},
    {ok, Config, Env};
group_t(setup, Env) -> {ok, Env};
group_t(teardown, _Env) -> ok.

set_t1(config, Env) ->
    Config = #{members => [p/1], set => lists:seq(1, 10)},
    {ok, Config, Env};
set_t1(setup, Env = #{pid := Test}) -> setup(set_t1, Test), {ok, Env};
set_t1(teardown, _Env) -> ok.

seq_t1(config, Env) ->
    Config = #{members => [p/1], sequence => lists:seq(1, 10)},
    {ok, Config, Env};
seq_t1(setup, Env = #{pid := Test}) -> setup(set_t1, Test), {ok, Env};
seq_t1(teardown, _Env) -> ok.

%% /0 test, /1 test part of a group/set /2 group/set
t() -> assert(x, y).

p(#{element := 1}) -> assert(a, 1);
p(#{element := 2}) -> assert(b, 1);
p(#{element := 3}) -> assert(c, 1).

p(#{index := 1}) -> assert(a, 1);
p(#{index := 2}) -> assert(b, 1);
p(#{index := 3}) -> assert(c, 1).

%% jhn_unit:signal(foo, Env), 

assert(_, _) -> ok.

assert_match(_, _) -> ok.

assert_exit(_Class, _Reason, _Expr) -> ok.
assert_exit(_Class, _Expr) -> ok.

assert_exit_match(_Class, _Reason, _Expr) -> ok.

probe(X) -> probe(X, 500).
probe(_X, _T) -> ok.

sync(X) -> sync_probe(X, 500).
sync(_X, _T) -> ok.

a() ->
    try lists:append(#{})
    catch C:R:T ->
            case T of
                [{M, F, _, _} | _] -> {C, R, {M, F}};
                _ -> foo
            end
    end.

c() ->
    try erlang:throw(xxx)
    catch C:R:T -> {C, R, T}
    end.
