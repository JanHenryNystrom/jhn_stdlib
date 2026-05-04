-module(jhn_dds).

-export([start_link/1, total/1, sum/1, insert/2, merge/2, quantile/2]).

-export([init/1, request/2, message/2]).

-opaque ddsketch() :: #state{}.

-export_type([ddsketch/0]).

-record(state, {data = #{} :: #{number() => non_neg_integer()},
                total = 0 :: non_neg_integer(),
                sum = 0 :: number(),
                table :: ets:tid()
               }).

-record(value, {id = jhn_uuid:gen(7, [binary]),
                content :: number()
               }).

start_link(Name) -> jhn_server:create(?MODULE, [{name, Name}]).



-spec new() -> ddsketch().
new() -> #state{}.

-spec total(ddsketch()) -> non_neg_integer().
total(#state{total = Total}) -> Total.

-spec sum(ddsketch()) -> non_neg_integer().
sum(#state{sum = Sum}) -> Sum.

-spec insert(number(), ddsketch()) -> ddsketch().

-spec merge(ddsketch(), ddsketch()) -> ddsketch().
merge(DDS1, DDS2) ->
    #state{data = Data1, total = Total1, sum = Sum1} = DDS1,
    #state{data = Data2, total = Total2, sum = Sum2} = DDS2,
    Sum = fun(_, Val1, Val2) -> Val1 + Val2 end,
    Data = maps:merge_with(Sum, Data1, Data2),
    #state{data = Data, total = Total1 + Total2, sum = Sum1 + Sum2}.

-spec quantile(number(), ddsketch()) -> number() | undefined.
quantile(Quant, #state{data = Data, total = Tl}) when 0 =< Quant, Quant =< 1 ->
    quantile(lists:sort(maps:to_list(Data)), T * Quant, 0).

quantile([], _, _) -> undefined;
quantile([{K, V} | _], Total, Rank) when Rank + V >= Total -> K;
quantile([{_, V} | T], Total, Rank) -> quantile(T, TotalQuant, Rank + V).

%%====================================================================
%% jhn_server callbacks
%%====================================================================

init(Name) ->
    self() ! compact,
    Tid = ets:new(Name,
                  [named_table, {write_concurrency, true}, {keypos, value#id}]),
    {ok, #state{table = Tid}}.

request(Req, State) ->
    unexpected(request, Req),
    {hibernate, State}.

message(compact, State = #state{interval = Interval}) ->
    erlang:send_after(Interval, self(), load),
    {hibernate, compact(State)};
message(Msg, State) ->
    unexpected(message, Msg),
    {hibernate, State}.

%% ===================================================================
%% Internal functions.
%% ===================================================================

add(Val, S = #state{data = D = #{Val := V}, total = Total, sum = Sum}) ->
    S#state{data = D#{Val => V + 1}, total = Total + 1, sum = Sum + Val};
add(Val, S = #state{data = D, total = Total, sum = Sum}) ->
    S#state{data = D#{Val => 1}, total = Total + 1, sum = Sum + Val}.

