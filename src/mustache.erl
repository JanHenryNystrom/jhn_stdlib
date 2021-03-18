%%==============================================================================
%% Copyright 2017-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   Mustache template rendering.
%%%
%%%   The context is given either as a property list or a map with the
%%%   keys being the tags as atoms. The partials are realised by recursively
%%%   call rendering until no tags remain.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2017-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(mustache).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions.
-export([render/2, render/3]).

%% Types
-type template() :: binary().
-type context() :: plist:plist() | map().
-type option() :: binary | iolist.

%% Exported Types
-export_type([template/0, context/0]).

%% Records
-record(state, {start = <<"{{">>,
                stop = <<"}}">>,
                context,
                escape = true,
                inverted = false}).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: render(Template, Context) -> String
%% @doc
%%   Renders a mustache template, the same as render(Template, Context, iolist).
%% @end
%%--------------------------------------------------------------------
-spec render(template(), context()) -> iolist().
%%--------------------------------------------------------------------
render(Template, Context) -> render(Template, Context, iolist).

%%--------------------------------------------------------------------
%% Function: render(Template, Context, Option) -> String
%% @doc
%%   Renders a mustache template.
%%   Option is:
%%     binary -> a binary is returned
%%     iolist -> an iolist is returned (default)
%% @end
%%--------------------------------------------------------------------
-spec render(template(), context(), option()) -> iolist() | binary().
%%--------------------------------------------------------------------
render(Template, Context, iolist) -> do_render(Template, Context);
render(Template, Context, binary) ->
    iolist_to_binary(do_render(Template, Context)).

%% ===================================================================
%% Internal functions.
%% ===================================================================

do_render(Template, Context) ->
    do_render(Template, <<>>, [], #state{context = Context}).

do_render(<<>>, Latest, Acc, _) -> lists:reverse([Latest | Acc]);
do_render(<<H, T/binary>>, Latest, Acc,State=#state{start = <<H,S1/binary>>}) ->
    case is_start_tag(T, S1, <<Latest/binary, H>>) of
        {true, T1} ->
            {Tag, T2} = render_tag(T1, State),
            do_render(T2, <<>>, [Tag, Latest | Acc], State);
        {T1, Latest1} ->
            do_render(T1, Latest1, Acc, State)
    end;
do_render(<<H/utf8, T/binary>>, Latest, Acc, State) ->
    do_render(T, <<Latest/binary, H/utf8>>, Acc, State).

is_start_tag(T, <<>>, _) -> {true, T};
is_start_tag(<<H, T/binary>>, <<H, Start/binary>>, Acc) ->
    is_start_tag(T, Start, <<Acc/binary, H>>);
is_start_tag(T, _, Acc) ->
    {T, Acc}.

render_tag(<<$!, T/binary>>, State) -> render_comment(T, State);
render_tag(<<$#, T/binary>>, State) -> render_section(T, State);
render_tag(<<$>, T/binary>>, State) -> render_partial(T, <<>>, State);
render_tag(<<$=, T/binary>>, State) -> render_delimiter(T, State);
render_tag(<<$^, T/binary>>, State) ->
    render_section(T, State#state{inverted = true});
render_tag(<<$&, T/binary>>, State) ->
    render_simple(T, <<>>, State#state{escape = false});
render_tag(<<H/utf8, T/binary>>, State) ->
    render_simple(T, <<H>>, State).

render_simple(<<H, T/binary>>, Acc, State = #state{stop = <<H, S1/binary>>}) ->
    case is_stop_tag(T, S1, <<Acc/binary, H>>) of
        {true, T1} ->
            #state{escape = Escape, context = Context} = State,
            Value = find(Acc, Context, <<>>),
            case Escape of
                true -> {escape(Value), T1};
                false -> {Value, T1}
            end;
        {T1, Acc1} -> render_simple(T1, Acc1, State)
    end;
render_simple(<<H, T/binary>>, Acc, State) ->
    render_simple(T, <<Acc/binary, H>>, State).

escape(Tag) ->
    case needs_escape(Tag) of
        true -> escape(Tag, <<>>);
        false -> Tag
    end.

needs_escape(<<>>) -> false;
needs_escape(<<$&, _/binary>>) -> true;
needs_escape(<<$<, _/binary>>) -> true;
needs_escape(<<$>, _/binary>>) -> true;
needs_escape(<<_/utf8, T/binary>>) -> needs_escape(T).

escape(<<>>, Acc) -> Acc;
escape(<<$&, T/binary>>, Acc) -> escape(T, <<Acc/binary, "&amp;">>);
escape(<<$<, T/binary>>, Acc) -> escape(T, <<Acc/binary, "&lt;">>);
escape(<<$>, T/binary>>, Acc) -> escape(T, <<Acc/binary, "&gt;">>);
escape(<<H/utf8, T/binary>>, Acc) -> escape(T, <<Acc/binary, H/utf8>>).

render_comment(<<H, T/binary>>, State = #state{stop = <<H, S1/binary>>}) ->
    case is_stop_tag(T, S1, <<>>) of
        {true, T1} -> {[], T1};
        {T1, _} -> render_comment(T1, State)
    end;
render_comment(<<_/utf8, T/binary>>, State) ->
    render_comment(T, State).

render_delimiter(T, State) -> render_delimiter_start(T, <<>>, State).

render_delimiter_start(<<$\s, T/binary>>, Start, State) ->
    render_delimiter_stop(T, <<>>, State#state{start = Start});
render_delimiter_start(<<H/utf8, T/binary>>, Acc, State) ->
    render_delimiter_start(T, <<Acc/binary, H/utf8>>, State).

render_delimiter_stop(<<$=, T/binary>>, Stop, State = #state{stop = S}) ->
    {true, T1} = is_stop_tag(T, S, <<>>),
    {do_render(T1, <<>>, [], State#state{stop = Stop}), <<>>};
render_delimiter_stop(<<H/utf8, T/binary>>, Acc, State) ->
    render_delimiter_stop(T, <<Acc/binary, H/utf8>>, State).

render_section(T, State) -> render_section(T, <<>>, State).

render_section(<<H, T/binary>>, Acc, State = #state{stop = <<H, S1/binary>>}) ->
    case is_stop_tag(T, S1, <<Acc/binary, H>>) of
        {true, T1} ->
            #state{start = Start, stop = Stop, context = Context} = State,
            SectionEnd = <<Start/binary, $/, Acc/binary, Stop/binary>>,
            {Template, T2} =
                select_section(T1, <<>>, State#state{stop = SectionEnd}),
            Key = binary_to_atom(Acc, utf8),
            {section(find(Acc, Context, false), Key, Template, State), T2};
        {T1, Acc1} ->
            render_section(T1, Acc1, State)
    end;
render_section(<<H/utf8, T/binary>>, Acc, State) ->
    render_section(T, <<Acc/binary, H/utf8>>, State).

select_section(<<H, T/binary>>, Acc, State = #state{stop = <<H, S1/binary>>}) ->
    case is_stop_tag(T, S1, <<Acc/binary, H>>) of
        {true, T1} -> {Acc, T1};
        {T1, Acc1} -> select_section(T1, Acc1, State)
    end;
select_section(<<H/utf8, T/binary>>, Acc, State) ->
    select_section(T, <<Acc/binary, H/utf8>>, State).

section(false, _, _, #state{inverted = false}) -> [];
section([], _, _, #state{inverted = false}) -> [];
section([_ | _], _, _, #state{inverted = true}) -> [];
section([], _, Template, State = #state{inverted = true}) ->
    do_render(Template, <<>>, [], State);
section(false, _, Template, State = #state{inverted = true}) ->
    do_render(Template, <<>>, [], State);
section(Contexts = [#{} | _], _, Template, State=#state{context=C0}) ->
    [do_render(Template, <<>>, [], State#state{context = maps:merge(C0, C)}) ||
        C <- Contexts];
section(Contexts = [[{_, _} | _] | _], _, Template, State=#state{context=C0}) ->
    [do_render(Template, <<>>, [], State#state{context = C ++ C0}) ||
        C <- Contexts];
section(Values = [_ | _], Key, Template, State = #state{context = C = #{}}) ->
    [do_render(Template, <<>>, [], State#state{context = C#{Key => Value}}) ||
        Value <- Values];
section(Values = [_ | _], Key, Template, State = #state{context = C0}) ->
    [do_render(Template, <<>>, [], State#state{context = [{Key, Value} |C0]}) ||
        Value <- Values];
section(Fun, _, Template, #state{context = C}) when is_function(Fun, 2) ->
    Fun(Template, C);
section(Value, Key, Template, State = #state{context = C = #{}}) ->
    do_render(Template, <<>>, [], State#state{context = C#{Key => Value}});
section(Value, Key, Template, State = #state{context = C0}) ->
    do_render(Template, <<>>, [], State#state{context = [{Key, Value} | C0]}).

render_partial(<<H, T/binary>>, Acc, State = #state{stop = <<H, S1/binary>>}) ->
    case is_stop_tag(T, S1, <<Acc/binary, H>>) of
        {true, T1} ->
            #state{context = Context} = State,
            {do_render(find(Acc, Context, <<>>), Context), T1};
        {T1, Acc1} -> render_partial(T1, Acc1, State)
    end;
render_partial(<<H, T/binary>>, Acc, State) ->
    render_partial(T, <<Acc/binary, H>>, State).

is_stop_tag(T, <<>>, _) -> {true, T};
is_stop_tag(<<H, T/binary>>, <<H, Stop/binary>>, Acc) ->
    is_stop_tag(T, Stop, <<Acc/binary, H>>);
is_stop_tag(T, _, Acc) ->
    {T, Acc}.

find(Key, Context = #{}, Default) ->
    maps:get(binary_to_atom(bstring:strip(Key), utf8), Context, Default);
find(Key, Context, Default) ->
    plist:find(binary_to_atom(bstring:strip(Key), utf8), Context, Default).

