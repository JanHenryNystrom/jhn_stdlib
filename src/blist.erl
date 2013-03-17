%%==============================================================================
%% Copyright 2013 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   String Processing Functions for binary encoded strings.
%%%
%%%   This is a drop in replacement for the lists module in stdlib
%%%   working on binaries interpreted as strings of octets in Latin1.
%%%
%%%   The module generates ref binaries as much as possible so if
%%%   copies are more suitable apply binary/copy/1 on the result.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(blist).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([member/2
        ]).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: member(Elem, Binary) -> Boolean.
%% @doc
%%   Returns true if Elem matches some element of Binary, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec member(char(), binary()) -> boolean().
%%--------------------------------------------------------------------
member(_, <<>>) -> false;
member(C, <<C, _/binary>>) -> true;
member(C, <<_, B/binary>>) -> member(C, B).

