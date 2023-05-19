%%==============================================================================
%% Copyright 2016-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   Consistent hashing
%%%
%%%   Jump consistent hashing based on:
%%%     A Fast, Minimal Memory, Consistent Hash Algorithm by
%%%     John Lamping and Eric Veach
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2016-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_chash).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions.
-export([jump/2]).

-define(KSTEP, 2862933555777941757).
-define(JSTEP, 2147483648).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: jump(Key, Buckets) -> Bucket.
%% @doc
%%   Given a integer key and the number of buckets it select a bucket.
%% @end
%%--------------------------------------------------------------------
-spec jump(integer(), integer()) -> integer().
%%--------------------------------------------------------------------
jump(Key, Buckets) -> jump(0, undefined, Key, Buckets).

%% ===================================================================
%% Internal functions.
%% ===================================================================

jump(J, B, _, Buckets) when J >= Buckets -> B;
jump(J, _, Key, Buckets) ->
    <<Key0:64>> = <<(Key * ?KSTEP):64>>,
    Key1 = Key0 + 1,
    J1 = trunc((J + 1) * (?JSTEP / ((Key1 bsr 33) + 1))),
    jump(J1, J, Key1, Buckets).
