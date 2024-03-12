%%==============================================================================
%% Copyright 2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2024, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_math).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions.
-export([rotl32/2]).

%% Defines.

%% rotl32
-define(LEFT_1, 16#7FFFFFFF). % 16#FFFFFFFF bsr 1
-define(LEFT_2, 16#3FFFFFFF). % 16#FFFFFFFF bsr 2
-define(LEFT_3, 16#1FFFFFFF). % 16#FFFFFFFF bsr 3
-define(LEFT_4, 16#FFFFFFF).  % 16#FFFFFFFF bsr 4
-define(LEFT_5, 16#7FFFFFF).  % 16#FFFFFFFF bsr 5
-define(LEFT_6, 16#3FFFFFF).  % 16#FFFFFFFF bsr 6
-define(LEFT_7, 16#1FFFFFF).  % 16#FFFFFFFF bsr 7
-define(LEFT_8, 16#FFFFFF).   % 16#FFFFFFFF bsr 8
-define(LEFT_9, 16#7FFFFF).   % 16#FFFFFFFF bsr 9
-define(LEFT_10, 16#3FFFFF).  % 16#FFFFFFFF bsr 10
-define(LEFT_11, 16#1FFFFF).  % 16#FFFFFFFF bsr 11
-define(LEFT_12, 16#FFFFF).   % 16#FFFFFFFF bsr 12
-define(LEFT_13, 16#7FFFF).   % 16#FFFFFFFF bsr 13
-define(LEFT_14, 16#3FFFF).   % 16#FFFFFFFF bsr 14
-define(LEFT_15, 16#1FFFF).   % 16#FFFFFFFF bsr 15
-define(LEFT_16, 16#FFFF).    % 16#FFFFFFFF bsr 16
-define(LEFT_17, 16#7FFF).    % 16#FFFFFFFF bsr 17
-define(LEFT_18, 16#3FFF).    % 16#FFFFFFFF bsr 18
-define(LEFT_19, 16#1FFF).    % 16#FFFFFFFF bsr 19
-define(LEFT_20, 16#FFF).     % 16#FFFFFFFF bsr 20
-define(LEFT_21, 16#7FF).     % 16#FFFFFFFF bsr 21
-define(LEFT_22, 16#3FF).     % 16#FFFFFFFF bsr 22
-define(LEFT_23, 16#1FF).     % 16#FFFFFFFF bsr 23
-define(LEFT_24, 16#FF).      % 16#FFFFFFFF bsr 24
-define(LEFT_25, 16#7F).      % 16#FFFFFFFF bsr 25
-define(LEFT_26, 16#3F).      % 16#FFFFFFFF bsr 26
-define(LEFT_27, 16#1F).      % 16#FFFFFFFF bsr 27
-define(LEFT_28, 16#F).       % 16#FFFFFFFF bsr 28
-define(LEFT_29, 16#7).       % 16#FFFFFFFF bsr 29
-define(LEFT_30, 16#3).       % 16#FFFFFFFF bsr 30
-define(LEFT_31, 16#1).       % 16#FFFFFFFF bsr 31

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec rotl32(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
%%--------------------------------------------------------------------
rotl32(I, Steps) when I >= 0, I =< 16#FFFFFFFF -> do_rotl32(I, Steps rem 32).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% --------------------------------------------------------------------
%% rotl3232
%% --------------------------------------------------------------------
do_rotl32(I, 0) -> I;
do_rotl32(I, 1) -> ((I band ?LEFT_1) bsl 1) bor (I bsr 31);
do_rotl32(I, 2) -> ((I band ?LEFT_2) bsl 2) bor (I bsr 30);
do_rotl32(I, 3) -> ((I band ?LEFT_3) bsl 3) bor (I bsr 29);
do_rotl32(I, 4) -> ((I band ?LEFT_4) bsl 4) bor (I bsr 28);
do_rotl32(I, 5) -> ((I band ?LEFT_5) bsl 5) bor (I bsr 27);
do_rotl32(I, 6) -> ((I band ?LEFT_6) bsl 6) bor (I bsr 26);
do_rotl32(I, 7) -> ((I band ?LEFT_7) bsl 7) bor (I bsr 25);
do_rotl32(I, 8) -> ((I band ?LEFT_8) bsl 8) bor (I bsr 24);
do_rotl32(I, 9) -> ((I band ?LEFT_9) bsl 9) bor (I bsr 23);
do_rotl32(I, 10) -> ((I band ?LEFT_10) bsl 10) bor (I bsr 22);
do_rotl32(I, 11) -> ((I band ?LEFT_11) bsl 11) bor (I bsr 21);
do_rotl32(I, 12) -> ((I band ?LEFT_12) bsl 12) bor (I bsr 20);
do_rotl32(I, 13) -> ((I band ?LEFT_13) bsl 13) bor (I bsr 19);
do_rotl32(I, 14) -> ((I band ?LEFT_14) bsl 14) bor (I bsr 18);
do_rotl32(I, 15) -> ((I band ?LEFT_15) bsl 15) bor (I bsr 17);
do_rotl32(I, 16) -> ((I band ?LEFT_16) bsl 16) bor (I bsr 16);
do_rotl32(I, 17) -> ((I band ?LEFT_17) bsl 17) bor (I bsr 15);
do_rotl32(I, 18) -> ((I band ?LEFT_18) bsl 18) bor (I bsr 14);
do_rotl32(I, 19) -> ((I band ?LEFT_19) bsl 19) bor (I bsr 13);
do_rotl32(I, 20) -> ((I band ?LEFT_20) bsl 20) bor (I bsr 12);
do_rotl32(I, 21) -> ((I band ?LEFT_21) bsl 21) bor (I bsr 11);
do_rotl32(I, 22) -> ((I band ?LEFT_22) bsl 22) bor (I bsr 10);
do_rotl32(I, 23) -> ((I band ?LEFT_23) bsl 23) bor (I bsr 9);
do_rotl32(I, 24) -> ((I band ?LEFT_24) bsl 24) bor (I bsr 8);
do_rotl32(I, 25) -> ((I band ?LEFT_25) bsl 25) bor (I bsr 7);
do_rotl32(I, 26) -> ((I band ?LEFT_26) bsl 26) bor (I bsr 6);
do_rotl32(I, 27) -> ((I band ?LEFT_27) bsl 27) bor (I bsr 5);
do_rotl32(I, 28) -> ((I band ?LEFT_28) bsl 28) bor (I bsr 4);
do_rotl32(I, 29) -> ((I band ?LEFT_29) bsl 29) bor (I bsr 3);
do_rotl32(I, 30) -> ((I band ?LEFT_30) bsl 30) bor (I bsr 2);
do_rotl32(I, 31) -> ((I band ?LEFT_31) bsl 31) bor (I bsr 1).
