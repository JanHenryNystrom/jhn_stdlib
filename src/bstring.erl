%%==============================================================================
%% Copyright 2013-2021 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   This is a drop in replacement for the string module in stdlib
%%%   working on binaries interpreted as strings of octets in Latin1.
%%%
%%%   The module generates ref binaries as much as possible so if
%%%   copies are more suitable apply binary/copy/1 on the result.
%%%   Functions affected are the sub string family and tokens/2.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013-2021, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(bstring).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([len/1,equal/2,concat/2,chr/2,rchr/2,str/2,rstr/2,
         span/2,cspan/2, substr/2,substr/3, tokens/2, join/2,
         chars/2,chars/3,
         copies/2 ,words/1, words/2,
         sub_word/2,sub_word/3,
         strip/1,strip/2,strip/3, left/2, left/3,
         right/2,right/3, centre/2, centre/3,
         sub_string/2,sub_string/3,
         to_integer/1, to_float/1,
         to_upper/1, to_lower/1
        ]).

%% Types
-type direction() :: left | right | both.

%% Exported Types
-export_type([direction/0]).

%% Defines
-define(IS_UPCASE(C),
        $A =< C, C =< $Z; 16#C0 =< C, C =< 16#D6; 16#D8 =< C, C =< 16#DE).
-define(IS_DOWNCASE(C),
        $a =< C, C =< $z; 16#E0 =< C, C =< 16#F6; 16#F8 =< C, C =< 16#FE).
-define(IS_DIGIT(C), C >= $0, C =< $9).
-define(IS_SIGN(C), C == $- ; C == $+).


%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: Len(String) -> Length.
%% @doc
%%   Returns the number of characters in the string.
%% @end
%%--------------------------------------------------------------------
-spec len(binary()) -> non_neg_integer().
%%--------------------------------------------------------------------
len(B) -> byte_size(B).

%%--------------------------------------------------------------------
%% Function: equal(String1, String2) -> Boolean.
%% @doc
%%   Tests whether two strings are equal.
%%   Returns true if they are, otherwise false.
%% @end
%%--------------------------------------------------------------------
-spec equal(binary(), binary()) -> boolean().
%%--------------------------------------------------------------------
equal(B, B) -> true;
equal(_, _) -> false.

%%--------------------------------------------------------------------
%% Function: concat(String1, String2) -> String3.
%% @doc
%%   Concatenates two strings to form a new string. Returns the new string.
%% @end
%%--------------------------------------------------------------------
-spec concat(binary(), binary()) -> binary().
%%--------------------------------------------------------------------
concat(B1, B2) -> <<B1/binary, B2/binary>>.

%%--------------------------------------------------------------------
%% Function: chr(String, Char) -> Index.
%% @doc
%%   Returns the index of the first occurrence of Character in String.
%%   0 is returned if Character does not occur.
%% @end
%%--------------------------------------------------------------------
-spec chr(binary(), char()) -> non_neg_integer().
%%--------------------------------------------------------------------
chr(B, C) when is_integer(C) -> chr(B, C, 1).

chr(<<C, _/binary>>, C, I) -> I;
chr(<<_, B/binary>>, C, I) -> chr(B, C, I + 1);
chr(<<>>, _, _) -> 0.

%%--------------------------------------------------------------------
%% Function: rchr(String, Char) -> Index.
%% @doc
%%   Returns the index of the last occurrence of Character in String.
%%   0 is returned if Character does not occur.
%% @end
%%--------------------------------------------------------------------
-spec rchr(binary(), char()) -> non_neg_integer().
%%--------------------------------------------------------------------
rchr(B, C) when is_integer(C) -> rchr(B, C, 1, 0).

rchr(<<C, B/binary>>, C, I, _) -> rchr(B, C, I + 1, I);
rchr(<<_, B/binary>>, C, I, L) -> rchr(B, C, I + 1, L);
rchr(<<>>, _, _, L) -> L.

%%--------------------------------------------------------------------
%% Function: str(String, SubString) -> Index.
%% @doc
%%   Returns the position where the first occurrence of SubString begins
%%   in String. 0 is returned if SubString does not exist in String.
%% @end
%%--------------------------------------------------------------------
-spec str(binary(), binary()) -> non_neg_integer().
%%--------------------------------------------------------------------
str(B, Sub) when is_binary(B), is_binary(Sub) -> str(B, Sub, 1).

str(<<C, B/binary>>, Sub = <<C, Sub1/binary>>, I) ->
    case prefix(Sub1, B) of
        true -> I;
        false -> str(B, Sub, I + 1)
    end;
str(<<_, B/binary>>, Sub, I) ->
    str(B, Sub, I + 1);
str(<<>>, _, _) -> 0.

%%--------------------------------------------------------------------
%% Function: rstr(String, SubString) -> Index.
%% @doc
%%   Returns the position where the last occurrence of SubString begins
%%   in String. 0 is returned if SubString does not exist in String.
%% @end
%%--------------------------------------------------------------------
-spec rstr(binary(), binary()) -> non_neg_integer().
%%--------------------------------------------------------------------
rstr(B, Sub) when is_binary(B), is_binary(Sub) -> rstr(B, Sub, 1, 0).

rstr(<<C, B/binary>>, Sub = <<C, Sub1/binary>>, I, L) ->
    case prefix(Sub1, B) of
        true -> rstr(B, Sub, I + 1, I);
        false -> rstr(B, Sub, I + 1, L)
    end;
rstr(<<_, B/binary>>, Sub, I, L) ->
    rstr(B, Sub, I + 1, L);
rstr(<<>>, _, _, L) -> L.


%%--------------------------------------------------------------------
%% Function: span(String, Chars) -> Length
%% @doc
%%   Returns the length of the maximum initial segment of String, which
%%   consists entirely of characters from Chars.
%% @end
%%--------------------------------------------------------------------
-spec span(binary(), binary()) -> non_neg_integer().
%%--------------------------------------------------------------------
span(B, Cs) when is_binary(B), is_binary(Cs) -> span(B, Cs, 0).

span(<<C, B/binary>>, Cs, I) ->
    case blist:member(C, Cs) of
        true -> span(B, Cs,  I + 1);
        false -> I
    end;
span(<<>>, _, I) -> I.

%%--------------------------------------------------------------------
%% Function: cspan(String, Chars) -> Length
%% @doc
%%   Returns the length of the maximum initial segment of String, which
%%   consists entirely of characters not from Chars.
%% @end
%%--------------------------------------------------------------------
-spec cspan(binary(), binary()) -> non_neg_integer().
%%--------------------------------------------------------------------
cspan(B, Cs) when is_binary(B), is_binary(Cs) -> cspan(B, Cs, 0).

cspan(<<C, B/binary>>, Cs, I) ->
    case blist:member(C, Cs) of
        true -> I;
        false -> cspan(B, Cs,  I + 1)
    end;
cspan(<<>>, _, I) -> I.

%%--------------------------------------------------------------------
%% Function: substr(String, Start) -> SubString.
%% @doc
%%   Returns a substring of String, starting at the position Start,
%%   and ending at the end of the string.
%% @end
%%--------------------------------------------------------------------
-spec substr(binary(), non_neg_integer()) -> binary().
%%--------------------------------------------------------------------
substr(Binary, 1) when is_binary(Binary) -> Binary;
substr(Binary, Start) when is_binary(Binary), is_integer(Start) ->
    case {byte_size(Binary), Start - 1} of
        {Size, Size} -> <<>>;
         {Size, Pos} when Size > Pos ->
            erlang:binary_part(Binary, {Pos, Size - Pos})
    end.

%%--------------------------------------------------------------------
%% Function: substr(String, Start, Length) -> SubString.
%% @doc
%%   Returns a substring of String, starting at the position Start,
%%   and ending at the end of the string or at length Length.
%% @end
%%--------------------------------------------------------------------
-spec substr(binary(), pos_integer(), non_neg_integer()) -> binary().
%%--------------------------------------------------------------------
substr(Binary, Start, Length)
  when is_integer(Start), Start >= 1, is_integer(Length), Length >= 0 ->
    case byte_size(Binary) - Start of
        Length1 when Length1 >= Length ->
            erlang:binary_part(Binary, {Start - 1, Length});
        Length1 ->
            erlang:binary_part(Binary, {Start - 1, Length1 + 1})
    end.

%%--------------------------------------------------------------------
%% Function: tokens(String, Separators) -> Tokens.
%% @doc
%%   Returns a list of tokens in String, separated by the characters in
%%   Separators.
%% @end
%%--------------------------------------------------------------------
-spec tokens(binary(), binary()) -> [binary()].
%%--------------------------------------------------------------------
tokens(<<>>, _)  -> [];
tokens(Binary, <<>>) when is_binary(Binary) -> [Binary];
tokens(Binary, <<Sep>>) ->
    [Token || Token <- binary:split(Binary, <<Sep>>, [global, trim]),
              Token /= <<>>];
tokens(Binary, Separators) when is_binary(Binary), is_binary(Separators)->
    [Token || Token <- binary:split(Binary,
                                    [<<Sep>> || <<Sep>> <= Separators],
                                    [global, trim]),
              Token /= <<>>].

%%--------------------------------------------------------------------
%% Function: join(StringList, Separator) -> String.
%% @doc
%%   Returns a string with the elements of StringList separated by the
%%   string in Separator.
%% @end
%%--------------------------------------------------------------------
-spec join([binary()], binary()) -> binary().
%%--------------------------------------------------------------------
join([], Separator) when is_binary(Separator) -> <<>>;
join([H | T], Separator) when is_binary(H), is_binary(Separator) ->
    join(T, H, Separator).

join([], Acc, _) -> Acc;
join([H | T], Acc, Separator) when is_binary(H) ->
    join(T, <<Acc/binary, Separator/binary, H/binary>>, Separator).

%%--------------------------------------------------------------------
%% Function: chars(Character, Number) -> String.
%% @doc
%%   Returns a string consisting of Number of characters Character.
%% @end
%%--------------------------------------------------------------------
-spec chars(char(), non_neg_integer()) -> binary().
%%--------------------------------------------------------------------
chars(Char, No)  -> chars(Char, No, <<>>).

%%--------------------------------------------------------------------
%% Function: chars(Character, Number, Tail) -> String.
%% @doc
%%   Returns a string consisting of Number of characters Character,
%%   the string ends with the string Tail.
%% @end
%%--------------------------------------------------------------------
-spec chars(char(), non_neg_integer(), binary()) -> binary().
%%--------------------------------------------------------------------
chars(Char, No, T) when is_integer(Char), is_integer(No), is_binary(T) ->
    H = binary:copy(<<Char>>, No),
    <<H/binary, T/binary>>.

%%--------------------------------------------------------------------
%% Function: copies(String, Number) -> Copies
%% @doc
%%   Returns a string containing String repeated Number times.
%% @end
%%--------------------------------------------------------------------
-spec copies(binary(), non_neg_integer()) -> binary().
%%--------------------------------------------------------------------
copies(Binary, No) when is_binary(Binary), is_integer(No) ->
    binary:copy(Binary, No).

%%--------------------------------------------------------------------
%% Function: words(String) -> Count.
%% @doc
%%   Returns the number of words in String, separated by blanks.
%% @end
%%--------------------------------------------------------------------
-spec words(binary()) -> non_neg_integer().
%%--------------------------------------------------------------------
words(Binary) -> words(Binary, $\s).

%%--------------------------------------------------------------------
%% Function: words(String, Character) -> Count
%% @doc
%%   Returns the number of words in String, separated by Character.
%% @end
%%--------------------------------------------------------------------
-spec words(binary(), non_neg_integer()) -> non_neg_integer().
%%--------------------------------------------------------------------
words(<<>>, Separator) when is_integer(Separator) -> 1;
words(Binary = <<Separator, _/binary>>, Separator)
  when is_binary(Binary), is_integer(Separator) ->
    words(Binary, Separator, 0, false);
words(Binary, Separator) when is_binary(Binary), is_integer(Separator) ->
    words(Binary, Separator, 1, true).

words(<<>>, _, 0, _) -> 1;
words(<<>>, _, Count, _) -> Count;
words(<<Separator, T/binary>>, Separator, Count, false) ->
    words(T, Separator, Count, false);
words(<<Separator, T/binary>>, Separator, Count, true) ->
    words(T, Separator, Count, false);
words(<<_, T/binary>>, Separator, Count, false) ->
    words(T, Separator, Count + 1, true);
words(<<_, T/binary>>, Separator, Count, true) ->
    words(T, Separator, Count, true).

%%--------------------------------------------------------------------
%% Function: sub_word(String, Number) -> Word.
%% @doc
%%   Returns the word in position Number of String. Words are separated
%%   by blanks .
%% @end
%%--------------------------------------------------------------------
-spec sub_word(binary(), non_neg_integer()) -> binary().
%%--------------------------------------------------------------------
sub_word(Binary, Number) -> sub_word(Binary, Number, $\s).

%%--------------------------------------------------------------------
%% Function: sub_word(String, Number, Character) -> Word
%% @doc
%%   Returns the word in position Number of String. Words are separated
%%   by Characters.
%% @end
%%--------------------------------------------------------------------
-spec sub_word(binary(), non_neg_integer(), non_neg_integer()) -> binary().
%%--------------------------------------------------------------------
sub_word(<<>>, Number, _) when is_integer(Number) -> <<>>;
sub_word(<<Separator, T/binary>>, Number, Separator) ->
    sub_word(T, Number, 0, false, Separator);
sub_word(Binary, Number, Separator) ->
    sub_word(Binary, Number, 1, true, Separator).

sub_word(<<>>, _, _, _, _) -> <<>>;
sub_word(Binary, Number, Number, _, Separator) ->
    sub_word_get(Binary, <<>>, Separator);
sub_word(<<Separator, T/binary>>, Number, Sofar, false, Separator) ->
    sub_word(T, Number, Sofar, false, Separator);
sub_word(Binary, Number, Sofar, false, Separator) ->
    sub_word(Binary, Number, Sofar + 1, true, Separator);
sub_word(<<Separator, T/binary>>, Number, Sofar, true, Separator) ->
    sub_word(T, Number, Sofar, false, Separator);
sub_word(<<_, T/binary>>, Number, Sofar, true, Separator) ->
    sub_word(T, Number, Sofar, true, Separator).

sub_word_get(<<>>, Acc, _) -> Acc;
sub_word_get(<<Separator, _/binary>>, Acc, Separator) -> Acc;
sub_word_get(<<H, T/binary>>, Acc, Separator) ->
    sub_word_get(T, <<Acc/binary, H>>, Separator).

%%--------------------------------------------------------------------
%% Function: strip(String) -> string().
%% @doc
%%   Returns a string, where leading and trailing blanks have been removed.
%%   The function strip/1 is equivalent to strip(String, both).
%% @end
%%--------------------------------------------------------------------
-spec strip(binary()) -> binary().
%%--------------------------------------------------------------------
strip(Binary) -> strip(Binary, both).

%%--------------------------------------------------------------------
%% Function: strip(String, Direction) -> Stripped.
%% @doc
%%   Returns a string, where leading and/or trailing blanks have been removed.
%%   Direction can be left, right, or both and indicates from which direction
%%   blanks are to be removed.
%% @end
%%--------------------------------------------------------------------
-spec strip(binary(), direction()) -> binary().
%%--------------------------------------------------------------------
strip(Binary, Direction) -> strip(Binary, Direction, $\s).

%%--------------------------------------------------------------------
%% Function: strip(String, Direction, Character) -> Stripped.
%% @doc
%%   Returns a string, where leading and/or trailing blanks or a number
%%   of Character have been removed. Direction can be left, right, or
%%   both and indicates from which direction blanks are to be removed.
%% @end
%%--------------------------------------------------------------------
-spec strip(binary(), direction(), char()) -> binary().
%%--------------------------------------------------------------------
strip(Binary, left, Char) when is_binary(Binary), is_integer(Char) ->
    strip(Binary, true, false, Char, <<>>, <<>>);
strip(Binary, right, Char) when is_binary(Binary), is_integer(Char) ->
    strip(Binary, false, true, Char, <<>>, <<>>);
strip(Binary, both, Char) when is_binary(Binary), is_integer(Char) ->
    strip(Binary, true, true, Char, <<>>, <<>>).

%% strip(Bin, strip_left, strip_right, Char, Acc, RightSofar)
strip(<<>>, _, _, _, Acc, RightSofar) -> <<Acc/binary, RightSofar/binary>>;
strip(<<Char, T/binary>>, true, Right, Char, Acc, RightSofar) ->
    strip(T, true, Right, Char, Acc, RightSofar);
strip(Binary, true, false, _, _, _) ->
    Binary;
strip(Binary, true, true, Char, Acc, RightSofar) ->
    strip(Binary, false, true, Char, Acc, RightSofar);
strip(<<Char>>, _, _, Char, Acc, _) ->
    Acc;
strip(<<Char, T/binary>>, _, _, Char, Acc, RightSofar) ->
    strip(T, false, true, Char, Acc, <<RightSofar/binary, Char>>);
strip(<<H, T/binary>>, _, _, Char, Acc, <<>>) ->
    strip(T, false, true, Char, <<Acc/binary, H>>, <<>>);
strip(<<H, T/binary>>, _, _, Char, Acc, RightSofar) ->
    strip(T, false, true, Char, <<Acc/binary, RightSofar/binary, H>>, <<>>).

%%--------------------------------------------------------------------
%% Function: left(String, Number) -> Left
%% @doc
%%   Returns the String with the length adjusted in accordance with Number.
%%   The left margin is fixed. If the `length(String) < Number',
%%   String is padded with blanks.
%% @end
%%--------------------------------------------------------------------
-spec left(binary(), non_neg_integer()) -> binary().
%%--------------------------------------------------------------------
left(Binary, Length) -> left(Binary, Length, $\s).

%%--------------------------------------------------------------------
%% Function: left(String, Number, Character) -> Left
%% @doc
%%   Returns the String with the length adjusted in accordance with Number.
%%   The left margin is fixed. If the `length(String) < Number',
%%   String is padded with Characters.
%% @end
%%--------------------------------------------------------------------
-spec left(binary(), non_neg_integer(), char()) -> binary().
%%--------------------------------------------------------------------
left(Binary, 0, Char) when is_binary(Binary), is_integer(Char) -> <<>>;
left(Binary, Length, Char)
  when is_binary(Binary), is_integer(Length), is_integer(Char) ->
    case byte_size(Binary) of
        0 -> chars(Char, Length);
        Length -> Binary;
        Size when Size > Length -> binary_part(Binary, {0, Length});
        Size when Size < Length ->
            Pad = chars(Char, Length - Size),
            <<Binary/binary, Pad/binary>>
    end.

%%--------------------------------------------------------------------
%% Function: right(String, Number) -> Right.
%% @doc
%%   Returns the String with the length adjusted in accordance with Number.
%%   The right margin is fixed. If the length of `(String) < Number',
%%   String is padded with blanks.
%% @end
%%--------------------------------------------------------------------
-spec right(binary(), non_neg_integer()) -> binary().
%%--------------------------------------------------------------------
right(Binary, Length) -> right(Binary, Length, $\s).

%%--------------------------------------------------------------------
%% Function: right(String, Number, Character) -> Right
%% @doc
%%   Returns the String with the length adjusted in accordance with Number.
%%   The right margin is fixed. If the length of `(String) < Number',
%%   String is padded with Characters.
%% @end
%%--------------------------------------------------------------------
-spec right(binary(), non_neg_integer(), char()) -> binary().
%%--------------------------------------------------------------------
right(Binary, 0, Char) when is_binary(Binary), is_integer(Char) -> <<>>;
right(Binary, Length, Char)
  when is_binary(Binary), is_integer(Length), is_integer(Char) ->
    case byte_size(Binary) of
        0 -> chars(Char, Length);
        Length -> Binary;
        Size when Size > Length -> binary_part(Binary, {Size, - Length});
        Size when Size < Length ->
            Pad = chars(Char, Length - Size),
            <<Pad/binary, Binary/binary>>
    end.

%%--------------------------------------------------------------------
%% Function: centre(String, Number) -> Centered.
%% @doc
%%   Returns a string, where String is centred in the string and surrounded
%%   by blanks. The resulting string will have the length Number.
%% @end
%%--------------------------------------------------------------------
-spec centre(binary(), non_neg_integer()) -> binary().
%%--------------------------------------------------------------------
centre(Binary, Length) -> centre(Binary, Length, $\s).

%%--------------------------------------------------------------------
%% Function: centre(String, Number, Character) -> Centered.
%% @doc
%%   Returns a string, where String is centred in the string and surrounded
%%   by characters. The resulting string will have the length Number.
%% @end
%%--------------------------------------------------------------------
-spec centre(binary(), non_neg_integer(), char()) -> binary().
%%--------------------------------------------------------------------
centre(Binary, 0, Char) when is_binary(Binary), is_integer(Char) -> <<>>;
centre(Binary, Length, Char)
  when is_binary(Binary), is_integer(Length), is_integer(Char) ->
    case byte_size(Binary) of
        0 -> chars(Char, Length);
        Length -> Binary;
        Size when Size > Length ->
            binary_part(Binary, {(Size - Length) div 2, Length});
        Size when Size < Length ->
            N = (Length - Size) div 2,
            LPad = chars(Char, Length - (Size  + N)),
            RPad = chars(Char, N),
            <<RPad/binary, Binary/binary, LPad/binary>>
    end.

%%--------------------------------------------------------------------
%% Function: sub_string(String, Start) -> SubString.
%% @doc
%%   Returns a substring of String, starting at the position Start to
%%   the end of the string.
%% @end
%%--------------------------------------------------------------------
-spec sub_string(binary(), pos_integer()) -> binary().
%%--------------------------------------------------------------------
sub_string(Binary, Start) -> substr(Binary, Start).

%%--------------------------------------------------------------------
%% Function: sub_string(String, Start, Stop) -> SubString.
%% @doc
%%   Returns a substring of String, starting at the position Start to the
%%   end of the string, or to and including the Stop position.
%% @end
%%--------------------------------------------------------------------
-spec sub_string(binary(), pos_integer(), pos_integer()) -> binary().
%%--------------------------------------------------------------------
sub_string(Binary, Start, Stop) -> substr(Binary, Start, Stop - Start + 1).

%%--------------------------------------------------------------------
%% Function: to_float(String) -> {Float, Rest} | {error, Reason}.
%% @doc
%%   Argument String is expected to start with a valid text represented
%%   float (the digits being ASCII values). Remaining characters in the
%%   string after the float are returned in Rest.
%% @end
%%--------------------------------------------------------------------
-spec to_float(binary()) -> {float(), binary()} | {error, _}.
%%--------------------------------------------------------------------
to_float(<<H, T/binary>>) when ?IS_SIGN(H) -> to_float_digit(T, [H]);
to_float(Binary) when is_binary(Binary) -> to_float_digit(Binary, "");
to_float(_) -> {error, not_a_binary}.

to_float_digit(<<>>, _) -> {error, no_float};
to_float_digit(<<$., _/binary>>, []) -> {error, no_float};
to_float_digit(<<$., H, T/binary>>, Acc) when ?IS_DIGIT(H) ->
    to_float_frac(T, [H, $. | Acc]);
to_float_digit(<<H, T/binary>>, Acc) when ?IS_DIGIT(H)->
    to_float_digit(T, [H | Acc]);
to_float_digit(_, _) ->
    {error, no_float}.

to_float_frac(<<>>, Acc) -> {list_to_float(lists:reverse(Acc)), <<>>};
to_float_frac(<<H, T/binary>>, Acc) when ?IS_DIGIT(H) ->
    to_float_frac(T, [H | Acc]);
to_float_frac(<<$e, S, D, T/binary>>, Acc) when ?IS_SIGN(S), ?IS_DIGIT(D) ->
    to_float_exp(T, [D, S, $e | Acc]);
to_float_frac(<<$e, D, T/binary>>, Acc) when ?IS_DIGIT(D) ->
    to_float_exp(T, [D, $e | Acc]);
to_float_frac(Binary, Acc) ->
    {list_to_float(lists:reverse(Acc)), Binary}.

to_float_exp(<<>>, Acc) -> {list_to_float(lists:reverse(Acc)), <<>>};
to_float_exp(<<H, T/binary>>, Acc) when ?IS_DIGIT(H) ->
    to_float_exp(T, [H | Acc]);
to_float_exp(Binary, Acc) ->
    {list_to_float(lists:reverse(Acc)), Binary}.

%%--------------------------------------------------------------------
%% Function: to_integer(String) -> {Int, Rest} | {error, Reason}.
%% @doc
%%   Argument String is expected to start with a valid text represented
%%   integer (the digits being ASCII values). Remaining characters in the
%%   string after the integer are returned in Rest.
%% @end
%%--------------------------------------------------------------------
-spec to_integer(binary()) -> {integer(), binary()} | {error, _}.
%%--------------------------------------------------------------------
to_integer(<<H, T/binary>>) when ?IS_SIGN(H) -> to_integer(T, [H]);
to_integer(Binary) when is_binary(Binary) -> to_integer(Binary, "");
to_integer(_) -> {error, not_a_binary}.

to_integer(<<>>, Acc) ->
    try list_to_integer(lists:reverse(Acc)) of
        Integer -> {Integer, <<>>}
    catch
        _:_ -> {error, no_integer}
    end;
to_integer(<<H, T/binary>>, Acc) when ?IS_DIGIT(H) ->
    to_integer(T, [H | Acc]);
to_integer(Binary, Acc) ->
    try list_to_integer(lists:reverse(Acc)) of
        Integer  -> {Integer, Binary}
    catch
        _:_ -> {error, no_integer}
    end.

%%--------------------------------------------------------------------
%% Function: to_lower(String) -> Result
%% @doc
%%   The given string is case-converted. Note that the supported character
%%   set is ISO/IEC 8859-1 (a.k.a. Latin 1), all values outside this set
%%   is unchanged.
%% @end
%%--------------------------------------------------------------------
-spec to_lower(binary()) -> binary().
%%--------------------------------------------------------------------
to_lower(Binary) -> to_lower(Binary, <<>>).

to_lower(<<>>, Acc) -> Acc;
to_lower(<<H, T/binary>>, Acc) when ?IS_UPCASE(H) ->
    H1 = H + 32,
    to_lower(T, <<Acc/binary, H1>>);
to_lower(<<H, T/binary>>, Acc) ->
    to_lower(T, <<Acc/binary, H>>).

%%--------------------------------------------------------------------
%% Function: to_upper(String) -> Result
%% @doc
%%   The given string is case-converted. Note that the supported character
%%   set is ISO/IEC 8859-1 (a.k.a. Latin 1), all values outside this set
%%   is unchanged.
%% @end
%%--------------------------------------------------------------------
-spec to_upper(binary()) -> binary().
%%--------------------------------------------------------------------
to_upper(Binary) -> to_upper(Binary, <<>>).

to_upper(<<>>, Acc) -> Acc;
to_upper(<<H, T/binary>>, Acc) when ?IS_DOWNCASE(H) ->
    H1 = H - 32,
    to_upper(T, <<Acc/binary, H1>>);
to_upper(<<H, T/binary>>, Acc) ->
    to_upper(T, <<Acc/binary, H>>).

%% ===================================================================
%% Internal functions.
%% ===================================================================

prefix(<<>>, _) -> true;
prefix(<<C, Pre/binary>>, <<C, Binary/binary>>) -> prefix(Pre, Binary);
prefix(_, _) -> false.
