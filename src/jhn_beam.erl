%%==============================================================================
%% Copyright 2025 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%  A beam handling lib ...
%%%
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2025, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(jhn_beam).

%% Library functions
-export([list/1,
         decode/1, decode/2, decode/3,
         encode/2, encode/3, assemble/1, assemble/2,
         select/3, add/3, update/3, patch/3
        ]).

%% Includes
-include_lib("compiler/src/beam_opcodes.hrl").

%% Defines
-define(BINARY_ENCODED,
        [attributes, compile_info, docs, debug_info, meta, abstract_code]).

-define(FUNC_TABLE_ENCODED, [exports, imports, locals]).

-define(GENERATED, [atoms, imports, locals, strings, funs,
                    literals, line, type_info, meta]).

-define(UPDATABLE,
        [attributes, exports, code, compile_info, debug_info, docs, meta]).

%% compile_info, attributes slim is in compiler_opts

-define(TAG_U, ?tag_u).
-define(TAG_I, ?tag_i).
-define(TAG_A, ?tag_a).
-define(TAG_X, ?tag_x).
-define(TAG_Y, ?tag_y).
-define(TAG_F, ?tag_f).
-define(TAG_H, ?tag_h).
-define(TAG_Z, ?tag_z).

-define(TAG_FLOAT, 0).
-define(TAG_LIST, 1).
-define(TAG_FR, 2).
-define(TAG_ALLOC_LIST, 3).
-define(TAG_LITERAL, 4).
-define(TAG_TYPE_TAGGED, 5).

-define(ESSENTIAL,
        [atoms, code, strings, imports, exports, funs, literals, meta]).

-define(SLIM, ?ESSENTIAL ++ [attributes, type_info]).
-define(ALL, ?ESSENTIAL ++
            [locals, attributes, compile_info,
             %% Extra Chunks
             abstract_code, debug_info, docs,
             line, type_info]).

-define(DEPS, [atoms, literals, type_info]).

%% Records
-record(tables,
        {atoms     = gb_trees:empty() :: gb_tree:tree(pos_integer(), atom()),
         literals  = gb_trees:empty() :: gb_tree:tree(non_neg_integer(), _),
         type_info = gb_trees:empty() :: gb_tree:tree(non_neg_integer(),
                                                      beam_types:type())}).

%% Types
-export_type([beam/0]).

-type shorthand() :: all | essential | slim.
-type essential() :: atoms | code | strings | imports | exports | funs |
                     literals | meta.
-type slim()      :: essential() | attributes | type_info.
-type all()       :: slim() | locals | compile_info | abstract_code |
                     debug_info | docs | line.
-type generated() :: atoms | imports | locals | strings | funs | literals |
                     line | type_info | meta.
-type updatable() :: attributes | exports | code | compile_info | debug_info |
                     docs | meta.

-opaque beam() ::
          #{tables := #tables{},
            chunks := binary(),
            atoms =>
                #{original := binary(), decoded => atoms(), new => atoms()},
            code =>
                #{original := binary(), decoded => code(), new => code()},
            strings =>
                #{original := binary(), decoded => binary(), new => binary()},
            imports =>
                #{original := binary(), decoded => imports(), new => imports()},
            exports =>
                #{original := binary(), decoded => exports(), new => exports()},
            funs =>
                #{original := binary(), decoded => funs(), new => funs()},
            literals =>
                #{original := binary(),
                  decoded => literals(),
                  new => literals()},
            meta =>
                #{original := binary(), decoded => meta(), new => meta()},
            attributes =>
                #{original := binary(), decoded =>
                      attributes(),
                  new => attributes()},
            type_info =>
                #{original := binary(),
                  decoded => type_info(),
                  new => type_info()},
            locals =>
                #{original := binary(), decoded => locals(), new => locals()},
            compile_info =>
                #{original := binary(),
                  decoded => compile_info(),
                  new => compile_info()},
            abstract_code =>
                #{original := binary(), decoded => _, new => _},
            debug_info =>
                #{original := binary(), decoded => _, new => _},
            docs =>
                #{original := binary(), decoded => _, new => _},
            line =>
                #{original := binary(), decoded => line(), new => line()}
           }.

-type atoms() :: [{pos_integer(), atom()}].
-type code() :: #{instruction_set     := non_neg_integer(),
                  opcode_max          := pos_integer(),
                  number_of_labels    := pos_integer(),
                  number_of_functions := pos_integer(),
                  code                := [beam_asm:asm_function()]}.
-type exports() :: [{atom(), arity(), label()}].
-type imports() :: [{atom(), arity(), label()}].
-type funs() :: [{atom(), arity(), label(),
                  non_neg_integer(), non_neg_integer(), non_neg_integer()}].
-type literals() :: [{non_neg_integer(), _}].
-type meta() :: [{atom(), _}].
-type attributes() :: [{atom(), _}].
-type type_info() :: [{non_neg_integer(),beam_types:type()}].
-type locals() :: [{atom(), arity(), label()}].
-type compile_info() :: [{atom(), _}].
-type line() :: #{version := non_neg_integer(),
                  bits    := non_neg_integer(),
                  lines   := [{non_neg_integer(), pos_integer()}]}.

-type label() :: pos_integer().

-type stage() :: original | decoded | new.

%% ===================================================================
%% Library functions
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec list(beam() | binary() |module()) -> [all()] | {error, _}.
%%--------------------------------------------------------------------
list(BEAM = #{}) -> maps:keys(BEAM) -- [chunks, tables];
list(<<"FOR1", _:32, "BEAM", Cs/binary>>) -> list(Cs, []);
list(Mod) when is_atom(Mod) ->
    case code:get_object_code(Mod) of
        {_, Bin, _} -> list(Bin);
        _ -> {error, failed_to_read}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(module()) -> beam() | {error, _}.
%%--------------------------------------------------------------------
decode(Mod) -> decode(Mod, all).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(module(), shorthand() | [shorthand() | all()]) ->
          beam() |{error, _}.
%%--------------------------------------------------------------------
decode(Mod, Decode) -> decode(Mod, Decode, []).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(module() | binary(),
             shorthand() | [shorthand() | all()],
             shorthand() | [shorthand() | all()]) ->
          beam() |{error, _}.
%%--------------------------------------------------------------------
decode(Mod, Decode, Include) when is_atom(Mod) ->
    case code:get_object_code(Mod) of
        {_, Bin, _} -> decode(Bin, Decode, Include);
        _-> {error, failed_to_read}
    end;
decode(<<"FOR1", _:32, "BEAM", Cs/binary>>, Decode, Include) ->
    Decode1 = lists:usort(shorthand(Decode)),
    AddLocals =
        lists:member(locals, Decode1) andalso not has_locals(Cs),
    Decode2 = case AddLocals of
                  true -> Decode1 ++ [code, exports, funs];
                  false -> Decode1
              end,
    Decode3 = lists:usort(shorthand(Decode2)),
    Include1 = shorthand(Include),
    Deps = lists:usort(lists:append([depends_on(C) || C <- Decode2])),
    All = lists:usort(Deps ++ Decode1 ++ Include1),
    {PreChunks, Tables} =
        decode_pre(Deps, read_chunks(Cs, All, #{}), #tables{}),
    DC = decode_chunks(maps:to_list(PreChunks), Decode3,
                       Tables,
                       PreChunks#{chunks => Cs, tables => Tables}),
    case AddLocals of
        true -> add_locals(DC);
        false -> DC
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec encode([all()], beam()) -> beam().
%%--------------------------------------------------------------------
encode(_Chunks, _BEAM) -> <<>>.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec encode(module(), slim | all, beam()) -> binary().
%%--------------------------------------------------------------------
encode(Mod, Chunks, BEAM) ->
    case list(BEAM) -- ?ESSENTIAL of
        [] -> build_beam(Mod, Chunks, BEAM);
        Missing -> {error, {missing, Missing}}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec assemble(beam()) -> binary().
%%--------------------------------------------------------------------
assemble(BEAM) -> assemble(slim, BEAM).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec assemble(slim | all, beam()) -> binary().
%%--------------------------------------------------------------------
assemble(all, BEAM) -> assemble_beam(?ALL, BEAM);
assemble(slim, BEAM) -> assemble_beam(?SLIM, BEAM).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec select(all(), stage(), beam()) -> {ok, _} | false.
%%--------------------------------------------------------------------
select(Key, Stage, BEAM) ->
    case {Stage, BEAM} of
        {original, #{Key := #{original := Original}}} -> {ok, Original};
        {decoded, #{Key := #{decoded := Decoded}}} -> {ok, Decoded};
        {new, #{Key := #{new := New}}} -> {ok, New};
        _ -> false
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec add(updatable(), _, beam()) -> {ok, beam()} | {error, _}.
%%--------------------------------------------------------------------
add(Key, Value, BEAM) ->
    case {lists:member(Key, ?UPDATABLE), BEAM} of
        {true, #{Key := Value}} -> {error, {already_present, Key}};
        {false, _} -> {error, not_updatable};
        _ -> {ok, BEAM#{Key => #{new => Value}}}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec update(updatable(), _, beam()) -> {ok, beam()} | {error, _}.
%%--------------------------------------------------------------------
update(Key, Value, BEAM) ->
    case {lists:member(Key, ?UPDATABLE), BEAM} of
        {true, #{Key := V}} -> {ok, BEAM#{Key => V#{new => Value}}};
        {false, _} -> {error, not_updatable};
        _ -> {error, not_present}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec patch(generated(), _, beam()) -> {ok, beam()} | {error, _}.
%%--------------------------------------------------------------------
patch(Key, Value, BEAM) ->
    case {lists:member(Key, ?GENERATED), BEAM} of
        {true, #{Key := V}} -> {ok, BEAM#{Key => V#{new => Value}}};
        {false, _} -> {error, not_patchable};
        _ -> {ok, BEAM#{Key => #{patch => Value}}}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%%--------------------------------------------------------------------
%% List
%%--------------------------------------------------------------------
list(<<>>, Acc) -> lists:reverse(Acc);
list(<<Name:4/binary, Sz:32, T/binary>>, Acc) ->
    Pad = align_by_four(Sz) - Sz,
    <<_:Sz/binary, _:Pad/binary, T1/binary>> = T,
    list(T1, [decode_chunk_name(Name) | Acc]).

%%--------------------------------------------------------------------
%% Decode
%%--------------------------------------------------------------------
shorthand(all) -> ?ALL;
shorthand(slim) -> ?SLIM;
shorthand(essential) -> ?ESSENTIAL;
shorthand(Chunks) ->
    case lists:member(all, Chunks) of
        true -> ?ALL;
        false ->
            case lists:member(slim, Chunks) of
                true -> (Chunks -- [slim, essential]) ++ ?SLIM;
                false ->
                    case lists:member(essential, Chunks) of
                        true -> lists:delete(essential, Chunks) ++ ?ESSENTIAL;
                        false -> Chunks
                    end
            end
    end.

read_chunks(<<>>, _, Acc) -> Acc;
read_chunks(<<NameBin:4/binary, Sz:32, T/binary>>, Which, Acc) ->
    Pad = align_by_four(Sz) - Sz,
    <<Chunk:Sz/binary, _:Pad/binary, T1/binary>> = T,
    Name = decode_chunk_name(NameBin),
    case lists:member(Name, Which) of
        true -> read_chunks(T1, Which, Acc#{Name => Chunk});
        false -> read_chunks(T1, Which, Acc)
    end.

align_by_four(N) -> (4 * ((N + 3) div 4)).

decode_chunk_name(<<"AtU8">>) -> atoms;
decode_chunk_name(<<"ExpT">>) -> exports;
decode_chunk_name(<<"ImpT">>) -> imports;
decode_chunk_name(<<"Code">>) -> code;
decode_chunk_name(<<"StrT">>) -> strings;
decode_chunk_name(<<"Attr">>) -> attributes;
decode_chunk_name(<<"CInf">>) -> compile_info;
decode_chunk_name(<<"LocT">>) -> locals;
decode_chunk_name(<<"LitT">>) -> literals;
decode_chunk_name(<<"Abst">>) -> abstract_code;
decode_chunk_name(<<"Line">>) -> line;
decode_chunk_name(<<"FunT">>) -> funs;
decode_chunk_name(<<"Type">>) -> type_info;
decode_chunk_name(<<"Dbgi">>) -> debug_info;
decode_chunk_name(<<"Docs">>) -> docs;
decode_chunk_name(<<"Meta">>) -> meta.

decode_pre([], Chunks, Tables) -> {Chunks, Tables};
decode_pre([atoms | T], B = #{atoms := O}, Tables) ->
    D = decode_chunk(atoms, O, Tables),
    Tables1 = Tables#tables{atoms = gb_trees:from_orddict(lists:keysort(1, D))},
    decode_pre(T, B#{atoms => #{original => O, decoded => D}}, Tables1);
decode_pre([literals | T], B = #{literals := O}, Tables) ->
    D = decode_chunk(literals, O, Tables),
    Tables1 =
        Tables#tables{literals = gb_trees:from_orddict(lists:keysort(1, D))},
    decode_pre(T, B#{literals => #{original => O, decoded => D}}, Tables1);
decode_pre([type_info | T], B = #{type_info := O}, Tables) ->
    D = decode_chunk(type_info, O, Tables),
    Tables1 =
        Tables#tables{type_info = gb_trees:from_orddict(lists:keysort(1, D))},
    decode_pre(T, B#{type_info => #{original => O, decoded => D}}, Tables1).

decode_chunks([], _, _, Acc) -> Acc;
decode_chunks([{K, V = #{decoded := _}} | T], Decode, Tables, Acc) ->
    decode_chunks(T, Decode, Tables, Acc#{K => V});
decode_chunks([{K, V} | T], Decode, Tables, Acc) ->
    Acc1 =
        case lists:member(K, Decode) of
            true ->
                Decoded = decode_chunk(K, V, Tables),
                Acc#{K => #{original => V, decoded => Decoded}};
            false ->
                Acc#{K => #{original => V}}
        end,
    decode_chunks(T, Decode, Tables, Acc1).

decode_chunk(atoms, <<_:32, A/binary>>, _) -> decode_atoms(A, 1, []);
decode_chunk(funs, <<_:32, F/binary>>, Tables) -> decode_funs(F, Tables, []);
decode_chunk(line, L, _) -> decode_line(L);
decode_chunk(literals, <<_:32, L/binary>>, _) -> decode_literals(L);
decode_chunk(type_info, <<V:32, _:32, T/binary>>, _) -> decode_type_info(V, T);
decode_chunk(strings, S, _) -> S;
decode_chunk(code, C, Tables) -> decode_code(C, Tables);
decode_chunk(Name, Content, Tables) ->
    case encoding(Name) of
        binary -> binary_to_term(Content);
        func_table -> decode_table(Name, Content, Tables);
        other -> ok
    end.

encoding(Name) ->
    case lists:member(Name, ?BINARY_ENCODED) of
        true -> binary;
        false ->
            case lists:member(Name, ?FUNC_TABLE_ENCODED) of
                true -> func_table;
                false -> other
            end
    end.

decode_type_info(Version, Table) ->
    case beam_types:convert_ext(Version, Table) of
        none -> none;
        Table1 -> decode_type_info(Table1, 0, [])
    end.

decode_type_info(Table, N, Acc) ->
    case beam_types:decode_ext(Table) of
        done -> lists:reverse(Acc);
        {Type, Table1}  ->
            decode_type_info(Table1, N + 1, [{N, Type} | Acc])
    end.

%% -if(?OTP_RELEASE >= 28).

%% read_atom(<<Size:4, 0:1, _:3, T/binary>>) ->
%%     <<Atom:Size/binary, T1/binary>> = T,
%%     {Atom, T1};
%% read_atom(<<High:3, 0:1, 1:1, _:3, Low:8, T/binary>>) ->
%%     Size = (High bsl 8) bor Low,
%%     <<Atom:Size/binary, T1/binary>> = T,
%%     {Atom, T1}.

%% write_atom(Atom, T) ->
%%     case byte_size(Atom) of
%%         Size when Size =< 15 ->
%%             <<Size:4, 0:1, 0:3, Atom:Size/binary, T/binary>>;
%%         Size when Size =< 2047 ->
%%             <<_:5, High:3, Low:8>> = <<Size:16>>,
%%             <<High:3, 0:1, 1:1, 0:3, Low:8, Atom:Size/binary, T/binary>>
%%     end.

%% -else.

%% read_atom(<<Size:8, T/binary>>) ->
%%     <<Atom:Size/binary, T1/binary>> = T,
%%     {Atom, T1}.

%% write_atom(Atom, T) ->
%%     Size = byte_size(Atom),
%%     <<Size:8, Atom:Size/binary, T/binary>>.

%% -endif.

decode_atoms(<<>>, _, Acc) -> lists:reverse(Acc);
decode_atoms(<<Len, A:Len/binary, T/binary>>, N, Acc) ->
    decode_atoms(T, N + 1, [{N, binary_to_atom(A, utf8)} | Acc]).

decode_literals(L) ->
    <<_:32, T/binary>> = zlib:uncompress(L),
    decode_literals(T, 0, []).

decode_literals(<<>>, _, Acc) -> lists:reverse(Acc);
decode_literals(<<Size:32, Literal:Size/binary, T/binary>>, N, Acc) ->
    decode_literals(T, N + 1, [{N, binary_to_term(Literal)} | Acc]).

decode_table(Name, <<_:32, Table/binary>>, Tables) ->
    decode_table(Table, Name, Tables, []).

decode_table(<<>>, _, _, Acc) -> Acc;
decode_table(<<F:32, A:32, L:32, T/binary>>, Name, Tables, Acc) ->
    decode_table(T, Name, Tables, [decode_entry(Name, F, A, L, Tables) | Acc]).

decode_entry(exports, F, A, L, Tables) -> {atom(F, Tables), A, L};
decode_entry(imports, M, F, A, Tables) -> {atom(M, Tables), atom(F, Tables), A};
decode_entry(locals, F, A, L, Tables) -> {atom(F, Tables), A, L}.

decode_funs(<<>>, _, Acc) -> lists:reverse(Acc);
decode_funs(<<F:32, A:32, L:32, I:32, Free:32, U:32,T/binary>>, Tables, Acc) ->
    decode_funs(T, Tables, [{atom(F, Tables), A, L, I, Free, U} | Acc]).

decode_line(<<V:32, B:32, _:32, NumLines:32, _:32, LineTable/binary>>) ->
    {L, F} = decode_line(LineTable, 0, NumLines, 0, []),
    #{version => V, bits => B, lines => L, function_names => decode_fnames(F)}.

decode_fnames(F) -> decode_fnames(F, []).

decode_fnames(<<>>, Acc) -> lists:reverse(Acc);
decode_fnames(<<Size:16, F:Size/binary, T/binary>>, Acc) ->
    decode_fnames(T, [F | Acc]).

decode_line(T, _, NumLines, NumLines, Acc) -> {lists:reverse(Acc), T};
decode_line(T = <<_:5, _:1, ?TAG_I:2, _/binary>>, F, NumLines, C, Acc) ->
    {{_, Num}, T1} = decode_int(i, T),
    decode_line(T1, F, NumLines, C + 1, [{F, Num} | Acc]);
decode_line(T = <<_:5, _:1, ?TAG_A:2, _/binary>>, _, NumLines, C, Acc) ->
    {{_, Num}, T1} = decode_int(a, T),
    decode_line(T1, Num, NumLines, C, Acc).

decode_code(<<SubSize:32, Info:SubSize/binary, Code/binary>>, Tables) ->
    <<InstrSet:32, OpcodeMax:32, NoLabels:32, NoFuns:32>> = Info,
    #{instruction_set => InstrSet,
      opcode_max => OpcodeMax,
      number_of_labels => NoLabels,
      number_of_functions => NoFuns,
      code => gather_funs(decode_code(Code, Tables, []), [], [])
     }.

decode_code(<<>>, _, Acc) -> lists:reverse(Acc);
decode_code(<<H, T/binary>>, Tables, Acc) ->
    {Op, Arity} = beam_opcodes:opname(H),
    {I, T2} =
        case instr_type(Op) of
            select -> decode_select(Op, T, Tables);
            map -> decode_map(Op, Arity, T, Tables);
            index -> decode_index(Op, T, Tables);
            generic ->
                {Args, T1} = decode_args(Arity, T, Tables),
                {{Op, Args}, T1}
        end,
    decode_code(T2, Tables, [I | Acc]).

instr_type(select_val) -> select;
instr_type(select_tuple_arity) -> select;
instr_type(put_map_assoc) -> map;
instr_type(put_map_exact) -> map;
instr_type(get_map_elements) -> map;
instr_type(has_map_fields) -> map;
instr_type(put_tuple2) -> index;
instr_type(make_fun3) -> index;
instr_type(init_yregs) -> index;
instr_type(bs_create_bin) -> index;
instr_type(bs_match) -> index;
instr_type(update_record) -> index;
instr_type(_) -> generic.

decode_select(I, T, Tables) ->
    {X, T1} = decode_arg(T, Tables),
    {F, T2} = decode_arg(T1, Tables),
    {Z, T3} = decode_arg(T2, Tables),
    {U, T4} = decode_arg(T3, Tables),
    {u, Len} = U,
    {List, T5} = decode_args(Len, T4, Tables),
    {{I, [X, F, {Z, U, List}]}, T5}.

decode_map(I, Arity, T, Tables) ->
    {Args, T1} = decode_args(Arity, T, Tables),
    [Z | Args1]  = lists:reverse(Args),
    Args2 = lists:reverse(Args1),
    {U, T2} = decode_arg(T1, Tables),
    {u, Len} = U,
    {List, T3} = decode_args(Len, T2, Tables),
    {{I, Args2 ++ [{Z, U, List}]}, T3}.

decode_index(put_tuple2, T, Tables) ->
    {X, T1} = decode_arg(T, Tables),
    {Z, T2} = decode_arg(T1, Tables),
    {U, T3} = decode_arg(T2, Tables),
    {u, Len} = U,
    {List, T4} = decode_args(Len, T3, Tables),
    {{put_tuple2, [X, {Z, U, List}]}, T4};
decode_index(make_fun3, T, Tables) ->
    {Fun, T1} = decode_arg(T, Tables),
    {Dst, T2} = decode_arg(T1, Tables),
    {Z, T3} = decode_arg(T2, Tables),
    {U, T4} = decode_arg(T3, Tables),
    {u, Len} = U,
    {List, T5} = decode_args(Len, T4, Tables),
    {{make_fun3, [Fun, Dst, {Z, U, List}]}, T5};
decode_index(init_yregs, T, Tables) ->
    {Z, T1} = decode_arg(T, Tables),
    {U, T2} = decode_arg(T1, Tables),
    {u, Len} = U,
    {List, T3} = decode_args(Len, T2, Tables),
    {{init_yregs, [{Z, U, List}]}, T3};
decode_index(bs_create_bin, T, Tables) ->
    {A1, T1} = decode_arg(T, Tables),
    {A2, T2} = decode_arg(T1, Tables),
    {A3, T3} = decode_arg(T2, Tables),
    {A4, T4} = decode_arg(T3, Tables),
    {A5, T5} = decode_arg(T4, Tables),
    {Z, T6} = decode_arg(T5, Tables),
    {U, T7} = decode_arg(T6, Tables),
    {u, Len} = U,
    {List, T8} = decode_args(Len, T7, Tables),
    {{bs_create_bin, [{A1, A2, A3, A4, A5, Z, U, List}]}, T8};
decode_index(bs_match, T, Tables) ->
    {A1, T1} = decode_arg(T, Tables),
    {A2, T2} = decode_arg(T1, Tables),
    {Z, T3} = decode_arg(T2, Tables),
    {U, T4} = decode_arg(T3, Tables),
    {u, Len} = U,
    {List, T5} = decode_args(Len, T4, Tables),
    {{bs_match, [{A1, A2, Z, U, List}]}, T5};
decode_index(update_record, T, Tables) ->
    {Hint, T1} = decode_arg(T, Tables),
    {Size, T2} = decode_arg(T1, Tables),
    {Src, T3} = decode_arg(T2, Tables),
    {Dst, T4} = decode_arg(T3, Tables),
    {Z, T5} = decode_arg(T4, Tables),
    {U, T6} = decode_arg(T5, Tables),
    {u, Len} = U,
    {List, T7} = decode_args(Len, T6, Tables),
    {{update_record, [Hint, Size, Src, Dst, {{Z, U, List}}]}, T7}.

decode_args(N, T, Tables) -> decode_args(N, T, Tables, []).

decode_args(0, T, _, Acc) -> {lists:reverse(Acc), T};
decode_args(N, T, Tables, Acc) ->
    {A, T1} = decode_arg(T, Tables),
    decode_args(N - 1, T1, Tables, [A | Acc]).

decode_arg(T) -> decode_arg(T, #tables{}).

decode_arg(T = <<_:5, ?TAG_U:3, _/binary>>, _) -> decode_int(u, T);
decode_arg(T = <<_:5, ?TAG_I:3, _/binary>>, _) -> decode_int(i, T);
decode_arg(T = <<_:5, ?TAG_X:3, _/binary>>, _) -> decode_int(x, T);
decode_arg(T = <<_:5, ?TAG_Y:3, _/binary>>, _) -> decode_int(y, T);
decode_arg(T = <<_:5, ?TAG_F:3, _/binary>>, _) -> decode_int(f, T);
decode_arg(T = <<_:5, ?TAG_H:3, _/binary>>, _) -> decode_int(h, T);
decode_arg(<<?TAG_FLOAT:4, 0:1, ?TAG_Z:3, T/binary>>, _) -> decode_float(T);
decode_arg(<<?TAG_LIST:4, 0:1, ?TAG_Z:3, T/binary>>, _) -> {{z, 1}, T};
decode_arg(<<?TAG_FR:4, 0:1, ?TAG_Z:3, T/binary>>, _) -> decode_fr(T);
decode_arg(<<?TAG_ALLOC_LIST:4, 0:1, ?TAG_Z:3, T/binary>>, _) ->
    decode_alloc_list(T);
decode_arg(<<?TAG_LITERAL:4, 0:1, ?TAG_Z:3, T/binary>>, Tables) ->
    {{u, I}, T1} = decode_arg(T),
    case literal(I, Tables) of
        Float when is_float(Float) -> {{float, Float}, T1};
        Literal -> {{literal, Literal}, T1}
    end;
decode_arg(<<?TAG_TYPE_TAGGED:4, 0:1, ?TAG_Z:3, T/binary>>, Tables) ->
    decode_tr(T, Tables);
decode_arg(T = <<_:5, ?TAG_A:3, _/binary>>, Tables) ->
    case decode_int(a, T) of
        {{a, 0}, T1} -> {nil, T1};
        {{a, Idx}, T1} -> {{atom, atom(Idx, Tables)}, T1}
    end.

decode_int(Tag, <<N:4, 0:1, _:3, T/binary>>) -> {{Tag, N}, T};
decode_int(Tag, <<I:3, 1:2, _:3, H, T/binary>>) ->
    <<N:11>> = <<I:3, H>>,
    {{Tag, N}, T};
decode_int(Tag, <<7:3, _:5, T/binary>>) ->
    {{u, L}, T1} = decode_arg(T),
    decode_int_large(Tag, L + 9,  T1);
decode_int(Tag, <<I:3, _:5, T/binary>>) ->
    decode_int_large(Tag, I + 2, T).

decode_int_large(i, Len, T = <<1:1, _/bits>>) ->
    <<N:(8 * Len)/integer, T1/binary>> = T,
    {{i, decode_negative(N, Len)}, T1};
decode_int_large(Tag, Len, T) ->
    <<N:(8 * Len)/integer, T1/binary>> = T,
    {{Tag, N}, T1}.

decode_negative(N, Len) -> N - (1 bsl (Len * 8)).

decode_float(<<Float:64/float, T>>) -> {{float, Float}, T}.

decode_tr(T, Tables) ->
    {Reg, T1} = decode_arg(T),
    {{u, I}, T2} = decode_arg(T1),
    {{tr, [Reg, type_info(I, Tables)]}, T2}.

decode_fr(T) ->
    {{u, Fr}, T1} = decode_arg(T),
    {{fr, Fr}, T1}.

decode_alloc_list(T) ->
    {{u, N}, T1} = decode_arg(T),
    decode_alloc_list(N, T1, []).

decode_alloc_list(0, T, Acc) -> {{u, {alloc, lists:reverse(Acc)}}, T};
decode_alloc_list(N, T, Acc) ->
    {{u, Type}, T1} = decode_arg(T),
    {{u, Val}, T2} = decode_arg(T1),
    TypesName =
        case Type of
            0 -> words;
            1 -> floats;
            2 -> funs
        end,
    decode_alloc_list(N - 1, T2, [{TypesName, Val} | Acc]).

atom(I, #tables{atoms = Atoms}) -> gb_trees:get(I, Atoms).
literal(I, #tables{literals = Literals}) -> gb_trees:get(I, Literals).
type_info(I, #tables{type_info = TypeInfo}) -> gb_trees:get(I, TypeInfo).

gather_funs([H = {label, _} | T], Pre, Acc) -> gather_funs(T, [H | Pre], Acc);
gather_funs([H = {line, _} | T], Pre, Acc) -> gather_funs(T, [H | Pre], Acc);
gather_funs([Func = {func_info, [_, F, A]},Label = {label,[E]} | T], Pre,Acc) ->
    gather_funs(T, {info, F, A, E}, [Label, Func | Pre], Acc).

gather_funs(T = [{func_info, [_, _, _]} | _], Func, I, Acc) ->
    {info, {atom, F}, {u, A}, {u, E}} = Func,
    gather_funs(T, [], [{function, F, A, E, lists:reverse(I)} | Acc]);
gather_funs([{int_code_end, _}], Func, I, Acc) ->
    {info, {atom, F}, {u, A}, {u, E}} = Func,
    lists:reverse([{function, F, A, E, lists:reverse(I)} | Acc]);
gather_funs([H | T], Func, I, Acc) ->
    gather_funs(T, Func, [H | I], Acc).

depends_on(code) -> [atoms, literals, type_info];
depends_on(exports) -> [atoms];
depends_on(imports) -> [atoms];
depends_on(locals) -> [atoms];
depends_on(funs) ->  [atoms];
depends_on(_) -> [].

has_locals(<<>>) -> false;
has_locals(<<Name:4/binary, Sz:32, T/binary>>) ->
    Pad = align_by_four(Sz) - Sz,
    <<_:Sz/binary, _:Pad/binary, T1/binary>> = T,
    case decode_chunk_name(Name) of
        locals -> true;
        _ -> has_locals(T1)
    end.

add_locals(BEAM) ->
    #{code := #{decoded := Code},
      exports := #{decoded := Exp},
      funs := #{decoded := Funs}} = BEAM,
    Locals = find_locals(Code, Exp, Funs, []),
    BEAM#{locals => #{decoded => Locals}}.

find_locals([], _, _, Acc) -> lists:reverse(Acc);
find_locals([{function, F, A, _, _} | T], Exp, Funs, Acc) ->
    case member(F, A, Exp) orelse member(F, A, Funs) of
        true -> find_locals(T, Exp, Funs, Acc);
        false ->
            [{label, [{u, L}]} | T1] = T,
            find_locals(T1, Exp, Funs, [{F, A, L} | Acc])
    end.

member(_, _, []) -> false;
member(F, A, [{F, A, _} | _]) -> true;
member(F, A, [_ | T]) -> member(F, A, T).

%%--------------------------------------------------------------------
%% Encode
%%--------------------------------------------------------------------

build_beam(_Mod, _Chunks, _BEAM) ->
    <<>>.

assemble_beam(_Chunks, _BEAM) -> <<>>.

%% build_chunks(Chunks) -> [build_chunk(Name, Chunk) || Name := Chunk <- Chunks].

%% build_chunk(Name, #{new := New}) ->
%%     Chunk = encode_chunk(Name, New),
%%     Size = byte_size(Chunk),
%%     <<Name/binary, Size:32, Chunk/binary, 0:((Size rem 4) * 8)>>;
%% build_chunk(Name, #{original := Orig}) ->
%%     Size = byte_size(Orig),
%%     <<Name/binary, Size:32, Orig/binary, 0:((Size rem 4) * 8)>>.

%% encode_chunk(atoms, Atoms) ->
%%     Table = << <<(byte_size(B)):32, B>> ||
%%                 B <- [atom_to_binary(A) || {_, A} <- lists:sort(Atoms)] >>,
%%     <<"AtU8", (length(Atoms)):32, Table/binary>>;
%% encode_chunk(literals, Literals) ->
%%     Table = << <<(byte_size(B)):32, B/binary>> ||
%%                 B <- [binary_to_term(L) || {_, L} <- lists:sort(Literals)] >>,
%%     Bin = zlib:compress(Table),
%%     <<"LitT", (byte_size(Bin)):32, Bin/binary>>;
%% encode_chunk(strings, Strings) ->
%%     <<"StrT", (byte_size(Strings)):32, Strings/binary>>;
%% encode_chunk(Name, Chunk) ->
%%     case encoding(Name) of
%%         binary -> term_to_binary(Chunk);
%%         func_table -> encode_table(Name, Chunk);
%%         other -> erlang:error({unknown_chunk, Name})
%%     end.

%% encode_table(Name, _) -> encode_chunk_name(Name).

%% encode_chunk_name(atoms) -> <<"AtU8">>;
%% encode_chunk_name(exports) -> <<"ExpT">>;
%% encode_chunk_name(imports) -> <<"ImpT">>;
%% encode_chunk_name(code) -> <<"Code">>;
%% encode_chunk_name(strings) -> <<"StrT">>;
%% encode_chunk_name(attributes) -> <<"Attr">>;
%% encode_chunk_name(compile_info) -> <<"CInf">>;
%% encode_chunk_name(locals) -> <<"LocT">>;
%% encode_chunk_name(literals) -> <<"LitT">>;
%% encode_chunk_name(abstract_code) -> <<"Abst">>;
%% encode_chunk_name(line) -> <<"Line">>;
%% encode_chunk_name(funs) -> <<"FunT">>;
%% encode_chunk_name(type_info) -> <<"Type">>;
%% encode_chunk_name(debug_info) -> <<"Dbgi">>;
%% encode_chunk_name(docs) -> <<"Docs">>;
%% encode_chunk_name(meta) -> <<"Meta">>.
