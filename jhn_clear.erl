-module(jhn_clear).

-export([export_all/1, disasm/1]).

-include_lib("compiler/src/beam_opcodes.hrl").

-define(EXPORTS_KEY, "ExpT").
-define(CODE_KEY, "CODE").
-define(ATTR_KEY, "Attr").
-define(UUT, "jhn_clear Unit Under Test").

export_all(Mod) ->
    {_, B, _} = code:get_object_code(Mod),
    {ok, _, Cs} = beam_lib:all_chunks(B),
    Cs1 = lists:key_delete(?ATTR_KEY, 1, lists:key_delete(?EXPORTS_KEY, 1, Cs)),
    [Atoms, Exported, Attr] = chunks(B, [atoms, labeled_exports, attributes]),
    {Exported1, Len} = find_funcs(B, Atoms, Exported),
    AttrChunk = binary_to_term([{labeled_exports, Exported} | Attr]),
    ExptChunk = <<Len:32, (flatten_exports(Exported1))/binary>>,
    {ok, B1} = beam_lib:build_module([{?EXPORTS_KEY, ExptChunk},
                                      {?ATTR_KEY, AttrChunk} | Cs1]),
    {module, _} = code:load_binary(Mod, ?UUT, B1).

flatten_exports(Exps) -> << <<F:32, A:32, L:32>> || {F, A, L} <- Exps >>.

chunks(B, Chunks) ->
    case beam_lib:chunks(B, Chunks) of
        {ok, {_, Tuples}} -> [Chunk || {_, Chunk} <- Tuples];
        Error -> Error
    end.

find_funcs(B, Atoms, Exported) ->
    case chunks(B, [labeled_locals]) of
        [Locals] ->
            All = [{index(N, Atoms), A, L} || {N, A, L} <- Locals ++ Exported],
            {All, length(All)};
        _ ->
            [<<_:160, Code/binary>>] = chunks(B, ["Code"]),
            Funcs = disasm(Code, Atoms, []),
            {Funcs, length(Funcs)}
    end.

index(Name, Atoms) -> {Index, _} = lists:keyfind(Name, 2, Atoms), Index.

disasm(Mod) ->
    {_, B, _} = code:get_object_code(Mod),
    [Atoms, <<_:160, Bin/binary>>] = chunks(B, [atoms, "Code"]),
    disasm(Bin, Atoms, []).

disasm(<<>>, _, Acc) -> lists:reverse(Acc);
disasm(<<OP, T/binary>>, Atoms, Acc) ->
    case beam_opcodes:opname(OP) of
        {func_info, 3} -> 
            T1 = skip_arg(T),
            {N2, T2} = decode_int(T1),
            {N3, <<H, T3/binary>>} = decode_int(T2),
            {label, 1} = beam_opcodes:opname(H),
            {N4, T4} = decode_int(T3),
            case {lists:keyfind(N2, 1, Atoms), N3} of
                {{_, module_info}, 1} ->
                    lists:reverse([{N2, N3, N4} | Acc]);
                _ ->
                    disasm(T4, Atoms, [{N2, N3, N4} | Acc])
            end;
        {select_val, _} ->
            disasm(skip_depend(3, T), Atoms, Acc);
        {select_tuple_arity,_} ->
            disasm(skip_depend(3, T), Atoms, Acc);
        {put_map_assoc, Arity} ->
            disasm(skip_depend(Arity, T), Atoms, Acc);
        {put_map_exact, Arity} ->
            disasm(skip_depend(Arity, T), Atoms, Acc);
        {get_map_elements, Arity} ->
            disasm(skip_depend(Arity, T), Atoms, Acc);
        {has_map_fields, Arity} ->
            disasm(skip_depend(Arity, T), Atoms, Acc);
        {put_tuple2, _} ->
            disasm(skip_depend(2, T), Atoms, Acc);
        {make_fun3, _} ->
            disasm(skip_depend(3, T), Atoms, Acc);
        {init_yregs, _} ->
            disasm(skip_depend(1, T), Atoms, Acc);
        {bs_create_bin, _} ->
            disasm(skip_depend(6, T), Atoms, Acc);
        {bs_match, _}  ->
            disasm(skip_depend(3, T), Atoms, Acc);
        {update_record, _} ->
            disasm(skip_depend(5, T), Atoms, Acc);
        {_, Arity} ->
            T1 = skip_args(Arity, T),
            disasm(T1, Atoms, Acc)
    end.

skip_depend(Skip, T) ->
    T1 = skip_args(Skip, T),
    {Len, T2} = decode_int(T1),
    skip_args(Len, T2).

skip_args(0, T) -> T;
skip_args(N, T) -> skip_args(N - 1, skip_arg(T)).

skip_arg(A = <<H, T/binary>>) ->
    case H band 2#111 of                   % tag
        ?tag_z ->
            case H bsr 4 of
                0 -> skip_float(T);        % float
                1 -> T;                    % list
                2 -> skip_arg(T);          % fr
                3 -> skip_alloc_list(T);   % allocation list
                4 -> skip_arg(T);          % literal
                5 -> skip_arg(skip_arg(T)) % type-tagged register
            end;
        _ ->
            skip_int(A)
    end.

skip_float(<<_:8/binary, T/binary>>) -> T.

skip_alloc_list(T) ->
    {N, T1} = decode_int(T),
    skip_alloc_list(N, T1).

skip_alloc_list(0, T) -> T;
skip_alloc_list(N, T) -> skip_alloc_list(N - 1, skip_int(skip_int(T))).

skip_int(<<H, T/binary>>) when (H band 16#08) =:= 0 -> T;
skip_int(<<H, _, T/binary>>) when (H band 16#10) =:= 0 -> T;
skip_int(<<H, T/binary>>) ->
    {Len, T1} = decode_int_length(H, T),
    <<_:Len/binary, T2/binary>> = T1,
    T2.

decode_int_length(H, T) ->
    case H bsr 5 of
        7 ->
            {L, T1} = decode_int(T),
            {L + 9, T1};
        L ->
            {L + 2, T}
    end.

decode_int(<<H, T/binary>>) when (H band 16#08) =:= 0 -> {H bsr 4, T};
decode_int(<<H, H1, T/binary>>) when (H band 16#10) =:= 0 ->
    {((H band 2#11100000) bsl 3) bor H1, T};
decode_int(<<H, T/binary>>) ->
    {Len, T1} = decode_int_length(H, T),
    <<IntB:Len/binary, T2/binary>> = T1,
    {build_int(IntB, 0), T2}.

build_int(<<>>, N) -> N;
build_int(<<H, T/binary>>, N) ->
    build_int(T, (N bsl 8) bor H).
