-module(els_compile).

-include_lib("els.hrl").
-include_lib("els_docs.hrl").
-export([file/2, file/1, file_ast/2
        ]).

-spec file(string()) -> {ok, module(), binary()}.
file(File) ->
    file(File, []).

-spec file(string(), list()) -> {ok, module(), binary()}.
file(File, Opt) ->
    io:format("cwd ~p", [file:get_cwd()]),
    Module = m,
    {ok, Tokens} = els_scan:file(File, Opt),
    io:format("scan ~p", [Tokens]),
    {ok, Forms} = els_parser:parse(Tokens),
    Env=[],
    Ast = lists:map(fun(F) ->
                            R = els_transpile:form(F, Env),
                            io:format("Trans ~p~n", [R]),
                            R
                          end, Forms),
    io:format("Ast ~p~n", [Ast]),
    {ok, Binary} = merl:compile_and_load(Ast, [debug_info]),
    io:format("compiled ~p", [Binary]),
    {ok, Module, Binary}.


-spec compile_macro(sexp(), env()) -> sexp().
%% フォーム一つをトランスパイル
%% ASTをコンパイルしてmoduleに追加
%%  > macroはマクロリストに登録
%% 終りまでいったら、終了
%% 
compile_macro(A, E) ->
    [{MS, ModuleForm}] = lists:filtermap(fun([#item{type=atom, value="-module"}|R]=L) -> 
                                                 {true, 
                                                  {L, [#item{type=atom, value="quote"},  hd(R)]}};
                                            (_) -> false 
                                         end, A),
    [MS21,MS22 | _] = MS,
    ModuleName = erl_syntax:atom_value(els_transpile:form(ModuleForm, E)),
    io:format("modulename ~p ~p~n", [ModuleName, is_atom(ModuleName)]),
    M = lists:filtermap(fun([#item{type=atom, value="defmacro"}|R]) -> 
                                [#item{type=atom, value=MacroName}, Args| _Body] = R,
                                Macro = MacroName,
                                MacroFunc = yal_util:make_macro_funcname(Macro),
                                {true, {{Macro, length(Args)}, {ModuleName, MacroFunc}}};
                           (_) -> 
                                false 
                        end, A),
    Forms2 = lists:filter(fun([#item{type=atom, value="-export"}|_]) -> 
                                  false;
                             ([#item{type=atom, value="-macro_export"}|_]) -> 
                                  false;
                             ([#item{type=atom, value="-module"}|_]) -> 
                                  false;
                             ([#item{type=atom, value="-spec"}|_]) -> 
                                  false;
                             (_) -> 
                                  true
                          end, A),
    IEnv = els_transpile:merge_into_env(E, macros, maps:from_list(M)),
    Ret = lists:foldl(fun(S, {_Ret, [], EnvAct}) ->
                              Forms = [[MS21, MS22]]++[S], 
                              Ast = lists:map(fun(F) ->
                                                      els_transpile:form(F, EnvAct)
                                              end, Forms),
                              io:format("2222 ~p~n~p", [Forms, erl_syntax:revert(Ast)]),
                              {module, _Module, Binary} = 
                                  compile_and_write_beam(Ast, [debug_info, export_all]),
                              R = catch apply(ModuleName, main, [2,3]),
                              ?LOG_ERROR(#{module_info => R, length => length(Forms2)}),
                              {Binary, Forms, EnvAct};
                          (S, {_Ret, Acc, EnvAcc}) ->
                              Macros = els_transpile:getmacros_from_module(ModuleForm, EnvAcc),
                              io:format("merged macro1 ~p ~p", 
                                        [EnvAcc, maps:from_list(Macros)]),
                              NEnv = els_transpile:merge_into_env(EnvAcc, macros, maps:from_list(Macros)),
                              io:format("merged macro2 ~p", [NEnv]),
                              Forms = Acc++[S], 
                              io:format("merged macro3 ~p~n", [NEnv]),
                              Ast = lists:map(fun(F) ->
                                                      els_transpile:form(F, NEnv)
                                              end, Forms),
                              io:format("transpiled ~p~n", [Ast]),
                              {module, _Module, Binary} = 
                                  compile_and_write_beam(Ast, [debug_info, export_all]),
                              R = catch apply(ModuleName, module_info, [exports]),
                              ?LOG_ERROR(#{module_info2 => R}),
                              {Binary, Forms, NEnv}
                  end, {[], [], IEnv}, Forms2),
    ?LOG_ERROR(#{maros_list => IEnv}),
    Ret.

-spec compile_and_write_beam(sexp(), options()) -> {module, module(), binary()}.
compile_and_write_beam(Ast, Options) ->
    SS = merl:compile_and_load(Ast, Options),
    ?LOG_ERROR(#{compile2 => erl_syntax:revert(Ast), options=>Options, ss => SS}),
    {ok, Binary} =SS,
    Specs = extract_specs(Ast),
    {ok, DocsV1} = make_docs(Ast, Specs),
    io:format("compiled ~p", [Binary]),
    {ok, Module, Chunks} = beam_lib:all_chunks(Binary),
    ChunksAdded = lists:append(Chunks, [{"Docs", term_to_binary(DocsV1)}]),
    {ok, Binary2} = beam_lib:build_module(ChunksAdded),
    ModuleName = atom_to_list(Module),
    File = ModuleName ++ ".beam",
    file:write_file(File, Binary2),
    code:ensure_loaded(Module),
    {module, Module, Binary2}.

-spec file_ast(string, options()) -> {module, module(), binary(), sexp()}.
file_ast(File, Opt) ->
    io:format("cwd ~p", [file:get_cwd()]),
    {ok, Tokens} = els_scan:file(File, Opt),
    io:format("scan ~p", [Tokens]),
    {ok, Forms} = els_parser:parse(Tokens),
    {_, _, NEnv} = compile_macro(Forms, []),
    MR = Forms,
    Env=NEnv,
    ?LOG_ERROR(#{macro_compiled => MR, nenv => NEnv}),
    Ast = lists:map(fun(F) ->
                            R = els_transpile:form(F, Env),
                            io:format("Trans ~p~n", [R]),
                            R
                          end, Forms),
    io:format("Ast ~p~n", [Ast]),
%    {ok, Binary} = merl:compile_and_load(Ast, [debug_info]),
    {ok, Module, Binary} = merl:compile(Ast, [debug_info]),
    Specs = extract_specs(Ast),
    {ok, DocsV1} = make_docs(Ast, Specs),
    io:format("compiled ~p", [Binary]),
    {ok, Module, Chunks} = beam_lib:all_chunks(Binary),
    ChunksAdded = lists:append(Chunks, [{"Docs", term_to_binary(DocsV1)}]),
    io:format("Beam ~p", [ChunksAdded]),
    {ok, Binary2} = beam_lib:build_module(ChunksAdded),
    {ok, Module, Binary2, Ast}.

-spec extract_specs(list(erl_syntax:tree())) -> map().
extract_specs(Trees) ->
    R = lists:filtermap(fun(E) ->
                                case erl_syntax:type(E) of
                                    attribute ->
                                        case erl_syntax:atom_name(erl_syntax:attribute_name(E)) of
                                            "spec" ->
                                                {attribute, _, spec, {FA, S}} = E,
                                                {true, {FA, hd(S)}};
                                            _ ->
                                                false
                                        end;
                                    _ ->
                                        false
                                end
                        end, Trees),
    maps:from_list(R).

-spec extract_module_comment(erl_syntax:tree()) -> map() | none.
extract_module_comment(Tree) ->
    case erl_syntax:has_comments(Tree) of
        true ->
            CommentTrees = erl_syntax:get_postcomments(Tree),
            C = erl_syntax:comment_text(hd(CommentTrees)),
            #{<<"en">> => list_to_binary(C)};
        false ->
            none
    end.

-spec make_function_signature(erl_syntax:tree(), map()) -> signature().
make_function_signature(Tree, Specs) ->
    Name=erl_syntax:atom_value(erl_syntax:function_name(Tree)),
    Cs = erl_syntax:function_clauses(Tree),
    R = lists:map(fun(C) ->
                          Patterns = 
                              lists:map(fun(E) ->
                                                els_ast:to_list(E)
                                        end, erl_syntax:clause_patterns(C)),
                          Guard = case erl_syntax:clause_guard(C) of 
                                      none -> [];
                                      X -> X
                                  end,
                          S = 
                              unicode:characters_to_binary(els_pp:pp(els_pp:erl_to_ast([Name, Patterns, Guard])), utf8),
                          case maps:get({Name, length(Patterns)}, Specs, none) of
                              none ->
                                  S;
                              Spec ->
                                  N = [list_to_binary(els_typespec:fun_to_string(Name, Spec))],
                                  case N of
                                      [] ->
                                          S;
                                      N ->
                                          [S| N]
                                  end
                          end
                  end, Cs),
    lists:flatten(R).

-spec extract_comment(erl_syntax:tree(), kind(), map()) -> doc_entry().
extract_comment(Tree, Kind, Specs) ->
    case erl_syntax:has_comments(Tree) of
        true ->
            els_docs:make_docentry(Kind, 
                                     erl_syntax:atom_value(erl_syntax:function_name(Tree)),
                                     erl_syntax:function_arity(Tree),
                                     erl_syntax:get_pos(Tree),
                                     make_function_signature(Tree, Specs),
                                     #{<<"en">> => 
                                           list_to_binary(erl_syntax:get_precomments(Tree))}, 
                                     #{});
        false ->
            none
    end.
-spec make_docs([sexp()], map()) -> {ok, docs_v1()}.
make_docs(AstList, Specs) ->
    S=lists:foldr(fun(Ast, Acc) ->
                          Doc=maps:get(docs, Acc),
                          case erl_syntax:type(Ast) of
                            function ->
                                  E = extract_comment(Ast, function, Specs),
                                  Acc#{docs=> els_docs:add_docentry(Doc, E)};
                            attribute  ->
                                case erl_syntax:atom_value(erl_syntax:attribute_name(Ast)) of
                                    module ->
                                        E = els_docs:make_docs_v1(erl_syntax:get_pos(Ast),
                                                                     <<"text/markdown">>,
                                                                     extract_module_comment(Ast),
                                                                     #{},
                                                                     []),
                                        Acc#{docs=> els_docs:set_moduledoc(Doc, E)};
                                    _Other -> 
                                        %%[{_Other, Ast}|Acc]
                                        Acc
                                end;
                              _  ->
                                  O=maps:get(other, Acc),
                                  Acc#{other=>[{s,Ast}|O]}
                          end
                  end, #{docs=> els_docs:new_docs_v1(), other=>[]}, AstList),
    SS = maps:get(docs, S),
    {ok, SS}.

