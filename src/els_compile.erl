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
    io:format("cwd ~p~n", [file:get_cwd()]),
    Module = list_to_atom(filename:basename(File, ".elisp")),
    {ok, Tokens} = els_scan:file(File, Opt),
    io:format("scan ~p~n", [Tokens]),
    {ok, Forms} = els_parser:parse(Tokens),
    Env=[{macros, #{}}],
    {Ast0, {Errors, _Env}} = lists:mapfoldl(fun(F, {A, E}) ->
					  try
					      io:format("PRE: ~p~nEnv:(~p)~n", [F, E]),
					      R = els_transpile:form(F, E),
					      M = proplists:get_value(macros, E),
					      case els_localfun:register_local_func(R, M) of
						  {FunDic, Name, Arity} ->
						      LocalF = els_localfun:create_valuefun(FunDic),
						      MName = els_localfun:strip_macroname_string(Name),
						      io:format("NName(~p):Name(~p)~n", [MName, Name]),
						      AName = atom_to_list(Name),
						      NewMacros = if AName =/= MName ->
									  maps:put(
									    {MName, Arity},
									    {{local},  LocalF}, M);
								     true ->
									  M
								  end,
						      OEnv = proplists:delete(macros, E),
						      NewEnv = [{macros, NewMacros}|OEnv],
						      {R, {A, NewEnv}};
						  _ ->
						      {R, {A, E}}
					      end
					  catch
					      throw:Error when is_list(Error) ->
						  io:format("catched : ~p~n", [Error]),
						  {[],  {A ++ Error, E}}
					  end
				  end, {[], Env}, Forms),
    
    Ast = case Errors of
	      [] -> Ast0;
	      _ ->
		  throw(Errors)
	  end,
    io:format("Ast ~p~n Err ~p~n", [Ast, Errors]),
    {ok, Binary} = merl:compile_and_load(Ast, [debug_info]),
    io:format("compiled ~p~n", [Binary]),
    {ok, Module, Binary, Ast}.

old_file(File, Opt) ->
    io:format("cwd ~p~n", [file:get_cwd()]),
    Module = m,
    {ok, Tokens} = els_scan:file(File, Opt),
    io:format("scan ~p~n", [Tokens]),
    {ok, Forms} = els_parser:parse(Tokens),
    Env=[],
    {Ast0, Errors} = lists:mapfoldl(fun(F, A) ->
					  try
					      R = els_transpile:form(F, Env),
					      {R, A}
					  catch
					      throw:Error when is_list(Error) ->
						  io:format("catched : ~p~n", [Error]),
						  {[],  A++ Error}
					  end
				  end, [], Forms),
    Ast = case Errors of
	      [] -> Ast0;
	      _ ->
		  throw(Errors)
	  end,
    io:format("Ast ~p~n Err ~p~n", [Ast, Errors]),
    {ok, Binary} = merl:compile_and_load(Ast, [debug_info]),
    io:format("compiled ~p~n", [Binary]),
    {ok, Module, Binary}.


    
    
create_local_func(Name, Arity, Ast, FunDic) ->
    maps:put({Name, Arity}, Ast, FunDic).

			
merge_macro_env(MacroMap, Env) ->
    yal_util:proplists_replace(local, MacroMap, Env).

%%
%% defmacro/defunを処理して、新しいlocalfunマップを返す。
%% 
%% 
%%interprete_to_ast([#item{value=Keyword, type=atom}|T] = A, Acc, Env)
%%  when Keyword == "defmacro"; Keyword == "defun" ->
%%    Locals = prpplists:get_value(localfun, Env),
%%    LocalFun = fun() -> 
%%interprete_to_ast([#item{value=Keyword, type=atom}|T] = A, Acc, Env) ->
%%    #{value=> els_transpile:form(A, E), binding => Acc, environment => Env}.
%%
%%new_compile_macro([#item{value=Keyword, type=atom}|T] = A, Acc, E) 
%%  when Keyword == "defmacro"; Keyword == "defun" ->
%%    [#item{type=atom, value=Name}, Args|_Body] = T,
%%    Arity = length(Args),
%%    NewEnv = merge_macro_env(Acc, E),
%%    Dic = proplist:get_value(local, NewEnv, #{}),
%%    Ast = els_transpile:form(A, E),
%%    NewLocalFunMap = case els_localfun:create_local_func(Ast, Acc) of
%%			 {{Name, Arity}, Def} ->
%%			     maps:put({Name, Arity}, Def, Dic};
%%			 undef ->
%%			     Dic
%%		     end,
%%new_compile_macro(_, Acc, E) ->
%%    Acc.

-spec compile_macro(sexp(), env()) -> sexp().
%% フォーム一つをトランスパイル
%% ASTをコンパイルしてmoduleに追加
%%  > macroはマクロリストに登録
%% 終りまでいったら、終了
%% 
compile_macro(A, E) ->
    io:format("pre-compiled ~p~n", [A]),
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
    io:format("compile-macro: ~p~n", [M]),
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
    io:format("merge_env ~p ~p~n", [ModuleName, IEnv]),

    Ret = lists:foldl(fun(S, {_Ret, [], EnvAct}) ->
                              Forms = [[MS21, MS22]]++[S], 
                              Ast = lists:map(fun(F) ->
						      io:format("merge_form ~p~n", [F]),
                                                      Asst = els_transpile:form(F, EnvAct),
						      io:format("merge_ast ~p~n", [Asst]),
						      Asst
                                              end, Forms),
                              io:format("2222 ~p~n~p", [Forms, erl_syntax:revert_forms(Ast)]),
                              {module, _Module, Binary} = 
                                  compile_and_write_beam(Ast, [debug_info, export_all]),
                              R = catch apply(ModuleName, main, [2,3]),
                              ?LOG_DEBUG(#{module_info => R, length => length(Forms2)}),
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
                              ?LOG_DEBUG(#{module_info2 => R}),
                              {Binary, Forms, NEnv};
			 (S, AA) ->
			      io:format("error!!: ~p ~n~pn", [S, AA])
                  end, {[], [], IEnv}, Forms2),
    io:format("compiled-macro: ~p ~n", [Ret]),
    ?LOG_DEBUG(#{maros_list => IEnv}),
    Ret.

-spec compile_and_write_beam(sexp(), options()) -> {module, module(), binary()}.
compile_and_write_beam(Ast, Options) ->
    SS = merl:compile_and_load(Ast, Options),
    ?LOG_DEBUG(#{compile2 => erl_syntax:revert_forms(Ast), options=>Options, ss => SS}),
    {ok, Binary} =SS,
    Specs = extract_specs(Ast),
    io:format("before make_doc ~p~nAst ~p~n", [Specs, Ast]),
    {ok, DocsV1} = make_docs(Ast, Specs),
    io:format("after make_doc ~p~n", [DocsV1]),
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
    {ok, Module, Binary, Ast} = file(File, Opt),
    {module, Module, Binary2} = compile_and_write_beam(Ast, Opt),
    {ok, Module, Binary2, Ast}.
%    {ok, Module, Binary, Ast}.

old_file_ast(File, Opt) ->
    io:format("cwd ~p", [file:get_cwd()]),
    {ok, Tokens} = els_scan:file(File, Opt),
    io:format("scan ~p", [Tokens]),
    {ok, Forms} = els_parser:parse(Tokens),
    io:format("parsed ~p~n", [Forms]),
    
    Compiled = compile_macro(Forms, []),
    io:format("compiled ~p~n", [Forms]),
    {_, _, NEnv} = Compiled,
    io:format("compiled ~p", [NEnv]),
    MR = Forms,
    Env=NEnv,

    ?LOG_DEBUG(#{macro_compiled => MR, nenv => NEnv}),
    Ast = lists:map(fun(F) ->
			    els_transpile:form(F, Env) 
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
                                                {true, {FA, S}};
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

make_function_spec(Tree, Specs, MetaData) ->
    FName = erl_syntax:function_name(Tree),
    Name=erl_syntax:atom_value(FName),
    Cs = erl_syntax:function_clauses(Tree),
    io:format("make_function_signature ~p~nCs: ~p~n", [Specs, Cs]),
    Arity = length(erl_syntax:clause_patterns(hd(Cs))),
    case maps:get({Name, Arity}, Specs, none) of
	none ->
	    MetaData;
	SpecAsts ->
	    A = lists:map(fun(E) ->
				  els_typespec:variable_titled(E)
			  end, SpecAsts),
	    maps:put(spec, [{attribute, 0, spec, {{Name, Arity}, A}}], MetaData)
    end.
    
-spec make_function_signature(erl_syntax:tree(), map()) -> signature().
make_function_signature(Tree, Specs) ->
    FName = erl_syntax:function_name(Tree),
    Name=erl_syntax:atom_value(FName),
    Cs = erl_syntax:function_clauses(Tree),
    io:format("make_function_signature ~p~nCs: ~p~n", [Specs, Cs]),
    Arity = length(erl_syntax:clause_patterns(hd(Cs))),
    R = lists:map(fun(C) ->
                          Patterns = 
                              lists:map(fun(E) ->
                                                els_ast:to_list(E)
                                        end, erl_syntax:clause_patterns(C)),
                          Guard = case erl_syntax:clause_guard(C) of 
                                      none -> [];
                                      X -> X
                                  end,
			  Tc = els_typespec:variable_titled(C),
                          unicode:characters_to_binary(els_pp:erlast_to_str(FName, Tc), utf8)
                  end, Cs),
    io:format("make_function_signatureR ~p~n", [R]),
    lists:flatten(R).

-spec extract_comment(erl_syntax:tree(), kind(), map()) -> doc_entry().
extract_comment(Tree, Kind, Specs) ->
    io:format("CommentTree: ~p~n", [Tree]),
    case erl_syntax:has_comments(Tree) of
        true ->
	    CommentList = lists:flatten(erl_syntax:comment_text(erl_syntax:get_precomments(Tree))),
	    io:format("Comments: ~p~n", [CommentList]),
	    io:format("Tree: ~p~n", [{erl_syntax:function_name(Tree),
				     erl_syntax:function_arity(Tree)
				     }]),
	    MetaData = make_function_spec(Tree, Specs, #{}),
            els_docs:make_docentry(Kind, 
				   erl_syntax:atom_value(erl_syntax:function_name(Tree)),
				   erl_syntax:function_arity(Tree),
				   erl_syntax:get_pos(Tree),
				   make_function_signature(Tree, Specs),
				   #{<<"en">> => 
					 list_to_binary(CommentList)},
				   MetaData
				  );
        false ->
            none
    end.
-spec make_docs([sexp()], map()) -> {ok, docs_v1()}.
make_docs(AstList, Specs) ->
    S=lists:foldr(fun(Ast, Acc) ->
                          Doc=maps:get(docs, Acc),
			  io:format("make_docs ~p~n", [Ast]),
                          case erl_syntax:type(Ast) of
                            function ->
				  io:format("before ~p~n", [Ast]),
                                  E = extract_comment(Ast, function, Specs),
				  io:format("extracted comment ~p~n", [E]),
				  case E of
				      none ->
					  Acc;
				      _ ->
					  Acc#{docs=> els_docs:add_docentry(Doc, E)}
				  end;
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

