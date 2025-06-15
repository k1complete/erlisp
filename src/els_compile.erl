-module(els_compile).

-include_lib("els.hrl").
-include_lib("els_docs.hrl").
-export([file/2, file/1, file_ast/2
        ]).

-spec formcompile(list(), list(), list()) -> {erl_syntax:syntaxTree(), list(), list()}.
formcompile(Form, Errors, Env) ->
    io:format("PRE: ~p~nEnv:(~p)~n", [Form, Env]),
    R = els_transpile:form(Form, Env),
    M = proplists:get_value(macros, Env),
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
				io:format("LocalFDic ~p~n",
					  [FunDic]),
				FunDic
			end,
	    OEnv = proplists:delete(macros, Env),
	    NewEnv = [{macros, NewMacros}|OEnv],
	    {R, {Errors, NewEnv}};
	_ ->
	    {R, {Errors, Env}}
    end.


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
					      formcompile(F, A, E)
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
    {ok, Module, _Binary, Ast} = file(File, Opt),
    {module, Module, Binary2} = compile_and_write_beam(Ast, Opt),
    {ok, Module, Binary2, Ast}.
%    {ok, Module, Binary, Ast}.


-spec extract_specs(list(erl_syntax:tree())) -> map().
extract_specs(Trees) ->
    R = lists:filtermap(fun(E) ->
                                case erl_syntax:type(E) == attribute andalso 
				    erl_syntax:atom_name(erl_syntax:attribute_name(E)) == "spec" of
				    true ->
					{attribute, _, spec, {FA, S}} = E,
					{true, {FA, S}};
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
    %% Name=erl_syntax:atom_value(FName),
    Cs = erl_syntax:function_clauses(Tree),
    io:format("make_function_signature ~p~nCs: ~p~n", [Specs, Cs]),
    %% Arity = length(erl_syntax:clause_patterns(hd(Cs))),
    R = lists:map(fun(C) ->
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

