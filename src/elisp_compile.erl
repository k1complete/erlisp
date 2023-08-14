-module(elisp_compile).
-export([file/2, file/1, file_ast/2
        ]).

file(File) ->
    file(File, []).

file(File, Opt) ->
    io:format("cwd ~p", [file:get_cwd()]),
    Module = m,
    {ok, Tokens} = scan:file(File, Opt),
    io:format("scan ~p", [Tokens]),
    {ok, Forms} = parser:parse(Tokens),
    Env=[],
    Ast = lists:map(fun(F) ->
                            R = transpile:form(F, Env),
                            io:format("Trans ~p~n", [R]),
                            R
                          end, Forms),
    io:format("Ast ~p~n", [Ast]),
    {ok, Binary} = merl:compile_and_load(Ast, [debug_info]),
    io:format("compiled ~p", [Binary]),
    {ok, Module, Binary}.

file_ast(File, Opt) ->
    io:format("cwd ~p", [file:get_cwd()]),
    Module = m,
    {ok, Tokens} = scan:file(File, Opt),
    io:format("scan ~p", [Tokens]),
    {ok, Forms} = parser:parse(Tokens),
    Env=[],
    Ast = lists:map(fun(F) ->
                            R = transpile:form(F, Env),
                            io:format("Trans ~p~n", [R]),
                            R
                          end, Forms),
    io:format("Ast ~p~n", [Ast]),
    {ok, Binary} = merl:compile_and_load(Ast, [debug_info]),
    Specs = extract_specs(Ast),
    Ast2 = split_docs(Ast, Specs),
    io:format("compiled ~p", [Binary]),
    {ok, Module, Binary, Ast2}.

extract_specs(Trees) ->
    R = lists:filtermap(fun(E) ->
                                case erl_syntax:type(E) of
                                    attribute ->
                                        case erl_syntax:atom_name(erl_syntax:attribute_name(E)) of
                                            "spec" ->
                                                {attribute, _, spec, {FA, S}} = E,
                                                {true, {FA, hd(S)}};
                                            X ->
                                                false
                                        end;
                                    _ ->
                                        false
                                end
                        end, Trees),
    maps:from_list(R).

extract_module_comment(Tree) ->
    case erl_syntax:has_comments(Tree) of
        true ->
            CommentTrees = erl_syntax:get_postcomments(Tree),
            C = erl_syntax:comment_text(hd(CommentTrees)),
            {c, C};
        false ->
            []
    end.
    
make_function_signature(Tree, Specs) ->
    Name=erl_syntax:atom_value(erl_syntax:function_name(Tree)),
    Cs = erl_syntax:function_clauses(Tree),
    lists:map(fun(C) ->
                      Patterns = 
                          lists:map(fun(E) ->
                                            E
                                    end, erl_syntax:clause_patterns(C)),
                      Guard = erl_syntax:clause_guard(C),
                      Spec = maps:get({Name, length(Patterns)}, Specs),
                      N = elisp_typespec:fun_to_string(Name, Spec),
                      {Name, Patterns, Guard, Spec, N}
              end, Cs).

extract_comment(Tree, Kind, Specs) ->
    case erl_syntax:has_comments(Tree) of
        true ->
            {{Kind, 
              erl_syntax:atom_name(erl_syntax:function_name(Tree)),
              erl_syntax:function_arity(Tree)},
             erl_syntax:get_pos(Tree),
             {spec, make_function_signature(Tree, Specs)},
             erl_syntax:get_precomments(Tree), 
             []};
        false ->
            []
    end.
    
split_docs(AstList, Specs) ->
    S=lists:foldr(fun(Ast, Acc) ->
                        case erl_syntax:type(Ast) of
                            function ->
                                [{func,extract_comment(Ast, function, Specs)}|Acc];
                            attribute  ->
                                case erl_syntax:atom_value(erl_syntax:attribute_name(Ast)) of
                                    module ->
                                        [{module,
                                          extract_module_comment(Ast)}|Acc];
                                    _Other -> 
                                        %%[{_Other, Ast}|Acc]
                                        Acc
                                end;
                            _  ->
                                [{s,Ast}|Acc]
                        end
                end, [], AstList),
    {S, length(S), length(AstList)}.
    
