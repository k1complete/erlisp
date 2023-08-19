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
    {ok, DocsV1} = make_docs(Ast, Specs),
    io:format("compiled ~p", [Binary]),
    {ok, Module, Chunks} = beam_lib:all_chunks(Binary),
    ChunksAdded = lists:append(Chunks, [{"Docs", term_to_binary(DocsV1)}]),
    io:format("Beam ~p", [ChunksAdded]),
    {ok, Binary2} = beam_lib:build_module(ChunksAdded),
    {ok, Module, Binary2, Ast}.

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
            #{<<"en">> => list_to_binary(C)};
        false ->
            []
    end.
    
make_function_signature(Tree, Specs) ->
    Name=erl_syntax:atom_value(erl_syntax:function_name(Tree)),
    Cs = erl_syntax:function_clauses(Tree),
    R = lists:map(fun(C) ->
                          Patterns = 
                              lists:map(fun(E) ->
                                                erlast:to_list(E)
                                        end, erl_syntax:clause_patterns(C)),
                          Guard = case erl_syntax:clause_guard(C) of 
                                      none -> [];
                                      X -> X
                                  end,
                          S = 
                              unicode:characters_to_binary(pp:pp(pp:erl_to_ast([Name, Patterns, Guard])), utf8),
                          case maps:get({Name, length(Patterns)}, Specs, none) of
                              none ->
                                  S;
                              Spec ->
                                  N = [list_to_binary(elisp_typespec:fun_to_string(Name, Spec))],
                                  case N of
                                      [] ->
                                          S;
                                      N ->
                                          [S| N]
                                  end
                          end
                  end, Cs),
    lists:flatten(R).

extract_comment(Tree, Kind, Specs) ->
    case erl_syntax:has_comments(Tree) of
        true ->
            elisp_docs:make_docentry(Kind, 
                                     erl_syntax:atom_value(erl_syntax:function_name(Tree)),
                                     erl_syntax:function_arity(Tree),
                                     erl_syntax:get_pos(Tree),
                                     make_function_signature(Tree, Specs),
                                     #{<<"en">> => 
                                           list_to_binary(erl_syntax:get_precomments(Tree))}, 
                                     #{});
        false ->
            []
    end.
    
make_docs(AstList, Specs) ->
    S=lists:foldr(fun(Ast, Acc) ->
                          Doc=maps:get(docs, Acc),
                          case erl_syntax:type(Ast) of
                            function ->
                                  E = extract_comment(Ast, function, Specs),
                                  Acc#{docs=> elisp_docs:add_docentry(Doc, E)};
                            attribute  ->
                                case erl_syntax:atom_value(erl_syntax:attribute_name(Ast)) of
                                    module ->
                                        E = elisp_docs:make_docs_v1(erl_syntax:get_pos(Ast),
                                                                     <<"text/markdown">>,
                                                                     extract_module_comment(Ast),
                                                                     #{},
                                                                     []),
                                        Acc#{docs=> elisp_docs:set_moduledoc(Doc, E)};
                                    _Other -> 
                                        %%[{_Other, Ast}|Acc]
                                        Acc
                                end;
                              _  ->
                                  O=maps:get(other, Acc),
                                  Acc#{other=>[{s,Ast}|O]}
                          end
                  end, #{docs=> elisp_docs:new_docs_v1(), other=>[]}, AstList),
    SS = maps:get(docs, S),
    {ok, SS}.

