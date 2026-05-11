-module(els_doc).
-include_lib("els.hrl").
-export([get_doc_v1/3,
	 get_spec/3,
	 get_ast/1,
	 build_signature/3,
	render_function/3,
	doc/3]).

id(E) ->
    E.

response_convert(E, _ModeFun) ->
    S = binary:bin_to_list(E),
    SBin = case erl_scan:string(S) of
	       {ok, Tokens, _Line} ->
		   case els_erlformat:parse(Tokens) of
		       {ok, Tree} ->
			   SSS = els_pp:pp(Tree),
			   binary:list_to_bin(SSS);
		       _ ->
			   E
		   end;
	       _ ->
		   E
	   end,
    SBin.
literal_convert(E, ModeFun) ->
    S = binary:bin_to_list(E),
    %%io:format("Before: ~p~n", [S]),
    {ok,Tokens, _Line} = erl_scan:string(S),
    {ok, ExprList} = erl_parse:parse_exprs(Tokens),
    Expr=hd(ExprList),
    %%io:format("Exp: ~p~n", [Expr]),
    SS = els_item:from_erl(Expr, ModeFun),
    %%io:format("Item: ~p~n", [SS]),
    SSS = els_pp:pp(SS),
    SBin = binary:list_to_bin(SSS),
    %%io:format("pp: ~p~n", [SBin]),
    SBin.


convert_expression(Prompt, Line, ModeFun) ->
    S = case binary:last(Line) of
	    $. ->
		literal_convert(Line, ModeFun);
	    _ ->
		response_convert(Line, ModeFun)
	end,
    binary:join([Prompt, S], <<"">>).

split_prompt(E) ->
    case re:run(E, <<"^(?<Prompt>[0-9]+\> )(?<Request>.*)$">>, 
		[{capture, all_names,binary}]) of
	nomatch ->
	    {<<"">>, E};
	{match, [Prompt, Request]} ->
	    {Prompt, Request}
    end.

split_doc_expression(Doc) ->
    DocList = re:split(Doc, <<"\n">>, [{return, binary}]),
    {_, _, _, Acc} = lists:foldl(
		       fun (<<"```erlang">> = E, {doc, _, _, Acc}) ->
			       {expression, "", <<"">>, [E|Acc]};
			   (<<"```">> = E, {expression, P, Line, Acc}) ->
			       %% io:format("convert P ~p~n ~p~n", [P, Line]),
			       Expressions = convert_expression(P, Line, fun id/1),
			       {doc, "", <<"">>, [E, Expressions | Acc]};
			   (E, {doc, _, _Line, Acc}) ->
			       {doc, "", <<"">>, [E|Acc]};
			   (<<$ , Cont/bitstring>>  = _E, {expression, P, Line, Acc}) ->
			       Next = binary:join([Line, Cont], <<"\n">>),
			       {expression, P, Next, Acc};
			   (E, {expression, _P, <<"">>, Acc}) ->
			       %% io:format("exp: ~p <<>>~n", [P]),
			       {Prompt, Request} = split_prompt(E),
			       {expression, Prompt, Request, Acc};
			   (E, {expression, P, Line, Acc}) ->
			       %% io:format("exp: ~p <<>>", [P]),
			       Expression = convert_expression(P, Line, fun id/1),
			       %% io:format("exp: ~p ~nLine: ~p~n", [P, Line]),
			       {Prompt, NewRequest} = split_prompt(E),
			       {expression, Prompt, NewRequest, [Expression | Acc]}
		       end, {doc, "", <<"">>, []},  DocList),
    Acc.


-spec consolidate_doc(Doc) -> Doc2 
	      when Doc :: binary(),
		   Doc2 :: binary().
consolidate_doc(Doc) ->
    Acc =split_doc_expression(Doc),
    %%io:format("consolidated ~p~n", [Acc]),
    binary:join(lists:reverse(Acc), <<"\n">>).
    

render_function(Function, Arity, Docs) ->
    {docs_v1, _MAnno, _Lang, _Format, _MDoc, _Meta, DocList} = Docs,
    [{{_K, _F, _A}, _Anno, Sig, Doc, _M}] = 
	lists:filter(fun(E) -> 
			  case E of
			      {{_Kind, Function, Arity}, _Anno, _Signature, _Doc, _Metadata} ->
				  true;
			      (_) ->
				  false
			  end
		     end,
		     DocList),
    SigN = binary:join(Sig, <<"\n">>),
    DocB = consolidate_doc(maps:get(<<"en">>, Doc)),
    DocEls = #{<<"en">> => DocB},
    io:format("~s~n~n~s~n", [SigN, maps:get(<<"en">>, DocEls)]).
    

get_doc_v1(Module, Function, Arity) ->
    %%io:format("F0 ~s : ~p~n", [Function, Arity]),
    {ok, Ast} = get_ast(Module),
    %%io:format("F1 ~s : ~p~n", [Function, Arity]),
    case code:get_doc(Module) of
	{ok, {docs_v1, MAnno, Lang, Formatter,
	      ModuleDoc, MetaData,
	      Docs}} ->
	    NDocs = lists:filtermap(fun({{Kind, Name, NArity}, Anno, _Signature, FDoc, FMetadata}) ->
					    case {Name, NArity} of
						{Function, Arity} ->
						    %% io:format("F ~s : ~p~n", [Name, Function]),
						    NSignature = build_signature(Module, Function, Arity, Ast),
						    %%io:format("F ~s : ~p~n", [Name, NSignature]),
						    BS = lists:map(fun(E) ->
									   binary:list_to_bin(E)
								   end,  string:split(NSignature, "\n")),
						    {true, {{Kind, Name, NArity}, Anno, BS, FDoc, FMetadata}};
						_ ->
						    true
					    end
				    end, Docs),
	    {docs_v1, MAnno, Lang, Formatter,
	     ModuleDoc, MetaData, NDocs};
	{error, Reason} ->
	    {error, Reason}
    end.

doc(Module, Function, Arity) ->
    case get_doc_v1(Module, Function, Arity) of
	{error, Reason} ->
	    {error, Reason};
	DocV1 ->
	    render_function(Function, Arity, DocV1)
    end.

	
    

create_args(N) ->
    lists:map(fun(E) ->
		      #item{type=atom, value=io_lib:format("Arg~p", [E])}
	      end, lists:seq(1, N)).

build_signature(Module, Function, Arity, Ast) ->
    RawSpecMap = get_spec(Module, Function, Arity, Ast),
    %% io:format("R ~p~n", [RawSpecMap]),
    StrFunction = atom_to_list(Function),
    SpecList = case maps:get({Function, Arity}, RawSpecMap, notfound) of
		   notfound ->
		       [#item{type=atom, value=StrFunction} | create_args(Arity)];
		   M ->
		       %% io:format("SpecIn ~p~n", [hd(M)]),
		       %% S = els_typespec:to_list(hd(M), fun makefun/1),
		       S = els_item:from_erl(hd(M)),
		       [#item{type=atom, value="-spec"}, [#item{type=atom, value=StrFunction} |hd(S)]] ++ tl(S)
	       end,
    %%io:format("SpecList ~p~n", [SpecList]),
    els_pp:pp(SpecList).

build_signature(Module, Function, Arity) ->
    {ok, Ast} = get_ast(Module),
    build_signature(Module, Function, Arity, Ast).

get_ast(Module) ->
    case code:get_object_code(Module) of
	{Module, Binary, _Filename} ->
	    case beam_lib:chunks(Binary, [debug_info]) of
		{ok, {Module, [{debug_info, {_DebugVersion, _Backend, {Ast, _Opt}}}]}} ->
		    {ok, Ast};
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

get_spec(Module, Function, Arity) ->
    case get_ast(Module) of
	{ok, Ast} ->
	    get_spec(Module, Function, Arity, Ast);
	_ ->
	    false
    end.

get_spec(_Module, Function, Arity, Ast) ->
    els_compile:extract_specs(Ast, Function, Arity).

	    

	      
