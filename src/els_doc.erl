-module(els_doc).
-include_lib("els.hrl").
-export([get_doc_v1/3,
	 get_spec/3,
	 get_ast/1,
	 build_signature/3,
	render_function/3,
	doc/3]).

literal_convert(E) ->
    S = binary:bin_to_list(E),
    io:format("Before: ~p~n", [S]),
    {ok,Tokens, _Line} = erl_scan:string(S),
    {ok, ExprList} = erl_parse:parse_exprs(Tokens),
    Expr=hd(ExprList),
    io:format("Exp: ~p~n", [Expr]),
    SS = els_pp:erl_to_ast(Expr),
    io:format("Item: ~p~n", [SS]),
    SSS = els_pp:pp(SS),
    SBin = binary:list_to_bin(SSS),
    io:format("pp: ~p~n", [SBin]),
    SBin.

-spec consolidate_doc(Doc) -> Doc2 
	      when Doc :: binary(),
		   Doc2 :: binary().
consolidate_doc(Doc) ->
    Lines = re:split(Doc, <<"\n">>, [{return, binary}]),
    {_, Doc2} = lists:foldl(fun(<<"```erlang">> = E, {normal, Acc}) ->
				    {open, [E|Acc]};
			       (<<"```">> = E, {_, Acc}) ->
				    {normal, [E|Acc]};
			       (E, {normal, Acc}) ->
				    {normal, [E|Acc]};
			       (E, {open, Acc}) ->
				    case re:run(E, <<"^(?<Prompt>[0-9]+\> )(?<Request>.*)$">>, 
						[{capture, all_names,binary}]) of
					nomatch ->
					    Exp = binary:join([E, <<".">>], <<"">>),
					    {open, [literal_convert(Exp)| Acc]};
					{match, [Prompt, Request]} -> 
					    {open, [binary:join([Prompt, literal_convert(Request)], <<"">>) | Acc]}
				    end
			    end, {normal, []}, Lines),
    binary:join(lists:reverse(Doc2), <<"\n">>).

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
    io:format("Doc ~p~n", [Doc]),
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

	
    
makefun(E) when is_atom(E) ->
    #item{type=atom, value=atom_to_list(E)};
makefun(E) ->
    E.

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
		       %% io:format("S ~p~n", [M]),
		       S = els_typespec:to_list(hd(M), fun makefun/1),
		       [#item{type=atom, value="-spec"}, [#item{type=atom, value=StrFunction} |hd(S)]] ++ tl(S)
	       end,
    %% io:format("SpecList ~p~n", [SpecList]),
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

	    

	      
