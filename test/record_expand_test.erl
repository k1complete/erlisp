-module(record_expand_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").


record_expand_test() ->
    C = "(defrecord rec1 a b c d)",
    Code= "(= A (#r rec1 (= a 1) (= b 2) (= c 3) (= d 4)))
             (#. A rec1 a)",
    Line = ?LINE,
    {ok, Tokens, Line2} = els_scan:from_string(C, Line),
    {ok, Trees1} = els_parser:parse(Tokens),
    {ok, Tokens2, _Line} = els_scan:from_string(Code, Line2),
    {ok, Trees2} = els_parser:parse(Tokens2),

    RecordDef = lists:map(fun(Tree) ->
			  erl_syntax:revert(els_transpile:form(Tree, []))
		  end, Trees1),
    Execute = lists:map(fun(Tree) ->
			  erl_syntax:revert(els_transpile:form(Tree, []))
		  end, Trees2),
    F = erl_syntax:function(erl_syntax:atom("function_test"), 
			    [erl_syntax:clause([], none, Execute)]),
    F2 = erl_syntax:revert(F),
    RD = erl_syntax:revert(hd(RecordDef)),
    io:format("CCC: ~p~n", [[RD, F2]]),
    B = [RD, F2],
    B1 = erl_expand_records:module(B, []),
    [_H,T] = B1,
    Cls = erl_syntax:function_clauses(T),
    Bodies = erl_syntax:clause_body(hd(Cls)),
    ?assertEqual({value, 1,[{'A',{rec1,1,2,3,4}},{rec0,1}]}, erl_eval:exprs(Bodies, [])),
    io:format("B ~p~n", [B]),
    Bodies2 = els_repl:extract_record_module([hd(RecordDef)], Execute),
    ?assertEqual({value, 1,[{'A',{rec1,1,2,3,4}},{rec0,1}]}, erl_eval:exprs(Bodies2, [])).


   
    

    
