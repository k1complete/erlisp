-module(scan_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

process(Expected, Got) ->
    Line=?LINE,
    {ok, Tokens, _Lines} = scan:string(Got, Line),
    {ok, Tree} = tlisp:parse(Tokens),
    Trees = transpile:form(Tree),
    {erl_prettypr:format(merl:quote(Expected)),
     erl_prettypr:format(Trees)}.

utf8_test() ->    
    Line=?LINE,
    Got = "(Aあ)",
    {ok, Tokens, _Lines} = scan:string(Got, Line),
    {ok, Tree} = tlisp:parse(Tokens),
    Trees = transpile:form(Tree),
    io:format("~ts~n", [erl_syntax:variable_literal(Trees)]),
    ?assertEqual("Aあ", erl_syntax:variable_literal(Trees)).
comparison_test() ->
    {A, B} = process("1==1", "(== 1 1)"),
    ?assertEqual(A, B),
    {A2, B2} = process("1/=1", "(/= 1 1)"),
    ?assertEqual(A2, B2),
    {A3, B3} = process("1=<1", "(=< 1 1)"),
    ?assertEqual(A3, B3).

eq_test() ->
    {A, B} = process("1==1", "(== 1 1)"),
    ?assertEqual(A, B).
plus2_test() ->
    {A, B} = process("A=1+1", "(match A (+ 1 1))"),
    ?assertEqual(A, B).
minus_test() ->
    {A, B} = process("A= 1-1", "(match A (- 1 1))"),
    ?assertEqual(A, B).
 
plus_test() ->
    Line=?LINE,
    S="(+ 1 1)",
    SR = scan:string(S, Line),
    SRR=element(2, SR),
    io:format("SRR:~p~n", [SRR]),
    {ok, SP}=tlisp:parse(SRR),
    io:format("SP:~p~n", [SP]),
    TP=transpile:form(SP),
    SE = merl:quote(Line, "1+1"),
    A=erl_prettypr:format(SE),
    B=erl_prettypr:format(TP),
    ?assertEqual(A, B).


