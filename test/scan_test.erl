-module(scan_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

loctoline(Tree) ->
    erl_syntax_lib:map(fun(E) ->
                               Loc=erl_syntax:get_pos(E),
                               Line = case Loc of
                                          {L, _} ->
                                              L;
                                          L when is_integer(L) ->
                                              L
                                      end,
                               erl_syntax:set_pos(E, Line)
                       end,
                       Tree).
        
process(Expected, Got) ->
    Line=?LINE,
    {ok, Tokens, _Lines} = scan:string(Got, Line),
    {ok, Tree} = parser:parse(Tokens),
    Trees = transpile:form(Tree, []),
%    {erl_prettypr:format(merl:quote(Expected)),
%     erl_prettypr:format(Trees)}.
    {merl:quote(Line, Expected),
     erl_syntax:revert(loctoline(Trees))}.
   

utf8_test() ->    
    Line=?LINE,
    Got = "(quote Aあ)",
    {ok, Tokens, _Lines} = scan:string(Got, Line),
    {ok, Tree} = parser:parse(Tokens),
    Trees = transpile:form(Tree, []),
    io:format("~ts~n", [erl_syntax:variable_literal(Trees)]),
    ?assertEqual("Aあ", erl_syntax:variable_literal(Trees)).
infix_test() ->
    lists:map(fun({AM, BM}) ->
                      {A, B} = process(AM, BM),
                      ?assertEqual(A, B)
              end,
              [{"1==1", "(== 1 1)"},
               {"1/=1", "(/= 1 1)"},
               {"1=<1", "(=< 1 1)"},
               {"1<1", "(< 1 1)"},
               {"1>=1", "(>= 1 1)"},
               {"1>1", "(> 1 1)"},
               {"1=:=1", "(=:= 1 1)"},
               {"1=/=1", "(=/= 1 1)"},
               {"1+2", "(+ 1 2)"},
               {"1-2", "(- 1 2)"},
               {"1*2", "(* 1 2)"},
               {"1/2", "(/ 1 2)"},
               {"bnot 1", "(bnot 1)"},
               {"1 div 2", "(div 1 2)"},
               {"3 rem 2", "(rem 3 2)"},
               {"6 band 4", "(band 6 4)"},
               {"8 bor 4", "(bor 8 4)"},
               {"8 bxor 4", "(bxor 8 4)"},
               {"8 bsl 2", "(bsl 8 2)"},
               {"8 bsr 2", "(bsr 8 2)"},
               {"8 bsl (8 bsr 2)", "(bsl 8 (bsr 8 2))"}
              ]).
match_test() ->
    {A, B} = process("_ = [1,2,3]", "(match _ (quote (1 2 3)))"),
    ?assertEqual(A, B).
match2_test() ->
    {A, B} = process("[X, Y, 3] = [1,2,3]", "(match (cons X (cons Y (quote (3)))) (quote (1 2 3)))"),
    ?assertEqual(A, B).
match3_test() ->
    {A, B} = process("'_' = [1,2,3]", "(match (quote _) (quote (1 2 3)))"),
    ?assertEqual(A, B).

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
    {ok, SP}=parser:parse(SRR),
    io:format("SP:~p~n", [SP]),
    TP=transpile:form(SP, []),
    SE = merl:quote(Line, "1+1"),
    A=erl_prettypr:format(SE),
    B=erl_prettypr:format(TP),
    ?assertEqual(A, B).

