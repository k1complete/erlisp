-module(let_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-define(TQ(Line, T), merl:quote(Line, T)).

binding_test() ->
    Line = ?LINE,
    Left = ?TQ(Line, "[[H|T], {T1, T2, T3}]"),
    Right = ?TQ(Line, "[[1,2,3], {a, 1, 2}]"),
    L = int:binding(Left, Right, [], []),
    ?assertEqual(?TQ(Line, "[[H|T], {T1,T2,T3}]  = [[1,2,3], {a, 1, 2}]"), 
                 erl_syntax:revert(L)).

let_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("(let ((a 1) (b 2)) (+ a b))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C = transpile:form(hd(Tree), []),
    {value, Result, BindingResult} = erl_eval:expr(erl_syntax:revert(C), []),
    ?assertEqual(3, Result).
let_with_tuple_pattern_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("(let (((tuple a b) (tuple 1 2)) (c 3)) (+ 3 (+ a b)))", 
                                           Line),
    {ok, Tree} = parser:parse(Tokens),
    C = transpile:form(hd(Tree), []),
    {value, Result, BindingResult} = erl_eval:expr(erl_syntax:revert(C), []),
    ?assertEqual(6, Result).
let_with_list_pattern_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("(let (((list a b) '(1 2)) (c 3)) (+ 3 (+ a b)))", 
                                           Line),
    {ok, Tree} = parser:parse(Tokens),
    C = transpile:form(hd(Tree), []),
    {value, Result, BindingResult} = erl_eval:expr(erl_syntax:revert(C), []),
    ?assertEqual(6, Result).
