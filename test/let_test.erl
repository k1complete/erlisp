-module(let_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-define(TQ(Line, T), merl:quote(Line, T)).


let_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(let ((a 1) (b 2)) (+ a b))", Line),
    {ok, Tree} = els_parser:parse(Tokens),
    C = els_transpile:form(hd(Tree), []),
    {value, Result, _BindingResult} = erl_eval:expr(erl_syntax:revert(C), []),
    ?assertEqual(3, Result).
let_with_tuple_pattern_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(let (((tuple a b) (tuple 1 2)) (c 3)) (+ 3 (+ a b)))", 
                                           Line),
    {ok, Tree} = els_parser:parse(Tokens),
    C = els_transpile:form(hd(Tree), []),
    {value, Result, _BindingResult} = erl_eval:expr(erl_syntax:revert(C), []),
    ?assertEqual(6, Result).
let_with_list_pattern_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(let (((list a b) '(1 2)) (c 3)) (+ 3 (+ a b)))", 
                                           Line),
    {ok, Tree} = els_parser:parse(Tokens),
    C = els_transpile:form(hd(Tree), []),
    {value, Result, _BindingResult} = erl_eval:expr(erl_syntax:revert(C), []),
    ?assertEqual(6, Result).
let_nest_with_list_pattern_test() ->
    Line=?LINE,
    A = "(let (((list a b) '(1 2)) (c 3))"++
        "(+ c (let ((a (+ a b)) (c 10)) (* (* a c) b)))"++
        ")",
    {ok, Tokens, _Line} = els_scan:from_string(A, Line),
    {ok, Tree} = els_parser:parse(Tokens),
    C = els_transpile:form(hd(Tree), []),
    {value, Result, _BindingResult} = erl_eval:expr(erl_syntax:revert(C), []),
    ?assertEqual(63, Result).

