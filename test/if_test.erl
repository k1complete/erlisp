-module(if_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

if_test() ->
    Line = ?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(if ((== x 1) 'ok) ('true 'ng))", Line),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding=erl_eval:add_binding('x', 1, erl_eval:new_bindings()),
    ?assertEqual({value, ok, [{'x', 1}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).

if_with_guard_test() ->
    Line = ?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(if ((when (== x 'true )) 'ok) ('false 'ng))", Line),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding=erl_eval:add_binding('x', true, erl_eval:new_bindings()),
    ?assertEqual({value, ok, [{'x', true}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).

if_with_multistatement_test() ->
    Line = ?LINE,
    Cmd = ["(if ",
           "  ((when (== (hd x) 2))",
           "     (match y (* (hd (tl x)) 10))",
           "     (+ y (hd x)))",
           "  ('true  1))"],
    {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(Cmd), Line),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding=erl_eval:add_binding(x, [2,3], erl_eval:new_bindings()),
    ?assertEqual({value, 32, [{x, [2,3]}, {y, 30}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).

if_with_disjunctive_test() ->
    Line = ?LINE,
    Cmd = ["(if ",
           "  ((== (hd x) 2) (== x (list 2 1))",
           "     (match y (* (hd (tl x)) 10))",
           "     (+ y (hd x)))",
           "  ('true  1))"],
    {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(Cmd), Line),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    io:format("if_with_dicjunctive_test ~p~n", [C]),
    io:format("if_with_dicjunctive_test ~p~n", [erl_syntax:revert(C)]),
    Binding=erl_eval:add_binding(x, [2,3], erl_eval:new_bindings()),
    ?assertEqual({value, 32, [{x, [2,3]}, {y, 30}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).
if_with_disjunctive2_test() ->
    Line = ?LINE,
    Cmd = ["(if ",
           "  ((whend (whenc (== (hd x) 2))",
	   "          (whenc (== x (list 3 2))) )",
           "     (match y (* (hd (tl x)) 10))",
           "     (+ y (hd x)))",
           "  ('true  1))"],
    {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(Cmd), Line),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    io:format("if_with_dicjunctive_test2 ~p~n", [C]),
    io:format("if_with_dicjunctive_test2 ~p~n", [erl_syntax:revert(C)]),
    Binding=erl_eval:add_binding(x, [3, 2], erl_eval:new_bindings()),
    ?assertEqual({value, 23, [{x, [3,2]}, {y, 20}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).


