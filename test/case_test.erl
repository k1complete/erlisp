-module(case_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

case_test() ->
    Line = ?LINE,
    {ok, Tokens, _Line} = scan:from_string("(case (== 1 1) ('true 'ok) ('false 'ng))", Line),
    {ok, [Tree]} =parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    ?assertEqual({value, ok, [{'A', 1}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).

case_with_guard_test() ->
    Line = ?LINE,
    {ok, Tokens, _Line} = scan:from_string("(case (== 1 1) (x (when (== x 'true)) 'ok) ('false 'ng))", Line),
    {ok, [Tree]} =parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    ?assertEqual({value, ok, [{'A', 1}, {x, true}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).

case_with_multistatement_test() ->
    Line = ?LINE,
    Cmd = ["(case (tl param)",
           "  (x (when (== (hd x) 2))",
           "     (match y (* (hd (tl x)) 10))",
           "     (+ y (hd x)))",
           "  (s s))"],
    {ok, Tokens, _Line} = scan:from_string(lists:flatten(Cmd), Line),
    {ok, [Tree]} =parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Binding=erl_eval:add_binding(param, [1,2,3], erl_eval:new_bindings()),
    ?assertEqual({value, 32, [{param, [1,2,3]}, {x, [2,3]}, {y, 30}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).


