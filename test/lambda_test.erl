-module(lambda_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

lambda_test() ->
    Line = ?LINE,
    Cmd = ["(match f (lambda (a b) ",
           "  (match c (case (== a b)",
           "    ('true 'ok)",
           "    ('false 'ng)))",
           "   (atom_to_list c)))",
           "(apply f `(,A 1))"],
    Cmd0 = lists:flatten(Cmd),
    Env0 = els_util:env_init(),

    {ok, Tokens, _Line} = els_scan:from_string(Cmd0, Line),
    {ok, Trees} =els_parser:parse(Tokens),
    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    C = lists:foldl(fun(Tree, {_, B}) -> 
                            C = els_transpile:form(Tree, Env0),
                            io:format("Tree ~p~n", [C]),
                            {value, Result, NewBinding} = erl_eval:expr(erl_syntax:revert(C), B),
                            {Result, NewBinding}
                    end, {[], Binding}, Trees),
    ?assertEqual("ok", element(1, C)).

lambda_with_guard_test() ->
    Line = ?LINE,
    Cmd = ["(match f (lambda ((a b) (when (is_integer a))",
           "  (match c (case (== a b)",
           "    ('true 'ok)",
           "    ('false 'ng)))",
           "  (atom_to_list c))",
           "((a b)",
           " (match c (case (=/= a b)",
           "    ('true 'ok)",
           "    ('false 'ng)))"
           " (atom_to_list c))))",
           "(apply f `(,A 1))"],
    Cmd0 = lists:flatten(Cmd),
    Env0 = els_util:env_init(),

    {ok, Tokens, _Line} = els_scan:from_string(Cmd0, Line),
    {ok, Trees} =els_parser:parse(Tokens),
    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    C = lists:foldl(fun(Tree, {_, B}) -> 
                            C = els_transpile:form(Tree, Env0),
                            io:format("Tree ~p~n", [C]),
                            {value, Result, NewBinding} = erl_eval:expr(erl_syntax:revert(C), B),
                            {Result, NewBinding}
                    end, {[], Binding}, Trees),
    ?assertEqual("ok", element(1, C)).

case_with_multistatement_test() ->
    Line = ?LINE,
    Cmd = ["(case (tl param)",
           "  (x (when (== (hd x) 2))",
           "     (match y (* (hd (tl x)) 10))",
           "     (+ y (hd x)))",
           "  (s s))"],
    Env0 = els_util:env_init(),

    {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(Cmd), Line),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, Env0),
    Binding=erl_eval:add_binding(param, [1,2,3], erl_eval:new_bindings()),
    ?assertEqual({value, 32, [{param, [1,2,3]}, {x, [2,3]}, {y, 30}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).


named_fun_test() ->
    Line = ?LINE,
    Cmd = ["(match f ", 
	   "   (named_fun frac ((0) 1) ",
	   "                   ((n) (* n (apply frac `(,(- n 1)))))))",
	   "(apply f '(10))"],
    Cmd0 = lists:flatten(Cmd),
    Env0 = els_util:env_init(),

    {ok, Tokens, _Line} = els_scan:from_string(Cmd0, Line),
    {ok, Trees} =els_parser:parse(Tokens),
    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    C = lists:foldl(fun(Tree, {_, B}) -> 
                            C = els_transpile:form(Tree, Env0),
                            io:format("Tree ~p~n", [C]),
                            {value, Result, NewBinding} = erl_eval:expr(erl_syntax:revert(C), B),
                            {Result, NewBinding}
                    end, {[], Binding}, Trees),
    ?assertEqual(3628800, element(1, C)).

