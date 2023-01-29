-module(eval_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

append_test() ->
    B = merl:qquote("lists:append([[1,2],[A]])", []),
    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    ?assertEqual({value, [1,2,1], [{'A', 1}]},
                 erl_eval:expr(B, Binding)).

little_letters_variable_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("(lists:reverse (list 1 2 b))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C = transpile:form(hd(Tree), []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual([3,2,1], Result).

backquote_unquote_variable_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("(lists:reverse `(1 2 ,b))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C = transpile:form(hd(Tree), []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual([3,2,1], Result),
    Expect = merl:qquote(Line, "lists:reverse(lists:append([[1],[2], [_@b]]))", 
                         [{b,erl_syntax:set_pos(erl_syntax:variable('b'), Line)}]),
    ?assertEqual("lists:reverse(lists:append([[1], [2], [b]]))",
                 erl_prettypr:format(C)).
%%    ?assertEqual(erl_syntax:revert(transpile:locline(Expect)), 
%%                 erl_syntax:revert(transpile:locline(C))).

backquote_unquote_variable_dot_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("`(reverse (1 2 . ,b))", Line),
    {ok, [Tree]} = parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual([reverse, [1, 2 | 3]], Result).

backquote_unquote_splice_variable_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("`(reverse (1 ,@(lists:reverse '(4 5)) 2))", Line),
    {ok, [Tree]} = parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [erl_syntax:revert(C), Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual([reverse, [1, 5, 4, 2]], Result).

list_dot_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("'(reverse . 1)", Line),
    {ok, [Tree]} = parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [erl_syntax:revert(C), Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual([reverse | 1], Result).

nested_list_dot_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("'(reverse . (1 2 . a))", Line),
    {ok, [Tree]} = parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [erl_syntax:revert(C), Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual([reverse, 1, 2 | a], Result).

defun_test() ->
    Line=?LINE,
    Cmd = ["(module c)",
           "(export (add 2))",
           "(defun add (a b)",
           "  (match c (+ a b))",
           "  (* c c))"
          ],
    {ok, Tokens, _Line} = scan:from_string(lists:flatten(Cmd), Line),
    {ok, Tree} = parser:parse(Tokens),
    C = lists:map(fun(E) -> transpile:form(E, []) end, Tree),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    {ok, Binary} = merl:compile_and_load(C, [debug_info]),
    Result = apply(c, add, [2, 3]),
    ?assertEqual(25, Result).


