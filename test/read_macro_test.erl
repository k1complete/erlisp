-module(read_macro_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

quote_test() ->
    Line = 1,
    {ok, Tokens, _Line} = scan:from_string("(lists:reverse '(1 2 3))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Expect = merl:quote(Line, "lists:reverse([1,2,3])"),
    ?assertEqual(Expect,
                 erl_syntax:revert(transpile:locline(C))).

backquote_test() ->
    Line = 1,
    {ok, Tokens, _Line} = scan:from_string("(lists:reverse `(1 2 b))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Expect = merl:quote(Line, "lists:reverse(lists:append([[1],[2],[b]]))"),
    io:format("Formed: ~p~n", [C]),
    ?assertEqual("lists:reverse(lists:append([[1], [2], [b]]))",
                 erl_prettypr:format(C)).
%%    ?assertEqual(Expect, erl_syntax:revert(transpile:locline(C))).

backquote_atom_test() ->
    Line = 1,
    {ok, Tokens, _Line} = scan:from_string("(atom_to_list `b)", Line),
    {ok, Tree} = parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Expect = merl:quote(Line, "atom_to_list(b)"),
    ?assertEqual(Expect, erl_syntax:revert(transpile:locline(C))).

backquote_unquote_form_test() ->
    Line = 1,
    {ok, Tokens, _Line} = scan:from_string("(lists:reverse `,(list 'a 'b))", Line),
    io:format("Line: [~p]~p~n", [Tokens, Line]),
    {ok, Tree} = parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Expect = merl:quote(Line, ["lists:reverse([a, b])"]),
    ?assertEqual(Expect, erl_syntax:revert(transpile:locline(C))).

backquote_general_test() ->    
    Line = 1,
    {ok, Tokens, _Line} = scan:from_string("`(,(lists:reverse (list x1 'x2 'x3)) . xn)", Line),
    {ok, Tree} = parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Binding = erl_eval:add_binding(x1, 1, erl_eval:new_bindings()),
    Expect = merl:quote(Line, ["[lists:reverse([1, x2, x3]) | xn]"]),
    ?assertEqual(erl_eval:expr(Expect, Binding), 
                 erl_eval:expr(erl_syntax:revert(transpile:locline(C)), Binding)).
    

backquote_unquote_test2() ->
    Line = 1,
    {ok, Tokens, _Line} = scan:from_string("(lists:reverse `(1 2 ,(+ 1 2)))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Expect = merl:quote(Line, "lists:reverse([1,2,b])"),
    ?assertEqual(Expect, erl_syntax:revert(transpile:locline(C))).

    
