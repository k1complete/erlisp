-module(map_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

map_literal_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("(map 'a b 'c 'd 'e 'f)", Line),
    {ok, [Tree]} = parser:parse(Tokens),
    C = transpile:form(Tree, []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual(#{a => 3, c => d, e => f}, Result).
