-module(list_comp_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

list_comp_test() ->
    Line = ?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(lc|| v (<- v (list 'a 'b 'c 'd)) (=/= v 'c))", Line),
    io:format("tokens ~p~n", [Tokens]),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    io:format("TransForm ~p~n", [erl_syntax:revert(C)]),

    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    ?assertEqual({value, [a, b, d], [{'A', 1}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).
    
