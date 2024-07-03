-module(map_comp_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

map_field_test() ->
    Line = ?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(map 'a 'b)", Line),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    ?assertEqual({value, #{a => b}, [{'A', 1}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).
map_generator_test() ->
    Line = ?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(mc|| v k (<- k v (map 'a 'b 'c 'd)) (== k 'c))", Line),
    io:format("tokens ~p~n", [Tokens]),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    io:format("TransForm ~p~n", [erl_syntax:revert(C)]),

    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    ?assertEqual({value, #{d => c}, [{'A', 1}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).
    
