-module(map_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

map_literal_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(map 'a b 'c 'd 'e 'f)", Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual(#{a => 3, c => d, e => f}, Result).
map_null_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(map )", Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual(#{}, Result).

map_update_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(maps:update 'k1 2 a)", Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding = erl_eval:add_binding(a, #{k0 => 0, k1 => 1, k2 => 2}, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual(#{k0 => 0, k1 => 2, k2 => 2}, Result).

map_pattern_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(= (map* (:= 'k1 (tuple 1 b))) a)", Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding = erl_eval:add_binding(a, #{k0 => 0, k1 => {1, 2}, k2 => 2}, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    {value, Result, Binding2} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual(#{k0 => 0, k1 => {1,2}, k2 => 2}, Result),
    ?assertEqual([ {a, #{k0 => 0, k1 => {1,2}, k2 => 2}}, {b, 2}], Binding2).

map_empty_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(map)", Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding = erl_eval:add_binding(a, #{k0 => 0, k1 => {1, 2}, k2 => 2}, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    {value, Result, Binding2} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual(#{}, Result),
    ?assertEqual([ {a, #{k0 => 0, k1 => {1,2}, k2 => 2}}], Binding2).
    
