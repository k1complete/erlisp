-module(bit_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

bit_literal_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(binary 1 2 3 4)", Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual(<<1,2,3,4>>, Result).
bit_null_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(binary )", Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual(<<>>, Result).

binary_field_test() ->
    Line=?LINE,
    Cmd = ["(binary (:bf 1 / integer) (:bf 2 2 integer-unit:3))"],
    {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(Cmd), Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    io:format(" revert------ ~p ~n", [erl_syntax:revert(C)]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual(<<1/integer, 2:2/integer-unit:3>>, Result).

bitstring_field_test() ->
    Line=?LINE,
    Cmd = ["(binary (:bf 1 / integer) (:bf (binary \"abc\") / bitstring))"],
    {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(Cmd), Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("------ ~p ~n binding ~p~n", [C, Binding]),
    io:format(" revert------ ~p ~n", [erl_syntax:revert(C)]),
    {value, Result, Binding} = erl_eval:expr(erl_syntax:revert(C), Binding),
    ?assertEqual(<<1/integer, (<<"abc">>)/bitstring>>, Result).
    

