-module(maybe_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

maybe_test() ->
    Line = ?LINE,
    Cmd = ["(maybe ",
	   "  (?match (tuple 'ok A) (tuple 'ok 1))",
	   "  (match 'true (> A  0))",
	   "  (?match (tuple 'ok B) (tuple 'ok 2))",
	   "  (+ A B))"],
        {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(Cmd), Line),
    io:format("tokens ~p~n", [Tokens]),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    io:format("TransForm ~p~n", [erl_syntax:revert(C)]),

    Binding=erl_eval:add_binding('A1', undefined, erl_eval:new_bindings()),
    
    ?assertEqual({value, 3, [{'A1', undefined}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).
    
