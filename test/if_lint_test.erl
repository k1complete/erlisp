-module(if_lint_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

if_lint_test() ->
    Line = ?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(if ((== x 1) 'ok) ('true 'ng))", Line),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding1=erl_eval:add_binding('x', 1, erl_eval:new_bindings()),
    ?assertEqual({value, ok, [{'x', 1}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding1)),
    Binding2=erl_eval:add_binding('x', 2, erl_eval:new_bindings()),
    ?assertEqual({value, ng, [{'x', 2}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding2)).

if_lint_whenm_test() ->
    Line = ?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(if ((== x 1) (==x 2)) (1 'ng))", Line),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding=erl_eval:add_binding('x', 1, erl_eval:new_bindings()),
    ?assertEqual({value, false, [{'x', 1}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)).


if_lint_err_test() ->
    Line = ?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(if ((== x 1)) (1 'ng))", Line),
                                              %%1234567
    {ok, [Tree]} =els_parser:parse(Tokens),
    {error, Loc, C} = try 
			  els_transpile:form(Tree, [])
		      catch 
			  throw:[{error, L, Reason}] ->
			      {error, L, Reason}
		      end,
    ?assertEqual(non_body, C),
    ?assertEqual({Line, 7}, Loc).


