-module(record_access_expr_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

record_access_a_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(#. f a r)", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:form(hd(Ret), []),
    Expected = {record_field,{Line,2},{var,{Line,5},f},a,{atom,{Line,9},r}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).

record_expr_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(#r r (= a 1) (= b 2))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:form(hd(Ret), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = {record, {Line,2},r,
		[{record_field,{Line,8},{atom,{Line,10},a}, {integer,0,1}},
		 {record_field,{Line,16},{atom,{Line,18},b}, {integer, 0,2}}]},
    io:format("expr_test ~p~n", [Ast]),
    ?assertEqual(Expected, erl_syntax:revert(Ast)).

record_expr_argument_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(#r s r (= a 1) (= b 2))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:form(hd(Ret), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = {record, {Line,2},
		{var, {Line, 5}, s},
		r,
		[{record_field,{Line,10},{atom,{Line,12},a}, {integer,0,1}},
		 {record_field,{Line,18},{atom,{Line,20},b}, {integer, 0,2}}]},
    io:format("expr_test ~p~n", [Ast]),
    ?assertEqual(Expected, erl_syntax:revert(Ast)).
