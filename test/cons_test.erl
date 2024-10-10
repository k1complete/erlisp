-module(cons_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

-define(TQ(Line, T), merl:quote(Line, T)).

cons_test() ->
    Line = ?LINE,
    {ok, Tokens, _Lines} = els_scan:string("(cons 1 2)", Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    C4 = els_transpile:form(Tree, []),
    C5 = merl:quote(Line, "[1 | 2]"),
    ?assertEqual(C5, erl_syntax:revert(els_transpile:locline(C4))).

    

cons_bad_arity_test() ->
    Line = ?LINE,
    {ok, Tokens, _Lines} = els_scan:string("(cons 1)", Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    ?assertThrow(
       [{error, {bad_arity, {Line, 2}, {[1], 1}}}],
       els_transpile:form(Tree, [])).
cons_bad_arity3_test() ->
    Line = ?LINE,
    {ok, Tokens, _Lines} = els_scan:string("(cons 1 2 3)", Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    ?assertThrow(
       [{error, {bad_arity, {Line, 2}, {[1,2,3], 1}}}],
       els_transpile:form(Tree, [])).
