-module(prompt_paren_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

-define(TQ(Line, T), merl:quote(Line, T)).

paren_test() ->
    Line = ?LINE,
    L = [{'(',{4,1}},
	 {symbol,{4,2},"list"},
	 {'{',{4,8}},
	 {integer,{4,10},1},
	 {integer,{5,2},2}],
    Tokens = els_scan:get_parens([], L, 2),
    Expect = "({",
    ?assertEqual(Expect, Tokens).
paren_close_test() ->
    Line = ?LINE,
    L = [{'(',{4,1}},
	 {symbol,{4,2},"list"},
	 {'{',{4,8}},
	 {'}',{4,8}},
	 {integer,{4,10},1},
	 {integer,{5,2},2}],
    Tokens = els_scan:get_parens([], L, 2),
    Expect = "(",
    ?assertEqual(Expect, Tokens).
