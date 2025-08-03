-module(erlformat_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

-include_lib("els.hrl").

list_test() ->
    S = "[1,2,3]",
    {ok, Tokens, _Line} = erl_scan:string(S, 1),
    {ok, Tree} = els_erlformat:parse(Tokens),
    Expect = [#item{type=integer, value=1},
	      #item{type=integer, value=2},
	      #item{type=integer, value=3}],
    ?assertEqual(Expect, Tree).
pid_test() ->
    S = "[1,2,<0.84.0>]",
    {ok, Tokens, _Line} = erl_scan:string(S, 1),
    io:format("Tokens: ~p~n", [Tokens]),
    {ok, Tree} = els_erlformat:parse(Tokens),
    Expect = [#item{type=integer, value=1},
	      #item{type=integer, value=2},
	      [#item{type=atom, value="pid"},
	       #item{type=float, value=0.84},
	       #item{type=integer, value=0}]],
    ?assertEqual(Expect, Tree).
tuple_test() ->
    S = "[1,2,{0.84,0}]",
    {ok, Tokens, _Line} = erl_scan:string(S, 1),
    io:format("Tokens: ~p~n", [Tokens]),
    {ok, Tree} = els_erlformat:parse(Tokens),
    Expect = [#item{type=integer, value=1},
	      #item{type=integer, value=2},
	      [#item{type=atom, value="tuple"},
	       #item{type=float, value=0.84},
	       #item{type=integer, value=0}]],
    ?assertEqual(Expect, Tree).
map_test() ->
    S = "#{a=>1, b=>2, c=>3}",
    {ok, Tokens, _Line} = erl_scan:string(S, 1),
    io:format("Tokens: ~p~n", [Tokens]),
    {ok, Tree} = els_erlformat:parse(Tokens),
    Expect = [#item{type=atom, value="map"},
	      [#item{type=atom, value="=>"},
	       #item{type=atom, value="a"},
	       #item{type=integer, value=1}],
	      [#item{type=atom, value="=>"},
	       #item{type=atom, value="b"},
	       #item{type=integer, value=2}],
	      [#item{type=atom, value="=>"},
	       #item{type=atom, value="c"},
	       #item{type=integer, value=3}]],
    ?assertEqual(Expect, Tree).

dott_test() ->
    S = "#{a=>1, b=>2, c=>3, ...}",
    {ok, Tokens, _Line} = erl_scan:string(S, 1),
    io:format("Tokens: ~p~n", [Tokens]),
    {ok, Tree} = els_erlformat:parse(Tokens),
    Expect = [#item{type=atom, value="map"},
	      [#item{type=atom, value="=>"},
	       #item{type=atom, value="a"},
	       #item{type=integer, value=1}],
	      [#item{type=atom, value="=>"},
	       #item{type=atom, value="b"},
	       #item{type=integer, value=2}],
	      [#item{type=atom, value="=>"},
	       #item{type=atom, value="c"},
	       #item{type=integer, value=3}],
	      #item{type=atom, value="..."}],

    ?assertEqual(Expect, Tree).

    
    
