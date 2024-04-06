-module(scanlist_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include_lib("els.hrl").

scanlist_test() ->
    A =[yal_util:make_symbol('try'), 
	[yal_util:make_symbol('a'), [1]],
	yal_util:make_symbol('of'),
	[[1], [yal_util:make_symbol('foo1'), []]],
	[[2], [yal_util:make_symbol('foo2'), []]],
	yal_util:make_symbol('catch'),
	[[3], [yal_util:make_symbol('foo3'), []]],
	[[4], [yal_util:make_symbol('foo4'), []]],
	yal_util:make_symbol('after'),
	[yal_util:make_symbol('foo5'), []],
	[yal_util:make_symbol('foo6'), []]],
    K = ["try", "of", "catch", "after"],
    Hd = hd(A),
    R2 = els_util:scanlist(tl(A), tl(K), {Hd#item.value, []}, #{}),
    R = els_util:scanlist(A, K),
    ?assertEqual(#{"try" => [[yal_util:make_symbol('a'), [1]]],
		   "of" => [[[1], [yal_util:make_symbol(foo1), []]],
			    [[2], [yal_util:make_symbol(foo2), []]]],
		   "catch" => [[[3], [yal_util:make_symbol(foo3), []]],
			       [[4], [yal_util:make_symbol(foo4), []]]],
		   "after" => [[yal_util:make_symbol(foo5), []],
			       [yal_util:make_symbol(foo6), []]]}, R).



			  





