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
    R2 = els_util:scanlist(tl(A), tl(K), {Hd#item.value, []}, #{}, #{hd(K) => hd(A)}),
    {R, LocH} = els_util:scanlist(A, K),
    ?assertEqual(#{"try" => [[yal_util:make_symbol('a'), [1]]],
		   "of" => [[[1], [yal_util:make_symbol(foo1), []]],
			    [[2], [yal_util:make_symbol(foo2), []]]],
		   "catch" => [[[3], [yal_util:make_symbol(foo3), []]],
			       [[4], [yal_util:make_symbol(foo4), []]]],
		   "after" => [[yal_util:make_symbol(foo5), []],
			       [yal_util:make_symbol(foo6), []]]}, R).
scanlist_catch_test() ->
    A =[yal_util:make_symbol('try'), 
	[yal_util:make_symbol('a'), [1]],
	yal_util:make_symbol('catch'),
	[[3], [yal_util:make_symbol('foo3'), []]],
	[[4], [yal_util:make_symbol('foo4'), []]],
	yal_util:make_symbol('after'),
	[yal_util:make_symbol('foo5'), []],
	[yal_util:make_symbol('foo6'), []]],
    K = ["try", "of", "catch", "after"],
    Hd = hd(A),
    R2 = els_util:scanlist(tl(A), tl(K), {Hd#item.value, []}, #{}, #{hd(K) => hd(A)}),

    {R, LocH} = els_util:scanlist(A, K),
    ?assertEqual(#{"try" => [[yal_util:make_symbol('a'), [1]]],
		   "catch" => [[[3], [yal_util:make_symbol(foo3), []]],
			       [[4], [yal_util:make_symbol(foo4), []]]],
		   "after" => [[yal_util:make_symbol(foo5), []],
			       [yal_util:make_symbol(foo6), []]]}, R).
scanmap_list_test() ->
    In = #{"try" => [[yal_util:make_symbol('a'), 1]],
		   "of" => [[[1], [yal_util:make_symbol(foo1), 1]],
			    [[2], [yal_util:make_symbol(foo2), 1]]],
		   "catch" => [[[3], [yal_util:make_symbol(foo3), 1]],
			       [[4], [yal_util:make_symbol(foo4), 1]]],
		   "after" => [[yal_util:make_symbol(foo5), 1],
			       [yal_util:make_symbol(foo6), 1]]},
    E=[],
    R = maps:map(fun(K, V) when K == "try"; K== "after"->
			 lists:map(fun(S) ->
					   io:format("after: ~p~n", [S]),
					   els_transpile:form(S, E)
				   end, V);
		    (K, V)  ->
			 lists:map(fun(S) ->
					   io:format("S: ~p~n", [S]),
					   R = els_transpile:clause_(S, 0, E),
					   R
				   end, V)
		 end, In),
    Expect =#{"after" =>
                       [{call,0,{atom,0,foo5},[{integer,0,1}]},
                        {call,0,{atom,0,foo6},[{integer,0,1}]}],
	      "catch" =>
		  [{clause,0,
		    [{integer,0,3}],
		    [],
		    [{call,0,{atom,0,foo3},[{integer,0,1}]}]},
		   {clause,0,
		    [{integer,0,4}],
		    [],
		    [{call,0,{atom,0,foo4},[{integer,0,1}]}]}],
	      "of" =>
		  [{clause,0,
		    [{integer,0,1}],
		    [],
		    [{call,0,{atom,0,foo1},[{integer,0,1}]}]},
		   {clause,0,
		    [{integer,0,2}],
		    [],
		    [{call,0,{atom,0,foo2},[{integer,0,1}]}]}],
	      "try" => [{call,0,{atom,0,a},[{integer,0,1}]}]},
    R2 = maps:map(fun(_K, V) ->
			       lists:map(fun(E) ->
						 erl_syntax:revert(E)
					 end, V)
		  end, R),
    ?assertEqual(Expect, R2).

scanmap_list_catch_only_group_test() ->
    In = #{"try" => [[yal_util:make_symbol('a'), 1]],
		   "catch" => [[[3], [yal_util:make_symbol(foo3), 1]],
			       [[4], [yal_util:make_symbol(foo4), 1]]],
		   "after" => [[yal_util:make_symbol(foo5), 1],
			       [yal_util:make_symbol(foo6), 1]]},
    E=[],
    R = maps:map(fun(K, V) when K == "try"; K== "after"->
			 lists:map(fun(S) ->
					   io:format("after: ~p~n", [S]),
					   els_transpile:form(S, E)
				   end, V);
		    (K, V)  ->
			 lists:map(fun(S) ->
					   io:format("S: ~p~n", [S]),
					   R = els_transpile:clause_(S, 0, E),
					   R
				   end, V)
		 end, In),
    Expect =#{"after" =>
                       [{call,0,{atom,0,foo5},[{integer,0,1}]},
                        {call,0,{atom,0,foo6},[{integer,0,1}]}],
	      "catch" =>
		  [{clause,0,
		    [{integer,0,3}],
		    [],
		    [{call,0,{atom,0,foo3},[{integer,0,1}]}]},
		   {clause,0,
		    [{integer,0,4}],
		    [],
		    [{call,0,{atom,0,foo4},[{integer,0,1}]}]}],
	      "try" => [{call,0,{atom,0,a},[{integer,0,1}]}]},
    R2 = maps:map(fun(_K, V) ->
			       lists:map(fun(E) ->
						 erl_syntax:revert(E)
					 end, V)
		  end, R),
    ?assertEqual(Expect, R2).
