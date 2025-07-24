-module(els_help_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include_lib("els.hrl").

makefun(E) when is_atom(E) ->
    #item{type=atom, value=atom_to_list(E)};
makefun(E) ->
    E.

get_spec_test() ->
    R = els_doc:get_spec(lists, reverse, 1),
    M = maps:get({reverse,1}, R),
    S = els_typespec:to_list(hd(M), fun makefun/1),
    S1 = [#item{type=atom, value="-spec"},  [#item{type=atom, value="reverse"} |hd(S)]] ++ tl(S),
    %%P = els_typespec:fun_to_list(reverse, S),
    Exp = ['-spec',[ 'reverse', ['List1'], 'List2'], 
	   ['when', ['List1', '::', [list, 'T']], ['List2', '::', [list, 'T']],
	    ['T', '::', [term] ]]],
    P = els_pp:pp(S1),
    ?assertEqual("(-spec (reverse (List1) List2)\n  (when (List1 :: [T]) (List2 :: [T]) (T :: (term))))", P).

build_signature_test() ->
    S1 = els_doc:build_signature(lists, reverse, 1),
    ?assertEqual("(-spec (reverse (List1) List2)\n  (when (List1 :: [T]) (List2 :: [T]) (T :: (term))))", S1),
    S2 = els_doc:build_signature(els_doc, build_signature, 3),
    ?assertEqual("(build_signature Arg1 Arg2 Arg3)", S2).
    
get_doc_v1_test() ->
    S2 = els_doc:get_doc_v1(lists, reverse, 1),
    io:format("getops ~p~n", [io:getopts()]),
    D = els_doc:render_function(reverse, 1, S2),
    io:format("Doc ~p~n", [D]),
    ?assertEqual(ok , D).
    
build_signature_more_test() ->
    A = {type,{1385,16},
         bounded_fun,
         [{type,{1385,16},
	   'fun',
	   [{type,{1385,16},
	     product,
	     [{var,{1385,17},'Key'},
	      {var,{1385,22},'N'},
	      {var,{1385,25},'TupleList1'}]},
	    {var,{1385,40},'TupleList2'}]},
          [{type,{1386,7},
	    constraint,
	    [{atom,{1386,7},is_subtype},
	     [{var,{1386,7},'Key'},{type,{1386,14},term,[]}]]},
           {type,{1387,7},
	    constraint,
	    [{atom,{1387,7},is_subtype},
	     [{var,{1387,7},'N'},{type,{1387,12},pos_integer,[]}]]},
           {type,{1388,7},
	    constraint,
	    [{atom,{1388,7},is_subtype},
	     [{var,{1388,7},'TupleList1'},
	      {type,{1388,21},list,[{var,{1388,22},'Tuple'}]}]]},
           {type,{1389,7},
	    constraint,
	    [{atom,{1389,7},is_subtype},
	     [{var,{1389,7},'TupleList2'},
	      {type,{1389,21},list,[{var,{1389,22},'Tuple'}]}]]},
           {type,{1390,7},
	    constraint,
	    [{atom,{1390,7},is_subtype},
	     [{var,{1390,7},'Tuple'},{type,{1390,16},tuple,any}]]}]]},
    B = els_typespec:to_list(A, fun(E) -> E end),
    ?assertEqual([[['Key','N','TupleList1'],'TupleList2'],
		  ['when',
		   ['Key','::',[term]],
		   ['N','::',[pos_integer]],
		   ['TupleList1','::',[list,'Tuple']],
		   ['TupleList2','::',[list,'Tuple']],
		   ['Tuple','::',[tuple,[any]]]]], B),
    Keyfind = {type,{115,14},
               bounded_fun,
               [{type,{115,14},
		 'fun',
		 [{type,{115,14},
		   product,
		   [{var,{115,15},'Key'},
		    {var,{115,20},'N'},
		    {var,{115,23},'TupleList'}]},
		  {type,{115,37},
		   union,
		   [{var,{115,37},'Tuple'},{atom,{115,45},false}]}]},
                [{type,{116,7},
		  constraint,
		  [{atom,{116,7},is_subtype},
		   [{var,{116,7},'Key'},{type,{116,14},term,[]}]]},
                 {type,{117,7},
		  constraint,
		  [{atom,{117,7},is_subtype},
		   [{var,{117,7},'N'},{type,{117,12},pos_integer,[]}]]},
                 {type,{118,7},
		  constraint,
		  [{atom,{118,7},is_subtype},
		   [{var,{118,7},'TupleList'},
		    {type,{118,20},list,[{var,{118,21},'Tuple'}]}]]},
                 {type,{119,7},
		  constraint,
		  [{atom,{119,7},is_subtype},
		   [{var,{119,7},'Tuple'},
		    {type,{119,16},tuple,any}]]}]]},
    K = els_typespec:to_list(Keyfind, fun(E) -> E end),
    ?assertEqual([[['Key','N','TupleList'],['|','Tuple',false]],
                  ['when',
                   ['Key','::',[term]],
                   ['N','::',[pos_integer]],
                   ['TupleList','::',[list,'Tuple']],
                   ['Tuple','::',[tuple,[any]]]]], K).
    
