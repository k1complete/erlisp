-module(transpile_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").


length_test() ->
    C4=transpile:form(?Q("[length, [cons, 1, [cons, 2, []]]]")),
    io:format("~p~n", [C4]),
    {tree,application,
     {attr,70,[],none},
     {application,
      {atom,70,length},
      [{tree,list,
        {attr,65,[],none},
        {list,
         [{integer,7,1}],
         {tree,list,
          {attr,65,[],none},
          {list,[{integer,7,2}],{nil,7}}}}}]}}.
length2_test() ->
    C4=transpile:form(?Q("[length, [cons, 1, [cons, 2, []]]]")), C5=?Q("length([1,2])"),
    
    ?assertEqual(C5, erl_syntax:revert(C4)).
    
quote_var_test() ->
    C4 = transpile:form(?Q("[quote, A]")), C5 = ?Q("A"),
    ?assertEqual(C4, C5).

quote_list_test() ->
    C4 = transpile:form(?Q("[quote, [a, A, 1]]")), C5 = ?Q("[a, A, 1]"),
    ?assertEqual(C4, C5).
    
