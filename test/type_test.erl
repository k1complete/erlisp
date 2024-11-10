-module(type_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

annon_type_spec_a_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-spec (foo (a :: (integer)) (integer)) (integer))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:spec_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = 
	{attribute,
	 {Line,2},
	 spec,
	 {{foo,2},
	  [{type,{Line, 2},'fun',
	    [{type,{Line, 2},product,
	      [{ann_type,
		{Line,14},
		[{atom,{Line,14},a},
		 {type,{Line,20},integer,[]}]},
	       {type,{Line,31},integer,[]}]},
	     {type,{Line,42},integer,[]}]}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).

annon_type_func_spec_a_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-spec (foo (a :: (lambda ((integer)) (integer)))) (integer))", Line),
%%    {ok, Tokens, _Line} = els_scan:from_string("(-spec (foo ((a :: (integer)) (integer)) (integer)))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:spec_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = 
	{attribute,
	 {Line,2},
	 spec,
	 {{foo,1},
	  [{type,{Line, 2},'fun',
	    [{type,{Line, 2},product,
	      [{ann_type,
		{Line,14},
		[{atom,{Line,14},a},
		 {type,
		  {Line,20},
		  'fun',
		  [{type,{Line,20},product,[{type,{Line,29},integer,[]}]},
		   {type,{Line,40},integer,[]}]}]}]},
                      {type,{Line,53},integer,[]}]}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).
