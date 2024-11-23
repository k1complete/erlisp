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
		[{var,{Line,14},a},
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
		[{var,{Line,14},a},
		 {type,
		  {Line,20},
		  'fun',
		  [{type,{Line,20},product,[{type,{Line,29},integer,[]}]},
		   {type,{Line,40},integer,[]}]}]}]},
                      {type,{Line,53},integer,[]}]}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).

nil_type_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-spec (foo (list) (integer)) (integer))", Line),
%%    {ok, Tokens, _Line} = els_scan:from_string("(-spec (foo ((a :: (integer)) (integer)) (integer)))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:spec_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = {attribute,
                     {Line,2},
                     spec,
                     {{foo,2},
                      [{type,
                           {Line,2},
                           'fun',
                           [{type,
                                {Line,2},
                                product,
                                [{type,{Line,14},list,[]},
                                 {type,{Line,21},integer,[]}]},
                            {type,{Line,32},integer,[]}]}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).
    
    
type_attr_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-type (f) (lambda ( (Asm :: (integer) )) (Odd :: (integer))))", Line),
%%    {ok, Tokens, _Line} = els_scan:from_string("(-spec (foo ((a :: (integer)) (integer)) (integer)))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    TypeErl = "-type f() ::  fun((Asm :: integer()) -> Odd:: integer())",
    Type = {attribute,{Line,2},
	    'type',
	    {f,{type,{Line,13},
		'fun',
		[{'type',{Line,13},
		  product,
		  [{ann_type,{Line,23},
		    [{var,{Line,23},'Asm'},{type,{Line, 31},integer,[]}]}]},
		 {ann_type,{Line,44},
		  [{var,{Line,44},'Odd'},{type,{Line,52},integer,[]}]}]}},
	     []},
    Ast = els_transpile:type_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = Type,
    ?assertEqual(Expected, Ast).
    
    
