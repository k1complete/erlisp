-module(record_type_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

record_a_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(defrecord r a b)", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:record_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = {attribute,{Line,12},
		record,
		{r,[{record_field,{Line,14},{atom,{Line,14},a}},
		    {record_field,{Line,16},{atom,{Line,16},b}}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).

record_a_exp_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(defrecord r (= a 1) b)", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:record_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = {attribute,{Line,12},
		record,
		{r,[{record_field,{Line,15},{atom,{Line,17},a}, {integer,0,1}},
		    {record_field,{Line,22},{atom,{Line,22},b}}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).

record_a_type_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(defrecord r (a (integer)) b)", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:record_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = {attribute,
		{Line,12},
		record,
		{r,[{typed_record_field,
		     {record_field,{Line,15},{atom,{Line,15},a}},
		     {type,{Line,18},integer,[]}},
		    {record_field,{Line,28},{atom,{Line,28},b}}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).

record_ae_type_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(defrecord r ((= a 1) (integer)) b)", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:record_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected ={attribute,
	       {Line,12},
	       record,
	       {r,[{typed_record_field,
		    {record_field,
		     {Line,16},
		     {atom,{Line,18},a},
		     {integer,0,1}},
		    {type,{Line,24},integer,[]}},
		   {record_field,{Line,34},{atom,{Line,34},b}}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).

record_integer_range_type_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(defrecord r (a (.. 1 2)) b)", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:record_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected =
	{attribute,
	       {Line,12},
	       record,
	       {r,[{typed_record_field,
		    {record_field, {Line,15},  {atom,{Line,15},a}},
		    {type,
		     {Line,18},
		     range,
		     [{integer,0,1},{integer,0,2}]}},
		   {record_field,{Line,27},{atom,{Line,27},b}}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).
record_integer_charagtor_literal_type_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(defrecord r (a 10) (b \"ast\") (c ast))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:record_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected =
	{attribute,
	 {Line,12},
	 record,
	 {r,[{typed_record_field,
	      {record_field,{Line,15},{atom,{Line,15},a}},
	      {integer,0,10}},
	     {typed_record_field,
	      {record_field,{Line,22},{atom,{Line,22},b}},
	      {string,0,"ast"}},
	     {typed_record_field,
	      {record_field,{Line,32},{atom,{Line,32},c}},
	      {atom,{Line,34},ast}}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).

record_binary_type_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(defrecord r (a (binary)) (b  (nonempty_bitstring)))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:record_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected =
	{attribute,
	 {Line,12},
	 record,
	 {r,[{typed_record_field,
	      {record_field,{Line,15},{atom,{Line,15},a}},
	      {type,{Line,18},binary,[{nil,0},{integer,0,8}]}},
	     {typed_record_field,
	      {record_field,{Line,28},{atom,{Line,28},b}},
	      {type,
	       {Line,32},
	       binary,
	       [{integer,0,1},{integer,0,1}]}}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).
record_user_defiend_binary_type_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(defrecord r (a (binary_range 1 3)) (b  (nonempty_bitstring)))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:record_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected =
	{attribute,
	 {Line,12},
	 record,
	 {r,[{typed_record_field,
	      {record_field,{Line,15},{atom,{Line,15},a}},
	      {type,
	       {Line,18},
	       binary,
	       [{integer,0,1},{integer,0,3}]}},
	     {typed_record_field,
	      {record_field,{Line,38},{atom,{Line,38},b}},
	      {type,
	       {Line,42},
	       binary,
	       [{integer,0,1},{integer,0,1}]}}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).

record_list_nil_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(defrecord r (a ()) (b  (nil)))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:record_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected =
	{attribute,
	 {Line,12},
	 record,
	 {r,[{typed_record_field,
	      {record_field,{Line,15},{atom,{Line,15},a}},
                             {type,0,nil,[]}},
	     {typed_record_field,
	      {record_field,{Line,22},{atom,{Line,22},b}},
                             {type,{Line,26},nil,[]}}]}},

    ?assertEqual(Expected, erl_syntax:revert(Ast)).
