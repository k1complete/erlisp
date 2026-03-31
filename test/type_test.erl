-module(type_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

annon_type_spec_a_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-spec foo ((a :: (integer)) (integer)) (integer))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Tree = hd(Ret),
    Ast = els_transpile:spec_(hd(Tree), tl(Tree), []),
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
    {ok, Tokens, _Line} = els_scan:from_string("(-spec foo ((a :: (lambda ((integer)) (integer)))) (integer))", Line),
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
    ?assertEqual(Expected, Ast).



spec_type_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-spec foo ((list) (integer)) (integer))", Line),
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

multi_clause_type_test() ->
    Line=?LINE,
    Spec = ["(-spec foo ((list))    (integer)",
	    "           ((integer)) (list))"],   
    {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(Spec), Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:spec_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = {attribute,
                     {Line,2},
                     spec,
                     {{foo,1},
                      [{type,
                           {Line,2},
                           'fun',
                           [{type, {Line,2}, product,[{type,{Line,14},list,[]}]}, 
			    {type,{Line,25},integer,[]}]},
		       {type,
                           {Line,2},
                           'fun',
                           [{type, {Line,2}, product,[{type,{Line,46},integer,[]}]}, 
			    {type,{Line,57},list,[]}]}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)).

multi_clause_when_type_test() ->
    Line=?LINE,
    Spec = ["(-spec foo ((list X)) (integer) (when (X :: (atom)))",
	    "           ((integer)) (list))"],   
    Src = """
(-spec foo
    ((list X)) (integer) (when (X :: (atom)))
    ((integer)) (list))

""",
    {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(Spec), Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Ast = els_transpile:spec_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = {attribute, {Line,2},
		spec,
		{{foo,1},
		 [{type, {Line,2},
		   bounded_fun,
		   [{type, {Line,2},
		     'fun',
		     [{type, {Line,2}, product, [{type,{Line,14},list,[{var,{Line,19},'X'}]}]},
		      {type,{Line,24},integer,[]}]},
		    [{type, {Line,42},
		      constraint,
		      [{atom,{Line,42},is_subtype},
		       [{var,{Line,40},'X'},{type,{Line,46},atom,[]}]]}]]},
		  {type, {Line,2},
		   'fun',
		   [{type,{Line,2},product,[{type,{Line,66},integer,[]}]},
		    {type,{Line,77},list,[]}]}]}},
    ?assertEqual(Expected, erl_syntax:revert(Ast)),
    {attribute, _, spec, A}=erl_syntax:revert(Ast),
    {{Name, _Arity}, FunTypes} = A,
    D = els_typespec:fun_to_list(Name, FunTypes),
    ?assertEqual(Src, D).
    
    
type_attr_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-type (f) (lambda ( (Asm :: (integer) )) (Odd :: (integer))))", Line),
%%    {ok, Tokens, _Line} = els_scan:from_string("(-spec (foo ((a :: (integer)) (integer)) (integer)))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    %% TypeErl = "-type f() ::  fun((Asm :: integer()) -> Odd:: integer())",
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
    
    
type_map_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-type (item) (map (:= a (integer)) (:= b (list))))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Type = {attribute,
                     {Line,2},
                     type,
                     {item,
                         {type,
                             {Line,16},
                             map,
                             [{type,
                                  {Line,21},
                                  map_field_exact,
                                  [{atom,{Line,24},a},{type,{Line, 27},integer,[]}]},
                              {type,
                                  {Line,38},
                                  map_field_exact,
                                  [{atom,{Line,41},b},{type,{Line,44},list,[]}]}]}},
                     []},
    Ast = els_transpile:type_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = Type,
    ?assertEqual(Expected, Ast).
    
    
type_integer_op_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-type (item) (map (:= a (* 1 2)) (:= b (list))))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Type = {attribute,
                     {Line,2},
                     type,
                     {item,
                         {type,
                             {Line,16},
                             map,
                             [{type,
                                  {Line,21},
                                  map_field_exact,
                                  [{atom,{Line,24},a},
				   {op,
				    {Line,27},
				    '*',
				    {integer,{Line,27},1},
				    {integer,{Line,27},2}}]},
                              {type,
                                  {Line,36},
                                  map_field_exact,
                                  [{atom,{Line,39},b},{type,{Line,42},list,[]}]}]}},
                     []},
    Ast = els_transpile:type_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = Type,
    ?assertEqual(Expected, Ast).
    
    
type_record_field_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-type (item) (record the (a :: (integer)) (b :: (pid))))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Type = {attribute,
	    {Line,2},
	    type,
	    {item,
	     {type,
	      {Line,16},
	      record,
	      [{atom,{Line,23},the},
	       {type,
		{Line,30},
		field_type,
		[{atom,{Line,28},a},
		 {type,{Line,34},integer,[]}]},
	       {type,
		{Line,47},
		field_type,
		[{atom,{Line,45},b},
		 {type,{Line,51},pid,[]}]}]}},
	    []},
    Ast = els_transpile:type_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = Type,
    ?assertEqual(Expected, Ast).
    
    
type_remote_type_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-type (item) (record the (a :: (erl_anno:anno)) (b :: (pid))))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Type = {attribute,
	    {Line,2},
	    type,
	    {item,
	     {type,
	      {Line,16},
	      record,
	      [{atom,{Line,23},the},
	       {type,
		{Line,30},
		field_type,
		[{atom,{Line,28},a},
		 {remote_type,
		  {Line,34},
		  [{atom,{Line,34},erl_anno},
		   {atom,{Line,34},anno},
		   []]}]},
	       {type,
		{Line,53},
		field_type,
		[{atom,{Line,51},b},
		 {type,{Line,57},pid,[]}]}]}},
	    []},
    Ast = els_transpile:type_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = Type,
    ?assertEqual(Expected, Ast).
type_tuple_type_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-type (item) (record the (a :: (tuple)) (b :: (tuple (integer) (pid)))))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Type = {attribute,
	    {Line,2},
	    type,
	    {item,
	     {type,
	      {Line,16},
	      record,
	      [{atom,{Line,23},the},
	       {type,
		{Line,30},
		field_type,
		[{atom,{Line,28},a},
		 {type,{Line,34},tuple,any}]},
	       {type,
		{Line,45},
		field_type,
		[{atom,{Line,43},b},
		 {type,
		  {Line,49},
		  tuple,
		  [{type,{Line,56},integer,[]},
		   {type,{Line,66},pid,[]}]}]}]}},
	    []},
    Ast = els_transpile:type_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = Type,
    ?assertEqual(Expected, Ast).
type_union_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-type (item) (| (tuple) (integer)))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Type = {attribute,
	    {Line,2},
	    type,
	    {item,
	     {type,
	      {Line,16},
	      union,
	      [{type,{Line,19},tuple,any},
	       {type,{Line,27},integer,[]}]}},
	    []},
    Ast = els_transpile:type_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = Type,
    ?assertEqual(Expected, Ast).
type_user_define_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(-type (item) (| (tuple2) (integer) ))", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    Type = {attribute,
	    {Line,2},
	    type,
	    {item,
	     {type,
	      {Line,16},
	      union,
	      [{user_type,{Line,19},tuple2,[]},
	       {type,{Line,28},integer,[]}]}},
	    []},
    Ast = els_transpile:type_(hd(hd(Ret)), tl(hd(Ret)), []),
    %%?assertEqual(a, erl_syntax:revert(Ast)).
    Expected = Type,
    ?assertEqual(Expected, Ast).
    
    
