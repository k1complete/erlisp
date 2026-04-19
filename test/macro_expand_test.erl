-module(macro_expand_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

macro_expand_test() ->
    _Line = 1,
    Src0 = "(defmacro add (a b) `(+ ,a ,b))\n",
    Src1 = "(macro-expand (add 2 3))",
    {value, Ret0, _Env0} =  els_repl:source(Src0++Src1, []),
    %%{value, Ret1, Env1} =  els_repl:source(Src1, Env0),
    ?assertEqual(['+', 2, 3], Ret0).
    %%?assertEqual(5, Ret0).

macro_expand_quote_test() ->
    _Line = 1,
    Src0 = "(defmacro add (a b) `(+ ,a ,b))\n",
    Src2 = "(macro-expand '(add 2 3))",
    {value, Ret1, _Env1} =  els_repl:source(Src0++Src2, []),
    ?assertEqual([quote, [add, 2, 3]], Ret1).

macro_expand_backquote_test() ->
    _Line = 1,
    Src0 = "(defmacro add (a b) `(+ ,a ,b))\n",
    Src3 = "(macro-expand `(add 2 3))",
    {value, Ret2, _Env2} =  els_repl:source(Src0++Src3, []),
    ?assertEqual(['lists:append', 
		  [list, [list, [quote, add]],
		   [list, [quote, 2]],
		   [list, [quote, 3]]]], Ret2).

