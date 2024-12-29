-module(interprete_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").


local_fun_test() ->
    Line = ?LINE,
    S = ["(defun fib ((0) 1) ",
	 "  ((1) 1)",
	 "  ((n) (io:format \"aa\" ()) (+ (fib (- n 1)) (fib (- n 2))) )",
	 ")"],
    {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(lists:join("\n", S)), Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    FunDic = #{},
    NFunDic = els_localfun:create_local_func(C, FunDic),
    els_localfun:put_nfundic(NFunDic),
    LocalF = fun els_localfun:valuefun/2,
    S2 = merl:quote("fib(4)"),
    Ret2 = erl_eval:expr(S2, [], {value, LocalF}),
    ?assertEqual({value, 5, []}, Ret2).

    

