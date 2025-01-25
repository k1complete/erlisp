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
    {NFunDic, Name, Arity} = els_localfun:register_local_func(C, #{}),
    LocalF = els_localfun:create_valuefun(NFunDic),
    S2 = merl:quote("fib(4)"),
    Ret2 = erl_eval:expr(S2, [], {value, LocalF}),
    ?assertEqual({value, 5, []}, Ret2).

local_macro_test() ->
    Line = ?LINE,
    S = ["(defmacro strlen (s) ",
	 " `(length ,s))"],
    S1 = [
	 "(defun main (a b)", 
	 "  (strlen a))"
	 ],
    {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(lists:join("\n", S)), Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    io:format("CC: ~p~n", [Tree]),
    C = els_transpile:form(Tree, []),
    {NFunDic, Name, Arity} = els_localfun:register_local_func(C, #{}),
    LocalF = els_localfun:create_valuefun(NFunDic),
    Macros = maps:put({"strlen", 1},{{local},  LocalF}, #{}),
    {ok, Tokens1, _Line} = els_scan:from_string(lists:flatten(lists:join("\n", S1)), Line),
    {ok, [Tree1]} = els_parser:parse(Tokens1),
    io:format("CC1: ~p~n", [Tree1]),
    Ret = els_transpile:expand_macro(Tree1, [], Macros),
    C2 = els_transpile:form(Ret, []),
    io:format("Ret: ~p~n", [Ret]),
    {NFunDic2, Name2, Arity2} = els_localfun:register_local_func(C2, NFunDic),
    LocalF2 = els_localfun:create_valuefun(NFunDic2),
    S2 = merl:quote("main(\"c12\", \"d12\")"),
    Ret2 = erl_eval:expr(S2, [], {value, LocalF2}),
    ?assertEqual({value, 3, []}, Ret2).

local_macro_form_test() ->
    Line = ?LINE,
    S = ["(defmacro strlen (s) ",
	 " `(length ,s))"],
    S1 = [
	 "(defun main (a b)", 
	 "  (strlen a))"
	 ],
    {ok, Tokens, _Line} = els_scan:from_string(lists:flatten(lists:join("\n", S)), Line),
    {ok, [Tree]} = els_parser:parse(Tokens),
    io:format("CC: ~p~n", [Tree]),
    C = els_transpile:form(Tree, []),
    {NFunDic, Name, Arity} = els_localfun:register_local_func(C, #{}),
    LocalF = els_localfun:create_valuefun(NFunDic),
    Macros = maps:put({"strlen", 1},{{local},  LocalF}, #{}),
    {ok, Tokens1, _Line} = els_scan:from_string(lists:flatten(lists:join("\n", S1)), Line),
    {ok, [Tree1]} = els_parser:parse(Tokens1),
    Env = [{macros, Macros}],
    io:format("CC1---: ~p~n", [Tree1]),
    io:format("Env---: ~p~n", [Env]),
    %Ret = els_transpile:expand_macro(Tree1, [], Macros),
    C2 = els_transpile:form(Tree1, Env),
    io:format("Ret: ~p~n", [C2]),
    {NFunDic2, Name2, Arity2} = els_localfun:register_local_func(C2, NFunDic),
    LocalF2 = els_localfun:create_valuefun(NFunDic2),
    S2 = merl:quote("main(\"c12\", \"d12\")"),
    Ret2 = erl_eval:expr(S2, [], {value, LocalF2}),
    ?assertEqual({value, 3, []}, Ret2).

    

