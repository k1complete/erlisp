-module(script_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

hello_world_test() ->
    Line =?LINE,
    A = """
        (= a 1)
        (let ((a (+ a 1)) )
           (+ a a))
    """,
    Opt = [{'?Line', Line}],
    ?assertEqual(4, element(2, els_repl:source(A, Opt))).

local_macro_test() ->
    Line =?LINE,
    A = """
        (defun strlen (str)
          (length str))
        (defmacro alen (n str)
          `(* ,n ,(strlen (element 2 str))))
        (defmacro aif (bool istrue else)
          `(case (match it ,bool)
             ('true ,istrue)
             ('false ,else)))
        (defun main (a b)
          (aif (> a b)
               it
               'it))
        (defun main2 (a b)
           (alen (* a b) "abc"))
        (main (main2 2 3) 2)
    """,
    Opt = [{'?Line', Line}],
    S = els_repl:source(A, Opt),
    ?assertEqual({value, true, undefined}, setelement(3, S, undefined)).
    %%true.

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
    C2Ext = {tree,function,
                  {attr,{Line,2},[],none},
                  {func,
                   {tree,atom,{attr,0,[],none},main},
                   [{tree,clause,
                     {attr,{Line,2},[],none},
                     {clause,
                      [{tree,variable,{attr,{Line,14},[],none},a},
                       {tree,variable,{attr,{Line,16},[],none},b}],
                      none,
                      [{tree,application,
                        {attr,0,[],none},
                        {application,
                         {tree,atom,{attr,0,[],none},length},
                         [{tree,variable,{attr,{Line+1,11},[],none},a}]}}]}}]}},
    ?assertEqual(C2Ext, C2),
    io:format("Ret: ~p~n", [C2]),
    {NFunDic2, Name2, Arity2} = els_localfun:register_local_func(C2, NFunDic),
    LocalF2 = els_localfun:create_valuefun(NFunDic2),
    S2 = merl:quote("main(\"c12\", \"d12\")"),
    Ret2 = erl_eval:expr(S2, [], {value, LocalF2}),
    ?assertEqual({value, 3, []}, Ret2).
   
    
compile_with_macro2_test() ->
    {ok, Module, _Binary, _Ast} = els_compile:file_ast("test/testdata/macrotest.elisp", []),
    ?assertEqual({module, macrotest}, code:ensure_loaded(macrotest)),
    io:format("MACRO2: ~p", [_Ast]),
    %% {file, _File} = code:is_loaded(Module),
    ?assertEqual(18 , macrotest:main2(3,2)),
    ?assertEqual(18 , macrotest:main2(2,3)).
