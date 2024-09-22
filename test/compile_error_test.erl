-module(compile_error_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include_lib("els.hrl").
-include_lib("els_docs.hrl").
compile_test() ->
    ?assertException(throw, {error, {bad_arity, {4,9}, {"*", 1}}},
		     els_compile:file("test/testdata/c1_error.elisp", [])).


compile_doctest_test() ->
    RR = els_compile:file_ast("test/testdata/doctest.elisp", []),
    {ok, Module, Binary, _Ast} = RR,
    error_logger:info_report(_Ast),
    io:format("doctest: ~p~n", [_Ast]),
    ModuleName = atom_to_list(Module),
    File = ModuleName ++ ".beam",
    file:write_file(File, Binary),
    %%code:load_file(Module),
    false = code:purge(Module),
    {module, Module} = code:load_binary(Module, File, Binary),
    {file, File} = code:is_loaded(Module),
    ?LOG_ERROR(#{loaded => Module}),
    {Module, _Binary2, _File2} = code:get_object_code(Module),
    {ok, MMM} = code:get_doc(Module),
    ?assertMatch(#docs_v1{}, MMM),
    %%error_logger:info_report(MMM),
    ?assertEqual(2 , doctest:main(1)).

compile_with_macro_test() ->
    {ok, Module, _Binary, _Ast} = els_compile:file_ast("test/testdata/macrotest.elisp", []),
    ?assertEqual({module, macrotest}, code:ensure_loaded(macrotest)),
    %%error_logger:info_report(Ast).
    {file, _File} = code:is_loaded(Module),
    ?assertEqual(true , macrotest:main(3,2)),
    ?assertEqual('it' , macrotest:main(1,2)).

compile_with_macrofunccall_test() ->
    {ok, Module, _Binary, Ast} = els_compile:file_ast("test/testdata/macrofunccall.elisp", []),
    ?assertEqual({module, macrofunccall}, code:ensure_loaded(macrofunccall)),
    error_logger:info_report(erl_syntax:revert_forms(Ast)),
    {file, _File} = code:is_loaded(Module),
    ?assertEqual(1 , macrofunccall:main("a",2)).
    
