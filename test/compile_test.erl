-module(compile_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include_lib("erlisp.hrl").
-include_lib("elisp_docs.hrl").
compile_test() ->
    {ok, _ModuleName, _Binary} = elisp_compile:file("test/testdata/c1.elisp", []),
    ?assertEqual(2 , c1:erun(1)).


compile_doctest_test() ->
    {ok, Module, Binary, _Ast} = elisp_compile:file_ast("test/testdata/doctest.elisp", []),
    %%error_logger:info_report(Ast),
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
    {ok, Module, _Binary, _Ast} = elisp_compile:file_ast("test/testdata/macrotest.elisp", []),
    ?assertEqual({module, macrotest}, code:ensure_loaded(macrotest)),
    %%error_logger:info_report(Ast).
    {file, _File} = code:is_loaded(Module),
    ?assertEqual(true , macrotest:main(3,2)),
    ?assertEqual('it' , macrotest:main(1,2)).

compile_with_macrofunccall_test() ->
    {ok, Module, _Binary, Ast} = elisp_compile:file_ast("test/testdata/macrofunccall.elisp", []),
    ?assertEqual({module, macrofunccall}, code:ensure_loaded(macrofunccall)),
    error_logger:info_report(erl_syntax:revert(Ast)),
    {file, _File} = code:is_loaded(Module),
    ?assertEqual(1 , macrofunccall:main("a",2)).
    
