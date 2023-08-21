-module(compile_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include_lib("elisp_docs.hrl").
compile_test() ->
    {ok, _ModuleName, _Binary} = elisp_compile:file("test/testdata/c1.elisp", []),
    ?assertEqual(2 , c1:erun(1)).


compile_doctest_test() ->
    {ok, ModuleName, Binary, _Ast} = elisp_compile:file_ast("test/testdata/doctest.elisp", []),
    %%error_logger:info_report(Ast),
    File = atom_to_list(ModuleName) ++ ".beam",
    file:write_file(File, Binary),
    code:load_file(ModuleName),
    %%error_logger:info_report(ModuleName),
    {ok, MMM} = code:get_doc(ModuleName),
    ?assertMatch(#docs_v1{}, MMM),
    %%error_logger:info_report(MMM),
    ?assertEqual(2 , doctest:main(1)).

