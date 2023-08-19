-module(compile_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

compile_test() ->
    {ok, ModuleName, Binary} = elisp_compile:file("test/testdata/c1.elisp", []),
    ?assertEqual(2 , c1:erun(1)).


compile_doctest_test() ->
    {ok, ModuleName, Binary, Ast} = elisp_compile:file_ast("test/testdata/doctest.elisp", []),
    error_logger:info_report(Ast),
    File = atom_to_list(ModuleName) ++ ".beam",
    file:write_file(File, Binary),
    code:load_file(ModuleName),
    error_logger:info_report(ModuleName),
    MMM = code:get_doc(ModuleName),
    error_logger:info_report(MMM),
    ?assertEqual(2 , doctest:main(1)).

