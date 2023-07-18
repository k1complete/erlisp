-module(compile_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

compile_test() ->
    {ok, ModuleName, Binary} = elisp_compile:file("test/testdata/c1.elisp", []),
    ?assertEqual(2 , c1:erun(1)).


compile_doctest_test() ->
    {ok, ModuleName, Binary} = elisp_compile:file("test/testdata/doctest.elisp", []),
    ?assertEqual(2 , doctest:main(1)).

