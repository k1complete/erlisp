-module(elisp_compile).
-export([file/2, file/1
        ]).

file(File) ->
    file(File, []).

file(File, Opt) ->
    io:format("cwd ~p", [file:get_cwd()]),
    Module = m,
    {ok, Tokens} = scan:file(File, Opt),
    io:format("scan ~p", [Tokens]),
    {ok, Forms} = parser:parse(Tokens),
    Env=[],
    Ast = lists:map(fun(F) ->
                            R = transpile:form(F, Env),
                            io:format("Trans ~p~n", [R]),
                            R
                          end, Forms),
    io:format("Ast ~p~n", [Ast]),
    {ok, Binary} = merl:compile_and_load(Ast, [debug_info]),
    io:format("compiled ~p", [Binary]),
    {ok, Module, Binary}.
