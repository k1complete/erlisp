-module(repl).
-include_lib("erlisp.hrl").

-export([repl/4]).

repl(IN, _OUT, Line, Env) ->
    {ok, Tokens, NextLine, _Rest} = scan:read(IN, "erlisp[~B]> ", Line, [], 0),
    io:format("~p~n", [NextLine]),
    {ok, Forms}  = parser:parse(Tokens),
    Exp = transpile:form(Forms, Env),
    {value, Result, NextEnv} = erl_eval:expr(erl_syntax:revert(Exp), Env),
    io:format("~p~n", [Result]),
    repl(IN, _OUT, NextLine, NextEnv).
    
