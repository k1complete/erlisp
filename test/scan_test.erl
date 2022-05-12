-module(scan_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

plus_test() ->
    Line=?LINE,
    S="(+ 1 1)",
    SR = scan:string(S, Line),
    SRR=element(2, SR),
    io:format("SRR:~p~n", [SRR]),
    {ok, SP}=tlisp:parse(SRR),
    io:format("SP:~p~n", [SP]),
    TP=transpile:form(SP),
    SE = merl:quote(Line, "1+1"),
    A=erl_prettypr:format(SE),
    B=erl_prettypr:format(TP),
    ?assertEqual(A, B).

