-module(pattern_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

string_prefix_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = els_scan:from_string("(= (++ \"prefix\" a) \"prefixlist\")", Line),
    {ok, Ret} = els_parser:parse(Tokens),
    io:format("tokens ~p~n", [Ret]),
    C = els_transpile:form(hd(Ret), []),
    Cc = erl_syntax:revert(C),
    io:format("forms  ~p~n revert ~p ~n", [C, Cc]),
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    {value, Result, Binding2}  = erl_eval:expr(Cc, Binding),
    ?assertEqual("prefixlist", Result),
    ?assertEqual([{a, "list"}, {b, 3}], Binding2).
    


