-module(try_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

try_test() ->
    Line = ?LINE,
    L = lists:flatten(["(try (/ 1 A) ",
		       "catch (('error b) (io:format \"~p~n\" (list b))))"]),
    {ok, Tokens, _Line} = els_scan:from_string(L),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    ?assertEqual({value, 1.0, [{'A', 1}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)),
    Binding0=erl_eval:add_binding('A', 0, erl_eval:new_bindings()),
    ?assertEqual({value, ok, [{'A', 0}, {b, badarith}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding0)).

