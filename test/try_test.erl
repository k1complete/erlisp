-module(try_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

try_catch_test() ->
    %% Line = ?LINE,
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

try_after_test() ->
    %% Line = ?LINE,
    L = lists:flatten(["(try (/ 1 A) ",
		       "after (put 'b 2))"]),
    {ok, Tokens, _Line} = els_scan:from_string(L),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    put(b, 0),
    ?assertEqual({value, 1.0, [{'A', 1}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding)),
    ?assertEqual(2, get(b)).

try_of_pattern_catch_test() ->
    %% Line = ?LINE,
    L = lists:flatten(["(try (/ 1 A) of ",
		       "(a (when (> a  0)) ",
		       "   'plus)",
		       "(a (when (< a  0)) ",
		       "   'minus)",
		       "catch ",
		       " (('error c)",
		       "   (io:format \"~p~n\" (list c))",
		       "   c)",
		       "after ",
		       " (put 'b 2)",
		       ")"
		      ]),
    {ok, Tokens, _Line} = els_scan:from_string(L),
    {ok, [Tree]} =els_parser:parse(Tokens),
    C = els_transpile:form(Tree, []),
    Binding1=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    ?assertEqual({value, plus, [{'A', 1}, {a, 1.0}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding1)),

    Binding2=erl_eval:add_binding('A', -1, erl_eval:new_bindings()),
    put(b, 0),
    ?assertEqual({value, minus, [{'A', -1}, {a, -1.0}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding2)),
    ?assertEqual(2, get(b)),

    Binding30=erl_eval:add_binding('A', 0, erl_eval:new_bindings()),
    %Binding3=erl_eval:add_binding('b', [1,2,3], Binding30),
    put(b, 0),
    ?assertEqual({value, badarith, [{'A', 0}, {c, badarith}]},
                 erl_eval:expr(erl_syntax:revert(C), Binding30)),
    ?assertEqual(2, get(b)).
    
try_no_clause_test() ->
    %% Line = ?LINE,
    L = lists:flatten(["(try (/ 1 A) )"]),
    {ok, Tokens, _Line} = els_scan:from_string(L),
    {ok, [Tree]} =els_parser:parse(Tokens),
    ?assertThrow(
       [{error, {try_must_be_after_or_catch_clause, 
		 {0,2}, 
		 _}}],
       els_transpile:form(Tree, [])
      ).


