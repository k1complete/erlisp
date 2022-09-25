-module(match_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-define(TQ(Line, T), merl:quote(Line, T)).

binding_test() ->
    Line = ?LINE,
    Left = ?TQ(Line, "[[H|T], {T1, T2, T3}]"),
    Right = ?TQ(Line, "[[1,2,3], {a, 1, 2}]"),
    L = int:binding(Left, Right, [], []),
    ?assertEqual(?TQ(Line, "[[H|T], {T1,T2,T3}]  = [[1,2,3], {a, 1, 2}]"), 
                 erl_syntax:revert(L)).

do_eval_match_test() ->
    Line = ?LINE,
    Left = ?TQ(Line, "[[H|T], {T1, T2, T3}]"),
    Right = ?TQ(Line, "[[1,2,3], {a, 1, 2}]"),
    L = int:binding(Left, Right, [], []),
    ?assertEqual(?TQ(Line, "[[H|T], {T1,T2,T3}]  = [[1,2,3], {a, 1, 2}]"), 
                 erl_syntax:revert(L)),
    L2 = pattern_match:do_eval_match(L, [], #{}),
    ?assertEqual(#{"H" => {integer, Line, 1},
                   "T" => {tree, list,
                           {attr, 0, [], none},
                           {list, [{integer, Line, 2},{integer, Line, 3}], none}},
                   "T1" => {atom, Line, a},
                   "T2" => {integer, Line, 1},
                   "T3" => {integer, Line, 2}
                  }, 
                 L2).

do_eval_underscore_match_test() ->
    Line = ?LINE,
    Left = ?TQ(Line, "[[H|_], {H, _, T3}]"),
    Right = ?TQ(Line, "[[1,2,3], {a, 1, 2}]"),
    L = int:binding(Left, Right, [], []),
    ?assertEqual(?TQ(Line, "[[H|_], {H,_,T3}]  = [[1,2,3], {a, 1, 2}]"), 
                 erl_syntax:revert(L)),
    ?assertException(throw, 
                     {error, {badmatch, {"H", 
                                         {atom, Line, a}, 
                                         {integer, Line, 1}}}},  
                     pattern_match:do_eval_match(L, [], #{})).
do_eval_match_in_match_test() ->
    Line = ?LINE,
    Left = ?TQ(Line, "[[H|_], [match, M, {T1, T2, T3}]]"),
    Right = ?TQ(Line, "[[1,2,3], {a, 1, 2}]"),
    L = int:binding(Left, Right, [], []),
    ?assertEqual(?TQ(Line, "[[H|_], [match, M, {T1,T2,T3}]]  = [[1,2,3], {a, 1, 2}]"), 
                 erl_syntax:revert(L)),
    L2 = pattern_match:do_eval_match(L, [], #{}),
    ?assertEqual(#{"H" => {integer, Line, 1},
                   "M" => {tree,tuple,
                           {attr,46,[],none},
                           [{atom,46,a},{integer,46,1},{integer,46,2}]},
                   "T1" => {atom, Line, a},
                   "T2" => {integer, Line, 1},
                   "T3" => {integer, Line, 2}
                  },
                 L2).
do_eval_macro() ->
    Line = ?LINE,
    Left = ?TQ(Line, "[[H|_], [match, M, {T1, T2, T3}]]"),
    Right = ?TQ(Line, "[[1,2,3], {a, 1, 2}]"),
    L = int:binding(Left, Right, [], []),
    ?assertEqual(?TQ(Line, "[[H|_], [match, M, {T1,T2,T3}]]  = [[1,2,3], {a, 1, 2}]"), 
                 erl_syntax:revert(L)),
    Env= #{{"unquote_splice",1} => [[T], []]}
    L2 = pattern_match:do_eval_match(L, [], #{}),
    ?assertEqual(#{"H" => {integer, Line, 1},
                   "M" => {tree,tuple,
                           {attr,46,[],none},
                           [{atom,46,a},{integer,46,1},{integer,46,2}]},
                   "T1" => {atom, Line, a},
                   "T2" => {integer, Line, 1},
                   "T3" => {integer, Line, 2}
                  },
                 L2).
