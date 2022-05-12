-module(transpile_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

-define(TQ(Line, T), merl:quote(Line, T)).

length_test() ->
    Line = ?LINE,
    C4=transpile:form(?TQ(Line, "[length, [cons, 1, [cons, 2, []]]]")),
    C5=?TQ(Line, "length([1,2])"),
    ?assertEqual(C5, erl_syntax:revert(C4)).
    
lists_reverse_test() ->
    Line = ?LINE,
    C4=transpile:form(?TQ(Line, "['lists:reverse', [cons, 1, [cons, 2, []]]]")), 
    C5=?TQ(Line, "lists:reverse([1,2])"),
    ?assertEqual(C5, erl_syntax:revert(C4)).

lists_reverse2_test() ->
    Line = ?LINE,
    C4=transpile:form(?TQ(Line, "['lists:reverse', [quote, [1, 2, 3]]]")),
    C5=?TQ(Line, "lists:reverse([1,2,3])"),
    ?assertEqual(C5, erl_syntax:revert(C4)).
    
quote_var_test() ->
    Line = ?LINE,
    C4 = transpile:form(merl:quote(Line, "[quote, A]")), 
    C5 = merl:quote(Line, "A"),
    ?assertEqual(C4, C5).

quote_list_test() ->
    Line = ?LINE,
    C4 = transpile:form(?TQ(Line, "[quote, [a, A, 1]]")), 
    C5 = ?TQ(Line, "[a, A, 1]"),
    ?assertEqual(C4, C5).
    
defun_form_test() ->
    Line = ?LINE,
    C4 = transpile:form(?TQ(Line, "[defun, plus, [A, B], ['+', A, B]]")),
    C5 = merl:quote(Line, "plus(A, B) -> A + B."),
    ?assertEqual(C5, erl_syntax:revert(C4)).

defun_form2_test() ->
    Line = ?LINE,
    C4 = transpile:form(?TQ(Line, "[defun, plus, [A, B], ['+', A, B], ['+', A, B]]")),
    C5 = merl:quote(Line, "plus(A, B) -> A + B, A + B."),
    ?assertEqual(C5, erl_syntax:revert(C4)).

defun_match_test() ->
    Line = ?LINE,
    C4 = transpile:form(?TQ(Line, 
                            ["[defun, plus, [[[match, A,1], B], ",
                             "['+', A, B]], ",
                             "[[A, B], ['+', A, B]]]"])),
    C5 = merl:quote(Line, ["plus(A=1, B) ->", 
                           " A+B;",
                          "plus(A, B) -> A + B."]),
    ?assertEqual(C5, erl_syntax:revert(C4)).
export_test() ->
    Line = ?LINE,
    C4 = transpile:form(?TQ(Line,
                            ["[export, [a, 2], [b, 3]]"])),
    C5 = merl:quote(Line, ["-export([a/2, b/3])."]),
    ?assertEqual(C5, erl_syntax:revert(C4)).
module_test() ->
    Line = ?LINE,
    C4 = transpile:form(?TQ(Line,
                            ["[module, b]"])),
    C5 = merl:quote(Line, ["-module(b)."]),
    ?assertEqual(C5, erl_syntax:revert(C4)).
