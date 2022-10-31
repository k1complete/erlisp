-module(transpile_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

-define(TQ(Line, T), merl:quote(Line, T)).

length_test() ->
    Line = ?LINE,
    {ok, Tokens, _Lines} = scan:string("(length (cons 1 (cons 2 nil)))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C4=transpile:form(Tree, []),
    C5=?TQ(Line, "length([1,2])"),
    ?assertEqual(C5, erl_syntax:revert(transpile:locline(C4))).
    
lists_reverse_test() ->
    Line = ?LINE,
    {ok, Tokens, _Lines} = scan:string("(lists:reverse (cons 1 (cons 2 nil)))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C4=transpile:form(Tree, []),
    C5=?TQ(Line, "lists:reverse([1,2])"),
    ?assertEqual(C5, erl_syntax:revert(transpile:locline(C4))).

lists_reverse2_test() ->
    Line = ?LINE,
    {ok, Tokens, _Lines} = scan:string("(lists:reverse (quote (1 2 3)))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C4=transpile:form(Tree, []),
    C5=?TQ(Line, "lists:reverse([1,2,3])"),
    ?assertEqual(C5, erl_syntax:revert(transpile:locline(C4))).
    
quote_var_test() ->
    Line = ?LINE,
    {ok, Tokens, _Lines} = scan:string("(quote A)", Line),
    {ok, Tree} = parser:parse(Tokens),
    C4=transpile:form(Tree, []),
    C5 = merl:quote(Line, "'A'"),
    ?assertEqual(C5, erl_syntax:revert(transpile:locline(C4))).

quote_list_test() ->
    Line = ?LINE,
    {ok, Tokens, _Lines} = scan:string("(quote ((quote a) A 1))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C4=transpile:form(Tree, []),
    C5 = ?TQ(Line, "[a, 'A', 1]"),
    ?assertEqual(erl_syntax:revert(C5), erl_syntax:revert(transpile:locline(C4))).
    
defun_form_test() ->
    Line = ?LINE,
    {ok, Tokens, _Lines} = scan:string("(defun plus (A B) (+ A B))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C4=transpile:form(Tree, []),
    C5 = merl:quote(Line, "plus(A, B) -> A + B."),
    ?assertEqual(C5, erl_syntax:revert(transpile:locline(C4))).

defun_form2_test() ->
    Line = ?LINE,
    {ok, Tokens, _Lines} = scan:string("(defun plus (A B) (+ A B) (+ A B))", Line),
    {ok, Tree} = parser:parse(Tokens),
    C4 = transpile:form(Tree, []),
    C5 = merl:quote(Line, "plus(A, B) -> A + B, A + B."),
    ?assertEqual(C5, erl_syntax:revert(transpile:locline(C4))).

defun_match_test() ->
    Line = ?LINE,
    Cmd = ["(defun plus (((match A 1) B)\n",
           "(+ A B))\n",
           "((A B) (+ A B)))\n"],
    {ok, Tokens, _Lines} = scan:string(lists:flatten(Cmd), Line),
    {ok, Tree} = parser:parse(Tokens),
    C4=transpile:form(Tree, []),
    io:format("~nC4 ~p~n", [C4]),
    C5 = merl:quote(Line, ["plus(A=1, B) ->", 
                           " A+B;",
                          "plus(A, B) -> A + B."]),
    ?assertEqual(C5, erl_syntax:revert(transpile:locline(C4))).
export_test() ->
    Line = ?LINE,
    C4 = transpile:form(?TQ(Line,
                            ["[export, [a, 2], [b, 3]]"]), []),
    C5 = merl:quote(Line, ["-export([a/2, b/3])."]),
    ?assertEqual(C5, erl_syntax:revert(C4)).
module_test() ->
    Line = ?LINE,
    C4 = transpile:form(?TQ(Line,
                            ["[module, b]"]), []),
    C5 = merl:quote(Line, ["-module(b)."]),
    ?assertEqual(C5, erl_syntax:revert(C4)).

equal_test() ->
    Line=?LINE,
%    C4 = transpile:form(?Q(Line, "['==', 1, 2]")),
    C4 = transpile:form({cons,1,
                         {atom,1,'=='},
                         {cons,1,{integer,1,1},{cons,1,{integer,1,2},{nil,1}}}}, []),
    C5 = merl:quote( "1 == 2"),
    ?assertEqual(C5, erl_syntax:revert(C4)).

macro_test() ->
    C4 = macro:expand_form({cons,1,
                            {atom,1,'=='},
                            {cons,1,
                             {integer,1,1},
                             {cons,1,
                              {integer,2,2},
                              {nil,2}}}},
                    #{"==" => fun(L) ->
                                      H = erl_syntax:list_head(L),
                                      T = erl_syntax:list_tail(L),
                                      NH = erl_syntax:atom("!="),
                                      NNH=erl_syntax:copy_pos(H, NH),
                                      R=erl_syntax:cons(NNH, T),
                                      io:format("~p~n", [NNH]),
                                      RR=erl_syntax:copy_pos(H, R),
                                      io:format("L:~p~n", [L]),
                                      io:format("T:~p~n", [T]),
                                      io:format("R:~p~n", [R]),
                                      io:format("RR:~p~n", [RR]),
                                      RR
                              end}
                   ),
    C5= {cons,1,
         {atom,1,'!='},
         {cons,1,{integer,1,1},{cons,1,{integer,2,2},{nil,1}}}},
    ?assertEqual(C5, erl_syntax:revert(C4)).


macro_2_test() ->
    C4 = macro:expand_form({cons,1,
                            {atom,1,'=='},
                            {cons,1,
                             {integer,1,1},
                             {cons,1,
                              {atom,1,'=='},
                              {cons,1,
                               {integer,2,2},
                               {nil,2}}}}},
                    #{"==" => fun(L) ->
                                      H = erl_syntax:list_head(L),
                                      T = erl_syntax:list_tail(L),
                                      NH = erl_syntax:atom("!="),
                                      NNH=erl_syntax:copy_pos(H, NH),
                                      R=erl_syntax:cons(NNH, T),
                                      RR=erl_syntax:copy_pos(H, R),
                                      RR
                              end}
                   ),
    C5= {cons,1,
         {atom,1,'!='},
         {cons,1,{integer,1,1},
          {cons,1,
           {atom,1,'!='},
           {cons, 1, 
            {integer,2,2},{nil,1}}}}},
    ?assertEqual(C5, erl_syntax:revert(C4)).


