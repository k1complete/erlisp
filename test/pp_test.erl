-module(pp_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

%%% new-pp
%%%[list, [tuple, a, b], c]
%%%-->
%%%[{a b} c]
-include("els.hrl").
pp2(#item{type=atom, value=V}=S, L, R, _Direction) ->
    S#item{value=L++V++R};
pp2([S], LChar, RChar, _Dir) ->
    R = pp2(S, LChar++"(", ")" ++ RChar, both),
    [R];
pp2([H|T], LChar, RChar, _Dir) when not is_list(T) ->
    Head = pp2(H, LChar++"(", "", open),
    Last = pp2(T, "", ")"++RChar, close),
    Middle = [#item{type=atom, value="."}],
    Head++Middle++Last;
pp2(S, LChar, RChar, _Dir) when is_list(S) ->
    Head = pp2(hd(S), LChar++"(", "", open),
    Last = pp2(lists:last(S), "", ")"++RChar, close),
    Middle = lists:map(fun(E) ->
			       pp2(E, "", "", none)
		       end, lists:sublist(S, 2, length(S) -2)),
    [Head]++Middle++[Last].
pp2_test() ->
    R = [#item{type=atom, value="list"}, 
	 [#item{type=atom, value="tuple"}, 
	  #item{type=atom, value="a"}, 
	  #item{type=atom, value="b"}],
	 #item{type=atom, value="c"}],
    Exp = [#item{type=atom, value="(list"}, 
	 [#item{type=atom, value="(tuple"}, 
	  #item{type=atom, value="a"}, 
	  #item{type=atom, value="b)"}],
	 #item{type=atom, value="c)"}],
    ?assertEqual(Exp, pp2(R, "", "", none)).
pp2_2_test() ->
    R = [#item{type=atom, value="list"}, 
	 [#item{type=atom, value="tuple"}, 
	  #item{type=atom, value="a"}, 
	  #item{type=atom, value="b"}]],
    Exp = [#item{type=atom, value="(list"}, 
	   [#item{type=atom, value="(tuple"}, 
	    #item{type=atom, value="a"}, 
	    #item{type=atom, value="b))"}]],
    ?assertEqual(Exp, pp2(R, "", "", none)).

	 



%%%

pp_test() ->
    A="""
(a b
  (defun a (a b c)
      ((cars) ad aa))
  c)
""",
    B = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A))))), 
				  "", "", none)),
    ?assertEqual([A], io_lib:format("~s", [prettypr:format(B)])),
    A2="""
(a b
  (defun a (a (b b) c)
      ((car) ad ab))
  c)
""",
    B2 = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A2))))),
                           "", "", none)),
    ?assertEqual([A2], io_lib:format("~s", [prettypr:format(B2)])).
pp_spec_test() ->
    A="""
(-spec a ((any) (any) (any)) (any))
""",
    B = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A))))), 
                           "", "", none)),
    ?assertEqual([A], io_lib:format("~s", [prettypr:format(B)])).
pp_multi_spec_test() ->
    A="""
(-spec main
    ((a :: 1)) 2
    ((integer)) (integer))

""",
    B = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A))))), 
                           "", "", none)),
    ?assertEqual([A], io_lib:format("~s", [prettypr:format(B)])).
pp_multi_spec2_test() ->
    A="""
(-spec m
    ((a :: 1)) (integer)
    ((list)) (list))

""",
    B = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A))))), 
                           "", "", none)),
    ?assertEqual([A], io_lib:format("~s", [prettypr:format(B)])).
