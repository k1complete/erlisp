-module(pp_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

pp_test() ->
    A="""
(a b
   (defun a (a b c)
       ((cars) ad aa))
   c)
""",
    B = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A))))), 
                          {0, "("}, {0, ")"}, none)),
    ?assertEqual([A], io_lib:format("~s", [prettypr:format(B)])),
    A2="""
(a b
   (defun a (a (b b) c)
       ((car) ad ab))
   c)
""",
    B2 = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A2))))),
                           {0, "("}, {0, ")"}, none)),
    ?assertEqual([A2], io_lib:format("~s", [prettypr:format(B2)])).
pp_spec_test() ->
    A="""
(-spec a ((any) (any) (any)) (any))
""",
    B = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A))))), 
                          {0, "("}, {0, ")"}, none)),
    ?assertEqual([A], io_lib:format("~s", [prettypr:format(B)])).
pp_multi_spec_test() ->
    A="""
(-spec main
    ((a :: 1)) 2
    ((integer)) (integer))

""",
    B = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A))))), 
                          {0, "("}, {0, ")"}, none)),
    ?assertEqual([A], io_lib:format("~s", [prettypr:format(B)])).
pp_multi_spec2_test() ->
    A="""
(-spec m
    ((a :: 1)) (integer)
    ((list)) (list))

""",
    B = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A))))), 
                          {0, "("}, {0, ")"}, none)),
    ?assertEqual([A], io_lib:format("~s", [prettypr:format(B)])).
