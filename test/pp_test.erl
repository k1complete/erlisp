-module(pp_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

pp_test() ->
    A="(a b (defun a (a b c) ((car) ad aa)) c)",
    B = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A))))), 
                          {0, "("}, {0, ")"}, 0)),
    ?assertEqual([A], io_lib:format("~s", [prettypr:format(B)])),
    A2="(a b (defun a (a (b b) c) ((car) ad aa)) c)",
    B2 = els_pp:ppsexp(els_pp:pptr(hd(element(2, els_parser:parse( element(2, els_scan:from_string(A2))))),
                           {0, "("}, {0, ")"}, 0)),
    ?assertEqual([A2], io_lib:format("~s", [prettypr:format(B2)])).
