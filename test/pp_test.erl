-module(pp_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

pp_test() ->
    A="(a b (defun a (a b c) ((car) ad aa)) c)",
    B = pp:ppsexp(pp:pptr(hd(element(2, parser:parse( element(2, scan:from_string(A))))), 
                          {0, "("}, {0, ")"}, 0)),
    ?assertEqual([A], io_lib:format("~s", [prettypr:format(B)])),
    A2="(a b (defun a (a (b b) c) ((car) ad aa)) c)",
    B2 = pp:ppsexp(pp:pptr(hd(element(2, parser:parse( element(2, scan:from_string(A2))))),
                           {0, "("}, {0, ")"}, 0)),
    ?assertEqual([A2], io_lib:format("~s", [prettypr:format(B2)])).
