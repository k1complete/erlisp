-module(mod).
-export(['MACRO_reverse'/1]).
'MACRO_reverse'(L) ->
    io:format("reverse ~p~n", [L]),
    [yal_util:make_symbol(list) | lists:reverse(L)].
