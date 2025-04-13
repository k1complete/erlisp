-module(script_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

hello_world_test() ->
    Line =?LINE,
    A = """
        (= a 1)
        (let ((a (+ a 1)) )
           (+ a a))
    """,
    Opt = [{line, Line}],
    ?assertEqual({value, 4, [{binding, [{a, 1}]}, {'?Line', 3}]}, els_repl:source(A, Opt)).
    
