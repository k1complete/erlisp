-module(int_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

car_test() ->
    E = #{},
    T = merl:quote(1, "[ car, [quote,  [ 1, car, [quote, [3, 4]]]]]"),
    A = #{},
    {ok, T2, E} = int:step(T, A, E),
    ?assertEqual(
       erl_syntax:concrete(merl:quote(1, "1")),
       erl_syntax:concrete(T2)).
cdr_test() ->
    E = #{},
    T = merl:quote(1, "[ cdr, [quote,  [ 1, car, [quote, [3, 4]]]]]"),
    A = #{},
    {ok, T2, E} = int:step(T, A, E),
    ?assertEqual(
       erl_syntax:concrete(merl:quote(1, "[car, [quote, [3, 4]]]")),
       erl_syntax:concrete(T2)).

quote_test() ->
    E = #{},
    T = merl:quote(1, "[quote,  [ 1, car, [quote, [3, 4]]]]"),
    A = #{},
    {ok, T2, E} = int:step(T, A, E),
    ?assertEqual(
       erl_syntax:concrete(merl:quote(1, "[1, car, [quote, [3, 4]]]")),
       erl_syntax:concrete(T2)).
set_test() ->
    E = #{},
    T = merl:quote(1, "[set, T, [quote,  [ 1, car, [quote, [3, 4]]]]]"),
    A = #{},
    {ok, T2, E2} = int:step(T, A, E),
    io:format("~p~n", [E2]),
    ?assertEqual([1, car, [quote, [3, 4]]],
       erl_syntax:concrete(maps:get("T", E2))),
    ?assertEqual(
       erl_syntax:concrete(merl:quote(1, "[1, car, [quote, [3, 4]]]")),
       erl_syntax:concrete(T2)).
hd_test() ->
    E = #{},
    T = merl:quote(1, "[erlang:hd, [quote,  [ 1, car, [quote, [3, 4]]]]]"),
    A = #{},
    {ok, T2, E2} = int:step(T, A, E),
    io:format("~p~n", [E2]),
    ?assertEqual(1,
       erl_syntax:concrete(T2)).
varreplace_test() ->
    E = #{"T" => merl:quote(1, "[ 1, car, [quote, [3, 4]]]")},
    T = merl:quote(1, "[erlang:hd, T]"),
    A = #{},
    {ok, T2, E2} = int:step(T, A, E),
    io:format("~p~n", [E2]),
    ?assertEqual(1,
       erl_syntax:concrete(T2)).
varreplace2_test() ->
    E = #{"T" => merl:quote(1, "[ 1, car, [quote, [3, 4]]]")},
    T = merl:quote(1, "T"),
    A = #{},
    {ok, T2, E2} = int:step(T, A, E),
    io:format("~p~n", [E2]),
    ?assertEqual([1, car, [quote, [3, 4]]],
       erl_syntax:concrete(T2)).

