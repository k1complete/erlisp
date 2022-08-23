-module(int_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

car_test() ->
    E = #{},
    T = merl:quote(1, "[ cdr, [quote,  [ 1, car, [quote, [3, 4]]]]]"),
    A = #{},
    {ok, T2, E} = int:step(T, A, E),
    ?assertEqual(erl_syntax:concrete(T2), 
                 erl_syntax:concrete(merl:quote(1, "[car, [quote, [3, 4]]]"))).
   
