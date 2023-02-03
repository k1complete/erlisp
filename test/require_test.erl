-module(require_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

require_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("(require 'mod)(erlang:hd (mod:reverse (1 2 'a)))", Line),
    Ret = parser:parse(Tokens),
    io:format("Ret: ~p: ~n~p~n", [Ret, Tokens]),
    {ok, Tree} = Ret,
    ets:new(require, [named_table]),
    C = lists:map(fun(E) ->
                          erl_syntax:revert(transpile:form(E, [{require, require}]))
                  end, Tree),
    
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("---~p~n--- ~p ~n binding ~p~n", [Tree, C, Binding]),
    
    {value, Result, Binding} = erl_eval:expr_list(C, Binding),
    ?assertEqual([1], Result).

