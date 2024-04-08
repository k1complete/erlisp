-module(receive_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

receive_test() ->
    Line = ?LINE,
    Command = lists:flatten(["(! (self) A)", 
			     "(receive (A (tuple 'ok A)))"]),
    {ok, Tokens, _Line} = els_scan:from_string(Command, Line),
    {ok, Tree} =els_parser:parse(Tokens),
    C = lists:map(fun(E) ->
			  els_transpile:form(E, [])
		  end, Tree),
    Cr = lists:map(fun(E) ->
			   erl_syntax:revert(E)
		   end, C),
    Binding=erl_eval:add_binding('A', 1, erl_eval:new_bindings()),
    ?assertEqual({value, {ok, 1}, [{'A', 1}]},
                 erl_eval:exprs(Cr, Binding)).
