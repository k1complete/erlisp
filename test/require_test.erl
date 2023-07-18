-module(require_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

require_test() ->
    Line=?LINE,
    {ok, Tokens, _Line} = scan:from_string("(-require 'mod)(erlang:hd (mod:reverse (1 2 'a)))", Line),
    Ret = parser:parse(Tokens),
    io:format("Ret: ~p: ~n~p~n", [Ret, Tokens]),
    {ok, Tree} = Ret,
    case ets:info(require) of
        undefined ->
            ets:new(require, [named_table]);
        _ ->
            require
    end,
    C = lists:map(fun(E) ->
                          erl_syntax:revert(transpile:form(E, [{'-require', require}]))
                  end, Tree),
    
    Binding = erl_eval:add_binding(b, 3, erl_eval:new_bindings()),
    io:format("---~p~n--- ~p ~n binding ~p~n", [Tree, C, Binding]),
    
    {value, Result, Binding}  = erl_eval:exprs(C, Binding),
    ?assertEqual(a, Result).

getmacros_test() ->
    A = yal_util:getmacros(yal_macro),
    ?assertEqual(#{{"yal_macro", "backquote", 1} => {yal_macro, 'MACRO_backquote'}},
                 A).

-define(TQ(Line, T), merl:quote(Line, T)).
macro_export_test() ->
    Line = ?LINE,
    Cmd = ["(-macro_export (a 2) (b 3))"],
    {ok, Tokens, _Lines} = scan:string(lists:flatten(Cmd), Line),
    {ok, [Tree]} = parser:parse(Tokens),
    C4=transpile:form(Tree, []),
    C5 = merl:quote(Line, ["-export(['MACRO_a'/2, 'MACRO_b'/3])."]),
    ?assertEqual(C5, erl_syntax:revert(transpile:locline(C4))).

