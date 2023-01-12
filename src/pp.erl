-module(pp).
-include("erlisp.hrl").
-export([pp/1]).
-export([test/1]).
-export([ppsexp/1]).
-export([pptr/4]).
-export([pptr_form/1]).

pps(S) when is_list(S) ->
    [<<"(">>, lists:join(<<" ">>, lists:map(fun(E) ->
                                                    pps(E)
                                            end, S)),
     <<")">>];
pps(S) when is_integer(S) ->
    integer_to_binary(S);
pps(S) when is_record(S, item) ->
    list_to_binary(S#item.value).


pp(S) ->
    io:format("~p~n", [S]),
    pps(S).
ppliteral(Value, {LLevel, LChar}, {_RLevel, _RChar}, 1) ->
    Chars = lists:foldl(fun(_E, A) -> A++LChar end, [], lists:seq(1,LLevel)),
    Chars ++ Value;
ppliteral(Value, {_LLevel, _LChar}, {RLevel, RChar}, -1) ->
    Chars = lists:foldl(fun(_E, A) -> A++RChar end, [], lists:seq(1,RLevel)),
    Value ++ Chars;
ppliteral(Value, _, _, _) ->
    Value.

pptr(#item{type=atom, value=V}=S, L, R, Direction) ->
    S#item{value=ppliteral(V, L, R, Direction)};
pptr(V, L, R, Direction) when is_integer(V)  ->
    #item{value=ppliteral(integer_to_list(V), L, R, Direction), type=string};
pptr([S], {LLevel, LChar}, {RLevel, RChar}, _Direction) ->
    Head = pptr(S, {LLevel+1, LChar}, {0, RChar}, 1),
    pptr(Head, {0, LChar}, {RLevel+1, RChar}, -1);
pptr(S, {LLevel, LChar}, {RLevel, RChar}, _Direction) when is_list(S) ->
    Head =  pptr(hd(S), {LLevel+1, LChar} ,{0, RChar}, 1),
    Last =  pptr(lists:last(S), {0, LChar} ,{RLevel+1, RChar}, -1),
    Middle = lists:map(fun(E) ->
                               pptr(E, {0, LChar}, {0, RChar}, 0)
                       end,
                       lists:sublist(S, 2, length(S) - 2)),
    lists:append([[Head], Middle, [Last]]).

pptr_form(S) ->
    pptr(S, {0, "("}, {0, ")"}, 0).

ppsexp(S) when is_list(S), length(S) > 2 ->
    [H1,H2|T] = S,
    H1S = ppsexp(H1),
    Indent = length("("++prettypr:format(H1S)),
    H2S = ppsexp(H2),
    Seps = lists:map(fun(E) ->
                             ppsexp(E)
                     end, T),
    prettypr:par([H1S, H2S, prettypr:sep(Seps)], Indent);
ppsexp(S) when is_list(S) andalso length(S) == 2 ->
    Pars = lists:map(fun(E) ->
                             ppsexp(E)
                     end, S),
    prettypr:par(Pars);
ppsexp(#item{type=atom, value=V}) ->    
    prettypr:text(V);
ppsexp(#item{type=string, value=V}) ->    
    prettypr:text(V);
ppsexp(S) when is_integer(S) ->
    prettypr:text(integer_to_list(S)).

test(S) ->
    ppsexp(S).
