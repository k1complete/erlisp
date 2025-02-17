-module(els_pp).
-include("els.hrl").
-export([pp/1]).
-export([test/1]).
-export([ppsexp/1]).
-export([pptr/4]).
-export([form/1]).
-export([erl_to_ast/1]).
-export([format/2]).
-export([npp/4]).

pps(S) when is_list(S) ->
    [<<"(">>, lists:join(<<" ">>, lists:map(fun(E) ->
                                                    pps(E)
                                            end, S)),
     <<")">>];
pps(S) when is_integer(S) ->
    integer_to_binary(S);
pps(S) when is_record(S, item) ->
    list_to_binary(S#item.value).

npp([H], Left, Right, Direct) ->
    io:format("in single open ~p ~p~n", [H, {Left, Right, Direct}]),
    R = npp(H, Left+1, Right+1, both),
    [R];
npp([H|T], Left, Right, Direct) ->
    io:format("list ~p, ~p ~p~n", [H, T, {Left, Right, Direct}]),
    %Head = npp(H, Left+1, Right, open),
    Head = npp(H, Left+1, 0, open),
    LLast = lists:last(T),
    io:format("listlast  ~p~n", [LLast]),
    Last = npp(lists:last(T), 0, Right+1, close),
    Mid = lists:map(fun(E) -> npp(E, Left, Right, none) end, lists:sublist(T, 1, length(T) -1)),
    [Head]++Mid++[Last];
npp({H}, Left, _Right, open) ->
    R = lists:foldl(fun(_E, A) -> "("++ A end, H, lists:seq(1, Left)),
    {R};
npp({H}, _Left, Right, close) ->
    R = lists:foldl(fun(_E, A) -> A++ ")" end, H, lists:seq(1, Right)),
    {R};
npp({H}, Left, Right, both) ->
    io:format("leaf both ~p ~p ~p~n", [H, Left, Right]),
    R = npp(npp({H}, Left, Right, open), Left, Right, close),
    R;
npp({H}, _Left, _Right, none) ->
    {H}.
    
pp(S) ->
    io:format("~p~n", [S]),
    pps(S).
ppliteral(Value, {LLevel, LChar}, {_RLevel, _RChar}, open) ->
    Chars = lists:foldl(fun(_E, A) -> A++LChar end, [], lists:seq(1,LLevel)),
    io:format("ppliteral-l: ~p ~p~n", [Value, Chars]),
    Chars ++ Value;
ppliteral(Value, {_LLevel, _LChar}, {RLevel, RChar}, close) ->
    Chars = lists:foldl(fun(_E, A) -> A++RChar end, [], lists:seq(1,RLevel)),
    io:format("ppliteral-r: ~p ~p~n", [Value, Chars]),
    Value ++ Chars;
ppliteral(Value, {LLevel, LChar}, {RLevel, RChar}, both) ->
    io:format("ppliteral-b: ~p ~p~n", [Value, {LLevel, RLevel}]),
    R = ppliteral(Value, {LLevel, LChar}, {RLevel, RChar}, open),
    ppliteral(R, {LLevel, LChar}, {RLevel, RChar}, close);
ppliteral(Value, _, _, none) ->
    Value.

pptr(#item{type=integer, value=V}=S, L, R, Direction) ->
    S#item{value=ppliteral(V, L, R, Direction)};
pptr(#item{type=atom, value=V}=S, L, R, Direction) ->
    S#item{value=ppliteral(V, L, R, Direction)};
pptr(#item{type=string, value=V}=S, L, R, Direction) ->
    S#item{value=ppliteral("\""++escape(V)++"\"", L, R, Direction)};
pptr(V, L, R, Direction) when is_integer(V)  ->
    #item{value=ppliteral(integer_to_list(V), L, R, Direction), type=integer};
pptr(V, L, R, Direction) when is_float(V)  ->
    #item{value=ppliteral(float_to_list(V), L, R, Direction), type=float};
pptr([S], {LLevel, LChar}, {RLevel, RChar}, Direction) ->
    io:format("pptr1: ~p, levell: ~p Dir: ~p~n", [[S], {LLevel, RLevel}, Direction]),
    {NL, NR} = {LLevel, RLevel},
    io:format("pptr1-1: ~p, levell: ~p Dir: ~p~n", [[S], {NL, NR}, Direction]),
    R = pptr(S, {NL+1, LChar}, {NR+1, RChar}, both),
    R;
pptr(S, {LLevel, LChar}, {RLevel, RChar}, Direction) when is_list(S) ->
    io:format("pptr: ~p, levell: ~p Dir: ~p~n", [S, {LLevel, RLevel}, Direction]),
    Head =  pptr(hd(S), {LLevel+1, LChar} ,{0, RChar}, open),
    io:format("pptr-head: ~p ~n", [Head]),
    Last =  pptr(lists:last(S), {0, LChar} ,{RLevel+1, RChar}, close),
    io:format("pptr-af: ~p~n", [Head]),
    Middle = lists:map(fun(E) ->
                               pptr(E, {0, LChar}, {0, RChar}, none)
                       end,
                       lists:sublist(S, 2, length(S) - 2)),
    lists:append([[Head], Middle, [Last]]).

form(S) ->
    S1 = ?MODULE:erl_to_ast(S),
    S2 = pptr(S1, {0, "("}, {0, ")"}, 0),
    ?MODULE:ppsexp(S2).

format(S, Column) ->
    prettypr:format(form(S), Column).

ppbody(Body) ->
    BS = lists:map(fun(E) ->
			   prettypr:break(ppsexp(E))
		   end, Body),
    Sep = prettypr:sep(BS),
    prettypr:nest(2, Sep).

ppclause([Pattern, When=[#item{value="("++W}|_] | Body]) when W=:="whenc"; W=:="whend"; W=:="when" ->
    io:format("A ~n", []),
    prettypr:par([ppsexp(Pattern),
		  ppsexp(When),
		  ppbody(Body)], 2);
ppclause([Pattern | Body]) ->
    io:format("B ~n", []),
    prettypr:par([ppsexp(Pattern),
		  ppbody(Body)], 2).

pparg_returntype([], Acc) ->
    io:format("returntype[~p] ~p~n", [length(Acc), Acc]),
    lists:reverse(Acc);
pparg_returntype([Arg, Return|Rest], Acc) ->
    io:format("Arg: ~p~n", [Arg]),
    E = prettypr:par([ppsexp(Arg), ppsexp(Return)], 0),
    pparg_returntype(Rest, [E|Acc]).

pparg_returntype(A) ->    
    pparg_returntype(A, []).

ppsexp([#item{value="(-spec"}=H1, #item{} = H2, Args=[#item{type=atom, value="(("++N}|_], Return |  Body]) 
  when N=/=$( ->
    io:format("spec ~p ~n", [Args]),
    H1S = ppsexp(H1),
    H2S = ppsexp(H2),
    Sep = pparg_returntype([Args, Return| Body]),
    ReturnType = case length(Sep) of
		     1 -> 
			 io:format("single", []),
			 hd(Sep);
		     _ ->
			 C = lists:map(fun(E) -> prettypr:break(E) end, Sep),
			 prettypr:nest(2, prettypr:sep(C))
		 end,
    prettypr:par([H1S, H2S, ReturnType],2);
ppsexp([#item{value="(defun"}=H1, #item{}=H2, Args=[#item{type=atom, value="("++[N|_]}|_] |  Body]) 
  when N=/=$( ->
    io:format("normaldefun ~p ~n", [Args]),
    H1S = ppsexp(H1),
    H2S = ppsexp(H2),
    AS = ppsexp(Args),
    BS = ppbody(Body),
    prettypr:par([H1S, H2S, AS, BS], 2);
ppsexp([#item{value="(defun"}=H1, #item{}=H2 | Clauses]) ->
    io:format("clausedefun ~p ~n", [Clauses]),
    H1S = ppsexp(H1),
    H2S = ppsexp(H2),
    C = lists:map(fun(E) ->
		      prettypr:break(ppclause(E))
	      end, Clauses),
    prettypr:par([H1S, H2S | C], 2);
ppsexp(S) when is_list(S), length(S) > 2 ->
    io:format("ppsexp ~p ~n", [S]),
    [H1,H2|T] = S,
    H1S = ppsexp(H1),
    Indent = length("("++prettypr:format(H1S)),
    H2S = ppsexp(H2),
    Seps = lists:map(fun(E) ->
                             ppsexp(E)
                     end, T),
    prettypr:par([H1S, H2S, prettypr:sep(Seps)], Indent);
ppsexp(S) when is_list(S) andalso length(S) == 2 ->
    io:format("ppsexp2 ~p ~n", [S]),
    Pars = lists:map(fun(E) ->
                             ppsexp(E)
                     end, S),
    prettypr:par(Pars);
ppsexp(#item{type=atom, value=V}) when is_atom(V) ->    
    prettypr:text(atom_to_list(V));
ppsexp(#item{type=atom, value=V}) ->
    prettypr:text(V);
ppsexp(#item{type=string, value=V}) ->    
    prettypr:text(V);
ppsexp(#item{type=float, value=V}) ->    
    prettypr:text(V);
ppsexp(#item{type=integer, value=V}) ->    
    prettypr:text(V);
ppsexp(#item{type=_, value=V}) ->    
    prettypr:text(V);
ppsexp(I) when is_integer(I) ->
    prettypr:text(integer_to_list(I)).


escape(S) ->
    string:replace(S, "\"", "\\\"", all).

erl_to_ast(T) when is_list(T) ->
    S = try lists:all(fun(E) when is_integer(E) andalso 
                                  E =< 1114111 andalso 
                                  E >= 10 -> 
                              true;
                         (_)  -> 
                              false
                      end, T)
    catch _ ->
            false
    end,
    case S of
        true ->
            #item{type=string, value=io_lib:format("~s", [T])};
        false  ->
            lists:map(fun(E) ->
                              erl_to_ast(E)
                      end, T)
    end;
erl_to_ast(T) when is_tuple(T) ->
    TList = lists:map(fun(E) -> 
                              erl_to_ast(E) 
                      end, tuple_to_list(T)),
    [#item{type=atom, value="tuple"} | TList];
erl_to_ast(T) when is_binary(T) ->
    TList = binary_to_list(T),
    [#item{type=atom, value="binary"} | TList];
erl_to_ast(T) when is_integer(T) ->
    T;
erl_to_ast(T) when is_float(T) ->
    T;
erl_to_ast(T) when is_pid(T) ->
    [#item{type=atom, value="pid"}, #item{type=string, value=pid_to_list(T)}];
erl_to_ast(T) when is_map(T) ->
    TList = lists:foldl(
              fun({K, V}, A) ->
                      A++[erl_to_ast(K), erl_to_ast(V)]
              end, [], maps:to_list(T)),
    [#item{type=atom, value="map"} | TList] ;
erl_to_ast(T) when is_function(T) ->
    #item{type=string, value=erlang:fun_to_list(T)};
erl_to_ast(T) when is_atom(T) ->
    #item{type=atom, value=lists:flatten(io_lib:format("~p", [T]))}.
    
-define(T(X), prettypr:text(X)).

    

test(S) ->
    ppsexp(S).
