-module(els_pp).
-include("els.hrl").
-export([pp/1]).
-export([test/1]).
-export([ppsexp/1]).
-export([pptr/4]).
-export([form/1]).
-export([format/2]).
-export([npp/4]).
-export([erlast_to_str/2]).

-define(LISTMAX, 10).


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
    
%%pp(S) ->
%%    io:format("~p~n", [S]),
%%    pps(S).
pp(S) ->
    R = pptr(S, "", "", none),
    B = ppsexp(R),
    prettypr:format(B).

ppliteral(Value, LChar, RChar, _Dir) ->
    LChar++Value++RChar.

detect_paren_and_body(S) ->
    case S of
	#item{type=atom, value="list"} ->
	    {"[", "]", #item{value="", type=atom}};
	#item{type=atom, value="tuple"} ->
	    {"{", "}", #item{value="", type=atom}};
	#item{type=atom, value="map"} ->
	    {"#{", "}", #item{value="", type=atom}};
	_ ->
	    {"(", ")", S}
    end.

paren_control(S, L, R) ->
    case S of
	[S3] ->
	    Head = pptr(S3, L,R, both),
	    [Head];
	_ ->
	    Head = pptr(hd(S), L, "", open),
	    Last =  pptr(lists:last(S), "", R, close),
	    Middle = lists:map(fun(E) ->
				       pptr(E, "", "", none)
			       end,
			       lists:sublist(S, 2, length(S) - 2)),
	    lists:append([[Head], Middle, [Last]])
    end.


pptr([]=S, L, R, D) ->
    #item{type=atom, value=ppliteral("", L, R, D)};
pptr(#item{type=binary, value=V}=S, L, R, Direction) ->
    S#item{value=ppliteral(V, L, R, Direction)};
pptr(#item{type=integer, value=V}=S, L, R, Direction) when is_integer(V) ->
    S#item{value=ppliteral(integer_to_list(V), L, R, Direction)};
pptr(#item{type=float, value=V}=S, L, R, Direction) when is_float(V) ->
    S#item{value=ppliteral(float_to_list(V), L, R, Direction)};
pptr(#item{type=module_function, value={M, F}}=S, L, R, Direction) ->
    S#item{value=ppliteral(M++":"++F, L, R, Direction)};
pptr(#item{type=atom, value=V}=S, L, R, Direction) ->
    S#item{value=ppliteral(V, L, R, Direction)};
pptr(#item{type=variable, value=V}=S, L, R, Direction) ->
    S#item{value=ppliteral(V, L, R, Direction)};
pptr(#item{type=function, value=V}=S, L, R, Direction) ->
    S#item{value=ppliteral(V, L, R, Direction)};
pptr(#item{type=string, value=V}=S, L, R, Direction) ->
    S#item{value=ppliteral("\""++escape(V)++"\"", L, R, Direction)};
pptr(V, L, R, Direction) when is_integer(V)  ->
    #item{value=ppliteral(integer_to_list(V), L, R, Direction), type=integer};
pptr(V, L, R, Direction) when is_float(V)  ->
    #item{value=ppliteral(float_to_list(V), L, R, Direction), type=float};
pptr([#item{type=atom, value="quote"}, S], LChar, RChar, Direction) ->
    {NL, NR} = {"'", ""},
    R = pptr(S, LChar++NL, NR++RChar, both),
    R;
pptr([S], LChar, RChar, _Direction) ->
    {NL, NR} = {"(", ")"},
    R = pptr(S, LChar++NL, NR++RChar, both),
    [R];
pptr([H|T], LChar, RChar, _Direction) when not is_list(T) ->
    Head =  pptr(H, LChar ++ "(" , "", open),
    Last =  pptr(T, "", ")" ++ RChar, close),
    Middle = [#item{type=atom, value="."}],
    lists:append([[Head], Middle, [Last]]);
pptr(S, LChar, RChar, _Direction) when is_list(S) ->
    H = hd(S),
    {NL, NR, H2} = detect_paren_and_body(H),
    case H2 of
	#item{value=""} ->
	    S2 = tl(S),
	    paren_control(S2, LChar ++ NL, NR ++ RChar);
	_ ->
	    paren_control(S, LChar ++ NL, NR ++ RChar)
    end.

form(S) ->
    S1 = els_item:from_erl(S),
    S2 = pptr(S1, "", "", none),
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
    prettypr:par([ppsexp(Pattern),
		  ppsexp(When),
		  ppbody(Body)], 2);
ppclause([Pattern | Body]) ->
    prettypr:par([ppsexp(Pattern),
		  ppbody(Body)], 2).

pparg_returntype([], Acc) ->
    io:format("returntype[~p] ~p~n", [length(Acc), Acc]),
    lists:reverse(Acc);
pparg_returntype([Arg, Return, When=[#item{value="("++W}|_]|Rest], Acc) when W=:="when" ->
    io:format("When: ~p~n", [When]),
    E = prettypr:par([ppsexp(Arg), ppsexp(Return), ppsexp(When)], 0),
    pparg_returntype(Rest, [E|Acc]);
pparg_returntype([Arg, Return|Rest], Acc) ->
    io:format("Arg: ~p~n", [Arg]),
    io:format("Return: ~p~n", [Return]),
    E = prettypr:par([ppsexp(Arg), ppsexp(Return)], 0),
    pparg_returntype(Rest, [E|Acc]);
pparg_returntype([Arg|Rest], Acc) ->
    io:format("Arg: ~p~n", [Arg]),
    E = prettypr:par([ppsexp(Arg)], 0),
    pparg_returntype(Rest, [E|Acc]).

pparg_returntype(A) ->    
    pparg_returntype(A, []).

ppsexp([#item{value="(-spec"}=H1, #item{} = H2, Args=[[#item{value="(("++_N}|_]|_], Return |  Body]) 
%%  when hd(N)=/=$( ->
  ->
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
    H1S = ppsexp(H1),
    H2S = ppsexp(H2),
    AS = ppsexp(Args),
    BS = ppbody(Body),
    prettypr:par([H1S, H2S, AS, BS], 2);
ppsexp([#item{value="(defun"}=H1, #item{}=H2 | Clauses]) ->
    H1S = ppsexp(H1),
    H2S = ppsexp(H2),
    C = lists:map(fun(E) ->
		      prettypr:break(ppclause(E))
	      end, Clauses),
    prettypr:par([H1S, H2S | C], 2);
ppsexp(S) when is_list(S), length(S) > 2 ->
    [H1,H2|T] = S,
    H1S = ppsexp(H1),
%%    Indent = length("("++prettypr:format(H1S)),
    Indent = 2,
    H2S = ppsexp(H2),
    T2 = case length(T) > ?LISTMAX of
	     true ->
		 lists:sublist(T, ?LISTMAX-2) ++ [#item{type=string, value="..."}, lists:last(T)];
	     _ ->
		 T
	 end,
    Seps = lists:map(fun(E) ->
                             ppsexp(E)
                     end, T2),
%%    Seps = lists:map(fun(E) ->
%%                             ppsexp(E)
%%                     end, T),
    prettypr:par([H1S, H2S, prettypr:sep(Seps)], Indent);
ppsexp(S) when is_list(S) andalso length(S) == 2 ->
    Pars = lists:map(fun(E) ->
                             ppsexp(E)
                     end, S),
    prettypr:par(Pars);
ppsexp(S) when is_list(S) andalso length(S) == 1 ->
    Pars = lists:map(fun(E) ->
                             ppsexp(E)
                     end, S),
    prettypr:par(Pars);
ppsexp([]) ->
    prettypr:null_text();
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
ppsexp(#item{type=module_function, value={M, F}}) ->    
    prettypr:text(M++":"++F);
ppsexp(#item{type=_, value=V}) ->    
    prettypr:text(V);
ppsexp(I) when is_integer(I) ->
    prettypr:text(integer_to_list(I)).


escape(S) ->
    string:replace(S, "\"", "\\\"", all).

    
-define(T(X), prettypr:text(X)).

erlast_to_str(Name, Clause) ->
    Args = erl_syntax:clause_patterns(Clause),
    FunCall = erl_syntax:application(Name, Args),
    erl_prettypr:format(FunCall).
    

test(S) ->
    ppsexp(S).
