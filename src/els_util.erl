-module(els_util).
-include_lib("els.hrl").
-include_lib("els_scan.hrl").
-export([eq/2, scanitem/3, scanlist/5, scanlist/2]).
-export([getmodfun/1, term_make_variable/1, term_make_atom/1, term_make_atom/2]).
-export([macro_init/1, macro_init/0]).
-export([env_init/1, env_init/0]).

env_init() ->
    env_init([]).
env_init(Env) ->
    [{macros, macro_init()} | Env].

macro_init(Macros) ->
    maps:merge(#{{"backquote",  1} => {yal_macro, 'MACRO_backquote'},
		 {"make_symbol", 1} => {yal_util, 'make_symbol'}},
	       Macros).
macro_init() ->
    macro_init(#{}).

eq(#item{value=V}, V) ->
    true;
eq(_, _) ->
    false.

scanitem(#item{value=H, type=atom} =A, K, _R) ->
    case lists:dropwhile(fun(E) ->
				 H =/= E
			 end, K) of
	[] ->
	    {acc, A, K};
	Match ->
	    {breaking, A, Match}
    end;
scanitem(A, K, _R) ->
    {acc, A, K}.

scanlist([H|T], K, {AccKey, R}, R2, LocH) ->
    case scanitem(H, K, R) of
	{acc, _A, K} ->
	    scanlist(T, K, {AccKey, [H|R]}, R2, LocH);
	{breaking, _A, [MH|MT]} ->
	    %% io:format("Macth ~p: ~p in ~p~n", [MH, MT, H]),
	    Acc = maps:put(AccKey, lists:reverse(R), R2),
	    %% io:format("AddLoc ~p: ~p in ~p~n", [MH, H, LocH]),
	    LocAcc = maps:put(MH, H, LocH),
	    scanlist(T, MT, {MH, []}, Acc, LocAcc)
    end;
scanlist([], _K, {AccKey, R}, R2, LocH) ->
    {maps:put(AccKey, lists:reverse(R), R2), LocH}.
scanlist(L, K) ->
    LocH = #{hd(K) => hd(L)},
    %% io:format("scanlisthead ~p~n", [LocH]),
    scanlist(tl(L), tl(K), {hd(K), []}, #{}, LocH).

term_make_variable(Term) ->
    erl_syntax:set_pos(erl_syntax:variable(Term#item.value), Term#item.loc).

term_make_atom(Term) ->
    erl_syntax:set_pos(erl_syntax:atom(Term#item.value), Term#item.loc).
term_make_atom(Term, Prefix) ->
    erl_syntax:set_pos(erl_syntax:atom(Prefix++Term#item.value), Term#item.loc).

split(F) ->
    case string:split(F, ":") of
        S when length(S) > 1 ->
            {module_function, list_to_tuple(S)};
        S -> 
            {atom, hd(S)}
    end.


getmodfun(#item{type=Type, value=X, loc=Loc}) when Type == atom; Type== module_function->
    {NType, NX} = split(X),
    case NType of
        atom ->
            {undef, erl_syntax:set_pos(erl_syntax:atom(NX), Loc)};
        module_function ->
            {M, F} = NX,
            MA=erl_syntax:set_pos(erl_syntax:atom(M), Loc),
            FA=erl_syntax:set_pos(erl_syntax:atom(F), Loc),
            {MA, FA}
    end.
