-module(els_util).
-include_lib("els.hrl").
-include_lib("els_scan.hrl").
-export([eq/2, scanitem/3, scanlist/5, scanlist/2]).

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
	    io:format("Macth ~p: ~p in ~p~n", [MH, MT, H]),
	    Acc = maps:put(AccKey, lists:reverse(R), R2),
	    io:format("AddLoc ~p: ~p in ~p~n", [MH, H, LocH]),
	    LocAcc = maps:put(MH, H, LocH),
	    scanlist(T, MT, {MH, []}, Acc, LocAcc)
    end;
scanlist([], _K, {AccKey, R}, R2, LocH) ->
    {maps:put(AccKey, lists:reverse(R), R2), LocH}.
scanlist(L, K) ->
    LocH = #{hd(K) => hd(L)},
    io:format("scanlisthead ~p~n", [LocH]),
    scanlist(tl(L), tl(K), {hd(K), []}, #{}, LocH).
