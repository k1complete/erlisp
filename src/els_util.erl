-module(els_util).
-include_lib("els.hrl").
-include_lib("els_scan.hrl").
-export([eq/2, scanitem/3, scanlist/4, scanlist/2]).

eq(#item{value=V}, V) ->
    true;
eq(_, _) ->
    false.

scanitem(#item{value=H, type=atom} =A, K, R) ->
    case lists:splitwith(fun(E) ->
			    H == K
			 end, K) of
	{K, []} ->
	    {acc, A, K};
	{Skipped, Match} ->
	    {breaking, A, Match}
    end;
scanitem(A, K, R) ->
    {acc, A, K}.

scanlist([H|T], K, {AccKey, R}, R2) ->
    case scanitem(H, K, R) of
	{acc, A, K} ->
	    scanlist(T, K, {AccKey, [H|R]}, R2);
	{breaking, A, [MH|MT]} ->
	    scanlist(T, MT, {MH, []}, maps:put(AccKey, lists:reverse(R), R2))
    end;
scanlist([], K, {AccKey, R}, R2) ->
    maps:put(AccKey, lists:reverse(R), R2).
scanlist(L, K) ->
    scanlist(tl(L), tl(K), {hd(K), []}, #{}).
