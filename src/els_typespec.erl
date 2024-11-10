-module(els_typespec).
-export([to_string/1, to_list/1, fun_to_string/2,
        to_binary/1]).

to_binary(L) ->
    list_to_binary(to_string(L)).

sexp_to_string(List) ->
    S=lists:join(" ", List),
    lists:flatten(["(", S, ")"]).

fun_to_string(Name, Spec) ->
    {type, Loc, 'fun', [ArgsSpec, ReturnSpec]} = Spec,
    io:format("fun_to_string: ~nName: ~p~nSpec: ~p~n", [Name, Spec]),
    [Args, Return] = to_string(Spec),
    sexp_to_string([sexp_to_string([atom_to_list(Name)]++[Args]), 
                    Return]).
ss(S) when is_atom(S) ->
    atom_to_list(S);
ss(S) when is_list(S) ->
    SS = lists:join(" ", S),
    lists:flatten(["(", SS, ")"]).

to_string(L) ->
    to_list(L, fun(E) -> ss(E) end).
to_list(L) ->
    to_list(L, fun(E) ->
                        E 
               end).

sexp_to_list(List, F) ->
    F(List).


to_list({var, _, Arg}, F) ->
    F(Arg);
to_list({ann_type, _, [Name| Args]}, F) ->
    sexp_to_list(lists:map(fun(E) ->
                                     to_list(E, F)
                             end, [Name, {atom,0, "::"} |Args]), F);
to_list({type, _, 'product', Args}, F) ->
    ArgsM = [ hd(Args), {atom, 0, '::'}|tl(Args)],
    sexp_to_list(lists:map(fun(E) ->
                                     to_list(E, F)
                             end, ArgsM), F);
to_list({type, _, 'fun', Args}, F) ->
    [A, Return] = lists:map(fun(E) ->
                          to_list(E, F)
                  end, Args),
    [A, Return];
to_list({type, _, 'integer', []}, F) ->
    F(integer);
to_list({atom, _, A}, F)->
    F(A);
to_list({var, _, A}, F)->
    F(A).
