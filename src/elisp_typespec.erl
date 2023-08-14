-module(elisp_typespec).
-export([to_string/1, fun_to_string/2]).

sexp_to_string(List) ->
    S=lists:join(" ", List),
    lists:flatten(["(", S, ")"]).

fun_to_string(Name, Spec) ->
    [Args, Return] = to_string(Spec),
    sexp_to_string([sexp_to_string([atom_to_list(Name)]++[Args]), 
                    Return]).

to_string({var, _, Arg}) ->
    atom_to_list(Arg);
to_string({ann_type, _, Args}) ->
    sexp_to_string(lists:map(fun(E) ->
                                     to_string(E)
                             end, Args));
to_string({type, _, 'product', Args}) ->
    sexp_to_string(lists:map(fun(E) ->
                                     to_string(E)
                             end, Args));
to_string({type, _, 'fun', Args}) ->
    [A, Return] = lists:map(fun(E) ->
                          to_string(E)
                  end, Args),
    [A, Return];
to_string({type, _, 'integer', []}) ->
    "integer".
    
