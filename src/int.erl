-module(int).

-include_lib("syntax_tools/include/merl.hrl").
-export([step/3] ).
-export([test/0] ).

-type tree() :: erl_syntax:sytaxTree().
-type env() :: map().
-type binding() :: map().
-type reason() :: atom() | {atom(), any()}.

-spec step(tree(), binding(), env()) -> 
          {ok, tree(), env()} | 
          {error, reason()}.
step(T, A, E) ->
    case erl_syntax:type(T) of
        list ->
            LArgs = erl_syntax:list_tail(T),
            ArgLen = erl_syntax:list_length(LArgs),
            case erl_syntax:type(H=erl_syntax:list_head(T)) of
                atom ->
                    special_form(erl_syntax:atom_name(H), LArgs, ArgLen, A, E);
                module_qualifier ->
                    MF = {erl_syntax:atom_value(erl_syntax:module_qualifier_argument(H)),
                          erl_syntax:atom_value(erl_syntax:module_qualifier_body(H))},
                    special_form(MF, LArgs, ArgLen, A, E);
                _ ->
                    {error, {not_a_funcname, H}}
            end;
        integer ->
            {ok, T, E};
        variable ->
            var_replace(T, A, E);
        _ ->
            {error, {not_a_list, T}}
    end.

-spec var_replace(tree(), tree(), env()) -> 
          {ok, tree(), env()} | 
          {error, undefined_variable, tree()}.
var_replace(T, _S, Env) ->
    case maps:get(erl_syntax:variable_literal(T), Env, undefined) of
        undefined ->
            {error, undefined_variable, T};
        V ->
            {ok, V, Env}
    end.

-spec mf(string()) -> {module(), function()}.
mf(String) ->
    case string:split(String, ":") of
        [X] ->
            {erlang, list_to_atom(X)};
        [M,F] ->
            {list_to_atom(M), list_to_atom(F)};
        [M,F|_] ->
            {list_to_atom(M), list_to_atom(F)}
    end.
    
-spec eval_params(tree(), tree(), env()) -> 
          {ok, tree(), env()} | 
          {error, reason()}.
eval_params(P, FunArgs, Env) ->
    case erl_syntax:type(P) of
        variable ->
            step(P, FunArgs, Env);
        list ->
            step(P, FunArgs, Env);
        _ ->
            {error, not_a_argment}
    end.

-spec special_form(string(), tree(), integer(), binding(), env()) -> 
          {ok, tree(), env()} | 
          {error, reason()}.
special_form("set", T, 2, A, E) ->
    [A1, A2] = erl_syntax:list_elements(T),
    {ok, A2V, E2V}  =step(A2, A, E),
    case erl_syntax:type(A1) of 
        variable -> 
            {ok, A2V, maps:put(erl_syntax:variable_literal(A1), A2V, E2V)}
    end;
special_form("car", T, 1, A, E) ->
    {ok, B2, E2} = eval_params(erl_syntax:list_head(T), A, E),
    case erl_syntax:type(B2) of
        list ->
            {ok, erl_syntax:list_head(B2), E2};
        _ ->
            {error, not_a_list}
    end;
special_form("cdr", T, 1, A, E) ->
    {ok, B2, E2} = eval_params(erl_syntax:list_head(T), A, E),
    case erl_syntax:type(B2) of
        list ->
            {ok, erl_syntax:list_tail(B2), E2};
        _ ->
            {error, not_a_list}
    end;
special_form("quote", T, 1, _A, E) ->
    case erl_syntax:list_length(T) of
        1 ->
            {ok, erl_syntax:list_head(T), E};
        X  ->
            {error, {arity_error, X}}
    end;
special_form(S, T, TL, A, E) ->
    call_func(S, T, TL, A, E).

-spec call_func(atom(), tree(), tree(), tree(), env()) -> 
          {ok, tree(), env()} | 
          {error, reason()}.
call_func(Symbol, Tree, Treetail, Arguments, Env) ->
    {M, F} = case Symbol of
                 {X, Y} -> 
                     io:format("~p-~p~n", [X, Y]),
                     {X, Y};
                 X -> mf(X)
             end,
    case lists:keyfind(F, 1, M:module_info(functions)) of
        {F, An} when An == Treetail ->
            {Args, E2} = lists:mapfoldl(
                           fun(Elm, Ac) ->
                                   {ok, R, E2} = step(Elm, Arguments, Ac),
                                   {R, E2}
                           end, 
                           Env,
                           erl_syntax:list_elements(Tree)),
            Args2 = lists:map(
                      fun(Elm) ->
                              io:format("arg ~p~n", [Elm]),
                              erl_syntax:concrete(Elm)
                      end,
                      Args),
            io:format("apply ~p:~p~p~n", [M, F, Args2]),
            {ok, erl_syntax:abstract(apply(M, F, Args2 )), E2};
        {F, An} ->
            {error, {bad_arity, {F, An, Treetail}}};
        false ->
            {error, {undefine, {X, Treetail}}}
    end.

test() ->
    E = #{},
    T = merl:quote("[ cdr, [quote,  [ 1, car, [quote, [3, 4]]]]]"),
    A = #{},
    {ok, T2, E} = step(T, A, E),
    _R1=step(T2, A, E),
    T10 = merl:quote("[ tl, [lists:seq, 3, 10]]"),
    {ok, T12, E} = step(T10, A, E),
    %R2=step(T12, A, E),
    io:format("~p, ~p~n", [T12, erl_syntax:concrete(T12)]),
    T30 = merl:quote("[ set, T, [quote, [1, 2, 3]]]"),
    {ok, _T32, E3} = step(T30, A, E),
    T40 = merl:quote("[ car, T ]"),
    {ok, _T42, _E4} = step(T40, A, E3).
    %T20 = merl:quote("[ tl, [quote,  [ 1, hd, [seq, [3, 4]]]]]"),
    %{ok, T22, E} = step(T20, A, E),
    %io:format("~p~n", [T22]).
   
