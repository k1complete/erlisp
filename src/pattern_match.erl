-module(pattern_match).
-export([list_match/4, 
         tuple_match/4,
         do_eval_match/3,
         do_eval_match/4,
         value_from_env/2,
         body_value/2
]).

-type tree() :: erl_syntax:sytaxTree().
-type env() :: map().
-type binding() :: map().
-type reason() :: atom() | {atom(), any()}.


-spec list_match(term(), term(), any(), env()) -> env().
list_match(P, B, Arguments, Env) ->
    Ph = erl_syntax:list_head(P),
    Pt = erl_syntax:list_tail(P),
    case erl_syntax:type(B) of
        list ->
            Bh = erl_syntax:list_head(B),
            Bt = erl_syntax:list_tail(B),
            Envh = do_eval_match(Ph, Bh, Arguments, Env),
            do_eval_match(Pt, Bt, Arguments, Envh);
        _  ->
            {error, badmatch, B}
    end.

-spec tuple_match(list(), list(), any(), env()) -> env().
tuple_match([], [], _Arguments, Env) ->
    Env;
tuple_match(Pe, Be, Arguments, Env) ->
    [Ph|Pt] = Pe,
    [Bh|Bt] = Be,
    case erl_syntax:type(Ph) of
        underscore ->
            tuple_match(Pt, Bt, Arguments, Env);
        _ ->
            NewEnv = do_eval_match(Ph, Bh, Arguments, Env),
            tuple_match(Pt, Bt, Arguments, NewEnv)
    end.

-spec value_from_env(tree(), env()) -> {ok, any()} | {error, undefiend}.
value_from_env(Body, Env) ->
    Varname = erl_syntax:variable_literal(Body),
    case maps:find(Varname, Env) of
        error ->
            {error, undefined};
        {ok, Value} ->
            {ok, Value}
    end.
-spec body_value(tree(), env()) -> tree().
body_value(Body, Env) ->
    L = erl_syntax_lib:map_subtrees(
          fun(Tree) ->
                  case erl_syntax:type(Tree) of
                      variable ->
                          case value_from_env(Tree, Env) of
                              {error, _} ->
                                  throw({error, {undefined, Body}});
                              {ok, V}  ->
                                  V
                          end;
                      _ ->
                          Tree
                  end
          end, Body).

-spec do_eval_match(tree(), tree(), any(), env()) -> env().
do_eval_match(P, B, Arguments, Env) ->
    case erl_syntax:type(P) of
        variable ->
            Pname = erl_syntax:variable_literal(P),
            case maps:find(Pname, Env) of
                error ->
                    maps:put(Pname, body_value(B, Env), Env);
                {ok, Value} ->
                    throw({error, {badmatch, {Pname, B, Value}}})
            end;
        list ->
            Ph = erl_syntax:list_head(P),
            Pt = erl_syntax:list_head(P),
            case erl_syntax:type(Ph) of
                atom ->
                    case erl_syntax:atom_literal(Ph) of
                        "match" ->
                            [_, PP, PB] = erl_syntax:list_elements(P),
                            Bind = binding(PB, B, Arguments, Env),
                            Env2 = do_eval_match(Bind, Arguments, Env),
                            Bind2 = binding(PP, PB, Arguments, Env2),
                            do_eval_match(Bind2, Arguments, Env2);
                        _ ->
                            list_match(P, B, Arguments, Env)
                    end;
                _ ->
                    list_match(P, B, Arguments, Env)
            end;
        tuple ->
            Pe = erl_syntax:tuple_elements(P),
            Be = erl_syntax:tuple_elements(B),
            tuple_match(Pe, Be, Arguments, Env);
        underscore ->
            Env;
        _ ->
            Env
    end.

binding(Left, Right, _Arguments, _Env) ->
    erl_syntax:copy_pos(Left, erl_syntax:match_expr(Left, Right)).

do_eval_match(Matching, Arguments, Env) ->
    Pattern = erl_syntax:match_expr_pattern(Matching),
    Body = erl_syntax:match_expr_body(Matching),
    do_eval_match(Pattern, Body, Arguments, Env).
