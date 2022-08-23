-module(macro).
-export([expand_forms/2,
         expand_apply/3,
         expand_term/2,
         expand_form/2]).


-type tree() :: erl_syntarx:syntaxTree().

-type macroenv() :: #{atom() => 
                          fun((tree()) -> tree())
                     }.

-spec expand_forms(tree(), macroenv()) -> tree().
expand_forms(Forms, MacroEnv) ->
    list = erl_syntax:type(Forms),
    erl_syntax_lib:map(fun(F) ->
                               expand_form(F, MacroEnv)
                       end, Forms).
-spec expand_apply(tree(), atom(), macroenv()) -> tree().
expand_apply(Form, M, MacroEnv) ->
    B = maps:get(M, MacroEnv),
    B(Form).

-spec expand_term(tree(), macroenv()) -> tree().
expand_term(T, MacroEnv) ->
    case erl_syntax:type(T) of
        list ->
            expand_form(T, MacroEnv);
        _ ->
            T
    end.
-spec expand_form(tree(), macroenv()) -> tree().
expand_form(Form, MacroEnv) ->
    list = erl_syntax:type(Form),
    H = erl_syntax:list_head(Form),
    T = erl_syntax:list_tail(Form),
    RT = erl_syntax_lib:map(fun(E) ->
                                    expand_term(E,  MacroEnv)
                            end, T),
    Form2=erl_syntax:cons(H, erl_syntax:copy_pos(T, RT)),
    Form3=erl_syntax:copy_pos(Form, Form2),
    case erl_syntax:type(H) of
        atom ->
            EA=expand_apply(Form3, erl_syntax:atom_name(H), MacroEnv);
        list ->
            expand_form(H, MacroEnv);
        _ ->
            Form3
    end.

