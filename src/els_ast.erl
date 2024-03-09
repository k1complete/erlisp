-module(els_ast).

-export([to_list/2, to_list/1]).

to_list(Tree) ->
    to_list(Tree, fun(E) -> E end).

to_list(Tree, F) ->
    case erl_syntax:type(Tree) of
        atom ->
            erl_syntax:atom_value(Tree);
        attribute ->
            ["-"++to_list(erl_syntax:attribute_name(Tree), F),
             to_list(erl_syntax:attribute_argments(Tree), F)];
        integer ->
            erl_syntax:integer_value(Tree);
        list ->
            lists:map(fun(E) ->
                              to_list(E, F)
                      end,erl_syntax:list_elements(Tree));
        variable ->
            erl_syntax:variable_name(Tree);
        _ ->
            Tree
    end.
