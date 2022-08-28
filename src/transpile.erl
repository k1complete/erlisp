-module(transpile).
-include_lib("syntax_tools/include/merl.hrl").
-compile([{debug_info, true}]).
-export([lst/0]).

-export([form/1, term/1, infix_op/2, cons_/1, length_/1]).
-type tree() :: erl_syntax:syntaxTree().

term(A) ->
    X = erl_syntax:type(A),
    case X of
        atom ->
            A;
        integer ->
            A;
        list ->
            form(A);
        _ ->
            A
    end.

-spec form(tree()) -> tree().
form(A) ->
    X = erl_syntax:list_head(A),
    T = erl_syntax:list_tail(A),
    H = erl_syntax:type(X),
    case H of 
        atom ->
            Hn = erl_syntax:atom_name(X),
            Args = erl_syntax:list_elements(T),
            if 
                H == atom, Hn == "add" ->
                    infix_op(Args, "+");
                H == atom, Hn == "mul" ->
                    infix_op(Args, "*");
                H == atom, Hn == "cons" ->
                    cons_(T);
                H == atom, Hn == "quote" ->
                    quote_(T);
                H == atom, Hn == "length" ->
                    length_(T);
                true ->
                    X
            end;
        _ ->
            X
    end.

infix_op([Left|T], Op) ->
    case T of
        [] -> Left;
        [Right|Tail] ->
            Exp = erl_syntax:infix_expr(Left, erl_syntax:operator(Op), Right),
            infix_op([Exp|Tail], Op)
    end.

cons_(L) ->
    Head = erl_syntax:list_head(L),
    Tail = erl_syntax:list_elements(erl_syntax:list_tail(L)),
    case Tail of
        [] -> Head;
        [X] ->
            HHead = term(Head),
            TTail = term(X),
            merl:qquote(erl_syntax:get_pos(Head), "[_@HHead|_@TTail]", [{'HHead', HHead}, {'TTail', TTail}])
    end.
length_(L) ->
    Head = erl_syntax:list_head(L),
    FHead = term(Head),
    io:format("LLL ~p", [L]),
    merl:qquote(erl_syntax:get_pos(Head), "length(_@FHead)", [{'FHead', FHead}]).
quote_(L) ->    
    Head = erl_syntax:list_head(L),
    io:format("Head: ~p~n", [Head]),
    Head.

lst() ->
    A=?Q("_@Ee+1") = ?Q("B+1"),
    ?Q("B+1+1"),
    C=form(?Q("[add, 1, 2, 3]")),
    C2=form(?Q("[mul, 1, 2, 3]")),
    C3=form(?Q("[cons, 1, 2]")),
    C4=form(?Q("[length, [cons, 1, [cons, 2, []]]]")),
    {C, C2, C3, C4}.

