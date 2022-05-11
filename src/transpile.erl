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
                H == atom, Hn == "match" ->
                    match_op(Args);
                H == atom, Hn == "+" ->
                    io:format("+", []),
                    infix_op(Args, "+");
                H == atom, Hn == "add" ->
                    infix_op(Args, "+");
                H == atom, Hn == "mul" ->
                    infix_op(Args, "*");
                H == atom, Hn == "export" ->
                    export_(Args);
                H == atom, Hn == "cons" ->
                    cons_(T);
                H == atom, Hn == "quote" ->
                    quote_(T);
                H == atom, Hn == "length" ->
                    length_(T);
                H == atom, Hn == "defun" ->
                    defun_(T);
                H == atom, Hn == "lists:reverse" ->
                    lists_reverse_(T);
                true ->
                    X
            end;
        _ ->
            X
    end.
export_(L) ->
    Aq = lists:map(fun(E) ->
                           [F, A] = erl_syntax:list_elements(E),
                           erl_syntax:arity_qualifier(term(F), term(A))
                   end, L),
    E = erl_syntax:attribute(erl_syntax:atom(export),[erl_syntax:list(Aq)]),
    erl_syntax:copy_pos(hd(L), E).

match_op([Left,Right]) ->
    io:format("Match: ~p ~p~n", [Left, Right]),
    Me = erl_syntax:match_expr(Left, Right),
    erl_syntax:copy_pos(Left, Me).

infix_op([Left|T], Op) ->
    case T of
        [] -> Left;
        [Right|Tail] ->
            Nexp = erl_syntax:infix_expr(Left, erl_syntax:operator(Op), Right),
            Exp = erl_syntax:copy_pos(Right, Nexp),
            infix_op([Exp|Tail], Op)
    end.

-define(MQ(L, T, B), merl:qquote(erl_syntax:get_pos(L), T, B)).

cons_(L) ->
    Head = erl_syntax:list_head(L),
    Tail = erl_syntax:list_elements(erl_syntax:list_tail(L)),
    case Tail of
        [] -> Head;
        [X] ->
            HHead = term(Head),
            TTail = term(X),
            ?MQ(Head, "[_@HHead|_@TTail]", [{'HHead', HHead}, {'TTail', TTail}])
    end.
forms(L) ->    
    case erl_syntax:list_length(L) of
        1 ->
            io:format("L ~p~n", [L]),
            LF = form(erl_syntax:list_head(L)),
            LF;
        _ -> lists:map(fun(E) ->
                               io:format("E ~p~n", [E]),
                               form(E)
                       end, erl_syntax:list_elements(L))
    end.

clause_(L) ->
    [Args, When| Tail] = erl_syntax:list_elements(L),
    PArgs = lists:map(fun(E) ->
                        %      io:format("P ~p~n", [E]),
                              term(E) 
                      end, erl_syntax:list_elements(Args)),
    io:format("When: ~p~n", [Tail]),
    {Guard, Body} = case erl_syntax:is_atom(erl_syntax:list_head(When), 'when') of
                        true ->
                            {form(When), form(Tail)};
                        false ->
                            NTail = case Tail of
                                        [] -> erl_syntax:nil();
                                        _ -> Tail
                                    end,
                            NBody = erl_syntax:cons(When, NTail),
                            {[], forms(NBody)}
                    end,
    io:format("G: ~p, B: ~p~n", [Guard, Body]),
    ?MQ(Args, "(_@PArgs) when _@__Guard -> _@Body",
        [{'PArgs', PArgs}, {'Guard', Guard}, {'Body', Body} ]).

match_defun_(Name, Clauses) ->
    Md = erl_syntax:function(Name, lists:map(fun(E) ->
                                                     clause_(E)
                                             end, Clauses)),
    erl_syntax:copy_pos(Name, Md).

defun_(L) ->
    [Name, Args | Rest] = erl_syntax:list_elements(L),
    case erl_syntax:type(erl_syntax:list_head(Args)) of
        list ->
            match_defun_(Name, [Args|Rest]);
        variable ->
            Body = lists:map(fun(E) -> form(E) end, Rest),
            ?MQ(Name, "'@Name'(_@@Args) -> _@@Body.", 
                [{'Name', Name}, 
                 {'Args', erl_syntax:list_elements(Args)}, 
                 {'Body', Body}]);
        X ->
            io:format("Y: ~p~n", [X])
    end.

length_(L) ->
    Head = erl_syntax:list_head(L),
    FHead = term(Head),
    io:format("LLL ~p", [L]),
    ?MQ(Head, "length(_@FHead)", [{'FHead', FHead}]).

lists_reverse_(T) ->
    FHead = term(erl_syntax:list_head(T)),
    ?MQ(FHead, "lists:reverse(_@FHead)", [{'FHead', FHead}]).
    
quote_(L) ->    
    Head = erl_syntax:list_head(L),
    Head.

lst() ->
    A=?Q("_@Ee+1") = ?Q("B+1"),
    ?Q("B+1+1"),
    C=form(?Q("[add, 1, 2, 3]")),
    C2=form(?Q("[mul, 1, 2, 3]")),
    C3=form(?Q("[cons, 1, 2]")),
    C4=form(?Q("[length, [cons, 1, [cons, 2, []]]]")),
    {C, C2, C3, C4}.

