-module(transpile).
-include_lib("syntax_tools/include/merl.hrl").
-compile([{debug_info, true}]).
-export([lst/0]).

-export([form/2, form_trans/2, term/2, infix_op/3]).
-type tree() :: erl_syntax:syntaxTree().


term(A, E) ->
    X = erl_syntax:type(A),
    case X of
        atom ->
            A,
            B = case erl_syntax:atom_name(A) of
                    "_" ->
                        erl_syntax:underscore();
                    N ->
                        %S=binary_to_list(unicode:characters_to_binary(N)),
                        S = N,
                        io:format("S ~ts", [S]),
                        erl_syntax:variable(S)
                end,
            erl_syntax:copy_pos(A, B);
        integer ->
            A;
        list ->
            form(A, E);
        variable  ->
            A;
        underscore ->
            A;
        _ ->
            A
    end.

dispatch_infix_op(A) ->
    L = #{
          "==" => fun infix_op/3,
          "/=" => fun infix_op/3,
          "=<" => fun infix_op/3,
          "<" => fun infix_op/3,
          ">=" => fun infix_op/3,
          ">" => fun infix_op/3,
          "=:=" => fun infix_op/3,
          "=/=" => fun infix_op/3,
          "+" => fun infix_op/3,
          "-" => fun infix_op/3,
          "*" => fun infix_op/3,
          "/" => fun infix_op/3,
          "bnot" => fun infix_op/3,
          "div" => fun infix_op/3,
          "rem" => fun infix_op/3,
          "band" => fun infix_op/3,
          "bor" => fun infix_op/3,
          "bxor" => fun infix_op/3,
          "bsl" => fun infix_op/3,
          "bsr" => fun infix_op/3
         },
    maps:get(A, L, undef).
dispatch_special(A) ->
    L = #{"match" => fun match_op/3,
          "export" => fun export_/3,
          "module" => fun module_/3,
          "cons" => fun cons_/3,
          "quote" => fun quote_/3,
          "defun" => fun defun_/3
         },
    maps:get(A, L, undef).

expand_macro(A, _E) ->
    A.

-spec form(tree(), any()) -> tree().
form(A, E) ->
    B = expand_macro(A, E),
    form_trans(B, E)
    .
form_trans(A, E) ->
    X = erl_syntax:list_head(A),
    T = erl_syntax:list_tail(A),
    H = erl_syntax:type(X),
    case H of 
        atom ->
            Hn = erl_syntax:atom_name(X),
            io:format("Form: ~p~n", [T]),
            Args = erl_syntax:list_elements(T),
            io:format("FormL: ~p~n Hn: ~p~n", [Args, Hn]),
            case Inf=dispatch_infix_op(Hn) of
                undef ->
                    case Spf=dispatch_special(Hn) of
                        undef ->
                            call_function(X, T, E);
                        Spf ->
                            io:format("SPf: ~p~n: ~p~n", [X, T]),
                            Spf(X, T, E)
                    end;
                Inf ->
                    Op = erl_syntax:copy_pos(X, erl_syntax:operator(Hn)), 
                    Inf(Op, Args, E)
            end;
        _ ->
            X
    end.
export_(X, L, _E) ->
    Aq = lists:map(fun(E) ->
                           [F, A] = erl_syntax:list_elements(E),
                           erl_syntax:arity_qualifier(F, A)
                   end, erl_syntax:list_elements(L)),
    E = erl_syntax:attribute(erl_syntax:atom(export),[erl_syntax:list(Aq)]),
    erl_syntax:copy_pos(X, E).

module_(X, L, _E) ->
    Module = erl_syntax:list_head(L),
    E = erl_syntax:attribute(erl_syntax:atom(module), [Module]),
    erl_syntax:copy_pos(X, E).

match_op(X, L, E) ->
    [Left, Right] = erl_syntax:list_elements(L),
    io:format("Match: ~p ~p~n", [Left, Right]),
    Me = erl_syntax:match_expr(term(Left, E), term(Right, E)),
    io:format("Match2: ~p~n", [Me]),
    erl_syntax:copy_pos(X, Me).

anary_op(Op, Left, _E) ->
    Nexp = erl_syntax:prefix_expr(Op, Left),
    erl_syntax:copy_pos(Op, Nexp).

infix_op(Op, [Left|T], E) ->
    infix_op_do(Op, [term(Left, E)|T], E).

infix_op_do(Op, [Left|T], E) ->
    case T of
        [] -> 
            anary_op(Op, Left, E);
        [Right|Tail] ->
            case Tail of
                [] ->
                    Nexp = erl_syntax:infix_expr(Left, Op, term(Right, E)),
                    Exp = erl_syntax:copy_pos(Right, Nexp),
                    Exp;
                _ ->
                    Nexp = erl_syntax:infix_expr(Left, Op, term(Right, E)),
                    Exp = erl_syntax:copy_pos(Right, Nexp),
                    infix_op_do(Op, [Exp|Tail], E)
            end
    end.

-define(MQ(L, T, B), merl:qquote(erl_syntax:get_pos(L), T, B)).

cons_(C, L, E) ->
    io:format("cons: ~p~n", [L]),
    Head = erl_syntax:list_head(L),
    Tail = erl_syntax:list_elements(erl_syntax:list_tail(L)),
    case Tail of
        [] -> Head;
        [X] ->
            HHead = term(Head, E),
            io:format("HHead: ~p~n", [HHead]),
            TTail = term(X, E),
            io:format("TTail: ~p~n", [TTail]),

            ?MQ(C, "[_@HHead|_@TTail]", [{'HHead', HHead}, {'TTail', TTail}])
    end.

forms(L, E) ->    
    case erl_syntax:list_length(L) of
        1 ->
            io:format("L ~p~n", [L]),
            LF = form(erl_syntax:list_head(L), E),
            LF;
        _ -> lists:map(fun(A) ->
                               io:format("E ~p~n", [A]),
                               form(A, E)
                       end, erl_syntax:list_elements(L))
    end.

clause_(L, E) ->
    [Args, When| Tail] = erl_syntax:list_elements(L),
    PArgs = lists:map(fun(A) ->
                        %      io:format("P ~p~n", [A]),
                              term(A, E) 
                      end, erl_syntax:list_elements(Args)),
    io:format("When: ~p~n", [Tail]),
    {Guard, Body} = case erl_syntax:is_atom(erl_syntax:list_head(When), 'when') of
                        true ->
                            {form(When, E), form(Tail, E)};
                        false ->
                            NTail = case Tail of
                                        [] -> erl_syntax:nil();
                                        _ -> Tail
                                    end,
                            NBody = erl_syntax:cons(When, NTail),
                            {[], forms(NBody, E)}
                    end,
    io:format("G: ~p, B: ~p~n", [Guard, Body]),
    ?MQ(Args, "(_@PArgs) when _@__Guard -> _@Body",
        [{'PArgs', PArgs}, {'Guard', Guard}, {'Body', Body} ]).


match_defun_(Name, Clauses, E) ->
    Md = erl_syntax:function(Name, 
                             lists:map(fun(A) ->
                                               clause_(A, E)
                                       end, Clauses)),
    erl_syntax:copy_pos(Name, Md).

defun_(X, L, E) ->
    [Name, Args | Rest] = erl_syntax:list_elements(L),
    case erl_syntax:type(erl_syntax:list_head(Args)) of
        list ->
            match_defun_(Name, [Args|Rest], E);
        variable ->
            Body = lists:map(fun(A) -> form(A, E) end, Rest),
            ?MQ(X, "'@Name'(_@@Args) -> _@@Body.", 
                [{'Name', Name}, 
                 {'Args', erl_syntax:list_elements(Args)}, 
                 {'Body', Body}]);
        X ->
            io:format("Y: ~p~n", [X])
    end.

call_function(X, T, E) ->
    io:format("call X ~p~nT ~p~n", [X, T]),
    FHead = term(erl_syntax:list_head(T), E),
    {M, F} = getmf(X),
    case M of
        undef ->
            ?MQ(X, "'@F'(_@FHead)", 
                [{'F', F},
                 {'FHead', FHead}]);
        _ ->
            ?MQ(X, "'@M':'@F'(_@FHead)", 
                [{'M', M},
                 {'F', F},
                 {'FHead', FHead}])
    end.


getmf(X) ->
    [M|F] = string:split(erl_syntax:atom_name(X), ":"),
    case F of
        [] ->
            {undef, erl_syntax:copy_pos(X, erl_syntax:atom(M))};
        _ ->
            {erl_syntax:copy_pos(X, erl_syntax:atom(M)),
             erl_syntax:copy_pos(X, erl_syntax:atom(hd(F)))}
    end.

quote_(_X, L, _E) ->
    io:format("quote ~p~n", [L]),
    L2 = erl_syntax_lib:map(fun(A) ->
                                    case erl_syntax:type(A) of
                                        variable ->
                                            V = erl_syntax:atom(erl_syntax:variable_name(A)),
                                            erl_syntax:copy_pos(A, V);
                                        _ ->
                                            A
                                    end
                            end, L),
    erl_syntax:list_head(L2).



lst() ->
    E = [],
    ?Q("B+1+1"),
    C=form(?Q("[add, 1, 2, 3]"), E),
    C2=form(?Q("[mul, 1, 2, 3]"), E),
    C3=form(?Q("[cons, 1, 2]"), E),
    C4=form(?Q("[length, [cons, 1, [cons, 2, []]]]"), E),
    {C, C2, C3, C4}.

