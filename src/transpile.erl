-module(transpile).
-include_lib("syntax_tools/include/merl.hrl").
-compile([{debug_info, true}]).
-export([lst/0]).

-export([form/1, term/1, infix_op/2]).
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

dispatch_infix_op(A) ->
    L = #{
          "==" => fun infix_op/2,
          "/=" => fun infix_op/2,
          "=<" => fun infix_op/2,
          "<" => fun infix_op/2,
          ">=" => fun infix_op/2,
          ">" => fun infix_op/2,
          "=:=" => fun infix_op/2,
          "=/=" => fun infix_op/2,
          "+" => fun infix_op/2,
          "-" => fun infix_op/2,
          "*" => fun infix_op/2,
          "/" => fun infix_op/2
         },
    maps:get(A, L, undef).
dispatch_special(A) ->
    L = #{"match" => fun match_op/2,
          "export" => fun export_/2,
          "module" => fun module_/2,
          "cons" => fun cons_/2,
          "quote" => fun quote_/2,
          "defun" => fun defun_/2
         },
    maps:get(A, L, undef).

-spec form(tree()) -> tree().
form(A) ->
    X = erl_syntax:list_head(A),
    T = erl_syntax:list_tail(A),
    H = erl_syntax:type(X),
    case H of 
        atom ->
            Hn = erl_syntax:atom_name(X),
            Args = erl_syntax:list_elements(T),
            case Inf=dispatch_infix_op(Hn) of
                undef ->
                    case Spf=dispatch_special(Hn) of
                        undef ->
                            call_function(X, T);
                        Spf ->
                            Spf(X, T)
                    end;
                Inf ->
                    Op = erl_syntax:copy_pos(X, erl_syntax:operator(Hn)), 
                    Inf(Op, Args)
            end;
        _ ->
            X
    end.
export_(X, L) ->
    Aq = lists:map(fun(E) ->
                           [F, A] = erl_syntax:list_elements(E),
                           erl_syntax:arity_qualifier(F, A)
                   end, erl_syntax:list_elements(L)),
    E = erl_syntax:attribute(erl_syntax:atom(export),[erl_syntax:list(Aq)]),
    erl_syntax:copy_pos(X, E).

module_(X, L) ->
    Module = erl_syntax:list_head(L),
    E = erl_syntax:attribute(erl_syntax:atom(module), [Module]),
    erl_syntax:copy_pos(X, E).

match_op(X, L) ->
    [Left, Right] = erl_syntax:list_elements(L),
    io:format("Match: ~p ~p~n", [Left, Right]),
    Me = erl_syntax:match_expr(term(Left), term(Right)),
    erl_syntax:copy_pos(X, Me).

infix_op(Op, [Left|T]) ->
%    Ops = erl_syntax:copy_pos(Op, erl_syntax:operator(Op)),
    case T of
        [] -> Left;
        [Right|Tail] ->
            Nexp = erl_syntax:infix_expr(Left, Op, Right),
            Exp = erl_syntax:copy_pos(Right, Nexp),
            infix_op(Op, [Exp|Tail])
    end.

-define(MQ(L, T, B), merl:qquote(erl_syntax:get_pos(L), T, B)).

cons_(C, L) ->
    io:format("cons: ~p~n", [L]),
    Head = erl_syntax:list_head(L),
    Tail = erl_syntax:list_elements(erl_syntax:list_tail(L)),
    case Tail of
        [] -> Head;
        [X] ->
            HHead = term(Head),
            io:format("HHead: ~p~n", [HHead]),
            TTail = term(X),
            io:format("TTail: ~p~n", [TTail]),

            ?MQ(C, "[_@HHead|_@TTail]", [{'HHead', HHead}, {'TTail', TTail}])
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

defun_(X, L) ->
    [Name, Args | Rest] = erl_syntax:list_elements(L),
    case erl_syntax:type(erl_syntax:list_head(Args)) of
        list ->
            match_defun_(Name, [Args|Rest]);
        variable ->
            Body = lists:map(fun(E) -> form(E) end, Rest),
            ?MQ(X, "'@Name'(_@@Args) -> _@@Body.", 
                [{'Name', Name}, 
                 {'Args', erl_syntax:list_elements(Args)}, 
                 {'Body', Body}]);
        X ->
            io:format("Y: ~p~n", [X])
    end.

call_function(X, T) ->
    FHead = term(erl_syntax:list_head(T)),
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

quote_(_X, L) ->
    erl_syntax:list_head(L).


lst() ->
    ?Q("B+1+1"),
    C=form(?Q("[add, 1, 2, 3]")),
    C2=form(?Q("[mul, 1, 2, 3]")),
    C3=form(?Q("[cons, 1, 2]")),
    C4=form(?Q("[length, [cons, 1, [cons, 2, []]]]")),
    {C, C2, C3, C4}.

