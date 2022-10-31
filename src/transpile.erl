-module(transpile).
-include_lib("syntax_tools/include/merl.hrl").
-include_lib("erlisp.hrl").
-compile([{debug_info, true}]).
-export([lst/0]).
-include("scan.hrl").
-export([form/2, form_trans/2, term/2, infix_op/4,
        locline/1]).
-type tree() :: erl_syntax:syntaxTree().

erl_type(A) ->
    io:format("A is ~p\n", [A]),
    case A of
        #term{value=X, loc=Loc,type=T} ->
            T;
        X when is_atom(X) ->
            atom;
        X when is_binary(X) -> 
            binary;
        X when is_integer(X) ->
            integer;
        X when is_list(X)  ->
            list;
        X when is_bitstring(X) ->
            bitstring
    end.

module_function(A, Loc) ->
    {M, F} = A#term.value,
    erl_syntax:set_pos(erl_syntax:module_qualifier(
                         erl_syntax:set_pos(erl_syntax:atom(M), Loc), 
                         erl_syntax:set_pos(erl_syntax:atom(F), Loc)), 
                       Loc).

term(A, E) ->
    term(A, 0, E).

term(A, Loc, E) ->
    X = erl_type(A),
    io:format("termX ~p~p[~p]~n", [X, A, Loc]),
    case X of
        module_function ->
            module_function(A, Loc);
        atom ->
            #term{value=Atom, loc=Aloc} = A,
            A2 = erl_syntax:atom(Atom),
            io:format("A2 ~p~n", [A2]),
            B = case erl_syntax:atom_name(A2) of
                    "_" ->
                        erl_syntax:underscore();
                    "nil" ->
                        erl_syntax:nil();
                    N ->
                        %S=binary_to_list(unicode:characters_to_binary(N)),
                        S = N,
                        io:format("<<<Variable ~ts>>>", [S]),
                        erl_syntax:variable(S)
                end,
            erl_syntax:set_pos(B, Aloc);
        integer ->
            erl_syntax:set_pos(erl_syntax:integer(A), Loc);
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
          "==" => fun infix_op/4,
          "/=" => fun infix_op/4,
          "=<" => fun infix_op/4,
          "<" => fun infix_op/4,
          ">=" => fun infix_op/4,
          ">" => fun infix_op/4,
          "=:=" => fun infix_op/4,
          "=/=" => fun infix_op/4,
          "+" => fun infix_op/4,
          "-" => fun infix_op/4,
          "*" => fun infix_op/4,
          "/" => fun infix_op/4,
          "bnot" => fun infix_op/4,
          "div" => fun infix_op/4,
          "rem" => fun infix_op/4,
          "band" => fun infix_op/4,
          "bor" => fun infix_op/4,
          "bxor" => fun infix_op/4,
          "bsl" => fun infix_op/4,
          "bsr" => fun infix_op/4
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

-type sexp() :: list().

-spec form(sexp(), any()) -> tree().
form(A, E) ->
    B = expand_macro(A, E),
    R = form_trans(B, E),
    io:format("formed ~p~n", [R]),
    R
    .

form_trans([XT=#term{value=X, loc=Loc}| T], E) ->
    io:format("X: ~p, T: ~p~n", [X, T]),
    R=case Inf=dispatch_infix_op(X) of
          undef ->
            case Spf=dispatch_special(X) of
                undef ->
                    call_function(XT, T, E);
                Spf ->
                    io:format("SPf: ~p~nArgs: ~p~n", [X, T]),
                    R1=Spf(XT, T, E),
                    io:format("R1: ~p~n", [R1]),
                    R1
            end;
          Inf ->
              Op = X,
              Args = T,
              io:format("Ops: ~p : Loc ~p~n", [Op, Loc]),
              Inf(Op, Loc, Args, E)
      end,
    io:format("form trans : ~p~n", [R]),
    R
    ;
form_trans([List| T], E) when is_list(List) ->
    io:format("nested ~p~n", [List]),
    form_trans([form_trans(List, E)| T], E).
term_make_atom(Term) ->
    erl_syntax:set_pos(erl_syntax:atom(Term#term.value), Term#term.loc).

export_(X, L, E) ->
    Loc = X#term.loc,
    io:format("export X ~p~n", [X]),
    Aq = lists:map(fun([Fn, Arg]) ->
                           F = term_make_atom(Fn),
                           A = term(Arg, Loc, E),
                           io:format("FA Fis ~p~n Ais ~p~n", [F, A]),
                           erl_syntax:arity_qualifier(F, A)
                   end, L),
    R = erl_syntax:attribute(erl_syntax:atom(export),[erl_syntax:list(Aq)]),
    erl_syntax:set_pos(R, Loc).

module_(X, L, _E) ->
    Module = erl_syntax:list_head(L),
    E = erl_syntax:attribute(erl_syntax:atom(module), [Module]),
    erl_syntax:copy_pos(X, E).
match_op(#term{value=X, loc=Loc}, L, E) ->
    [Left, Right] = L,
    io:format("Match: ~p ~p~n", [Left, Right]),
    LeftT = term(Left, Loc, E),
    RightT= term(Right, Loc, E),
    io:format("MatchT: ~p ~p~n", [LeftT, RightT]),
    Me = erl_syntax:match_expr(term(Left, Loc, E), term(Right, Loc, E)),
    io:format("Match2: ~p~n", [Me]),
    erl_syntax:set_pos(Me, Loc).

anary_op(Op, Left, _E) ->
    Nexp = erl_syntax:prefix_expr(Op, Left),
    erl_syntax:copy_pos(Op, Nexp).

infix_op(Op, Loc, Arg=[Left|Right], E) ->
    io:format("TreeInfix~n", []),
    OpType = erl_syntax:set_pos(erl_syntax:operator(Op), Loc),
    Xp =infix_op_do(OpType, [term(Left, Loc, E) |Right], E),
    io:format("TreeInfix ~p~nLoc ~p~n", [Xp, Loc]),
    erl_syntax:set_pos(Xp, Loc).

infix_op_do(Op, [Left|T], E) ->
    io:format("infix L: ~p, R: ~p~n", [Left, T]),
    Pos = erl_syntax:get_pos(Left),
    case T of
        [] -> 
            anary_op(Op, Left, E);
        [Right|Tail] ->
            case Tail of
                [] ->
                    RightTerm = term(Right, Pos, E),
                    Nexp = erl_syntax:infix_expr(Left, Op, RightTerm),
                    Exp = erl_syntax:copy_pos(RightTerm, Nexp),
                    Exp;
                _ ->
                    RightEx = term(Right, Pos, E),
                    Nexp = erl_syntax:infix_expr(Left, Op, RightEx),
                    Exp = erl_syntax:copy_pos(Right, Nexp),
                    infix_op_do(Op, [Exp|Tail], E)
            end
    end.

-define(MQ(L, T, B), merl:qquote(erl_syntax:get_pos(L), T, B)).
-define(MQP(L, T, B), merl:qquote(L, T, B)).


cons_(C, L, E) ->
    io:format("cons: ~p~n", [L]),
    [Head|Tail] = L,
    #term{loc=Loc} = C,
    case Tail of
        [] -> Head;
        [X] ->
            HHead = term(Head, Loc, E),
            io:format("HHead: ~p~n", [HHead]),
            TTail0 = term(X, Loc, E),
%            TTail = erl_syntax:set_pos(erl_syntax:cons(TTail0, erl_syntax:nil()), Loc),
            TTail = TTail0,
            io:format("TTail: ~p C: ~p~n", [TTail, C]),
            ?MQP(Loc, "[_@HHead|_@TTail]", [{'HHead', HHead}, {'TTail', TTail}])
    end.

forms(L, E) ->    
    case erl_syntax:list_length(L) of
        1 ->
            io:format("L ~p~n", [L]),
            LF = form(erl_syntax:list_head(L), E),
            LF;
        _ -> lists:map(fun(A) ->
                               io:format("E ~p~n", [A]),
                               form_trans(A, E)
                       end, erl_syntax:list_elements(L))
    end.

clause_(L, E) ->
    [Args, When| Tail] = L,
    PArgs = lists:map(fun(A) ->
                              R = term(A, E),
                              io:format("<P> ~p~n", [R]),
                              R
                      end, Args),
    io:format("When: ~p~n", [(hd(When))#term.value]),
    {Guard, Body} = case (hd(When))#term.value=='when' of
                        true ->
                            {form(When, E), form(Tail, E)};
                        false ->
                            NBody = [When | Tail],
                            io:format("NBody ~p~n", [NBody]),
                            NBodyList =case length(NBody) of
                                         1 ->
                                             form(hd(NBody), E);
                                         _ ->
                                             NBodies = lists:map(fun(Elem) ->
                                                        form(Elem, E)
                                                                 end, NBody),
                                             erl_syntax:list(NBodies)
                                       end,
                            {[], NBodyList}
                    end,
    io:format("Arg: ~p~nG: ~p~nB: ~p~n", [PArgs, Guard, Body]),
    Loc = erl_syntax:get_pos(hd(PArgs)),
    S=?MQP(Loc, "(_@PArgs) when _@__Guard -> _@Body",
        [{'PArgs', PArgs}, {'Guard', Guard}, {'Body', Body} ]),
    io:format("SSS: ~p~n", [S]),
    S.


match_defun_(Name, Clauses, E) ->
    io:format("match-defun ~p~n", [Name]),
    FuncName = erl_syntax:set_pos(erl_syntax:atom(Name#term.value), Name#term.loc),
    Md = erl_syntax:function(FuncName, 
                             lists:map(fun(A) ->
                                               AST = A,
                                               io:format("AST ~p~n", [AST]),
                                               clause_(AST, E)
                                       end, Clauses)),
    Ret=erl_syntax:copy_pos(FuncName, Md),
    io:format("~nmatch_defun_output ~p~n", [erl_syntax:get_pos(Ret)]),
    merl:print(Ret),
    io:format("~n ", []),
    Ret.

defun_(X, L, E) ->
    io:format("defun_ : ~p~n", [X]),
    Line = X#term.loc,
    [Name, Args | Rest] = L,
    io:format("Name, Args | Rest =~n  ~p~n ~p~n ~p ~n", [Name, Args, Rest]),
    case hd(Args) of
        A when is_list(A) ->
            match_defun_(Name, [Args|Rest], E);
        _  ->
            Body = lists:map(fun(A) -> form(A, E) end, Rest),
            io:format("simpleArgs ~p ~n", [Args]),
            ArgList = lists:map(fun(A) -> term(A, E) end, Args),
            FunName = erl_syntax:atom(Name#term.value),
            io:format("MO: ~p ~p~n", [Line, FunName]),
            MQ=?MQP(Line, "'@name'(_@@args) -> _@@body.", 
                 [{'name', FunName}, 
                  {'args', ArgList},
                  {'body', Body}]),
            io:format("MQ2: ~p~n", [MQ]),
            MQ;
        X ->
            io:format("Y: ~p~n", [X])
    end.

locconv(E) ->
    Line = erl_anno:line(erl_syntax:get_pos(E)),
    Pos = erl_anno:new(Line),
    erl_syntax:set_pos(E, Pos).
    
locline(F) ->
    erl_syntax_lib:map(fun(E) -> 
                               locconv(E)
                       end, F).

call_function(Fun=#term{value=X, loc=Loc}, T, E) ->
    io:format("call X ~p~nT ~p~nFun ~p~n", [X, T, Fun]),
    FHead = term(hd(T), E),
    io:format("call X2 ~p~nT ~p~n", [term(Fun,E, Loc), FHead]),
    %FName = erl_syntax:set_pos(erl_syntax:atom(X), Loc),
    %FName = term(Fun, E, Loc),
    {M, F} = getmodfun(Fun),
    io:format("MQP: ~p : ~p : arg ~p~n", [M, F, FHead]),
    case M of
        undef ->
            io:format("MQ: ~p : arg ~p~n", [F, FHead]),
            ?MQP(Loc, "'@F'(_@FHead)", 
                [{'F', F},
                 {'FHead', FHead}]);
        _ ->
            io:format("MQMF: ~p ~n FUN: ~p ~n arg: ~p~n", [M, F, FHead]),
            ?MQP(Loc, "'@M':'@F'(_@FHead)", 
                [{'M', M},
                 {'F', F},
                 {'FHead', FHead}])
    end.

getmodfun(#term{type=Type, value=X, loc=Loc}) ->
    case Type of
        atom ->
            {undef, erl_syntax:set_pos(erl_syntax:atom(X), Loc)};
        module_function ->
            {M, F} = X,
            MA=erl_syntax:set_pos(erl_syntax:atom(M), Loc),
            FA=erl_syntax:set_pos(erl_syntax:atom(F), Loc),
            {MA, FA}
    end.

getmf(X) ->
    case erl_syntax:type(X) of
        atom ->
            [M|F] = string:split(erl_syntax:atom_name(X), ":"),
            case F of
                [] ->
                    {undef, erl_syntax:copy_pos(X, erl_syntax:atom(M))};
                _ ->
                    {erl_syntax:copy_pos(X, erl_syntax:atom(M)),
                     erl_syntax:copy_pos(X, erl_syntax:atom(hd(F)))}
            end;
        module_qualimodifier ->
            {erl_syntax:module_qualifier_argument(X),
             erl_syntax:module_qualifier_body(X)}
    end.


quote_(_X, [], _E) ->
    #term{loc=Pos} = _X,
    erl_syntax:set_pos(erl_syntax:nil(), Pos);
quote_(_X, L, _E) when is_list(L), length(L)>1 ->
    #term{loc=Pos} = _X,
    R =lists:map(fun(E) ->
                         quote_(_X, E, _E)
                 end, L),
    erl_syntax:set_pos(erl_syntax:list(R), Pos)
    ;
quote_(_X, L, _E) ->
    io:format("quote ~p ~p~n", [_X, L]),
    #term{loc=Pos} = _X,
    
    E = case L of
            [Elem] -> Elem;
            _ -> L
        end,
    S = case E of
            #term{value='_', type=atom} ->
                erl_syntax:underscore();
            #term{value=X} -> 
                io:format("atomtry ~ts", [X]),
                erl_syntax:atom(X);
            E when is_integer(E) ->
                erl_syntax:integer(E);
            %%list_to_atom(X);
            [H|T] -> 
                HV = erl_syntax:set_pos(term(H, _E), Pos),
                erl_syntax:cons(HV, quote_(_X, T, _E));
            _ -> E
        end,
    R = erl_syntax:set_pos(S, Pos),
    io:format("quote_ R: ~p~n", [R]),
    R.


lst() ->
    E = [],
    ?Q("B+1+1"),
    C=form(?Q("[add, 1, 2, 3]"), E),
    C2=form(?Q("[mul, 1, 2, 3]"), E),
    C3=form(?Q("[cons, 1, 2]"), E),
    C4=form(?Q("[length, [cons, 1, [cons, 2, []]]]"), E),
    {C, C2, C3, C4}.

