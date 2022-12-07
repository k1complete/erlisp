-module(transpile).

-include_lib("syntax_tools/include/merl.hrl").
-include_lib("erlisp.hrl").
-compile([{debug_info, true}]).
-export([lst/0]).
-include("scan.hrl").
-export([form/2, form_trans/2, term/2, infix_op/4,
        locline/1]).
-type tree() :: erl_syntax:syntaxTree().
-type env() :: list().


module_function(A, Loc) ->
    {M, F} = A#item.value,
    erl_syntax:set_pos(erl_syntax:module_qualifier(
                         erl_syntax:set_pos(erl_syntax:atom(M), Loc), 
                         erl_syntax:set_pos(erl_syntax:atom(F), Loc)), 
                       Loc).

term(A, Env) ->
    term(A, 0, Env).
term(A, Loc, Env) ->
    term_to_ast(A, Loc, Env, false).

term_to_ast(A, Loc, Env, Quote) ->
    case A of
        #item{type=module_function} ->
            module_function(A, Loc);
        #item{type=atom, value="nil", loc=Aloc} ->
            R = erl_syntax:nil(),
            erl_syntax:set_pos(R, Aloc);
        #item{type=atom, value=Atom, loc=Aloc} when Quote == true ->
            A2 = erl_syntax:atom(Atom),
            io:format("A2 ~p~n", [A2]),
            S = erl_syntax:atom_name(A2),
            R = erl_syntax:atom(S),
            erl_syntax:set_pos(R, Aloc);
        #item{type=atom, value="_", loc=Aloc} when Quote == false; Quote == 0 ->
            R = erl_syntax:underscore(),
            erl_syntax:set_pos(R, Aloc);
        #item{type=atom, value=Atom, loc=Aloc} when Quote == false; Quote == 0 ->
            A2 = erl_syntax:atom(Atom),
            io:format("A2 ~p~n", [A2]),
            S = erl_syntax:atom_name(A2),
            io:format("<<<Variable ~ts>>>", [S]),
            R =erl_syntax:variable(S),
            erl_syntax:set_pos(R, Aloc);
        #item{type=atom, value=Atom, loc=Aloc} when Quote > 0 ->
            A2 = erl_syntax:atom(Atom),
            io:format("A2 ~p~n", [A2]),
            S = erl_syntax:atom_name(A2),
            R = erl_syntax:atom(S),
            erl_syntax:set_pos(R, Aloc);
        Integer when is_integer(Integer) ->
            erl_syntax:set_pos(erl_syntax:integer(Integer), Loc);
        _ when is_list(A), Quote == false ->
            form_trans(A, Env);
        [] when is_list(A) ->
            erl_syntax:set_pos(erl_syntax:nil(), Loc);
        [H|T] when is_list(A) ->
            R = erl_syntax:cons(term_to_ast(H, Loc, Env, Quote), term_to_ast(T, Loc, Env, Quote)),
            erl_syntax:set_pos(R, Loc)
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
          "list" => fun list_/3,
          "quote" => fun quote_/3,
          "unquote" => fun unquote_/3,
          "backquote" => fun backquote_/3,
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

form_trans([XT=#item{value=X, loc=Loc}| T], E) ->
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
    form_trans([form_trans(List, E)| T], E);
form_trans(#item{value=Term, loc=Loc, type=atom}, E) ->
    erl_syntax:set_pos(erl_syntax:variable(Term), Loc).
    
term_make_atom(Term) ->
    erl_syntax:set_pos(erl_syntax:atom(Term#item.value), Term#item.loc).

export_(X, L, E) ->
    Loc = X#item.loc,
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
    Loc = X#item.loc,
    Module = hd(L),
    M = term_make_atom(Module),
    io:format("module_ ~p~n", [M]),
    E = erl_syntax:attribute(erl_syntax:atom(module), [M]),
    erl_syntax:set_pos(E, Loc).
match_op(#item{value=_X, loc=Loc}, L, E) ->
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

infix_op(Op, Loc, [Left|Right], E) ->
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
    #item{loc=Loc} = C,
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

is_when(#item{type=atom, value="when"}) ->
    true;
is_when(_) ->
    false.

-spec clause_(list(), env()) -> tree().
clause_(L, E) ->
    [Args, When| Tail] = L,
    PArgs = lists:map(fun(A) ->
                              R = term(A, E),
                              io:format("<P> ~p~n", [R]),
                              R
                      end, Args),
    io:format("WArg: ~p~n", [When]),
    io:format("When: ~p~n", [(hd(When))#item.value]),
    {Guard, NBodyList} = case is_when(hd(When)) of
                             true ->
                                 io:format("Guard: ~p~n", [tl(When)]),
                                 [WhenGuard | _] = tl(When),
                                 io:format("Body: ~p~n", [Tail]),
                                 {form(WhenGuard, E), Tail};
                             false ->
                                 {[], [When | Tail]}
                         end,
    Body = lists:map(fun(Elem) -> form(Elem, E) end, NBodyList),
    io:format("Arg: ~p~nG: ~p~nB: ~p~n", [PArgs, Guard, Body]),
    Loc = erl_syntax:get_pos(hd(PArgs)),
    S=?MQP(Loc, "(_@PArgs) when _@__Guard -> _@Body",
        [{'PArgs', PArgs}, {'Guard', Guard}, {'Body', Body} ]),
    io:format("SSS: ~p~n", [S]),
    S.


match_defun_(Name, Clauses, E) ->
    io:format("match-defun ~p~n", [Name]),
    FuncName = erl_syntax:set_pos(erl_syntax:atom(Name#item.value), Name#item.loc),
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
    Line = X#item.loc,
    [Name, Args | Rest] = L,
    io:format("Name, Args | Rest =~n  ~p~n ~p~n ~p ~n", [Name, Args, Rest]),
    case hd(Args) of
        A when is_list(A) ->
            match_defun_(Name, [Args|Rest], E);
        _  ->
            Body = lists:map(fun(A) -> form(A, E) end, Rest),
            io:format("simpleArgs ~p ~n", [Args]),
            ArgList = lists:map(fun(A) -> term(A, E) end, Args),
            FunName = erl_syntax:atom(Name#item.value),
            io:format("MO: ~p ~p~n", [Line, FunName]),
            MQ=?MQP(Line, "'@name'(_@@args) -> _@@body.", 
                 [{'name', FunName}, 
                  {'args', ArgList},
                  {'body', Body}]),
            io:format("MQ2: ~p~n", [MQ]),
            MQ
    end.

locconv(E) ->
    Line = erl_anno:line(erl_syntax:get_pos(E)),
    Pos = erl_anno:new(Line),
    erl_syntax:set_pos(E, Pos).
    
locline(F) ->
    erl_syntax_lib:map(fun(E) -> 
                               locconv(E)
                       end, F).

call_function(Fun=#item{value=X, loc=Loc}, T, E) ->
    io:format("call X ~p~nT ~p~nFun ~p~n", [X, T, Fun]),
    FHead = lists:map(fun(Elem) ->
                              term(Elem, E)
                      end, T),
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

getmodfun(#item{type=Type, value=X, loc=Loc}) ->
    case Type of
        atom ->
            {undef, erl_syntax:set_pos(erl_syntax:atom(X), Loc)};
        module_function ->
            {M, F} = X,
            MA=erl_syntax:set_pos(erl_syntax:atom(M), Loc),
            FA=erl_syntax:set_pos(erl_syntax:atom(F), Loc),
            {MA, FA}
    end.
    
list_(X, L, Env) ->
    R = lists:map(fun(Elem) ->
                          term(Elem, Env)
                  end, L),
    Loc = X#item.loc,
    erl_syntax:set_pos(erl_syntax:list(R), Loc).

oquote_(#item{loc=Pos}, [], _) ->
    erl_syntax:set_pos(erl_syntax:nil(), Pos);
oquote_(X, L, _Env) when is_list(L), length(L)>1 ->
    #item{loc=Pos} = X,
    R =lists:map(fun(E) ->
                         quote_(X, E, _Env)
                 end, L),
    erl_syntax:set_pos(erl_syntax:list(R), Pos)
    .
quote_(X, [E], _Env) ->
    io:format("quote ~p ~p~n", [X, E]),
    #item{loc=Pos} = X,
    R = term_to_ast(E, Pos, _Env, true),
    io:format("quote_ R: ~p~n", [R]),
    R.
% backquote
backquote_(X, [E], _Env) when not(is_list(E)) ->
    term_to_ast(E, X#item.loc, _Env, 1)
    ;
backquote_(X, [[#item{value="unquote"}, Form]], _Env) ->
    io:format("unquote Form:~p~n", [Form]),
    form(Form, _Env);
backquote_(X, [E], _Env) when is_list(E) ->
    io:format("bqquote L:~p~n", [E]),
    #item{loc=Pos} = X,
    [Last|Rest] = backquote_elem(E, _Env, []),
    NewElems = lists:reverse(Rest),
    NewLast = form(Last, _Env),
    NewParams = lists:map(fun(Form) ->
                                 form(Form, _Env)
                         end, NewElems),
    io:format("backquote_elemed: ~p~n", [NewParams]),
    R = erl_syntax:list(NewParams, NewLast),
    %R = form([make_symbol(append) | NewElem], Pos),
    % R = term_to_ast(E, Pos, _Env, 1),
    io:format("quote_ R: ~p~n", [R]),
    R.
make_symbol(S) ->
    #item{value=atom_to_list(S), loc=0, type=atom}.
make_symbol(S, Pos) ->
    #item{value=atom_to_list(S), loc=Pos, type=atom}.

backquote_elem([], Env, Acc)  ->
    [[make_symbol(quote), make_symbol('nil')]| Acc];
backquote_elem([H|T], Env, Acc) ->
    NAcc = [[make_symbol(backquote), H] | Acc],
    backquote_elem(T, Env, NAcc);
backquote_elem(A, Env, Acc) when is_record(A, item) ->
    [[make_symbol(quote), A]|Acc].


obackquote_(X, [E], _Env) when is_list(E) ->
    io:format("bqquote X:~p L:~p~n", [X, E]),
    #item{loc=Pos} = X,
    R = term_to_ast(E, Pos, _Env, 1),
    io:format("quote_ R: ~p~n", [R]),
    R.

    
unquote_(X, _L, _Env) ->    
    X.

lst() ->
    E = [],
    ?Q("B+1+1"),
    C=form(?Q("[add, 1, 2, 3]"), E),
    C2=form(?Q("[mul, 1, 2, 3]"), E),
    C3=form(?Q("[cons, 1, 2]"), E),
    C4=form(?Q("[length, [cons, 1, [cons, 2, []]]]"), E),
    {C, C2, C3, C4}.

