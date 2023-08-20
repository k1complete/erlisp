-module(transpile).

-include_lib("syntax_tools/include/merl.hrl").
-include_lib("erlisp.hrl").
-compile([{debug_info, true}]).
-export([lst/0]).
-include("scan.hrl").
-export([form/2, form_trans/2, term/2, infix_op/4,
        locline/1,
        expand_macro/2, atom_to_item/2]).
-type tree() :: erl_syntax:syntaxTree().
-type env() :: list().


module_function(A, Loc) ->
    {M, F} = A#item.value,
    erl_syntax:set_pos(erl_syntax:module_qualifier(
                         erl_syntax:atom(M),
                         erl_syntax:atom(F)),
%%                         erl_syntax:set_pos(erl_syntax:atom(M), Loc), 
%%                         erl_syntax:set_pos(erl_syntax:atom(F), Loc)), 
                       Loc).

term(A, Env) ->
    term(A, 0, Env).
term(A, Loc, Env) ->
    term_to_ast(A, Loc, Env, false).

term_to_ast(A, Loc, Env, Quote) ->
    case A of
        #item{type=module_function} ->
            module_function(A, Loc);
        #item{type=string, value=V} ->
            erl_syntax:string(V);
        #item{type=atom, value="nil", loc=Aloc} ->
            R = erl_syntax:nil(),
            erl_syntax:set_pos(R, Aloc);
        #item{type=atom, value=Atom, loc=Aloc} when Quote == true ->
            A2 = erl_syntax:atom(Atom),
            S = erl_syntax:atom_name(A2),
            R = erl_syntax:atom(S),
            erl_syntax:set_pos(R, Aloc);
        #item{type=atom, value="_", loc=Aloc} when Quote == false; Quote == 0 ->
            R = erl_syntax:underscore(),
            erl_syntax:set_pos(R, Aloc);
        #item{type=atom, value=Atom, loc=Aloc} when Quote == false; Quote == 0 ->
            A2 = erl_syntax:atom(Atom),
            S = erl_syntax:atom_name(A2),
            %%io:format("<<<Variable ~ts>>>", [S]),
            R =erl_syntax:variable(S),
            erl_syntax:set_pos(R, Aloc);
        #item{type=atom, value=Atom, loc=Aloc} when Quote > 0 ->
            A2 = erl_syntax:atom(Atom),
            S = erl_syntax:atom_name(A2),
            R = erl_syntax:atom(S),
            erl_syntax:set_pos(R, Aloc);
        Integer when is_integer(Integer) ->
            erl_syntax:set_pos(erl_syntax:integer(Integer), Loc);
        Float when is_float(Float) ->
            erl_syntax:set_pos(erl_syntax:float(Float), Loc);
        [[#item{type=atom, value="dot", loc=Aloc}, F]] when is_list(A) ->
            term_to_ast(F, Aloc, Env, Quote);
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
          "-export" => fun export_/3,
          "-macro_export" => fun macro_export_/3,
          "-module" => fun module_/3,
          "-spec" => fun spec_/3,
          "cons" => fun cons_/3,
          "list" => fun list_/3,
          "quote" => fun quote_/3,
          "unquote" => fun unquote_/3,
          "tuple" => fun tuple_/3,
          "binary" => fun binary_/3,
          "-require" => fun require_/3,
          "defun" => fun defun_/3,
          "defmacro" => fun defmacro_/3,
          "case" => fun case_/3,
          "let" => fun let_/3
         },
    maps:get(A, L, undef).



-type sexp() :: list().


walk(F, Env, Fun) when is_list(F) ->
    io:format("ww ~p~n", [F]),
    [H|T] = F,
    Arity = length(T),
    case H of
        #item{type=atom, value=V} ->
            case maps:get({V, Arity},  Env, undefined)  of
                {M, Macro} ->
                    io:format("call-M: ~p~n", [F]),
                    A = Fun(M, Macro, tl(F)),
                    io:format("call-M-Result: ~p~n", [A]),
                    walk(A, Env, Fun);
                undefined ->
                    [H | lists:map(fun (E) -> 
                                           walk(E, Env, Fun)
                                   end, T)]
            end;
        #item{type=module_function, value={Module, Function}} ->
            case maps:get({Module, Function, Arity},  Env, undefined)  of
                {M, Macro} ->
                    io:format("call2: ~p~n", [F]),
                    A = Fun(M, Macro, T),
                    io:format("call-Result: ~p~n", [A]),
                    walk(A, Env, Fun);
                undefined ->
                    [H | lists:map(fun (E) -> 
                                           walk(E, Env, Fun)
                                   end, T)]
            end;
        _ ->
            lists:map(fun (E) -> 
                              walk(E, Env, Fun)
                      end, F)
    end;
walk(F, _Env, _Fun) -> 
    F.

atom_to_item([A|Tail], _Env) when is_atom(A) ->
    [yal_util:make_symbol(A)|Tail];
atom_to_item(List, _Env) ->
    List.

expand_macro(A, E) ->
    R2 = proplists:get_value(require, E, require),
    In = #{{"backquote",  1} => {yal_macro, 'MACRO_backquote'}},
    Out = case ets:whereis(R2) of
              undefined ->
                  maps:new();
              Tid ->
                  maps:from_list(ets:tab2list(Tid))
          end,
    Env = maps:merge(In, Out),
    %Env = In,
    io:format("Map ~p~n", [Env]),
    Result = walk(A, Env, fun(Module, Function, Arguments) -> 
                                  R = apply(Module, Function, Arguments),
                                  io:format("result ~p~n", [R]),
                                  R3 = atom_to_item(R, Env),
                                  R3
                          end),
    io:format("Expanded ~p~n", [Result]),
    Result.

    
-spec form(sexp(), any()) -> tree().
form(A, E) ->
    B = expand_macro(A, E),
    io:format("form-E ~p ~nFrom ~p ~n To ~p~n", [E, A, B]),
    R = form_trans(B, E),
    R
    .

form_trans([XT=#item{value=X, loc=Loc}| T], E) ->
    R=case Inf=dispatch_infix_op(X) of
          undef ->
            case Spf=dispatch_special(X) of
                undef ->
                    call_function(XT, T, E);
                Spf ->
                    io:format("special XT ~p~n", [[XT, T]]),
                    R1=Spf(XT, T, E),
                    %%io:format("specialform: ~p~n", [R1]),
                    R1
            end;
          Inf ->
              Op = X,
              Args = T,
              Inf(Op, Loc, Args, E)
      end,
    R
    ;
form_trans([List| T], E) when is_list(List) ->
    %%io:format("nested ~p~n", [List]),
    form_trans([form_trans(List, E)| T], E).

%%form_trans(#item{value=Term, loc=Loc, type=atom}, _E) ->
%%    erl_syntax:set_pos(erl_syntax:variable(Term), Loc).
    
term_make_atom(Term) ->
    erl_syntax:set_pos(erl_syntax:atom(Term#item.value), Term#item.loc).
term_make_atom(Term, Prefix) ->
    erl_syntax:set_pos(erl_syntax:atom(Prefix++Term#item.value), Term#item.loc).

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
macro_export_(X, L, E) ->
    Loc = X#item.loc,
    io:format("macro_export X ~p~n", [X]),
    Aq = lists:map(fun([Fn, Arg]) ->
                           F = term_make_atom(Fn, "MACRO_"),
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
    E1 = erl_syntax:set_pos(E, Loc),
    E2 = case tl(L) of
             [] -> E1;
             [S|_] ->
                 case S of
                     #item{type=string, value=""} ->
                         E1;
                     S ->
                         {Line, Column} = Loc,
                         Comment = {Line,Column, 0, S#item.value},
                         {R,_} = erl_recomment:recomment_tree(E1, [Comment]),
                         R
                 end
         end,
    E2.
%% spec form
%% (spec (functionname (argname1 type1) 
%%           (or (argname1 type2) (argname3 type3)) ) (when (exp)) 
%%         returntype)
spec_(X, L, E) ->
    Loc = X#item.loc,
    io:format("spec ~p~n", [hd( hd(L) ) ] ),
    FuncName = term_make_atom(hd(hd(L))),
    Return = erl_syntax:type_application(term_make_atom(hd(tl(L))),[]),
    Args = lists:map(fun([AN, AT]) ->
                             NLoc = AN#item.loc,
                             Name = term(AN,NLoc, E),
                             Type=erl_syntax:type_application(term_to_ast(AT, AT#item.loc, E, true), []),
                             erl_syntax:annotated_type(Name, Type)
                     end, tl(hd(L))),
    FFtype = erl_syntax:function_type(Args, Return),
    Ftype = erl_syntax:list([FFtype]),
    Spec = erl_syntax:atom("spec"),
    FuncArity = erl_syntax:integer(length(Args)),
    SpecArg = erl_syntax:tuple([FuncName, FuncArity]),
    FF = erl_syntax:revert(FFtype),
    M = {attribute, Loc, spec, {{erl_syntax:concrete(FuncName),length(Args)}, [FF]}},
    %%M = erl_syntax:attribute(Spec, [erl_syntax:tuple([SpecArg, Ftype])]),
    io:format("Spec: ~p~n", [M]),
    erl_syntax:revert(M),
    M.



match_op(#item{value=_X, loc=Loc}, L, E) ->
    [Left, Right] = L,
    %%io:format("Match: ~p ~p~n", [Left, Right]),
    %%LeftT = term(Left, Loc, E),
    %%RightT= term(Right, Loc, E),
    %%io:format("MatchT: ~p ~p~n", [LeftT, RightT]),
    Me = erl_syntax:match_expr(term(Left, Loc, E), term(Right, Loc, E)),
    %%io:format("Match2: ~p~n", [Me]),
    erl_syntax:set_pos(Me, Loc).

anary_op(Op, Left, _E) ->
    Nexp = erl_syntax:prefix_expr(Op, Left),
    erl_syntax:copy_pos(Op, Nexp).

infix_op(Op, Loc, [Left|Right], E) ->
    %%io:format("TreeInfix~n", []),
    OpType = erl_syntax:set_pos(erl_syntax:operator(Op), Loc),
    Xp =infix_op_do(OpType, [term(Left, Loc, E) |Right], E),
    %%io:format("TreeInfix ~p~nLoc ~p~n", [Xp, Loc]),
    erl_syntax:set_pos(Xp, Loc).

infix_op_do(Op, [Left|T], E) ->
    %%io:format("infix L: ~p, R: ~p~n", [Left, T]),
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
    %%io:format("cons: ~p~n", [L]),
    [Head|Tail] = L,
    #item{loc=Loc} = C,
    case Tail of
        [] -> Head;
        [X] ->
            HHead = term(Head, Loc, E),
            %%io:format("HHead: ~p~n", [HHead]),
            TTail0 = term(X, Loc, E),
%            TTail = erl_syntax:set_pos(erl_syntax:cons(TTail0, erl_syntax:nil()), Loc),
            TTail = TTail0,
            %%io:format("TTail: ~p C: ~p~n", [TTail, C]),
            ?MQP(Loc, "[_@HHead|_@TTail]", [{'HHead', HHead}, {'TTail', TTail}])
    end.

is_when(#item{type=atom, value="when"}) ->
    true;
is_when(_) ->
    false.

guard_list(WhenClause, _Tail, _E) when not is_list(WhenClause) ->
    {[], WhenClause};
guard_list(WhenClause, Tail, E) ->
    {Guard, NBodyList} = case is_when(hd(WhenClause)) of
                             true ->
                                 io:format("Guard: ~p~n", [tl(WhenClause)]),
                                 [WhenGuard | _] = tl(WhenClause),
                                 io:format("Body: ~p~n", [Tail]),
                                 {form(WhenGuard, E), Tail};
                             false ->
                                 {[], [WhenClause | Tail]}
                         end,
    {Guard, NBodyList}.


-spec clause_(list(), env()) -> tree().
clause_(L, E) ->
    [Args, When| Tail] = L,
    PArgs = lists:map(fun(A) ->
                              R = term(A, E),
                              io:format("<P> ~p~n", [R]),
                              R
                      end, Args),
    io:format("WArg: ~p~n", [When]),
    {Guard, NBodyList} = guard_list(When, Tail, E),
    Body = case is_list(NBodyList) of
               true ->
                   lists:map(fun(Elem) -> form(Elem, E) end, NBodyList);
               false ->
                   term(NBodyList, E)
           end,
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

match_defun_comment(Name, Com, Clauses, E) ->
    %%io:format(standard_error, "match-defun-comment ~p~n", [Com]),
    Tree = match_defun_(Name, Clauses, E),
    case Com of 
        #item{type=string, value=""} ->
            Tree;
        Com ->
            Comment = {1, 1, 
                       0, Com#item.value},
            R=erl_recomment:recomment_forms(Tree, [Comment]),
            R
    end.


defun_comment(Name, A, [#item{type=string} = Com | Rest], E) ->
    match_defun_comment(Name, Com, [A|Rest], E);
defun_comment(Name, A, Rest, E) ->
    match_defun_comment(Name, #item{type=string, value="", loc=Name#item.loc}, [A|Rest], E).
make_comment({Line, Column}, Value) ->
    {Line, Column, 0, Value};
make_comment(undefined, Value) ->
    {1, 1, 0, Value}.
getcomment([#item{type=string}=Com|Rest], Pos) ->
    [make_comment(Pos, [Com#item.value]) | Rest];
getcomment(Rest, Pos) ->
    [make_comment(Pos, "")|Rest].
defun_(X, L, E) ->
    io:format("defun_ : ~p~n", [X]),
    Line = X#item.loc,
    [Name, Args | Rest] = L,
    io:format("Name, Args | Rest =~n  ~p~n ~p~n ~p ~n", [Name, Args, Rest]),
    case hd(Args) of
        A when is_list(A) ->
            %match_defun_(Name, [Args|Rest], E);
            defun_comment(Name, Args, Rest, E);
        _  ->
            Pos = Line,
            %%io:format(standard_error, "Getcom ~p~n", [Rest]),
            [Comm|RRest]  = getcomment(Rest, Pos),
            Body = lists:map(fun(A) -> form(A, E) end, RRest),
            io:format("simpleArgs ~p ~n", [Args]),
            ArgList = lists:map(fun(A) -> term(A, E) end, Args),
            %%  Register argument into environment.
            %%  replace body from environment(argment)
            FunName = erl_syntax:atom(Name#item.value),
            io:format("MO: ~p ~p~n", [Line, FunName]),
            MQ=?MQP(Line, "'@name'(_@@args) -> _@@body.", 
                 [{'name', FunName}, 
                  {'args', ArgList},
                  {'body', Body}]),
            io:format("MMQ1: ~p~n~p~n", [MQ, [Comm]]),
            MMQ = case Comm of 
                      {_,_,_, []} ->
                          MQ;
                      {_, _, _, Comment} ->
                          R = erl_syntax:set_precomments(MQ, Comment),
                          %%io:format(standard_error, "PreComment ~p~n", [R]),
                          R
                  end,
            %%MMQ=MQ,
            io:format("MMQ2: ~p~n", [MMQ]),
            MMQ
    end.

defmacro_(X, L, E) ->
    io:format("defmacro_ : ~p~n", [X]),
    Line = X#item.loc,
    [Name, Args | Rest] = L,
    Macro = Name#item{value="MACRO_" ++ Name#item.value},
    io:format("Name, Args | Rest =~n  ~p~n ~p~n ~p ~n", [Macro, Args, Rest]),
    case hd(Args) of
        A when is_list(A) ->
            match_defun_(Macro, [Args|Rest], E);
        _  ->
            Body = lists:map(fun(A) -> form(A, E) end, Rest),
            io:format("simpleArgs ~p ~n", [Args]),
            ArgList = lists:map(fun(A) -> term(A, E) end, Args),
            %%  Register argument into environment.
            %%  replace body from environment(argment)
            FunName = erl_syntax:atom(Macro#item.value),
            io:format("MO: ~p ~p~n", [Line, FunName]),
            MQ=?MQP(Line, "'@name'(_@@args) -> _@@body.", 
                 [{'name', FunName}, 
                  {'args', ArgList},
                  {'body', Body}]),
            io:format("MQ2: ~p~n", [MQ]),
            MQ
    end.
%% (case exp
%%   (pattern1 (when exp)
%%           form)
%%   (pattern2 
%%           form2
%%           form3))
%%   
case_(X, L, E) ->
    io:format("case_ : ~p~n", [X]),
    Line = X#item.loc,
    [Exp | Clauses] = L,
    io:format("clause : ~p~n", [Clauses]),
    ExpAst = term(Exp, E),
    ClauseAstList = lists:map(fun(Form) -> 
                                      [H|T] = Form,
                                      clause_([[H]|T], E) end, Clauses),
    C = erl_syntax:case_expr(ExpAst, ClauseAstList),
    R = erl_syntax:set_pos(C, erl_anno:new(Line)),
    io:format("case : ~p~n", [R]),
    R.

pattern(Term, Env) ->
    term(Term, Env).



%%
%% (let ((a b) (b c))
%%   (bodies1)
%%   (bodies2))
%% (let (((tuple a b) (when a b) b) (b c))
%%   (bodies1)
%%   (bodies2))
%%  list((pattern guard value)) = ArgumentsList 
%%  fun (list(pattern)) -> bodies end(list(value))
%%  compile to 
%%  
let_(X, L, E) ->
    io:format("let_ : ~p~n", [X]),
    Loc = X#item.loc,
    [Args | Rest] = L,
    io:format("Args | Rest =~n  ~p~n ~p ~n", [Args, Rest]),
    {Patterns, RArgs} = lists:foldl(fun(Arg, {P, A}) ->
                                    case Arg of
                                        [[_Pattern | _] = Match, Value] ->
                                            {P ++ [pattern(Match, E)], 
                                             A ++ [term(Value, E)]};
                                        [#item{type = atom} = Param, Value] ->
                                            {P ++ [term(Param, E)], 
                                             A ++ [term(Value, E)]}
                                    end
                              end, {[], []}, Args),
    Body = lists:map(fun(A) -> form(A, E) end, Rest),
    io:format("simpleArgs ~p ~n", [Args]),
    %%  Register argument into environment.
    %%  replace body from environment(argment)
    MQ=?MQP(Loc, "fun(_@@params) -> _@@body end(_@@args)", 
            [{'params', Patterns},
             {'body', Body},
             {'args', RArgs}
            ]),
    io:format("MQ2: ~p~n", [MQ]),
    MQ.




locconv(ES) ->
    E = erl_syntax_lib:map_subtrees(fun(E2) ->
                                           locconv(E2)
                                   end, ES),
    Loc = case erl_syntax:get_pos(E) of
              undefined -> 0;
              X -> X
          end,
    Line = erl_anno:line(Loc),
    Pos = erl_anno:new(Line),
    erl_syntax:set_pos(E, Pos).
    
locline(F) ->
    erl_syntax_lib:map(fun(E) -> 
                               locconv(E)
                       end, F).

call_function(Fun=#item{value=_X, loc=Loc}, T, E) ->
    io:format("call X ~p~nT ~p~nFun ~p~n", [_X, T, Fun]),
    FHead = lists:map(fun(Elem) ->
                              io:format("Term ~p~n", [Elem]),
%                              A = lists:map(fun(Arg) ->
%                                                    term(Arg, E)
%                                            end, Elem),
                              A = term(Elem, E),
                              io:format("TermAfter ~p~n", [A]),
                              A
                      end, T),
    %%io:format("call X2 ~p~nT ~p~n", [term(Fun,E, Loc), FHead]),
    %FName = erl_syntax:set_pos(erl_syntax:atom(X), Loc),
    %FName = term(Fun, E, Loc),
    {M, F} = getmodfun(Fun),
    %%io:format("MQP: ~p : ~p : arg ~p~n", [M, F, FHead]),
    case M of
        undef ->
            %% io:format("MQ: ~p : arg ~p~n", [F, FHead]),
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
                          io:format("List Term ~p~n", [Elem]),
                          A=term(Elem, Env),
                          io:format("List TermAfter ~p~n", [A]),
                          A
                  end, L),
    Loc = X#item.loc,
    erl_syntax:set_pos(erl_syntax:list(R), Loc).

quote_(X, [E], _Env) ->
    %%io:format("quote ~p ~p~n", [X, E]),
    #item{loc=Pos} = X,
    R = term_to_ast(E, Pos, _Env, true),
    %%io:format("quote_ R: ~p~n", [R]),
    R.
    
%backquote_(X, [E], _Env)  -> 
%    %%io:format("bqquote L:~p~n", [E]),
%    R = bc_item([X | [E]], _Env),
%    %%io:format("quote_ R: ~p~n", [R]),
%    R.

unquote_(X, _L, _Env) ->    
    X.

tuple_(#item{loc=Loc}, L, Env) ->
    LForm = lists:map(fun(E) ->
                              term(E, Env)
                      end, L),
    erl_syntax:set_pos(erl_syntax:tuple(LForm), Loc).

binary_(#item{loc=Loc}, L, Env) ->
    LForm = lists:map(fun(E) ->
                              erl_syntax:binary_field(term(E, Env))
                      end, L),
    erl_syntax:set_pos(erl_syntax:binary(LForm), Loc).


require_(#item{loc=_Loc}, L, Env) ->
    Mod = form(hd(L), Env),
    {value, Ret, Env} = erl_eval:expr(erl_syntax:revert(Mod), Env),
    R = proplists:get_value(require, Env, require),
    Macros = yal_util:required_macros(Ret),
    io:format("required module  ~p: ~p in  ~p ~n", [Ret, Macros, R]),
    ets:insert(R, Macros),
    erl_syntax:nil().

lst() ->
    E = [],
    ?Q("B+1+1"),
    C=form(?Q("[add, 1, 2, 3]"), E),
    C2=form(?Q("[mul, 1, 2, 3]"), E),
    C3=form(?Q("[cons, 1, 2]"), E),
    C4=form(?Q("[length, [cons, 1, [cons, 2, []]]]"), E),
    {C, C2, C3, C4}.

