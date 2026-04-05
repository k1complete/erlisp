-module(els_transpile).
-include_lib("stdlib/include/assert.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include_lib("els.hrl").
-compile([{debug_info, true}]).
-include("els_scan.hrl").
-export([clause_/3]).
-export([form/2, form_trans/2, sterm/2, infix_op/4,
        locline/1, merge_into_env/3,
         getmacros_from_module/2,
        expand_macro/3, atom_to_item/2]).
-type erl_tree() :: erl_syntax:syntaxTree().

-define(MQ(L, T, B), merl:qquote(erl_syntax:get_pos(L), T, B)).
-define(MQP(L, T, B), merl:qquote(L, T, B)).

-export([if_/3]).
-export([record_field_rep/2, record_/3, spec_/3, type_/3]).

-spec module_function(#item{}, location()) -> erl_tree().
module_function(#item{value={M, F}}, Loc) ->
    erl_syntax:set_pos(erl_syntax:module_qualifier(
                         erl_syntax:atom(M),
                         erl_syntax:atom(F)),
%%                         erl_syntax:set_pos(erl_syntax:atom(M), Loc), 
%%                         erl_syntax:set_pos(erl_syntax:atom(F), Loc)), 
                       Loc);
module_function(#item{value=A}, Loc) ->
    {module_function, {M, F}} = split(A),
    erl_syntax:set_pos(erl_syntax:module_qualifier(
                         erl_syntax:atom(M),
                         erl_syntax:atom(F)),
		       Loc).




sterm(A, Env) ->
    sterm(A, 0, Env).
sterm(A, Loc, Env) ->
    term_to_ast(A, Loc, Env, false).

term_to_ast(A, Loc, Env, Quote) ->
    case A of
        #item{type=module_function, loc=Aloc, value=V} when Quote == true ->
            Atom = erl_syntax:atom(V),
            erl_syntax:set_pos(Atom, Aloc);
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
	#item{type=character, value=Character, loc=Aloc} ->
	    A2 = erl_syntax:integer(Character),
            erl_syntax:set_pos(A2, Aloc);
        Integer when is_integer(Integer) ->
            erl_syntax:set_pos(erl_syntax:integer(Integer), Loc);
        Float when is_float(Float) ->
            erl_syntax:set_pos(erl_syntax:float(Float), Loc);
        [[#item{type=atom, value="dot", loc=Aloc}, F]] when is_list(A) ->
            term_to_ast(F, Aloc, Env, Quote);
        _ when is_list(A), Quote == false ->
	    %% io:format("STERM: ~p~n", [A]),
            form_trans(A, Env);
        nil ->
            erl_syntax:set_pos(erl_syntax:nil(), Loc);
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
          "+" => fun unary_op/4,
          "++" => fun infix_op/4,
          "-" => fun unary_op/4,
          "--" => fun infix_op/4,
          "*" => fun infix_op/4,
          "/" => fun infix_op/4,
	  "!" => fun infix_op/4,
          "and" => fun infix_op/4,
          "andalso" => fun infix_op/4,
          "band" => fun infix_op/4,
          "bnot" => fun unary_op/4,
          "bor" => fun infix_op/4,
          "bsl" => fun infix_op/4,
          "bsr" => fun infix_op/4,
          "bxor" => fun infix_op/4,
          "div" => fun infix_op/4,
          "not" => fun unary_op/4,
          "or" => fun infix_op/4,
          "orelse" => fun infix_op/4,
          "rem" => fun infix_op/4,
          "xor" => fun infix_op/4
         },
    maps:get(A, L, undef).

dispatch_special(A) ->
    L = #{
          "if" => fun if_/3,
          "case" => fun case_/3,
	  "maybe" => fun maybe_/3,
	  "bc||" => fun binary_comp_/3,
          "binary" => fun binary_/3,
          "defmacro" => fun defmacro_/3,
          "defun" => fun defun_/3,
          "cons" => fun cons_/3,
          "lambda" => fun lambda_/3,
	  "lc||" => fun list_comp_/3,
          "let" => fun let_/3,
          "let=" => fun letequal_/3,
          "list" => fun list_/3,
          "map" => fun map_/3,
	  "named_fun" => fun named_fun_/3,
          "map*" => fun mapp_/3,
	  "=" => fun match_op/3,
	  "?=" => fun maybe_match_/3,
	  "match" => fun match_op/3,
	  "mc||" => fun map_comp_/3,
          "quote" => fun quote_/3,
          "receive" => fun receive_/3,
          "try" => fun try_/3,
          "tuple" => fun tuple_/3,
          "unquote" => fun unquote_/3,
	  "?match" => fun maybe_match_/3,
          "-export" => fun export_/3,
          "-import" => fun import_/3,
          "-macro_export" => fun macro_export_/3,
          "-module" => fun module_/3,
          "-spec" => fun spec_/3,
          "-type" => fun type_/3,
          "-require" => fun require_/3,
          "defrecord" => fun record_/3,
          "#." => fun record_access_/3,
          "#r" => fun record_expr_/3,
	  "<-" => fun generator_/3,
	  "<=" => fun binary_generator_/3,
	  ":=" => fun map_field_exact_/3,
	  "macro-expand" => fun macro_expand_/3
         },
    %% io:format("dispatch [~p]~n", [A]),
    maps:get(A, L, undef).



atom_to_module_function(F) ->            
    case F of
        #item{type=atom, value=Value, loc=Loc} ->
            case split(Value) of
                {module_function, {Module, Function}} ->
                    F#item{type=module_function,
                           value={Module, Function},
			   loc=Loc};
                _ ->
                    F
            end;
        _ ->
            F
    end.

    

walk(F, Env, Fun) when is_list(F) ->
    %% io:format("ww ~p~n", [F]),
    [H|T] = F,
    Arity = length(T),
    Macros = proplists:get_value(macros, Env, #{}),
    case atom_to_module_function(H) of
        #item{type=atom, value=V} when V=="quote" ->
	    %%--
	    F;
        #item{type=atom, value=V, loc=Loc} ->
	    %%--
            case maps:get({V, Arity},  Macros, undefined)  of
		{{local}, Macro} ->
		    %% io:format("local ~p(~p)~n", [F, V]),
		    %%io:format("local-Macro ~p~n", [Macro]),
		    A = Macro(list_to_atom(V), T),
		    %%io:format("localafter ~p~n", [A]),
		    Env2 = [{loc, Loc}|Env],
		    A2 = atom_to_item(A, Env2),
		    %% io:format("localafter2 ~p~n", [A2]),
		    walk(A2, Env2, Fun);
                {M, Macro} ->
                    %% io:format("call-M: ~p~n", [F]),
                    A = Fun(M, Macro, tl(F)),
		    Env2 = [{loc, Loc}|Env],
		    A2 = atom_to_item(A, Env2),
                    %% io:format("call-M-Result: ~p~n", [{H, A2}]),
                    walk(A2, Env2, Fun);
                undefined ->
                    [H | lists:map(fun (E) -> 
                                           walk(E, Env, Fun)
                                   end, T)]
            end;
        #item{type=module_function, value={Module, Function}, loc=Loc} ->
            case maps:get({Module, Function, Arity},  Macros, undefined)  of
                {M, Macro} ->
                    %% io:format("call2: ~p~n", [F]),
                    A = Fun(M, Macro, T),
                    %% io:format("call-Result: ~p~n", [A]),
		    Env2 = [{loc, Loc}|Env],
		    A2 = atom_to_item(A, Env2),
                    walk(A2, Env2, Fun);
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

atom_to_item(A, Env) when is_list(A) ->
    Loc = proplists:get_value(loc, Env, 0),
    lists:map(fun(E) when is_atom(E) ->
                      yal_util:make_symbol(E, Loc);
                 (E) when is_list(E) ->
                      atom_to_item(E, Env);
                 (E) when is_record(E, item), E#item.loc =:= 0 ->
                      E#item{loc=Loc};
                 (E) ->
                      E
              end, A);
atom_to_item(List, _Env) ->
    List.
%%% expand macroではマクロ実行のしかたがlocalと違う
expand_macro(A, E, Macros) ->
    R2 = proplists:get_value(require, E, require),
    %% In = maps:merge(#{{"backquote",  1} => {yal_macro, 'MACRO_backquote'},
    %%                          {"make_symbol", 1} => {yal_util, 'make_symbol'}},
    %%                        Macros),
    Out = case ets:whereis(R2) of
              undefined ->
                  maps:new();
              Tid ->
                  maps:from_list(ets:tab2list(Tid))
          end,
    In = Macros,
    NewMacros = maps:merge(In, Out),
    Env = yal_util:proplists_replace(macros, NewMacros, E),
    %Env = In,
    %% io:format("MapMacoo A ~p ~n InEnv ~p~n", [A, Env]),
    Result = walk(A, Env, fun(Module, Function, Arguments) -> 
                                  %% io:format("Apply Before ~p~n", [Module]),
                                  R = apply(Module, Function, Arguments),
                                  %% io:format("Apply result ~p~n", [R]),
                                  R3 = atom_to_item(R, Env),
                                  %%R4 = expand_macro(R3, Env, Macros),
                                  %% io:format("resultR4 ~p~n", [R3]),
                                  R3
                          end),
    %% io:format("Expanded ~p~n", [Result]),
    Result.

merge_into_env(Env, Key, Value) ->
    MEnv = proplists:to_map(Env),
    %% io:format("merge_into_env ~p~n", [Env]),
    M = maps:get(Key, MEnv, maps:new()),
    %% io:format("merge_into_envM ~p ~p~n", [M, Value]),
    R = maps:merge(M, Value),
    Ret = proplists:from_map(maps:put(Key, MEnv, R)),
    %% io:format("merge_into_env ~p~n", [Ret]),
    Ret.
    

-spec form(sexp(), any()) -> erl_tree().
form(A, E) ->
    M = proplists:get_value(macros, E, maps:new()),
    %NFundic = els_localfun:get_nfundic(),
    %Macros = maps:merge(NFundic, M),
    Macros = M,
    %% io:format("A: ~p~n", [A]),
    B = case A of
	    [#item{value="quote"}|_T] ->
		%% io:format("Q: ~p~n", [T]),
		A;
	    _ ->
		expand_macro(A, E, Macros)
	end,
    %%io:format("form-E ~p ~nFrom ~p ~n To ~p~n", [E, A, B]),
    R = case is_list(B) of
	    true -> 
		Ret = form_trans(B, E),
		%% io:format("form output: ~p ~n to ~p~n", [B, Ret]),
		Ret;
	    false -> 
		sterm(B, E)
	end,
    R
    .

form_trans([XT=#item{value=X, loc=Loc}| T], E) ->
    %%io:format("Form Trans Input ~p~n", [[XT, T]]),
    R=case Inf=dispatch_infix_op(X) of
          undef ->
            case Spf=dispatch_special(X) of
                undef ->
                    call_function(XT, T, E);
                Spf ->
                    %%io:format("special XT ~p~n", [[XT, T]]),
                    R1=Spf(XT, T, E),
                    %%io:format("specialform: ~p~n", [R1]),
                    R1
            end;
          Inf ->
              Op = X,
              Args = T,
              Inf(Op, Loc, Args, E)
      end,
    %%io:format("Form Trans Input-[~p]~n to Output ~p~n", [[XT|T], R]),
    R
    ;
%form_trans([List| T], E) when is_list(List) ->
%    io:format("nested ~p~n", [List]),
%    form_trans([form_trans(List, E)| T], E).
form_trans([List| T], E) when is_list(List) ->
    %% io:format("nested ~p~n", [List]),
    Callable = form_trans(List, E),
    Loc = erl_syntax:get_pos(Callable),
    ?MQP(Loc, "_@F(_@Args)", 
         [{'F', Callable},
          {'Args', 
           lists:map(
             fun(S) -> 
                     sterm(S, E) 
             end, 
             T)}]).

%% nested ではtransしたあとは、beam astになっているので、 trans_formsしてはいけない。
%% これは、先頭要素をcallableとして残りの要素をtransしたあとで、callするのが正しい。

%%form_trans(#item{value=Term, loc=Loc, type=atom}, _E) ->
%%    erl_syntax:set_pos(erl_syntax:variable(Term), Loc).

%% expand macro in current environment
%% (macro-expand (macro args))
macro_expand_(X, [L], E) ->
    Loc = X#item.loc,
    M = proplists:get_value(macros, E, maps:new()),
    B = expand_macro(L, E, M),
    C = term_to_ast(B, Loc, E, true),
    %%C = term_to_ast(B, Loc, E, false),
    C2 = erl_syntax:set_pos(C, Loc),
    %% io:format("L : ~p~n", [C]),
    C2.

export_(X, L, E) ->
    Loc = X#item.loc,
    io:format("export X ~p~n", [X]),
    Aq = lists:map(fun([Fn, Arg]) ->
                           F = els_util:term_make_atom(Fn),
                           A = sterm(Arg, Loc, E),
                           io:format("FA Fis ~p~n Ais ~p~n", [F, A]),
                           erl_syntax:arity_qualifier(F, A)
                   end, L),
    R = erl_syntax:attribute(erl_syntax:atom(export),[erl_syntax:list(Aq)]),
    erl_syntax:set_pos(R, Loc).
macro_export_(X, L, E) ->
    Loc = X#item.loc,
    io:format("macro_export X ~p~n", [X]),
    Aq = lists:map(fun([Fn, Arg]) ->
                           F = els_util:term_make_atom(Fn, "MACRO_"),
                           A = sterm(Arg, Loc, E),
                           io:format("FA Fis ~p~n Ais ~p~n", [F, A]),
                           erl_syntax:arity_qualifier(F, A)
                   end, L),
    R = erl_syntax:attribute(erl_syntax:atom(export),[erl_syntax:list(Aq)]),
    erl_syntax:set_pos(R, Loc).


%%    ArgumentsAst = lists:map(fun(A) -> type_rep(A, E) end, Arguments),
%%    MF = make_module_qualifier(T),
%%    erl_syntax:set_pos(erl_syntax:type_application(MF, ArgumentsAst), Loc).
%%% record field
%%%     
%%% a
%%% (= a term)
%%% (a type)
%%% ((= a term) type)
%% a
record_field_rep(#item{type=atom, loc=Loc} = Name, _Env) ->
    NameAst = els_util:term_make_atom(Name),
    R = erl_syntax:set_pos(erl_syntax:record_field(NameAst), Loc),
    io:format("RF ~p~n", [erl_syntax:revert(R)]),
    R;
%% (= a expression)
record_field_rep([#item{type=atom, value="=", loc=OpLoc}, #item{type=atom}=A, E], Env) ->
    NameAst = els_util:term_make_atom(A),
    ValueAst = sterm(E, Env),
    erl_syntax:set_pos(erl_syntax:record_field(NameAst, ValueAst), OpLoc);
record_field_rep([#item{type=atom, loc=ALoc}=A, T], Env) ->
    RecordValue = record_field_rep(A, Env),
    TypeValue = els_typespec:rep(T, Env),
    R = erl_syntax:set_pos(erl_syntax:typed_record_field(RecordValue, TypeValue), ALoc),
    %%io:format("R: ~p~n R2: ~p~n", [R, erl_syntax:revert(R)]),
    R;
%% ((= a expression) type)
record_field_rep([[#item{type=atom, value="=", loc=OpLoc}, #item{type=atom}, _E]=AE, T], Env) ->
    RecordValue = record_field_rep(AE, Env),
    %%TypeValue = els_typespec:rep(T, E),
    TypeValue = els_typespec:rep(T, Env),
    erl_syntax:set_pos(erl_syntax:typed_record_field(RecordValue, TypeValue), OpLoc).
    

%% (defrecord name a b c)
%% (defrecord name ((=a v) t) b c))
%% (defrecord name (=a v) b c)
%% (defrecord name (a t) b c)
record_(#item{loc=Loc}, [#item{value=Name, type=atom, loc=Nloc} | Definitions], _E) ->    
    Record = erl_syntax:set_pos(erl_syntax:atom("record"), Nloc),
    NameAst = erl_syntax:set_pos(erl_syntax:atom(Name), Nloc),
    RecordFieldsAst = lists:map(fun(D) ->
					record_field_rep(D, Loc)
				end, Definitions),
    Body = erl_syntax:set_pos(erl_syntax:tuple(RecordFieldsAst), Nloc),
    R2 = erl_syntax:set_pos(erl_syntax:attribute(Record, [NameAst, Body]), Nloc),
    R2.


%% record access
%% (#. Exp recordName field)
%%  Exp#recordName.field
%% record expression
%% (#r recordName field1 fieild2....)
%% (# nil recordName field1 fieild2....))
%% (# Exp recordName field1 fieild2....))

record_access_(#item{loc=Loc}, [Exp, #item{type=atom}=RecordName,RecordField], E) ->
    Argument = sterm(Exp, E),
    Type = els_util:term_make_atom(RecordName),
    Field = els_util:term_make_atom(RecordField),
    R = erl_syntax:set_pos(erl_syntax:record_access(Argument, Type, Field), Loc),
    io:format("record_access: ~p~n~p~n", [R, erl_syntax:revert(R)]),
    R.

record_expr_do(Loc, Argument, [#item{type=atom}=RecordName |RecordFields], E) ->
    Type = els_util:term_make_atom(RecordName),
    Fields = lists:map(fun(F) -> record_field_rep(F, E) end, RecordFields),
    R = erl_syntax:set_pos(erl_syntax:record_expr(Argument, Type, Fields), Loc),
    io:format("record_expr0: ~p~n", [R]),
    io:format("record_expr: ~p~n~p~n", [R, erl_syntax:revert(R)]),
    R.

record_expr_(#item{loc=Loc}, [Exp, #item{type=atom}=RecordName |RecordFields], E) ->
    Argument = sterm(Exp, E),
    record_expr_do(Loc, Argument, [RecordName|RecordFields], E);
record_expr_(#item{loc=Loc}, [#item{type=atom}=RecordName|RecordFields], E) ->
    record_expr_do(Loc, none, [RecordName|RecordFields], E).

module_(X, L, _E) ->
    Loc = X#item.loc,
    Module = hd(L),
    M = els_util:term_make_atom(Module),
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
%% (-spec functionname (argname1 :: type1) 
%%           (or (argname1 :: type2) (argname3 :: type3)) ) (when (exp)) 
%%         returntype)
%% (-spec Funname Arg1 Result1 Args2 Result2... When)
spec_(X, L, E) ->
    Loc = X#item.loc,
    io:format("spec raw ~p~n", [L]),
    io:format("spec name ~p~n arg ~p~n return ~p~n", [hd(L), hd(tl(L)), hd(tl(tl(L)))]),
    FuncName = els_util:term_make_atom(hd(L)),
    
%%    Return = type_rep(hd(tl(tl(L))), E),
%%    Args = lists:map(fun(Elem) ->
%%			     io:format("argn: ~p~n", [Elem]),
%%			     Type = type_rep(Elem, E)
%%                     end, hd(tl(L))),
%%    io:format("SpecArgs: ~p~n", [Args]),
    

%%    FFtype = erl_syntax:set_pos(erl_syntax:function_type(Args, Return), Loc),
    #{funtype := FFtype, arity := ArgsLen} = els_typespec:fun_clause_arity(tl(L), E, Loc),
    FuncArity = erl_syntax:integer(ArgsLen),
    _SpecArg = erl_syntax:tuple([FuncName, FuncArity]),
    io:format("SpecFFtype: ~p~n", [FFtype]),
    %%FF = erl_syntax:revert(FFtype),

    M = {attribute, Loc, spec, {{erl_syntax:concrete(FuncName),ArgsLen}, FFtype}},
    %%M = erl_syntax:attribute(Spec, [erl_syntax:tuple([SpecArg, Ftype])]),
    io:format("Spec: ~p~n", [M]),
    M2 = erl_syntax:revert(M),
    %% M.
    M2.
%%
type_(X, L, E) ->
    Loc = X#item.loc,
    TypeName = els_util:term_make_atom(hd(hd(L))),
    TypeArg = lists:map(fun(Elem) -> els_util:term_make_variable(Elem) end, tl(hd(L))),
    TypeDef = els_typespec:rep(hd(tl(L)),E),
    io:format("type_ ~p~n", [TypeDef]),
    io:format("typerevert_ ~p~n", [erl_syntax:revert(TypeDef)]),
    {attribute, Loc, 'type', 
     {erl_syntax:atom_value(TypeName), erl_syntax:revert(TypeDef)},
     TypeArg}.

match_op(#item{value=_X, loc=Loc}, L, E) ->
    [Left, Right] = L,
    %%io:format("Match: ~p ~p~n", [Left, Right]),
    %%LeftT = term(Left, Loc, E),
    %%RightT= term(Right, Loc, E),
    %%io:format("MatchT: ~p ~p~n", [LeftT, RightT]),
    Me = erl_syntax:match_expr(sterm(Left, Loc, E), sterm(Right, Loc, E)),
    %%io:format("Match2: ~p~n", [Me]),
    erl_syntax:set_pos(Me, Loc).


unary_op(Op, Loc, [Item]=_List, E) ->
    %%io:format("error1 ~p~n", [{Op, List}]),
    unary_op_do(Op, Loc, Item, E);
unary_op(Op, Loc, [_Left, _Right] = List, E) 
  when Op == "+" ->
    %% io:format("error2 ~p~n", [{Op, List}]),
    infix_op(Op, Loc, List, E);
unary_op(Op, Loc, [_Left, _Right] = List, E) 
  when Op == "-" ->
    %% io:format("error3 ~p~n", [{Op, List}]),
    infix_op(Op, Loc, List, E);
unary_op(Op, Loc, List, _E) ->
    %% io:format("error4 ~p~n", [{Op, List}]),
    ?THROW({error, {bad_arity, Loc, {Op, length(List)}}}).

unary_op_do(Op, Loc, Left, E) ->
    OpType = erl_syntax:set_pos(erl_syntax:operator(Op), Loc),
    Operand = sterm(Left, E),
    Operand2 = erl_syntax:set_pos(Operand, Loc),
    %% io:format("error5 ~p~n", [{OpType, Operand2, Loc}]),
    Nexp = erl_syntax:prefix_expr(OpType, Operand2),
    %% io:format("error6 ~p~n", [{Nexp, Loc}]),
    erl_syntax:copy_pos(OpType, Nexp).

infix_op(Op, Loc, [_Left], _E) ->
    %%io:format("TreeInfix~n", []),
    ?THROW({error, {bad_arity, Loc, {Op, 1}}});
infix_op(Op, Loc, [Left|Right], E) ->
    %%io:format("TreeInfix~n", []),
    OpType = erl_syntax:set_pos(erl_syntax:operator(Op), Loc),
    Xp =infix_op_do(OpType, [sterm(Left, Loc, E) |Right], E),
    %%io:format("TreeInfix ~p~nLoc ~p~n", [Xp, Loc]),
    erl_syntax:set_pos(Xp, Loc).

infix_op_do(Op, [_Left], _E) ->
    ?THROW({error, {bad_arity, erl_syntax:get_pos(Op), {erl_syntax:atom_name(Op), 1}}});
infix_op_do(Op, [Left,Right], E) ->
    %%io:format("infix L: ~p, R: ~p~n", [Left, T]),
    Pos = erl_syntax:get_pos(Left),
    RightTerm = sterm(Right, Pos, E),
    Nexp = erl_syntax:infix_expr(Left, Op, RightTerm),
    Exp = erl_syntax:copy_pos(RightTerm, Nexp),
    Exp;
infix_op_do(Op, [Left|[Right|Tail]], E) ->
    Pos = erl_syntax:get_pos(Left),
    RightEx = sterm(Right, Pos, E),
    Nexp = erl_syntax:infix_expr(Left, Op, RightEx),
    Exp = erl_syntax:copy_pos(Right, Nexp),
    infix_op_do(Op, [Exp|Tail], E).


cons_(C, L, E) ->
    %%io:format("cons: ~p~n", [L]),
    [Head|Tail] = L,
    #item{loc=Loc} = C,
    case Tail of
        [X] ->
            HHead = sterm(Head, Loc, E),
            %%io:format("HHead: ~p~n", [HHead]),
            TTail0 = sterm(X, Loc, E),
%            TTail = erl_syntax:set_pos(erl_syntax:cons(TTail0, erl_syntax:nil()), Loc),
            TTail = TTail0,
            %%io:format("TTail: ~p C: ~p~n", [TTail, C]),
            ?MQP(Loc, "[_@HHead|_@TTail]", [{'HHead', HHead}, {'TTail', TTail}]);
	_ ->
	    ?THROW({error, {bad_arity, Loc, {L, 1}}})
    end.



-spec clause_(list(), term(), env()) -> erl_tree().
clause_(L, Loc, _E) when length(L) < 2 ->
    ?THROW({error, {no_body, Loc, L}} );
clause_(L, Loc, E) ->
    [Args, WhenCandidate| BodyCandidate] = L,
    {When, Body} = case WhenCandidate of
		       [#item{type=atom, value=V}|_] when V=="when"; V=="whend" ->
			   {WhenCandidate, BodyCandidate};
		       _ ->
			   {[], [WhenCandidate| BodyCandidate]}
		   end,
    io:format("clause_args: ~p~n", [Args]),
    clause_arg_guard_body(Args, When, Body, Loc, E).

%%
%% Args: (class) | (class body) | (class body stacktrace)
%% 
class_qualifier(Args, Loc, E) when length(Args) =< 3, length(Args) >= 1 ->
    Params = lists:map(fun(A) -> sterm(A, E) end, Args),
    Class = hd(Params),
    ClassQ = case length(Params) of
		 1 -> 
		     Throw = erl_syntax:set_pos(erl_syntax:atom("throw"),Loc),
		     erl_syntax:class_qualifier(Throw, Class);
		 2 ->
		     Body = lists:nth(2, Params),
		     erl_syntax:class_qualifier(Class, Body);
		 3 ->
		     Body = lists:nth(2, Params),
		     StackTrace = lists:nth(3, Params),
		     erl_syntax:class_qualifier(Class, Body, StackTrace)
	     end,
    erl_syntax:set_pos(ClassQ, Loc);
class_qualifier(Args, Loc, _E) ->
    ?THROW({error, {bad_class_qualifier, Loc, Args}}).


handler_(L, Loc, E) ->
    [Args, WhenCandidate| BodyCandidate] = L,
    {When, Body} = case WhenCandidate of
		       [#item{type=atom, value=V}|_] when V=="when"; V=="whend" ->
			   {WhenCandidate, BodyCandidate};
		       _ ->
			   {[], [WhenCandidate| BodyCandidate]}
		   end,
    ClassQualifier = class_qualifier(Args, Loc, E),
    clause_ast_guard_body([ClassQualifier], When, Body, Loc, E).


match_defun_(Name, Clauses, E) ->
    io:format("match-defun ~p~n", [Name]),
    FuncName = erl_syntax:set_pos(erl_syntax:atom(Name#item.value), Name#item.loc),
    ClauseAst0 = lists:map(fun(A) ->
%%				   io:format("AST ~p~n", [A]),
				   clause_(A, Name#item.loc, E)
			   end, Clauses),
    Md = erl_syntax:function(FuncName, ClauseAst0),
    io:format("Md ~p~n", [Md]),
    {MdTree, Comment} = erl_syntax_lib:mapfold_subtrees(
			      fun(Tree, Acc) ->
				      io:format(standard_error, "Md SubTree ~p~n", [Tree]),
				      case erl_syntax:type(Tree) of
					  clause -> 
					      case erl_syntax:has_comments(Tree) of
						  true ->
						      C = erl_syntax:get_precomments(Tree),
						      io:format(standard_error, "Md Tree ~p~n", [Tree]),
						      io:format(standard_error, "Md precomments ~p~n", [C]),
						      NC = lists:foldr(fun(CE, A) ->
									       [erl_syntax:comment_text(CE)|A]
								       end, [], C),
						      %% NC = erl_syntax:comment_text(hd(C)),
						      {erl_syntax:set_precomments(Tree, []), Acc++NC};
						  false ->
						      {Tree, Acc}
					      end;
					  _  -> {Tree, Acc}
				      end
			      end, [], Md),
    
    CommentNode = erl_syntax:comment(Comment),
    io:format("~nmatch_defun_comment ~p~ncomment: ~p~n", [CommentNode, Comment]),
    MdTreeComment = erl_syntax:set_precomments(MdTree, [CommentNode]),
    Ret=erl_syntax:copy_pos(FuncName, MdTreeComment),
    
    io:format("~nmatch_defun_output ~p~n", [erl_syntax:get_pos(Ret)]),
    io:format("~nmatch_defun_outputbody ~p~n", [Ret]),
    %%merl:print(Ret),
    io:format("~n ", []),
    Ret.

match_defun_comment(Name, Com, Clauses, E) ->
    io:format("match-defun-comment ~p~n", [Com]),
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
    io:format("defun_comment: A ~p~n Rest: ~p~n", [A, Rest]),
    match_defun_comment(Name, #item{type=string, value="", loc=Name#item.loc}, [A|Rest], E).
make_comment({Line, Column}, Value) ->
    {Line, Column, 0, Value};
make_comment(0, Value) ->
    {1, 1, 0, Value};
make_comment(undefined, Value) ->
    {1, 1, 0, Value}.
getcomment([#item{type=string}=Com|Rest], Pos) ->
    [make_comment(Pos, [Com#item.value]) | Rest];
getcomment(Rest, Pos) ->
    [make_comment(Pos, "")|Rest].
defun_(X, L, E) ->
    %% io:format("defun_ : ~p~n", [X]),
    Line = X#item.loc,
    [Name, Args | Rest] = L,
    %% io:format("Name, Args | Rest =~n  ~p~n ~p~n ~p ~n", [Name, Args, Rest]),
    case Args =/=nil andalso hd(Args) of
        A when is_list(A) -> 
	    %%% match defun
            %match_defun_(Name, [Args|Rest], E);
            Ret = defun_comment(Name, Args, Rest, E),
	    %% io:format("defun_output: ~p~n", [Ret]),
            Ret;
        _  ->
            Pos = Line,
            [Comm|RRest]  = getcomment(Rest, Pos),
            %% io:format(standard_error, "GetBody ~p~n", [RRest]),
            Body = lists:map(fun(A) -> form(A, E) end, RRest),
            %% io:format("simpleArgs ~p ~n", [Args]),
	    ArgList = case Args of
			  nil ->
			      [];
			  _ ->
			      lists:map(fun(A) -> sterm(A, E) end, Args)
		      end,
            %%  Register argument into environment.
            %%  replace body from environment(argment)
            FunName = erl_syntax:atom(Name#item.value),
            %% io:format("MO: ~p ~p~n", [Line, FunName]),
            MQ=?MQP(Line, "'@name'(_@@args) -> _@@body.", 
                 [{'name', FunName}, 
                  {'args', ArgList},
                  {'body', Body}]),
            %% io:format("MMQ1: ~p~n~p~n", [MQ, [Comm]]),
            MMQ = case Comm of 
                      {_,_,_, []} ->
                          MQ;
                      {_, _, _, Comment} ->
			  Com = erl_syntax:comment(Comment),
			  %%io:format("PreComment ~p~n", [Com]),
                          R = erl_syntax:set_precomments(MQ,[Com]), 
			  %%io:format("PreCommentAfter ~p~n", [R]),
                          %%io:format(standard_error, "PreComment ~p~n", [R]),
                          R
                  end,
            %%MMQ=MQ,
            %% io:format("MMQ2: ~p~n", [erl_syntax:revert(MMQ)]),
            MMQ
    end.

defmacro_(X, L, E) ->
    %% io:format("defmacro_ : ~p~n", [X]),
    [Name, Args | Rest] = L,
    Macro = Name#item{value="MACRO_" ++ Name#item.value},
    L2 = [Macro, Args | Rest],
    defun_(X, L2, E).


listsmap(F, L) when is_list(L) ->
    Fun = fun (E) when is_list(E) ->
                  listsmap(F, E);
              (E) ->
                  F(E)
          end,
    lists:map(Fun, L);
listsmap(F, L) ->
    F(L).

make_slist(L) ->
    listsmap(fun(E) when is_atom(E) ->
                     yal_util:make_symbol(E);
                (E) ->
                     E
             end, L).
	    

	    
%%%
%%% (try 
%%%  ((a b) (b c) (c d))
%%%  of 
%%%  (case-pattern-clause)
%%%  catch
%%%  (a (with (a)) (a) (b) : hander)
%%%  (a (a) (b)))
%%% (try 
%%%  ((lambda a b) (list 'a)) body
%%%  ((lambda a b) (list 'a))
%%%  of 
%%%  (case-pattern-clause1): clause
%%%  (case-pattern-clause2)
%%%  catch
%%%  (a (with (a)) (a) (b) : hander)
%%%  (a (a) (b)))
%%%  after 
%%%  (a b)
%%%  (c d))
%%% (try 
%%%  ((lambda a b) (list 'a)) body
%%%  ((lambda a b) (list 'a))
%%%  catch
%%%  (a (with (a)) (a) (b) : hander)
%%%  (a (a) (b)))
%%% 
try_(X, L, E) ->
    io:format("try_ : ~p~n", [[X|L]]),
    Line = X#item.loc,
    {M, LocH}  = els_util:scanlist([X|L], ["try", "of", "catch", "after"]),
    io:format("scanlist : ~p~n LockH : ~p~n", [M, LocH]),
    case lists:any(fun("catch") -> true;
		      ("after") -> true;
		      (_) -> false
		   end, maps:keys(M)) of
	false ->
	    ?THROW({error, {try_must_be_after_or_catch_clause, Line, LocH}});
	true -> true
    end,
    Cls = maps:map(fun(K, V) when K == "try"; K == "after" ->
			   lists:map(fun(S) ->
					     form(S, E)
				     end, V);
		      (K, V) when K=="of" ->
			   LocK = (maps:get(K, LocH))#item.loc,
			   lists:map(fun(S) ->
					     clause_(S, LocK, E)
				     end, V);
		      (K, V) ->
			   LocK = (maps:get(K, LocH))#item.loc,
			   lists:map(fun(S) ->
					     handler_(S, LocK, E)
				     end, V)
		   end, M),

    C = erl_syntax:try_expr(maps:get("try", Cls),
			    maps:get("of", Cls, []),
			    maps:get("catch", Cls, []),
			    maps:get("after", Cls, [])),
    R = erl_syntax:set_pos(C, erl_anno:new(Line)),
    io:format("try : ~p~n", [R]),
    R.

maybe_match_(X, [LH, RH], E) ->
    Line = X#item.loc,
    C = erl_syntax:maybe_match_expr(sterm(LH, E), sterm(RH, E)),
    io:format("maybe_match : ~p~n", [C]),
    R = erl_syntax:set_pos(C, erl_anno:new(Line)),
    R.

%%%
%% (maybe 
%%  (?match pattern exp)
%%  (match pattern exp)
%%  (?match pattern exp)
%%  else
%%  (pattern (when exp)
%%     form)
%%  (pattern (when exp)
%%     form))
maybe_(X, L, E) ->
    Line = X#item.loc,
    {M, LocH}  = els_util:scanlist([X|L], ["maybe", "else"]),
    io:format("maybe_ : ~p~n", [M]),
    Cls = maps:map(fun("maybe", V) ->
			   lists:map(fun(S) ->
					     sterm(S, E)
				     end, V);
		      ("else", V) ->
			   LocK = (maps:get("else", LocH))#item.loc,
			   Clauses = lists:map(fun(S) ->
						       [H|T]=S,
						       clause_([[H]|T], LocK, E)
					       end, V),
			   erl_syntax:else_expr(Clauses)
		   end, M),
    C = erl_syntax:maybe_expr(maps:get("maybe", Cls), 
			      maps:get("else", Cls, none)),
    io:format("maybe-2_ : ~p~n", [erl_syntax:revert(C)]),
    R = erl_syntax:set_pos(C, erl_anno:new(Line)),
    R.


receive_(X, L, E) ->
    io:format("receive_ : ~p~n", [[X|L]]),
    Line = X#item.loc,
    {M, LocH}  = els_util:scanlist([X|L], ["receive", "after"]),
    io:format("scanlist : ~p~n LockH : ~p~n", [M, LocH]),
    Cls = maps:map(fun(K, V) when K=="after" ->
			   [TimeoutTerm|AfterBody] = V,
			   Timeout = sterm(TimeoutTerm, E),
			   After = lists:map(fun(S) ->
						     sterm(S, E)
					     end, AfterBody),
			   {Timeout, After};
		      (K, V) ->
			   LocK = (maps:get(K, LocH))#item.loc,
			   lists:map(fun(S) ->
					     [H|T]=S,
					     clause_([[H]|T], LocK, E)
				     end, V)
		   end, M),
    io:format("trycl : ~p~n", [Cls]),
    {Timeout, After} = maps:get("after", Cls, {none, []}),
    Clauses = maps:get("receive", Cls),
    io:format("timeout: ~p, after : ~p~n", [Timeout, After]),
    C = erl_syntax:receive_expr(Clauses,
				Timeout, After),
    R = erl_syntax:set_pos(C, erl_anno:new(Line)),
    io:format("receive : ~p~n", [R]),
    R.



%%%
%%% (if ((when (isatom a) (bb) ) true )
%%% (if ((whend (when (isatom a) (bb)) (when (aaa).. )) true )
%%% (if ((when (, (isatom a) (bb)) (, (aaa) )) true )
%%%     (disjunctiive_form bod...)
%%%     (disjunctiive_form bod...))
%%%     
%%% (if ((, (isatom a) (bbb)) 1))
%%% (if ((isatom a) 1))
%%%     ((isatom a) (a) (b) (c))
%%% -->
%%% if if_clause1;
%%%    if_clause2;
%%%    if_clause3.
%%%
%%% disjunctive normal form:
%%% (; (, | list) (, | list))
%%% (; list) --> 
%%% 
conjunctive_form([#item{type = atom, value="when"}|Tail], Env) ->
    ?LOG_DEBUG(#{conjunctive_form => Tail}),
    L = lists:map(fun(V) ->
		      sterm(V, Env)
		  end, Tail),
    io:format("Conjunctive: ~p~n", [L]),
    L.
%%%
%%% (if (when (| (& (== 1 2) 
%%%               (== 2 2))
%%%               (a ) 
%%%            (b )  )    (1 ) ('true))
%%%     (when 'true 'ng))
%%% (if (& (== 1 2) 
%%%        (== 2 2))
%%% 
%%% (if ( test1  sexp1..) (test2 sexp2...))
%%% test-> (when cnf1 cnf2)
%%% cnf-> (, test1 test2...)
%%% test -> (, test1 test2..)
%%% test -> (other)
%%% (if (== 1 2)  'true) (when 'true 'ng))
%%% (if (when (== 1 2) (== 2 2) 'true) (when 'true 'ng))
%%% (if (whend (, (== 1 2) (== 2 2)) (a ) (b)  ) 'true) (when 'true 'ng))
disjunctive_form([#item{type = atom, value=A}|Tail], Env) when A == "whend" ->
    io:format("Disjuncti:: ~p~n", [Tail]),
    R0 = lists:map(fun([#item{type = atom, value = "when"}|_]=V) -> 
			   io:format("Disjuncti:::: ~p~n", [V]),
			   R = conjunctive_form(V, Env),
			   io:format("Disjuncti:::::: ~p ~n--> ~p~n", [V, R]),
			   R;
		      (V) ->
			   [sterm(V, Env)]
		   end, Tail),
    io:format("Disjuncti:: ~p ~n ---> ~p~n", [Tail, R0]),
    R0;
disjunctive_form(L, Env) ->
    io:format("Disjunction_form Other: ~p~n"< [L]),
    [[sterm(L, Env)]].


get_leastlefthand([#item{loc=G}|_], _) ->
    G;
get_leastlefthand([H|_L], G) ->
    get_leastlefthand(H, G);
get_leastlefthand(#item{loc=G}, _) ->
    G;
get_leastlefthand(_, G) ->
    G.

    
%% {Param, Test, Body}
%split_param_from_clause
detect_guard(Test, _Body, E) ->
    case Test of
	[#item{value="when"}|_] ->
	    [conjunctive_form(Test, E)];
	[#item{value="whend"}|_] ->
	    io:format("detect_guard ~p~n", [Test]),
	    disjunctive_form(Test, E);
	[[#item{loc=GL}|_]|_] ->
	    When=#item{value="when", loc=GL, type=atom},
	    [conjunctive_form([When|Test], E)];
	[#item{}|_] ->
	    [[sterm(Test, E)]];
	#item{} ->
	    [[sterm(Test, E)]];
	[] ->
	    [];
	_ ->
	    [[sterm(Test, E)]]
    end.

%% 
clause_ast_guard_body(Pattern, Test, Body, GL, E) ->
%%    GLine = get_leastlefthand(lists:flatten([Test|Body]), GL),
    GLine = GL,
    G = detect_guard(Test, Body, E),
    io:format("#{clause_mono_least => ~p~nBody: ~p~nGline: ~p~n", [Test, Body,GLine]),
    [DocItem|Body2] = getcomment(Body, GLine),
    B = lists:map(fun(V) -> 
			  io:format("#{clause_elem => ~p~n", [V]),
			  sterm(V, E) 
		  end, Body2),
    S = erl_syntax:clause(Pattern, G, B),
    ?LOG_DEBUG(#{clause_mono => S}),
    R = erl_syntax:set_pos(S, erl_anno:new(GLine)),
    case DocItem of
	{_,_,_,[]} ->
	    R;
	{_,_,_,Comment} ->
	    C = erl_syntax:comment(0, Comment),
	    R2 = erl_syntax:set_precomments(R, [C]),
	    R2
    end.

clause_arg_guard_body(Args, Test, Body, GL, E) ->
    GLine = get_leastlefthand(lists:flatten([[Args],Test|Body]), GL),
    Params = case Args of
		 Args when is_list(Args) ->
		     lists:map(fun(A) -> sterm(A, E) end, Args);
		 _ ->
		     [sterm(Args, E)]
	     end,
    % Params = lists:map(fun(A) -> sterm(A, E) end, Args),
    clause_ast_guard_body(Params, Test, Body, GLine, E).

%
%%%
%%% (if ((whend (when a b c) (when a b c)) 
%%%        explist)
%%%     ((when a b c) 
%%%        explist)
%%%     (guard explist))
%%%
if_(X, L, E) ->
    Line = X#item.loc,
    ?LOG_DEBUG(#{if_ => L}),
    ClauseAstList = lists:map(fun([Test|[]]) ->
				      ELoc = get_leastlefthand(Test, Line),
				      ?THROW({error, {no_body, ELoc, Test}});
				 ([Test|Body]) ->
				      clause_arg_guard_body([], Test, Body, Line, E)
			      end, L),
    C = erl_syntax:if_expr(ClauseAstList),
    R = erl_syntax:set_pos(C, erl_anno:new(Line)),
    io:format("if1 : ~p~n", [R]),
    io:format("if2 : ~p~n", [erl_syntax:revert(R)]),
    R.


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
    Exp2 = make_slist(Exp),
    io:format("exp : ~p~n", [Exp2]),
    io:format("clause : ~p~n", [Clauses]),
    ExpAst = form(Exp, E),
    ClauseAstList = lists:map(fun(Form) -> 
                                      [H|T] = Form,
                                      clause_([[H]|T], Line, E) end, Clauses),
    C = erl_syntax:case_expr(ExpAst, ClauseAstList),
    R = erl_syntax:set_pos(C, erl_anno:new(Line)),
    io:format("case : ~p~n", [R]),
    R.

pattern(Term, Env) ->
    sterm(Term, Env).

make_temp_var(V, I) ->
    list_to_atom(lists:flatten(io_lib:format("~s@~s", [V, I]))).

replace_vars_do([], Acct, Dic, _Line, _Env) ->
    {lists:reverse(Acct), Dic};
replace_vars_do([[Pattern, Body]| Rest], Acct, Dic, Line, Env) ->
    PatternAst = sterm(Pattern, Env),
    BodyAst = sterm(Body, Env),
    VariableSet = erl_syntax_lib:variables(PatternAst),
    NewDic = sets:fold(fun(Element, AccIn) ->
			       maps:put(Element, make_temp_var(Element, Line), AccIn)
		       end, Dic, VariableSet),
    Ret = erl_syntax_lib:map(fun(Element) ->
				     case erl_syntax:type(Element) of
					 variable ->
					     V = erl_syntax:variable_name(Element),
					     NewV = maps:get(V, NewDic, V),
					     erl_syntax:copy_pos(Element, erl_syntax:variable(NewV));
					 _  ->
					     Element
				     end
			     end, PatternAst),
    Ast = erl_syntax:copy_pos(PatternAst, erl_syntax:match_expr(Ret, BodyAst)),
    io:format("replace: ~p~n~p~nTo: ~p~n", [PatternAst, BodyAst, Ast]),
    io:format("Dict: ~p~nNewDict: ~p~n", [Dic, NewDic]),
    replace_vars_do(Rest, [Ast|Acct], NewDic, Line, Env).

replace_vars(ArgList, Dic, Line, Env) ->
    replace_vars_do(ArgList, [], Dic, Line, Env).
    
%% (let (( a b ) (c  (+ a d) )) bodylist)
%% -->
%% begin
%%   (= a@n b)
%%   (= c@n (+ a@n d) )
%%   bodylist
%% end
letequal_(X, L, E) ->
    io:format("let_ : ~p ~n", [X]),
    Loc = X#item.loc,
    [Args | Rest] = L,
    LocLine = lists:flatten(io_lib:format("~p_~p", [erl_anno:line(Loc),erl_anno:column(Loc)])),
    {NewArgs, Dic} = replace_vars(Args, #{}, LocLine, E),
    Body = lists:map(fun(A) -> 
			    B = form(A, E),
			    erl_syntax_lib:map(
			      fun(Tree) ->
				      case erl_syntax:type(Tree) of
					  variable ->
					      case maps:get(erl_syntax:variable_name(Tree), Dic, none) of
						  none ->
						      Tree;
						  NewValue ->
						      io:format("ReplaceBody: ~p to ~p~n", [Tree, NewValue]),
						      erl_syntax:copy_pos(Tree, erl_syntax:variable(NewValue))
					      end;
					  _  ->
					      Tree
				      end
			      end, B)
		    end, Rest),
    io:format("NewArgs: ~p~nBody: ~p~n", [NewArgs, Body]),
    erl_syntax:set_pos(erl_syntax:block_expr(NewArgs++Body), Loc).


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
                                             A ++ [sterm(Value, E)]};
                                        [#item{type = atom} = Param, Value] ->
                                            {P ++ [sterm(Param, E)], 
                                             A ++ [sterm(Value, E)]}
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
%%
parse_types(#item{loc=Loc, value=Val}) ->
    TList = string:split(Val, "-", all),
    lists:map(fun("unit:"++X) ->
		      {Int, []} = string:to_integer(X),
		      Body = erl_syntax:atom("unit"),
		      Size = erl_syntax:integer(Int),
		      Q = erl_syntax:size_qualifier(Body, Size),
		      erl_syntax:set_pos(Q, Loc);
		 (X) ->
		      Q = erl_syntax:atom(X),
		      erl_syntax:set_pos(Q, Loc)
	      end, TList).

%% typespecifierlist
%% (ts types or unit:[1-256])
%% (binary X)
%% (binary (:bf value size typespec) ) (: aaa ))
%% (binary (:bf value / aaa-sss-fff ) ...
binary_field_(#item{loc=Loc}, [Value], E) ->
    Body = sterm(Value, E),
    R = erl_syntax:binary_field(Body),
    erl_syntax:set_pos(R, Loc);
binary_field_(#item{loc=Loc}, [Value, Size], E) ->
    Body = sterm(Value, E),
    Size = sterm(Size, E),
    R = erl_syntax:binary_field(Body),
    erl_syntax:set_pos(R, Loc);
binary_field_(#item{loc=Loc}, [Value, #item{value="/"}, Types], E) ->
    Body = sterm(Value, E),
    TypeList = parse_types(Types),
    Q = erl_syntax:binary_field(Body, TypeList),
    erl_syntax:set_pos(Q, Loc);
binary_field_(#item{loc=Loc}, [Value, SizeP, Types], E) ->
    io:format("bf: ~p ~p ~p ~n", [Value, SizeP, Types]),
    Body = sterm(Value, E),
    Size = sterm(SizeP, E),
    TypeList = parse_types(Types),
    Q = erl_syntax:binary_field(Body, Size, TypeList),
    erl_syntax:set_pos(Q, Loc).


%% (bc|| a generators)
binary_comp_(_X, [TT|Rest] = _L, E) ->
    Template = sterm(TT, E),
    Body = lists:map(fun(A) -> sterm(A, E) end, Rest),
    MQ = erl_syntax:binary_comp(Template, Body),
    MQ.
			      
%% (lc|| a generators)
list_comp_(_X, [TT|Rest] = _L, E) ->
    Template = sterm(TT, E),
    Body = lists:map(fun(A) -> sterm(A, E) end, Rest),
    MQ = erl_syntax:list_comp(Template, Body),
    MQ.
			      
%%
%% (mc|| a b generators)
%%
%% 
map_comp_(_X, [KT, VT|Rest]=_L, E) ->
    Template = erl_syntax:map_field_assoc(sterm(KT, E), sterm(VT, E)),
    Body = lists:map(fun(A) -> sterm(A, E) end, Rest),
    MQ = erl_syntax:map_comp(Template, Body),
    MQ.

map_field_exact_(#item{loc=Loc}=_X, L, E) ->
    [Name, Value] = lists:map(fun(A) ->
			       sterm(A, E)
		       end, L), 
    S = erl_syntax:map_field_exact(Name, Value),
    erl_syntax:set_pos(S, Loc).


%% (<= x        (bitstring a 1 2 3))
binary_generator_(#item{loc=Loc}=_X, [K, Rest]=_L, E) ->
    Pattern = sterm(K, E),
    Body = sterm(Rest, E),
    S = erl_syntax:binary_generator(Pattern, Body),
    erl_syntax:set_pos(S, Loc).
%%
%% (<- x        (list a 1 2 3)) (=:= x 1)
%% (<- k v (maps 1 2 3 4))
%%
generator_(#item{loc=Loc}=_X, [K, Rest]=_L, E) ->
    Pattern = sterm(K, E),
    Body = sterm(Rest, E),
    S = erl_syntax:generator(Pattern, Body),
    erl_syntax:set_pos(S, Loc);
generator_(#item{loc=Loc}=_X, [K, V, Rest]=_L, E) ->
    PK = sterm(K, E),
    PV = sterm(V, E),
    Pattern = erl_syntax:map_field_exact(PK, PV),
    Body = sterm(Rest, E),
    S = erl_syntax:map_generator(Pattern, Body),
    erl_syntax:set_pos(S, Loc).


%% 
%% (named_fun name ((arg...) (whend ...) body)
%%                 ((arg...) (when  ...) body))
%% 
named_fun_(#item{loc=Loc}, [#item{type=atom, value=N, loc=NLoc}|Rest]=_L, E) ->
    Name = erl_syntax:set_pos(erl_syntax:variable(N), NLoc),
    Clauses = lists:map(fun(LE) ->
				clause_(LE, Loc, E)
			end, Rest),
    NamedFun = erl_syntax:named_fun_expr(Name, Clauses),
    io:format("NNNnamed_fun1: ~p~n~p~n", [erl_syntax:revert(NamedFun), Loc]),
    R = erl_syntax:set_pos(NamedFun, Loc),
    R.

%%
%% (lambda (a b)
%%   (+ a b))
%% (lambda clause1
%%         clanse2...)

lambda_(_X, [[#item{type=atom, value=_N, loc=Loc}|_ArgT]=Args|Rest]=_L, E) ->
    Params = lists:map(fun(A) -> sterm(A, E) end, Args),
    Body = lists:map(fun(A) -> sterm(A, E) end, Rest),
    MQ = ?MQP(Loc, "fun(_@@params) -> _@@body end", 
              [{'params', Params},
               {'body', Body}]),
    %%io:format("lambda: ~p~n", [MQ]),
    MQ;
lambda_(#item{loc=Loc} = _X, L, E) ->
    Clauses = lists:map(fun(LE) ->
                                clause_(LE, Loc, E)
                        end, L),
    Fun=erl_syntax:fun_expr(Clauses),
    R = erl_syntax:set_pos(Fun, Loc),
    io:format("lambda-2: ~p~n", [R]),
    R.

locconv(ES) ->
    E = erl_syntax_lib:map_subtrees(fun(E2) ->
                                           locconv(E2)
                                   end, ES),
    Loc = erl_syntax:get_pos(E),
    Line = erl_anno:line(Loc),
    Pos = erl_anno:new(Line),
    erl_syntax:set_pos(E, Pos).
    
locline(F) ->
    erl_syntax_lib:map(fun(E) -> 
                               locconv(E)
                       end, F).

call_function(Fun=#item{value=_X, loc=Loc}, T, E) ->
    %% io:format("call X ~p~nT ~p~nFun ~p~n", [_X, T, Fun]),
    FHead = lists:map(fun(Elem) ->
                              %% io:format("Term ~p~n", [Elem]),
%                              A = lists:map(fun(Arg) ->
%                                                    sterm(Arg, E)
%                                            end, Elem),
                              A = sterm(Elem, E),
                              %% io:format("TermAfter ~p~n", [erl_syntax:revert(A)]),
                              A
                      end, T),
    %%io:format("call X2 ~p~nT ~p~n", [sterm(Fun,E, Loc), FHead]),
    %FName = erl_syntax:set_pos(erl_syntax:atom(X), Loc),
    %FName = sterm(Fun, E, Loc),
    {M, F} = getmodfun(Fun),
    %%io:format("MQP: ~p : ~p : arg ~p~n", [M, F, FHead]),
    case M of
        undef ->
            %% io:format("MQ: ~p : arg ~p~n", [F, FHead]),
            ?MQP(Loc, "'@F'(_@FHead)", 
                [{'F', F},
                 {'FHead', FHead}]);
        _ ->
            %%io:format("MQMF: ~p ~n FUN: ~p ~n arg: ~p~n", [M, F, FHead]),
            ?MQP(Loc, "'@M':'@F'(_@FHead)", 
                [{'M', M},
                 {'F', F},
                 {'FHead', FHead}])
    end.

split(F) ->
    case string:split(F, ":") of
        S when length(S) > 1 ->
            {module_function, list_to_tuple(S)};
        S -> 
            {atom, hd(S)}
    end.
    
getmodfun(#item{type=Type, value=X, loc=Loc}) when Type == atom; Type== module_function->
    {NType, NX} = split(X),
    case NType of
        atom ->
            {undef, erl_syntax:set_pos(erl_syntax:atom(NX), Loc)};
        module_function ->
            {M, F} = NX,
            MA=erl_syntax:set_pos(erl_syntax:atom(M), Loc),
            FA=erl_syntax:set_pos(erl_syntax:atom(F), Loc),
            {MA, FA}
    end.
    
list_(X, L, Env) ->
    %% io:format("List Term ~p~n", [L]),
    R = lists:map(fun(Elem) ->
                          %%io:format("List Term ~p~n", [Elem]),
                          A=sterm(Elem, Env),
                          %%io:format("List TermAfter ~p~n", [A]),
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

mapp_(#item{loc=Loc}, L, Env) ->
    io:format("mapp-- ~p~n", [L]),
    MapElem = lists:map(fun(Elem) ->
				map_field(Elem, Env)
			end, L),
    erl_syntax:set_pos(erl_syntax:map_expr(MapElem), Loc).

% (map k1 v1 k1 v1)
% (map k1 v1 k1 v1)
% (map k1 v1 k1 v1 (:= k3 v3))


map_field([#item{value="=>", loc=VLoc}, K, V], Env) ->
    R = erl_syntax:map_field_assoc(sterm(K, Env), sterm(V, Env)),
    erl_syntax:set_pos(R, VLoc);
map_field([#item{value=":=", loc=VLoc}, K, V], Env) ->
    R = erl_syntax:map_field_exact(sterm(K, Env), sterm(V, Env)),
    erl_syntax:set_pos(R, VLoc);
map_field(E, Env) ->
    sterm(E, Env).
    
map_(#item{loc=Loc}, L, Env) ->
    {MapFields, _R, _Len} = 
	lists:foldl(fun([#item{value="=>", loc=VLoc}, K, V], {A, _K, _I}) ->
			    R = erl_syntax:map_field_assoc(sterm(K, Env), sterm(V, Env)),
			    {[erl_syntax:set_pos(R, VLoc)| A], [], 1};
		      ([#item{value=":=", loc=VLoc}, K, V], {A, _K, _I}) ->
			    R = erl_syntax:map_field_exact(sterm(K, Env), sterm(V, Env)),
			    {[erl_syntax:set_pos(R, VLoc)| A], [], 1};
		      (V, {A, _K, I}) when I rem 2 == 1 ->
			    {A, V, I+1};
		      (V, {A, K, I}) when I rem 2 == 0 ->
			    Key = sterm(K, Env),
			    VLoc = erl_syntax:get_pos(Key),
			    R = erl_syntax:map_field_assoc(Key, sterm(V, Env)),
			    {[erl_syntax:set_pos(R, VLoc)| A], [], 1}
		   end, {[], [], 1}, L),
    erl_syntax:set_pos(erl_syntax:map_expr(lists:reverse(MapFields)), Loc).
tuple_(#item{loc=Loc}, L, Env) ->
    LForm = lists:map(fun(E) ->
                              sterm(E, Env)
                      end, L),
    erl_syntax:set_pos(erl_syntax:tuple(LForm), Loc).

binary_(#item{loc=Loc}, L, Env) ->
    LForm = lists:map(fun([#item{value=":bf"}|_]=S) when is_list(S) ->
			       binary_field_(hd(S), tl(S), Env);
			 (S) ->
			      B = sterm(S, Env),
			      erl_syntax:binary_field(B)
                      end, L),
    erl_syntax:set_pos(erl_syntax:binary(LForm), Loc).

getmacrotable(Env) ->
    proplists:get_value(require, Env, require).

getmacros_from_module(ModForm, Env) ->
    Mod = form(ModForm, Env),
    io:format("getmacros: ~p -> ~n~p~n", [ModForm, erl_syntax:revert(Mod)]),
    {value, ModuleAtom, Env} = erl_eval:expr(erl_syntax:revert(Mod), Env),
    io:format("getmacro module: ~p~n", [ModuleAtom]),
    yal_util:required_macros(ModuleAtom).
    
require_(#item{loc=_Loc}, L, Env) ->
    Macros = getmacros_from_module(hd(L), Env),
    MacroTable=getmacrotable(Env),
    ets:insert(MacroTable, Macros),
    erl_syntax:nil().

import_(#item{loc=_Loc}, L, Env) ->
    Macros = getmacros_from_module(hd(L), Env),
    MacroTable=getmacrotable(Env),
    ets:insert(MacroTable, Macros),
    ImportedMacros = lists:map(fun({{_, F, A}, {Module, Function}}) ->
                                       {{F, A}, {Module, Function}}
                               end, Macros),
    ets:insert(MacroTable, ImportedMacros),
    erl_syntax:nil().

