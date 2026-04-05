-module(els_typespec).
-include_lib("els.hrl").
-export([to_string/1, to_list/1, to_list/2, fun_to_string/2, fun_to_list/3, fun_to_list/2,
        to_binary/1]).
-export([rep/2, fun_clause_arity/3]).
-export([fun_to_string2/3]).
-export([variable_titled/1]).


to_binary(L) ->
    list_to_binary(to_string(L)).

%sexp_to_string(List) ->
%    S=lists:join(" ", List),
%    lists:flatten(["(", S, ")"]).

from_ast({ann_type, Loc, [A, T]}) ->
    [from_ast(A), #item{value="::", loc=Loc, type=atom}, from_ast(T)];
from_ast({type, _, 'bounded_fun', [Ft, Fcs]}) ->
    io:format("Bounded: ~p~n~p~n", [Ft, Fcs]),
    [FtM, RetM] = from_ast(Ft),
    io:format("BoundedRet: ~p~n~p~n", [FtM, RetM]),
    FcM0= lists:map(fun(E) -> from_ast(E) end, Fcs),
    FcM = [#item{type=atom, value="when"} | FcM0],
    [FtM, RetM, FcM];
from_ast({type, _, 'fun', [{type, _, product, Args}, Ret]}) ->
    ArgM = lists:map(fun(E) -> from_ast(E) end, Args),
    io:format("RetIn: ~p~n", [Ret]),
    RetM =  from_ast(Ret),
    [ArgM, RetM];
from_ast({type, _Loc, constraint, [{atom, _Loc2, is_subtype}, [V, T]]}) ->
    [from_ast(V), #item{type=atom, value="::"}, from_ast(T)];
from_ast(L) when is_list(L) ->
    lists:map(fun(E) -> from_ast(E) end, L);
from_ast({var, Loc, Arg}) ->
    #item{type=variable, value=atom_to_list(Arg), loc=Loc};
from_ast({atom, Loc, Arg}) ->
    #item{type=atom, value=atom_to_list(Arg), loc=Loc};
from_ast({type, Loc, Fun, Arg}) ->
    io:format("Toast: ~p~n~p~n", [Fun, Loc]),
    ArgTerms = lists:map(fun(E) -> from_ast(E) end, Arg),
    R = [#item{type=function, value=atom_to_list(Fun), loc=Loc}| ArgTerms],
    io:format("ToastR: ~p~n", [R]),
    R;
from_ast({float, Loc, L}) ->
    #item{type=float, value=L, loc=Loc};
from_ast({integer, _Loc, L}) ->
    L.
%% from_ast({integer, Loc, L}) ->
%%    #item{type=integer, value=L, loc=Loc}.


fun_to_list(Name, Spec) when is_list(Spec) ->
    fun_to_list(Name, Spec, fun (E) -> E end).

fun_to_list(Name, Spec, _F) when is_list(Spec) ->
    Clauses = lists:foldl(fun(E, A) -> A++from_ast(E) end, [], Spec),
    M = [#item{value="-spec", type=function, loc=nil}, 
	 #item{value=atom_to_list(Name), type=function, loc=nil}| Clauses],
    io:format("FTL: ~p", [M]),
    io:format("FTLPP: ~p", [els_pp:pp(M)]),
    %%M2 = lists:foldl(fun(E, A) -> A ++ binary:bin_to_list(E) end, "", lists:flatten(els_pp:pp(M))),
    %%M2 = lists:flatten(io_lib:format("~s", [els_pp:pp(M)])),
    M2 = lists:flatten(io_lib:format("~s", [els_pp:pp(M)])),
    M2.

fun_to_string(Name, Spec) ->
    fun_to_list(Name, Spec).

fun_to_string2(Name, Arity, Specs) ->
    %% SpecName = erl_syntax:atom("sepc"),
    LSpecs = lists:map(
	      fun(Spec) ->
		      variable_titled(Spec)
	      end, Specs),
    Attribute = {attribute, 0, spec, {{Name,Arity}, LSpecs}},
    
    %%Attribute = erl_syntax:attribute(SpecName, [Name, Spec]),
    S = erl_prettypr:format([Attribute]),
    io:format("FuntoString2: ~p~n", [S]),
    S.

variable_titled(A) ->
    R = erl_syntax_lib:map(
	  fun(E) ->
		  case erl_syntax:type(E) of
		      'variable' ->
			  N = erl_syntax:variable_literal(E),
			  erl_syntax:variable(string:titlecase(N));
		      _ ->
			  E
		  end
	  end, A),
    erl_syntax:revert(R).

		   

%fun_to_string_old(Name, Spec) ->
%    {type, _Loc, 'fun', [_ArgsSpec, _ReturnSpec]} = Spec,
%    io:format("fun_to_string: ~nName: ~p~nSpec: ~p~n", [Name, Spec]),
%    [Args, Return] = to_string(Spec),
%    sexp_to_string([sexp_to_string([atom_to_list(Name)]++[Args]), 
%                    Return]).

ss(S) when is_atom(S) ->
    atom_to_list(S);
ss(S) when is_list(S) ->
    SS = lists:join(" ", S),
    lists:flatten(["(", SS, ")"]).

to_string(L) ->
    to_list(L, fun(E) -> ss(E) end).
to_list(L) ->
    to_list(L, fun(E) ->
                        E 
               end).

sexp_to_list(List, F) ->
    F(List).


to_list({var, _, Arg}, F) ->
    F(Arg);
to_list({ann_type, _, [Name| Args]}, F) ->
    sexp_to_list(lists:map(fun(E) ->
                                     to_list(E, F)
                             end, [Name, {atom,0, '::'} |Args]), F);
to_list({type, _, 'product', Args}, F) ->
    ArgsM = [ hd(Args), {atom, 0, '::'}|tl(Args)],
    sexp_to_list(lists:map(fun(E) ->
                                     to_list(E, F)
                             end, ArgsM), F);
to_list({type, _, 'bounded_fun', [Ft, Fc]}, F) ->
    Constraint = lists:map(fun(E) -> to_list(E, F) end, Fc),
    Ftype = to_list(Ft, F),
    sexp_to_list([Ftype,[F('when') | Constraint]], F);
to_list({type, _, 'bounded_fun', Args}, F) ->
    ArgsM = [ hd(Args), {atom, 0, '::'}|tl(Args)],
    sexp_to_list(lists:map(fun(E) ->
                                     to_list(E, F)
                             end, ArgsM), F);
to_list({type, _, 'constraint', [{atom, _, 'is_subtype'}, [V, T]]}, F) ->
    M = case is_list(T) of 
	    true ->
		io:format("LIST ~p~n", [T]),
		lists:map(fun(E) -> to_list(E, F) end, T);
	    false ->
		to_list(T, F)
	end,
    [to_list(V, F), F('::'), M];
to_list({type, _, 'fun', [{type, _, product, Args}, Ret]}, F) ->
    %% io:format("FUNPRO: ~p~n", [Args]),
    Return = to_list(Ret, F),
    A = lists:map(fun(E) ->
                          to_list(E, F)
                  end, Args),
    %% io:format("FUNRET: ~p~n", [[A, Return]]),
    [A, Return];
to_list({type, _, 'fun', Args}, F) ->
    io:format("FUN: ~p~n", [Args]),
    [A, Return] = lists:map(fun(E) ->
                          to_list(E, F)
                  end, Args),
    io:format("FUNRET: ~p~n", [[A, Return]]),
    [A, Return];
to_list({type, _, 'union', List}, F) when is_list(List) ->
    [F('|') | lists:map(fun(E) ->  to_list(E) end, List)];
to_list({type, _, 'tuple', any}, F) ->
    [F('tuple'), [F('any')]];
%%to_list({type, _, 'tuple', Args}, F) when is_list(Args) ->
%%    ArgsM = lists:map(fun(E) -> to_list(E, F) end, Args),
%%    [F('tuple')| ArgsM];
%%to_list({type, _, 'list', Args}, F) when is_list(Args) ->
%%    ArgsM = lists:map(fun(E) -> to_list(E, F) end, Args),
%%    [F('list')| ArgsM];
to_list({type, _, ContainerType, Args}, F) when is_list(Args) ->
    ArgsM = lists:map(fun(E) -> to_list(E, F) end, Args),
    [F(ContainerType) | ArgsM];
to_list({type, _, UserType, Args}, F) when is_atom(UserType), is_list(Args)  ->
    ArgsM = lists:map(fun(E) -> to_list(E, F) end, Args),
    %%[F(ContainerType) | ArgsM];
    %%F([F(UserType)]);
    [F(UserType) | ArgsM ];
to_list({atom, _, A}, F)->
    F(A).

make_module_qualifier(#item{loc=Loc} = MF) ->
    case els_util:getmodfun(MF) of
	{undef, FA} ->
	    FA;
	{MA, FA} ->
	    erl_syntax:set_pos(erl_syntax:module_qualifier(MA, FA), Loc)
    end.

builtin_rep(#item{type=atom, loc=Loc, value=Name}=T, Param, E) ->
    M = #{
	  "any"=>0,
	  "atom"=>0,
	  "binary_range" => {"binary", Param},
	  "binary" => {"binary", [[], 8|Param]},
	  "nonempty_binary" => {"binary", [8, 8|Param]},
	  "bitstring" => {"binary", [[], 1|Param]},
	  "nonempty_bitstring" => {"binary", [1, 1|Param]},
	  "term" =>0,
	  "none"=>0,
	  "dynamic"=>0,
	  "lambda"=> {"fun", Param},
	  "pid"=>0,
	  "integer" => 0,
	  "list" => 0,
	  "nonempty_list" => {"nonempty_list", Param},
	  "maybe_improper_list" => {"maybe_improper_list", Param},
	  "nonempty_improper_list" => {"nonempty_improper_list", Param},
	  ".." =>"range", %% (.. L H) L..H,
	  "map" => {"map", Param},
	  "port"=>0,
	  "reference"=>0,
	  "*" => {op, Param},
	  "div" => {op, Param},
	  "rem" => {op, Param},
	  "band" => {op, Param},
	  "+" => {op, Param},
	  "-" => {op, Param},
	  "bor" => {op, Param},
	  "bxor" => {op, Param},
	  "bsl" => {op, Param},
	  "bsr" => {op, Param},
	  "bnot" => {op, Param},
	  "record" => {"record", Param},
	  "tuple" => {"tuple", Param},
	  "|" => {"|", Param},
	  "nil"=>0, 
	  "float"=>0},
    case maps:get(Name, M, userdefined) of
	userdefined ->
	    case els_util:getmodfun(T) of
		{undef, FA} ->
		    ArgumentsAst = lists:map(fun(A) -> rep(A, E) end, Param),
		    erl_syntax:set_pos(erl_syntax:user_type_application(FA, ArgumentsAst), Loc);
		{MA, FA} ->
		    ArgumentsAst = lists:map(fun(A) -> rep(A, E) end, Param),
		    erl_syntax:set_pos(erl_syntax:type_application(MA, FA, ArgumentsAst), Loc)
	    end;
	{"binary", ParamTerm} ->
	    ArgumentsAst = lists:map(fun(A) -> rep(A, E) end, ParamTerm),
	    MF = make_module_qualifier(T#item{value="binary"}),
	    erl_syntax:set_pos(erl_syntax:type_application(MF, ArgumentsAst), Loc);
	{"fun", []} ->
	    erl_syntax:set_pos(erl_syntax:fun_type(), Loc);
	{"fun", [any_arity, Return]} ->
	    ReturnAst = rep(Return, E),
	    erl_syntax:set_pos(erl_syntax:function_type(any_arity, ReturnAst), Loc);
	{"fun", Rest} ->
	    #{funtype := Ftype, arity := _Arity}  = fun_clause_arity(Rest, E, Loc),
	    case length(Ftype) of
		1 ->
		    hd(Ftype);
		_ ->
		    throw({error, {fun_multiple_clause_not_allowd, Loc, Rest}})
	    end;
	{"map", []} ->
	    erl_syntax:set_pos(erl_syntax:map_type(), Loc);
	{"map", Params} ->
	    FieldAst = lists:map(fun(A) -> map_field_rep(A, E) end, Params),
	    erl_syntax:set_pos(erl_syntax:map_type(FieldAst), Loc);
	{op, Param} ->
	    Ops = els_transpile:form_trans([T| Param], E),
	    erl_syntax:revert(Ops);
	{"record", Param} ->
	    NameAst = els_util:term_make_atom(hd(Param)),
	    ParamAst = lists:map(fun(F) ->
					record_field_rep(F, E)
				end, tl(Param)),
	    erl_syntax:set_pos(erl_syntax:record_type(NameAst, ParamAst), Loc);
	{"tuple", Param} ->
	    ParamAst = case Param of
			   [] -> any_size;
			   _ ->
			       lists:map(fun(F) ->
						 rep(F, E)
					 end, Param)
		       end,
	    erl_syntax:set_pos(erl_syntax:tuple_type(ParamAst), Loc);
	{"|", Param} ->
	    ParamAst = case Param of
			   [] -> any_size;
			   _ ->
			       lists:map(fun(F) ->
						 rep(F, E)
					 end, Param)
		       end,
	    erl_syntax:set_pos(erl_syntax:type_union(ParamAst), Loc);
	0 ->
	    ArgumentsAst = lists:map(fun(A) -> rep(A, E) end, Param),
	    MF = make_module_qualifier(T),
	    erl_syntax:set_pos(erl_syntax:type_application(MF, ArgumentsAst), Loc);
	X  ->
	    %%io:format("X : ~p~n", [X]),
	    ArgumentsAst = lists:map(fun(A) -> rep(A, E) end, Param),
	    MF = make_module_qualifier(T#item{value=X}),
	    erl_syntax:set_pos(erl_syntax:type_application(MF, ArgumentsAst), Loc)
    end.


map_field_rep([#item{value="=>", loc=Loc}, K, V], E) ->
    erl_syntax:set_pos(erl_syntax:map_type_assoc(rep(K, E), rep(V, E)), Loc);
map_field_rep([#item{value=":=", loc=Loc}, K, V], E) ->
    erl_syntax:set_pos(erl_syntax:map_type_exact(rep(K, E), rep(V, E)), Loc).

record_field_rep([#item{type=atom} = Name, #item{value="::", loc=Loc}, Type], E) ->
    NameAst = els_util:term_make_atom(Name),
    TypeAst = rep(Type, E),
    erl_syntax:set_pos(erl_syntax:record_type_field(NameAst, TypeAst), Loc).

-spec rep(#item{}, []) -> erl_syntax:syntaxTree().

%% annoteted type A :: T_0 type
rep([#item{type=atom, loc=Loc}=N, #item{type=atom, value="::"}, [T | Arguments]], E) ->
    %%Nast = term_make_atom(N),
    Nast = els_util:term_make_variable(N),
    %%io:format("anonted ~p~n~p~n", [T, Arguments]),
    Type = builtin_rep(T, Arguments, E),
    erl_syntax:set_pos(erl_syntax:annotated_type(Nast, Type), Loc);
rep([#item{type=atom, loc=Loc}=N, #item{type=atom, value="::"}, L], E) ->
    %%Nast = term_make_atom(N),
    Nast = els_util:term_make_variable(N),
    Type = rep(L, E),
    erl_syntax:set_pos(erl_syntax:annotated_type(Nast, Type), Loc);
%% atom literal
rep(#item{type=atom, value=V} = T, _E) ->
    case hd(V) of
	X when X >= $A, X =< $Z ->
	    els_util:term_make_variable(T);
	_ ->
	    els_util:term_make_atom(T)
    end;
%% integer literal
rep(T, _E) when is_integer(T) ->
    erl_syntax:integer(T);
%% builtin types and (bitstring M N), (nil), 
%%         (lambda), (lambda any_arity T_0), (lamnbda (a b) T_0)
%%         (lamnbda (a b) T_0 when (var :: type) (var :: type)
%%         (.. M N) 
%%         (map) (map (=> k v)) (map (:= k v)
%%         (Op integer integer)
%%         (record name (fname :: type) ...)
%%         (Mod:Type ...)
%%         (tuple) 
%%         (tuple T_1 T_2 ...)
%%         (| T1 T2 ...) 
%%         else... userdefined type
rep([#item{type=atom}=T|Arguments], E) ->
    %%io:format("in ~p~n~p~n", [T, Arguments]),
    builtin_rep(T, Arguments, E);
rep([], _E) ->
    erl_syntax:nil();
rep(nil, _E) ->
    io:format("inNILL ~n", []),
    Nil= erl_syntax:atom("nil"),
    erl_syntax:type_application(Nil, []);
rep(#item{type=string, value=V}, _E) ->
    erl_syntax:abstract(V);
rep(L, _E) when is_integer(L) ->
    erl_syntax:abstract(L).

fun_clause_arity(Param, E, Loc) ->
    fun_clause_arity(Param, #{funtype => []}, E, Loc).
   
fun_clause_arity([], #{funtype := Acc, arity := ArgLen}, _E, _Loc) ->
    #{funtype => lists:reverse(Acc), arity => ArgLen};
fun_clause_arity([Param, Ret|Rest], #{funtype := Acc}, E, Loc) ->
    Return = els_typespec:rep(Ret, E),
    Args = lists:map(fun(Elem) ->
			     %%io:format("argn: ~p~n", [Elem]),
			     els_typespec:rep(Elem, E)
                     end, Param),
    FFtype = erl_syntax:set_pos(erl_syntax:function_type(Args, Return), Loc),
    io:format("fun_clause_arity ~p~n", [Rest]),
    {FT, Rest3} = case Rest of 
		      [[#item{value="when", type=atom} | WhenValues] | Rest2] ->
			  Cls = function_constraint(WhenValues, E),
			  V=erl_syntax:constrained_function_type(FFtype, Cls), 
			  VS = erl_syntax:set_pos(V, Loc),
			  {VS, Rest2};
		       _ ->
			  {FFtype, Rest}
		  end,
    io:format("fun_clause_arity Result ~p~n", [FT]),
    FF = erl_syntax:revert(FT),
    io:format("fun_clause_arity RFF ~p~n", [FF]),
    io:format("fun_clause_arity Rest3 ~p~n", [Rest3]),
    fun_clause_arity(Rest3, #{funtype => [FF|Acc], arity => length(Args)}, E, Loc).

function_constraint(When, Env) ->
    lists:map(fun([N, #item{value="::", type=atom, loc=Loc}, T ]) ->
		      Name = rep(N, Env),
		      Subtype = erl_syntax:set_pos(erl_syntax:atom("is_subtype"), Loc),
		      Type = rep(T, Env),
		      erl_syntax:set_pos(erl_syntax:constraint(Subtype, [Name, Type]), Loc)
	      end, When).

%% (-type (typename var1 var2...) typespec)
%%
