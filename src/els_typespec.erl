-module(els_typespec).
-include_lib("els.hrl").
-export([to_string/1, to_list/1, fun_to_string/2,
        to_binary/1]).
-export([rep/2]).


to_binary(L) ->
    list_to_binary(to_string(L)).

sexp_to_string(List) ->
    S=lists:join(" ", List),
    lists:flatten(["(", S, ")"]).

fun_to_string(Name, Spec) ->
    {type, _Loc, 'fun', [_ArgsSpec, _ReturnSpec]} = Spec,
    io:format("fun_to_string: ~nName: ~p~nSpec: ~p~n", [Name, Spec]),
    [Args, Return] = to_string(Spec),
    sexp_to_string([sexp_to_string([atom_to_list(Name)]++[Args]), 
                    Return]).
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
                             end, [Name, {atom,0, "::"} |Args]), F);
to_list({type, _, 'product', Args}, F) ->
    ArgsM = [ hd(Args), {atom, 0, '::'}|tl(Args)],
    sexp_to_list(lists:map(fun(E) ->
                                     to_list(E, F)
                             end, ArgsM), F);
to_list({type, _, 'fun', Args}, F) ->
    [A, Return] = lists:map(fun(E) ->
                          to_list(E, F)
                  end, Args),
    [A, Return];
to_list({type, _, 'integer', []}, F) ->
    F(integer);
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
%%	  "::" => 0,
	  "any"=>0,
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
	  "list" => {"list", Param},
	  "nonempty_list" => {"nonempty_list", Param},
	  "maybe_improper_list" => {"maybe_improper_list", Param},
	  "nonempty_improper_list" => {"nonempty_improper_list", Param},
	  ".." =>"range", %% (.. L H) L..H,
	  "port"=>0,
	  "reference"=>0,
	  "nil"=>0,
	  "float"=>0},
    case maps:get(Name, M, userdefined) of
	userdefined ->
	    io:format("Name: ~p~n", [Name]),
	    userdefined;
	0 ->
	    ArgumentsAst = lists:map(fun(A) -> rep(A, E) end, Param),
	    MF = make_module_qualifier(T),
	    erl_syntax:set_pos(erl_syntax:type_application(MF, ArgumentsAst), Loc);
	{"list", ParamTerm} ->
	    io:format("L ~p: ~p~n", [T, ParamTerm]),
	    ArgumentsAst = lists:map(fun(A) -> rep(A, E) end, ParamTerm),
	    MF = make_module_qualifier(T#item{value="list"}),
	    erl_syntax:set_pos(erl_syntax:type_application(MF, ArgumentsAst), Loc);
	{"binary", ParamTerm} ->
	    io:format("B ~p: ~p~n", [T, ParamTerm]),
	    ArgumentsAst = lists:map(fun(A) -> rep(A, E) end, ParamTerm),
	    MF = make_module_qualifier(T#item{value="binary"}),
	    erl_syntax:set_pos(erl_syntax:type_application(MF, ArgumentsAst), Loc);
	{"fun", [ParamTerm, Return]} ->
	    ArgumentsAst = case ParamTerm of
			       [] -> any_arity;
			       _ -> lists:map(fun(A) -> rep(A, E) end, ParamTerm)
			   end,
	    ReturnAst = rep(Return, E),
	    io:format("fun: arg ~p ~n return ~p~n", [ArgumentsAst, ReturnAst]),
	    erl_syntax:set_pos(erl_syntax:function_type(ArgumentsAst, ReturnAst), Loc);
	X  ->
	    ArgumentsAst = lists:map(fun(A) -> rep(A, E) end, Param),
	    MF = make_module_qualifier(T#item{value=X}),
	    erl_syntax:set_pos(erl_syntax:type_application(MF, ArgumentsAst), Loc)
    end.

-spec rep(#item{}, []) -> erl_syntax:tree().

%% builtin type
rep(#item{type=atom} = T, _E) ->
    els_util:term_make_atom(T);
%% builtin parameterized type
rep([#item{type=atom, loc=Loc}=N, #item{type=atom, value="::"}, [T | Arguments]], E) ->
    %%Nast = term_make_atom(N),
    Nast = els_util:term_make_variable(N),
    io:format("anonted ~p~n~p~n", [T, Arguments]),
    Type = case builtin_rep(T, Arguments, E) of
	       userdefined ->
		   %%userdefined(T, Arguments);
		   io:format("T ~p~n Arguments~p~n", [T, Arguments]),
		   userdefined;
	       R -> R
	   end,
    erl_syntax:set_pos(erl_syntax:annotated_type(Nast, Type), Loc);
rep([#item{type=atom}=T|Arguments], E) ->
    io:format("in ~p~n~p~n", [T, Arguments]),
    case builtin_rep(T, Arguments, E) of
	userdefined ->
	    %%userdefined(T, Arguments);
	    io:format("T ~p~n Arguments~p~n", [T, Arguments]),
	    userdefined;
	R -> R
    end;
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
