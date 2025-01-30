-module(els_localfun).

-export([get_nfundic/0,
	 put_nfundic/1,
	 create_local_func/3,
	 register_local_func/2,
	 create_valuefun/1,
	 strip_macroname_string/1,
	 valuefun/2
	]).
-define(NFUNDIC, nfundic).
-export_type([localfundict/0]).

-type localfundict() :: #{{atom(), integer()} => {{local}, erl_syntax:syntaxTree()}}.

-spec get_nfundic() -> localfundict().
get_nfundic() ->
    case get(?NFUNDIC) of
	undefined ->
	    #{};
	X -> X
    end.

-spec put_nfundic(localfundict()) -> localfundict().
put_nfundic(NFunDic) ->
    put(?NFUNDIC, NFunDic).

-spec create_local_func(list(), erl_syntax:syntaxTree(), localfundict()) -> localfundict().
create_local_func(Name, C, FunDic) ->
    case erl_syntax:revert(C) of 
	{function, Anno, _FName, Arity, Ast} ->
	    LocalFunc = {'named_fun', Anno, Name, Ast},
	    LocalFunAst = erl_syntax:revert(LocalFunc),
	    io:format("create_local_func: ~p=n", [LocalFunAst]),
	    maps:put({Name, Arity}, {{local}, LocalFunAst}, FunDic)
    end.


create_valuefun(Locals) ->
    fun(Name, Arg) ->
	    io:format("value fun ~p(~p)~nMap[~p]~n", [Name, length(Arg), Locals]),
	    {{local}, Func} = maps:get({Name, length(Arg)}, Locals),
	    io:format("value fun ~p~n", [Name]),
	    QArg = erl_syntax:revert(erl_syntax:abstract(Arg)),
	    io:format("---Func: ~p~nArg: ~p~n", [Func, QArg]),
	    Q = merl:qquote(?LINE, "apply(_@Func, _@Arg)", [{'Func', Func}, {'Arg', QArg}]),
	    QQ = erl_syntax:revert(Q),
	    io:format("Q: ~p~n", [QQ]),
	    {value, Value, _NewWEnv} = erl_eval:expr(QQ, [], {value, create_valuefun(Locals)}),
	    Value
    end.

strip_macroname_string(Name) ->
    case atom_to_list(Name) of
	"MACRO_"++Rest ->
	    Rest;
	X ->
	    X
    end.

register_local_func(Node, FunDic) ->
    case erl_syntax:revert(Node) of
	{function, Anno, Name, Arity, Ast} ->
	    FName = list_to_atom(strip_macroname_string(Name)),
	    LocalFunc = {'named_fun', Anno, FName, Ast},
	    LocalFunAst = erl_syntax:revert(LocalFunc),
	    NewFunDic = maps:put({FName, Arity}, {{local}, LocalFunAst}, FunDic),
	    {NewFunDic, Name, Arity};
	_ ->
	    FunDic
    end.

-spec valuefun(atom(), list(term())) -> term().
valuefun(Name, Arg) ->
    {{local}, Func} = maps:get({Name, length(Arg)}, get_nfundic()),
    QArg = erl_syntax:revert(erl_syntax:abstract(Arg)),
    io:format("---Func: ~p~nArg: ~p~n", [Func, QArg]),
    Q = merl:qquote(?LINE, "apply(_@Func, _@Arg)", [{'Func', Func}, {'Arg', QArg}]),
    QQ = erl_syntax:revert(Q),
    io:format("Q: ~p~n", [QQ]),
    {value, Value, _NewWEnv} = erl_eval:expr(QQ, [], {value, fun valuefun/2}),
    Value.
