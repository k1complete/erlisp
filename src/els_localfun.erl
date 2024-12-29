-module(els_localfun).

-export([get_nfundic/0,
	 put_nfundic/1,
	 create_local_func/2,
	 valuefun/2
	]).
-define(NFUNDIC, nfundic).

get_nfundic() ->
    get(?NFUNDIC).

put_nfundic(NFunDic) ->
    put(?NFUNDIC, NFunDic).


create_local_func(C, FunDic) ->
    case erl_syntax:revert(C) of 
	{function, Anno, Name, Arity, Ast} ->
	    LocalFunc = {'named_fun', Anno, Name, Ast},
	    NLocalFunc = erl_syntax:revert(LocalFunc),
	    io:format("create_local_func: ~p=n", [NLocalFunc]),
	    maps:put({Name, Arity}, NLocalFunc, FunDic);
	_ ->
	    FunDic
    end.


valuefun(Name, Arg) ->
    Func = maps:get({Name, length(Arg)}, get_nfundic()),
    QArg = erl_syntax:revert(erl_syntax:abstract(Arg)),
    io:format("---Func: ~p~nArg: ~p~n", [Func, QArg]),
    Q = merl:qquote(?LINE, "apply(_@Func, _@Arg)", [{'Func', Func}, {'Arg', QArg}]),
    QQ = erl_syntax:revert(Q),
    io:format("Q: ~p~n", [QQ]),
    {value, Value, _NewWEnv} = erl_eval:expr(QQ, [], {value, fun valuefun/2}),
    Value.
