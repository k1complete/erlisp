-module(els_repl).
-include_lib("els.hrl").
-export([
         repl/4,
         init/0,
         tty/0,
         execute/2,
	 eval/2,
        local_function_hander/2]).
-define(TABLE(), lobby).
-define(DEFAULT_MODULE(), lobby).

compile_and_register(Tab, Module, PreAst) ->
    {ok, Beam} = merl:compile_and_load(PreAst, [debug_info, export_all]),
    ets:insert(Tab, {Module, Beam}),
    {ok, Module, Beam}.

init() ->
    Tab = ets:new(?TABLE(), [named_table]),
    InitAst=merl:qquote("-module('@Lobby').", [{'Lobby', merl:term(?DEFAULT_MODULE())}]),
    compile_and_register(Tab, ?DEFAULT_MODULE(), InitAst),
    Tab,
    #{}.

is_ddl({function, _, Fun, Arity, _}) ->
    {ok, Fun, Arity};
is_ddl({attribute, _, record, {Name, Body}}) ->
    {ok, record, {Name, Body}};
is_ddl(_) ->
    false.


register_function(Ast, Env) ->
    Macros = proplists:get_value(macros, Env, #{}),
    case els_localfun:register_local_func(Ast, Macros) of
	{FunDic, _Name, _Arity} ->
	    %% io:format("registerd ~p: ~p: in ~p~n", [Name,Arity, FunDic]),
	    OldEnv = proplists:delete(macros, Env),
	    NewEnv = [{macros, FunDic} | OldEnv],
	    {Ast, NewEnv};
	_  ->
	    {Ast, Env}
    end.

execute(Revert, Env) ->
    case is_ddl(Revert) of
        {ok, FunName, Arity} ->
	    {_NewAst, NewEnv} = register_function(Revert, Env),
	    %% io:format("executed ~p~n", [NewEnv]),
            {value, [ok, FunName, Arity], NewEnv};
        false ->
	    Fun = els_localfun:create_valuefun(proplists:get_value(macros, Env, #{})),
            erl_eval:expr(Revert, Env, {value, Fun})
    end.

eval(List, Env) when is_list(List) -> 
    ErlTree = els_transpile:form(List, Env),
    Reverted = erl_syntax:revert(ErlTree),
    {value, Result, NewEnv} = execute(Reverted, Env),
    {value, Result, NewEnv};
eval(Term, Env) ->
    ErlTree = els_transpile:sterm(Term, Env),
    Reverted = erl_syntax:revert(ErlTree),
    {value, Result, NewEnv} = execute(Reverted, Env),
    {value, Result, NewEnv}.
    

repl(IN, OUT, Line, Env) ->
    {ok, Tokens, NextLine, _Rest} = els_scan:read(IN, "erlisp[~B]> ", Line, [], 0),
    %?LOG_DEBUG(#{nextline=> NextLine}),
    {ok, Forms}  = els_parser:parse(Tokens),
    %%
    io:format(OUT, "~p~n", [NextLine]),
    {_Results, NextEnv} = lists:mapfoldl(
                           fun(S, CEnv) -> 
				   {value, Result, NEnv} = eval(S, CEnv),
                                   %Exp = els_transpile:sterm(S, Env),
                                   %Revert = erl_syntax:revert(Exp),
                                   %{value, Result, NEnv, NewTab} = execute(CTab, Revert, CEnv),
                                   %{{value, Result, NEnv}, NewTab} = new_execute(CTab, Revert, CEnv),
                                   io:format(OUT, "~s~n", [els_pp:format(Result, 80)]),
                                   {Result, NEnv}
                           end, Env, 
                           Forms),
    %%Exp = erl_syntax:list(Exps),
    %%Exp = transpile:form(hd(Forms), Env),
    %Revert = erl_syntax:revert(Exp),
    %io:format("~p~n", [Revert]),
    %{value, Result, NextEnv} = execute(Tab, Revert, Env),
    io:format("~p~n", [NextLine]),
    repl(IN, OUT, NextLine, NextEnv).

local_function_hander(Name, Arg) ->
    ?LOG_DEBUG(#{local_function => {Name, Arg}}),
    apply(?DEFAULT_MODULE(), Name, Arg).

tty() ->
    S = logger:get_primary_config(),
    logger:update_primary_config(S#{level => debug}),
    repl(standard_io, standard_io, 1, []).

    
