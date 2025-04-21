-module(els_repl).
-include_lib("els.hrl").
-export([
         repl/4,
         init/0,
         tty/0,
         execute/2,
	 eval/2,
	 source/2,
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
	{FunDic, Name, Arity} ->
	    %% io:format("registerd ~p: ~p: in ~p~n", [Name,Arity, FunDic]),


	    LocalF = els_localfun:create_valuefun(FunDic),
	    MName = els_localfun:strip_macroname_string(Name),
	    io:format("NName(~p):Name(~p)~n", [MName, Name]),
	    AName = atom_to_list(Name),
	    NewMacros = if AName =/= MName ->
				DName = list_to_atom(MName),
				io:format("Before FunDic: ~p~nDName: ~p~n", [FunDic, DName]),
				NewDic = maps:remove({DName, Arity}, FunDic),
				maps:put(
				  {MName, Arity},
				  {{local},  LocalF}, NewDic);
			   true ->
				io:format("LocalFDic ~p~n",
					  [FunDic]),
				FunDic
			end,

	    OldEnv = proplists:delete(macros, Env),
	    %%NewEnv = [{macros, FunDic} | OldEnv],
	    NewEnv = [{macros, NewMacros} | OldEnv],

	    io:format("Env: ~p~n", [NewEnv]),
	    {Ast, NewEnv};
	_  ->
	    {Ast, Env}
    end.

env_to_binding(Env) ->
    BindList = proplists:get_value(binding, Env, []),
    lists:foldl(fun({K, V}, Acc) ->
			erl_eval:add_binding(K, V, Acc)
		end, erl_eval:new_bindings(), BindList).

execute(Revert, Env) ->
    case is_ddl(Revert) of
        {ok, FunName, Arity} ->
	    {_NewAst, NewEnv} = register_function(Revert, Env),
	    io:format("registered ~p ~n Env ~p~n", [_NewAst, NewEnv]),
            {value, [ok, FunName, Arity], NewEnv};
        false ->
	    Fun = els_localfun:create_valuefun(proplists:get_value(macros, Env, #{})),
	    Binding = env_to_binding(Env),
            {value, Result, NBinding} = erl_eval:expr(Revert, Binding, {value, Fun}),
	    {value, Result, env_update(binding, NBinding, Env)}
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
    


local_function_hander(Name, Arg) ->
    ?LOG_DEBUG(#{local_function => {Name, Arg}}),
    apply(?DEFAULT_MODULE(), Name, Arg).

env_update(Key, Value, Env) ->
    Old=proplists:delete(Key, Env),
    [{Key, Value}|Old].
env_get(Key, Env) ->
    proplists:get_value(Key, Env).

add_line(Env, Line) ->
    env_update('?Line', Line, Env).
get_line(Env) ->
    proplists:get_value('?Line', Env, 0).
    
repl_one(IN, OUT, Line, Env, Acc) ->
    io:format("REPLONE: ~p~n Env: ~p~n", [Line, Env]),
    case  els_scan:read(IN, "erlisp[~B]> ", Line, [], 0) of
	{ok, Tokens, NextLine, _Rest} ->
	    %%?LOG_DEBUG(#{nextline=> NextLine}),
	    io:format("Repl_one: ~p~n", [Tokens]),
	    {ok, Forms}  = els_parser:parse(Tokens),
	    %%n
	    Return = try lists:foldl(
			   fun(S, {value, _Ret, CEnv}) -> 
				   {value, Result, NEnv} = eval(S, CEnv),
				   %%Exp = els_transpile:sterm(S, Env),
				   %%Revert = erl_syntax:revert(Exp),
				   %%{value, Result, NEnv, NewTab} = execute(CTab, Revert, CEnv),
				   %%{{value, Result, NEnv}, NewTab} = new_execute(CTab, Revert, CEnv),
				   %%io:format(OUT, "~s~n", [els_pp:format(Result, 80)]),
				   {value, Result, NEnv}
			   end, {value, [], add_line(Env, NextLine)}, 
			   Forms) 
		     catch 
			 error:Reason:StackTrace -> {error, Reason, StackTrace}
		     end,
	    Return;
	{eof, [], _, _} ->
	    {eof, Acc, Env}
    end.


source_acc(Io, Out, Nline, Env0, RetAcc, OutFun) ->
    io:format("SA: ~p~n", [Env0]),
    case repl_one(Io, Out, Nline, Env0, RetAcc) of
	{value, Ret, Env} ->
	    OutFun(Out, {value, Ret, Env}),
	    source_acc(Io, Out, get_line(Env), Env, Ret, OutFun);
	{error, Ret, Env} ->
	    OutFun(Out, {error, Ret, Env}),
	    {error, Ret, Env};
	%%source_acc(Io, Out, get_line(Env), Env, Ret, OutFun);
	{eof, Ret, Env}  ->
	    {value, Ret, Env}
    end.

output(Out, {value, Value, Env}) ->
    io:format(Out, "~s~n", [els_pp:format(Value, 80)]);
output(Out, Error) ->
    io:format(Out, "~p~n", [Error]).

    
repl(Io, Out, Line, Env0) ->
    source_acc(Io, Out, Line, Env0, [], fun output/2).
						
source(Src, Opt) ->
    S = logger:get_primary_config(),
    logger:update_primary_config(S#{level => info}),
    Line = proplists:get_value('?Line', Opt, 1),
    io:format("Source: Line ~p~n", [Line]),
    Io = tiny_io_server:start_link(Src),
    {value, Ret, Env} = source_acc(Io, Io, Line, Opt, [], fun(_Out, E) -> E end),
    tiny_io_server:stop(Io),
    {value, Ret, Env}.


tty() ->
    S = logger:get_primary_config(),
    logger:update_primary_config(S#{level => debug}),
    repl(standard_io, standard_io, 1, []).

    
