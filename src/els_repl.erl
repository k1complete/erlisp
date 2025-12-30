-module(els_repl).
-include_lib("els.hrl").
-export([
	 start/1,
	 start/0,
         repl/4,
         init/0,
         tty/0,
         tty/1,
         execute/2,
	 eval/2,
	 start_tty/1,
	 source/2,
	 extract_record_module/2,
	 local_function_hander/2]).
-define(TABLE(), lobby).
-define(DEFAULT_MODULE(), lobby).

compile_and_register(Tab, Module, PreAst) ->
    {ok, Beam} = merl:compile_and_load(PreAst, [debug_info, export_all]),
    ets:insert(Tab, {Module, Beam}),
    {ok, Module, Beam}.

start() ->
    start([]).

start(Args) ->
    ok = shell:start_interactive({?MODULE, start_tty, [Args]}),
    %%io:format("Plain ~p~n", [init:get_plain_arguments()]),
    %%io:format("Arguments ~p~n", [init:get_arguments()]),
    %%io:format("Params ~p~n", [Args]),
    timer:sleep(infinity).

start_tty(Args) ->
    spawn(fun() ->
		  tty(Args)
	  end).

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
	    %% io:format("NName(~p):Name(~p)~n", [MName, Name]),
	    AName = atom_to_list(Name),
	    NewMacros = if AName =/= MName ->
				DName = list_to_atom(MName),
				io:format("Before FunDic: ~p~nDName: ~p~n", [FunDic, DName]),
				NewDic = maps:remove({DName, Arity}, FunDic),
				maps:put(
				  {MName, Arity},
				  {{local},  LocalF}, NewDic);
			   true ->
				%%io:format("LocalFDic ~p~n",  [FunDic]),
				FunDic
			end,

	    OldEnv = proplists:delete(macros, Env),
	    %%NewEnv = [{macros, FunDic} | OldEnv],
	    NewEnv = [{macros, NewMacros} | OldEnv],

	    %% io:format("Env: ~p~n", [NewEnv]),
	    {Ast, NewEnv};
	_  ->
	    {Ast, Env}
    end.

env_to_binding(Env) ->
    BindList = proplists:get_value(binding, Env, []),
    A = lists:foldl(fun({K, V}, Acc) ->
			    erl_eval:add_binding(K, V, Acc)
		    end, erl_eval:new_bindings(), BindList),
    EnvBinding = lists:filter(fun({env, _}) -> false;
				 (_) -> true
			      end, BindList),
    NewEnv = [{binding, EnvBinding} | proplists:delete(binding, Env)],
    %%io:format("IO: ~p~n", [EnvBinding]),
    erl_eval:add_binding(env, NewEnv, A).

get_record_defs(Env) ->
    proplists:get_value(record_defs, Env, []).
add_record_defs(Env, Ast) ->
    env_update(record_defs, [Ast| get_record_defs(Env)], Env).

execute(Revert, Env) ->
    RecordDefs = get_record_defs(Env),
    case is_ddl(Revert) of
        {ok, record, {Name, _Body}} ->
	    io:format("DEFRECORD ~p~n", [Revert]),
	    NewEnv = add_record_defs(Env, Revert),
            {value, [ok, record, Name], NewEnv};
        {ok, FunName, Arity} ->
	    Revert2 = extract_record_function(RecordDefs, Revert),
	    {_NewAst, NewEnv} = register_function(Revert2, Env),
	    %%{_NewAst, NewEnv} = register_function(Revert, Env),
	    %% io:format("registered ~p ~n Env ~p~n", [_NewAst, NewEnv]),
            {value, [ok, FunName, Arity], NewEnv};
        false ->
	    Fun = els_localfun:create_valuefun(proplists:get_value(macros, Env, #{})),
	    Binding = env_to_binding(Env),
	    %% io:format("execute: ~p~nRecord: ~p~n", [Revert, RecordDefs]),
	    [Revert2] = extract_record_module(RecordDefs, [Revert]),
	    %%io:format("Revert2: ~p~nRevert: ~p~n", [Revert2, Revert]),
	    %%io:format("Binding ~p ~n", [Binding]),
            %%{value, Result, NBinding} = erl_eval:expr(Revert, Binding, {value, Fun}),
	    %% io:format("Revert2: ~p~n", [Revert2]),
            {value, Result, NBinding} = erl_eval:expr(Revert2, Binding, {value, Fun}),
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
    
repl_one(IN, _OUT, Line, Env, Acc) ->
    %%io:format("REPLONE: ~p~n Env: ~p~n", [Line, Env]),
    case  els_scan:read(IN, "els[~B]> ", Line, [], 0) of
	{ok, [], NextLine, _Rest} ->
	    repl_one(IN, _OUT, NextLine, Env, Acc);
	{ok, Tokens, NextLine, _Rest} ->
	    %%?LOG_DEBUG(#{nextline=> NextLine}),
	    %%io:format("Repl_one: ~p~n", [Tokens]),
	    {ok, Forms}  = els_parser:parse(Tokens),
	    %%n
	    Return = 
		try lists:foldl(
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
		    error:Reason:Stack ->
			io:format("Catch ~p~nStack: ~p~n", [Reason, Stack]),
			throw({error, Reason, Env});
		    throw:Reason:_Stack ->
			throw({error, Reason, Env})
		end,
	    Return;
	{eof, [], _, _} ->
	    {eof, Acc, Env}
    end.


source_acc(Io, Out, Nline, Env0, RetAcc, OutFun) ->
    %%io:format("SA: ~p~n", [Env0]),
    V = fun(_Name, _Args) ->
		Env0
	end,
    K = {env,0},
    Env1 = env_update(macros, maps:put(K, {local, V}, env_get(macros, Env0)), Env0),
    case repl_one(Io, Out, Nline, Env1, RetAcc) of
	{value, Ret, Env} ->
	    OutFun(Out, {value, Ret, Env}),
	    source_acc(Io, Out, get_line(Env), Env, Ret, OutFun);
	{error, Ret, Env} ->
	    OutFun(Out, {error, Ret, Env});
	%%{error, Ret, Env};
	%%source_acc(Io, Out, get_line(Env), Env, Ret, OutFun);
	{eof, Ret, Env}  ->
	    {value, Ret, Env}
    end.

output(Out, {value, Value, _Env}) ->
    io:format(Out, "~s~n", [els_pp:format(Value, 80)]);
output(Out, Error) ->
    io:format(Out, "~s~n", [els_pp:format(Error, 80)]).

    
repl(Io, Out, Line, Env0) ->
    try 
	%% io:format("opt: ~p~n ~p~n", [Io, io:getopts(Io)]),
	source_acc(Io, Out, Line, Env0, [], fun output/2)
    catch
	throw:{error, Reason,Env1} ->
	    io:format("Catch ~p~n", [Reason]),
	    Line1 = proplists:get_value('?Line', Env1, 1),
	    output(Out, {error, Reason, #{}}),
	    repl(Io, Out, Line1, Env1)
    end.
						
source(Src, Opt) ->
    S = logger:get_primary_config(),
    logger:update_primary_config(S#{level => info}),
    Line = proplists:get_value('?Line', Opt, 1),
    io:format("Source: Line ~p~n", [Line]),
    Io = case Src of
	     standard_io ->
		 standard_io;
	     P when is_pid(P) ->
		 P;
	     _ ->
		 tiny_io_server:start_link(Src)
    end,
    Returns = source_acc(Io, Io, Line, init(Opt), [], fun(_Out, E) -> E end),
    tiny_io_server:stop(Io),
    %%%{value, Ret, Env}.
    Returns. 


init(_Env) ->
    Macros = #{},
    [{macros, Macros}|_Env].

tty() ->
    tty([]).

tty(Args) ->
    %%io:format("Args: ~p~n", [Args]),
    S = logger:get_primary_config(),
    %%io:format("getopts ~p~n", [io:getopts(standard_io)]),
    %%io:format("keymap ~p~n", [edlin:keymap()]),
    logger:update_primary_config(S#{level => debug}),
    repl(standard_io, standard_io, 1, init(Args)).

    
extract_record_module(RecordDefs, Trees) ->
    F = erl_syntax:function(erl_syntax:atom("function_test"),
			    [erl_syntax:clause([], none, Trees)]),
    Cls = extract_record_clause(RecordDefs, F),
    Bodies = erl_syntax:clause_body(hd(Cls)),
    Bodies.
			 
extract_record_clause(RecordDefs, Clause) ->
    B = extract_record_function(RecordDefs, Clause),
    erl_syntax:function_clauses(B).

extract_record_function(RecordDefs, Function) ->
    Rds = lists:map(fun(E) ->
			    erl_syntax:revert(E)
		    end, RecordDefs),
    F2 = erl_syntax:revert(Function),
    B = erl_expand_records:module(Rds++[F2], []), 
    lists:last(B).

    
