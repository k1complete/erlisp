-module(repl).
-include_lib("erlisp.hrl").

-export([repl/5, 
         repl/4,
         init/0,
         tty/0,
         execute/3,
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
    Tab.

is_ddl({function, _, Fun, Arity, _}) ->
    {ok, Fun, Arity};
is_ddl(_) ->
    false.

recompile(Tab, Module, Fun, Arity, Ast) ->
    [{Module, OldBeam}] = ets:lookup(Tab, Module),
    {ok, {_, [{abstract_code, {_, OldAst}}]}} = beam_lib:chunks(OldBeam, [abstract_code]),
    PreAst = lists:filter(fun(E) -> case is_ddl(E) of
                                        {ok, Fun, Arity} ->
                                            false;
                                        {ok, _, _} ->
                                            true;
                                        false -> 
                                            true
                                        end
                          end, OldAst),
    NewAst = lists:append(PreAst, [Ast]),
    compile_and_register(Tab, Module, NewAst).

execute(Tab, Revert, Env) ->
    case is_ddl(Revert) of
        {ok, Fun, Arity} ->
            {ok, M, _B} = recompile(Tab, ?DEFAULT_MODULE(), Fun, Arity, Revert),
            {value, [ok, M, b], Env};
        false ->
            Fun = fun(N, A) -> local_function_hander(N, A) end,
            erl_eval:expr(Revert, Env, {value, Fun})
    end.

repl(IN, OUT, Line, Env) ->
    repl(?TABLE(), IN, OUT, Line, Env).

repl(Tab, IN, _OUT, Line, Env) ->
    {ok, Tokens, NextLine, _Rest} = scan:read(IN, "erlisp[~B]> ", Line, [], 0),
    io:format("~p~n", [NextLine]),
    {ok, Forms}  = parser:parse(Tokens),
    Exp = transpile:form(Forms, Env),
    Revert = erl_syntax:revert(Exp),
    io:format("~p~n", [Revert]),
    {value, Result, NextEnv} = execute(Tab, Revert, Env),
    io:format("~s~n", [pp:format(Result, 60)]),
    repl(Tab, IN, _OUT, NextLine, NextEnv).

local_function_hander(Name, Arg) ->
    io:format("local function: ~p~n", [{Name, Arg}]),
    apply(?DEFAULT_MODULE(), Name, Arg).

tty() ->
    Table = init(),
    repl(Table, standard_io, standard_io, 1, []).

    