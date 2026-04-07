-module(els).
-export([main/1]).
%%#!env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/els/ebin
-include_lib("kernel/include/file.hrl").
-define(BeamExt, ".beam").
-define(ElsExt, ".elisp").

version(A, B) ->
    io:format("version~p~n~p~n", [A, B]).
compile(A) ->
    B = maps:get(file, A),
    Result = lists:map(fun(E) ->
			       try els_compile:file_ast(E,[], A) of
				   {ok, Module, _Beam, _Ast} ->
				       {ok, Module}
			       catch
				   Error ->
				       {error, Error}
			       end
		       end, B),
    case lists:all(fun(R) -> element(1, R) == ok end, Result) of
	true ->	   
	    0;
	false ->
	    1
    end.
		   

run(A) ->
    B = maps:get(file, A),
    
    S = try 
	file:open(B, [read, {encode, utf8}])
	catch 
	    Error0 ->
		{error, Error0}
	end,
    
    case S of
	{ok, F} ->
	    try 
		case els_repl:source(F,[]) of
		    {value, _Value, _} ->
			0
		end
	    catch 
		Error ->
		    io:format("Exception: ~p~n", [Error]),
		    1
	    end;
	{error, Reason} ->
	    io:format("Error: ~p~n", [Reason]),
	    0
    end.

    
main(Args) ->
    S=escript:script_name(),
    VSN = try
	      ok = application:load(els),
	      ok = application:ensure_started(els),
	      application:get_all_key(els)
	  catch
	      _ExeptionPattern ->
		  halt(0)
	  end,
    Opt = #{progname => S, command =>[S, "help"]},
    Cmd = #{arguments => 
		[
		 #{name => verbose,
		   long => "-verbose",
		   short => $V,
		   action => 'count',
		   type => boolean,
		   help => "verbose"}
		],
	    commands => 
		#{"version" => 
		      #{help => "print version",
			handler =>
			    fun(A) ->
				    {done, version(A, VSN)}
			    end
		       },
		  "help" => 
		      #{help=>"help for help",
			handler => 
			    fun(A) ->
				    io:format("HELP ~p~n", [A])
			    end
		       },
		  "shell" => 
		      #{help=>"start interactive shell",
			required => false,
			handler => 
			    fun(_A) -> 
				    %%io:format("A ~p~n", [A]),
				    els_repl:start([]) 
			    end
		       },
		  "compile" => 
		      #{help=>"compile file",
			arguments =>
			    [#{name => outputdir,
			       long => "-outputdir",
			       short => $o,
			       help => "set output dir",
			       type => string},
			     #{name => file,
			       nargs => all,
			       type => string}],
			handler => fun(A) -> 
					   {done, compile(A)}
				   end},
		  "run" => 
		      #{help=>"run script file",
			arguments =>
			    [#{name => file,
			       nargs => all,
			       type => string}],
			required => false,
			handler => fun(A) ->
					   {done, run(A)}
				   end}
		 },
	    help => "Ccommand"
	   },
    Opt = #{progname => S, command =>[S, "help"]},
    OptH = #{progname => S, command =>[S]},
    Res = argparse:run(Args, Cmd, Opt),
    case Res of
	{done, N} ->
	    halt(N);
	{ok, #{'help' := true}, _, _} ->
	    io:format("~s~n", [argparse:help(Cmd, Opt)]),
	    halt(0);
	{ok, #{version := true}, _, _} ->
	    io:format("version~n", []),
	    halt(0);
	ok ->
	    io:format("~s~n", [argparse:help(Cmd, OptH)]),
	    halt(0);
	{error, Reason} ->
	    Er = argparse:format_error(Reason),
	    io:format("Error ~p~n~n", [Reason]),
	    io:format("Reason ~s~n~n", [Er]),
	    exit(1)
    end,
    io:format("res: ~p~n",[Res]).
 
    
