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
    lists:map(fun(E) ->
		      {ok, Module, Beam, Ast} = els_compile:file_ast(E,[], A),
		      io:format("compile <~s>~n", [ Module])
	      end, B),
    halt(0).
    
    
main(Args) ->
    S=escript:script_name(),
    ok = application:load(els),
    ok = application:ensure_started(els),
    VSN = application:get_all_key(els),
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
				    version(A, VSN),
				    halt(0)
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
			    fun(A) -> 
				    io:format("A ~p~n", [A]),
				    els_repl:start([]) 
			    end
		       },
		  "compile" => 
		      #{help=>"compile file",
			arguments =>
			    [#{name => outputdir,
			       long => "-outputdir",
			       help => "set output dir",
			       type => string},
			     #{name => file,
			       nargs => all,
			       type => string}],
			handler => fun(A) -> 
					   compile(A)
				   end},
		  "run" => 
		      #{help=>"run script file",
			arguments =>
			    [#{name => files,
			       nargs => all,
			       type => string}],
			required => false
		       }
		 },
	    help => "Ccommand"
	   },
    Opt = #{progname => S, command =>[S, "help"]},
    OptH = #{progname => S, command =>[S]},
    Res = argparse:run(Args, Cmd, Opt),
    case Res of
	{ok, #{'help' := true}, _, _} ->
	    io:format("~s~n", [argparse:help(Cmd, Opt)]),
	    halt(0);
	{ok, #{version := true}, _, _} ->
	    io:format("version~n", []),
	    halt(0);
	{ok, #{}, _, _} ->
	    Res;
	ok ->
	    io:format("~s~n", [argparse:help(Cmd, OptH)]),
	    halt(0);
	{error, Reason} ->
	    Er = argparse:format_error(Reason),
	    io:format("Error ~p~n~n", [Reason]),
	    io:format("Reason ~s~n~n", [Er]),
	    exit(1)
    end,
    io:format("res: ~p~n",[Res]),
    els_repl:start().
 
    
