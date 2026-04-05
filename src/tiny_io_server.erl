-module(tiny_io_server).
-export([start_link/1, init/1, loop/1, until_newline/3, until_enough/3, stop/1]).

-define(CHARS_PER_REC, 10).

-record(state, {
                fd,
                lookahead = [],
                mode % binary | list
               }).

start_link(String) ->
    spawn_link(?MODULE,init,[String]).

-spec init(string()) -> no_return().
init(String) ->
    {ok, Fd} = file:open(unicode:characters_to_binary(String), [read, ram]),
    %% io:format("Inited: <~p>~n", [String]),
    ?MODULE:loop(#state{fd = Fd, mode=list}).

stop(Pid) ->
    unlink(Pid),
    Pid ! stop.

loop(State) ->
    receive
	{io_request, From, ReplyAs, Request} ->
	    case request(Request,State) of
		{ok, {eof, A}, _NewState} ->
		    {io_reply,_, _} = reply(From, ReplyAs, {eof, A}),
		    exit(normal);
		{Tag, Reply, NewState} when Tag =:= ok; Tag =:= error ->
		    {io_reply,_, _} = reply(From, ReplyAs, Reply),
		    ?MODULE:loop(NewState)
		%% ;
		%% {stop, Reply, _NewState} ->
		%%    reply(From, ReplyAs, Reply),
		%%    exit(Reply)
	    end;
	stop ->
            %%io:format(standard_error, "terminated!!!!", []),
            ok = file:close(State#state.fd),
            exit(normal);
	%% Private message
	{From, rewind} ->
	    From ! {self(), ok},
            ram_file:position(State#state.fd, 0);
	_Unknown ->
            io:format(standard_error, "Recieved!!!! ~p~n", [_Unknown]),
	    ?MODULE:loop(State)
    end.

reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

request({put_chars, Encoding, Chars}, State) ->
    put_chars(unicode:characters_to_list(Chars,Encoding),State);
request({put_chars, Encoding, Module, Function, Args}, State) ->
    try
	request({put_chars, Encoding, apply(Module, Function, Args)}, State)
    catch
	_:_ ->
	    {error, {error,Function}, State}
    end;
request({get_until, Encoding, _Prompt, M, F, As}, State) ->
    get_until(Encoding, M, F, As, State);
request({get_chars, Encoding, _Prompt, N}, State) ->
    %% To simplify the code, get_chars is implemented using get_until
    get_until(Encoding, ?MODULE, until_enough, [N], State);
request({get_line, Encoding, _Prompt}, State) ->
    %% To simplify the code, get_line is implemented using get_until
    get_until(Encoding, ?MODULE, until_newline, [$\n], State);
request({get_geometry,_}, State) ->
    {error, {error,enotsup}, State};
request({setopts, Opts}, State) ->
    setopts(Opts, State);
request(getopts, State) ->
    getopts(State);
request({requests, Reqs}, State) ->
     multi_request(Reqs, {ok, ok, State});
request(_Other, State) ->
    {error, {error, request}, State}.

multi_request([R|Rs], {ok, _Res, State}) ->
    multi_request(Rs, request(R, State));
multi_request([_|_], Error) ->
    Error;
multi_request([], Result) ->
    Result.

%shutdown(State) ->
%    file:close(State#state.fd).

setopts(Opts0,State) ->
    Opts = proplists:unfold(
	     proplists:substitute_negations(
	       [{list,binary}], 
	       Opts0)),
    case check_valid_opts(Opts) of
	true ->
	        case proplists:get_value(binary, Opts) of
		    true ->
			{ok,ok,State#state{mode=binary}};
		    false ->
			{ok,ok,State#state{mode=binary}};
		    _ ->
			{ok,ok,State}
		end;
	false ->
	    {error,{error,enotsup},State}
    end.
check_valid_opts([]) ->
    true;
check_valid_opts([{binary,Bool}|T]) when is_boolean(Bool) ->
    check_valid_opts(T);
check_valid_opts(_) ->
    false.

getopts(#state{mode=M} = S) ->
    {ok,[{binary, case M of
		      binary ->
			  true;
		      _ ->
			  false
		  end}],S}.

put_chars(Chars, #state{fd = T} = State) ->
    ok = ram_file:write(T, Chars),
    {ok, ok, State}.


get_until(Encoding, Mod, Func, As, 
	  #state{mode = M, fd = T, lookahead = LookAhead} = State) ->
    case get_loop(Mod,Func,As,T, [], LookAhead) of
	{done,Data, RestChars} when is_binary(Data); is_list(Data) ->
	    if M =:= binary -> 
		    {ok, 
		     unicode:characters_to_binary(Data, unicode, Encoding),
                     State#state{lookahead=RestChars}};
               true ->
		    case check(Encoding, 
		               unicode:characters_to_list(Data, unicode))
                    of
			{error, _} = E ->
			    {error, E, State};
			List ->
			    {ok, List, State#state{lookahead=RestChars}}
		    end
	    end;
	{done,Data, RestChars} ->
            %%io:format(standard_error, "done-- ~p/~p~n", [Data, RestChars]),
	    {ok, Data, State#state{lookahead=RestChars}};
        {error, {eof, Loc}} ->
            {ok, {eof, Loc}, State};
	Error ->
            %% io:format("getloop: ~p~n ", [Error]),
	    {error, Error, State}
    end.

get_loop(M,F,A,T,C, LookAhead) ->
    case getc(T, LookAhead) of
        {ok, L} ->
            case catch apply(M,F,[C,L|A]) of
                {done, List, Rest} ->
                    %% io:format(standard_error, "done ~p~n", [List]),
                    {done, List, Rest};
                {more, NewC} ->
                    %% io:format(standard_error, "more ~p~n", [NewC]),
                    get_loop(M,F,A, T,NewC, []);
                Errors ->
                    io:format(standard_error, "io_server error ~p~n", [Errors]),
                    {error,F}
            end;
        eof ->
            case catch apply(M,F,[C,eof|A]) of
                {done, List, Rest} ->
                    {done, List, Rest};
                _Errors ->
                    % io:format(standard_error, "error ~p~n", [Errors]),
                    {error, {eof, hd(A)}}
            end
    end.

check(unicode, List) ->
    List;
check(latin1, List) ->
    try 
	[ throw(not_unicode) || X <- List,
				X > 255 ],
	List
    catch
	throw:_ ->
	    {error,{cannot_convert, unicode, latin1}}
    end.


until_newline([],eof,_MyStopCharacter) ->
    {done,eof,[]};
until_newline(ThisFar,eof,_MyStopCharacter) ->
    {done,ThisFar,[]};
until_newline(ThisFar,CharList,MyStopCharacter) ->
    case
        lists:splitwith(fun(X) -> X =/= MyStopCharacter end,  CharList)
    of
	{L,[]} ->
            {more,ThisFar++L};
	{L2,[MyStopCharacter|Rest]} ->
	    {done,ThisFar++L2++[MyStopCharacter],Rest}
    end.

until_enough([],eof,_N) ->
    {done,eof,[]};
until_enough(ThisFar,eof,_N) ->
    {done,ThisFar,[]};
until_enough(ThisFar,CharList,N) 
  when length(ThisFar) + length(CharList) >= N ->
    {Res,Rest} = my_split(N,ThisFar ++ CharList, []),
    {done,Res,Rest};
until_enough(ThisFar,CharList,_N) ->
    {more,ThisFar++CharList}.

utfcheck(Bs) ->
    if 
	(Bs band 2#10000000) == 2#00000000 -> %% single bytes
	    0;
	(Bs band 2#1100000) == 2#10000000 -> %% follow bytes
	    0;
	(Bs band 2#11100000) == 2#11000000 -> %% first byte
	    if Bs >= 16#C2 andalso Bs =< 16#DF ->
		    1;
	       true ->
		    0
	    end;
	(Bs band 2#11110000) == 2#11100000 ->
	    2;
	(Bs band 2#11111000) == 2#11110000 ->
	    3;
	true ->
	    0
    end.

-spec getc(io:device(), list()) -> {ok, list()} | eof.
getc(Fd, []) ->
    case ram_file:read(Fd, 1) of
        eof ->
            eof;
	{ok, [Rest]} ->
	    {ok, D2} = case utfcheck(Rest) of 
			   0 ->
			       {ok, []};
			   X ->
			       ram_file:read(Fd, X)
		       end,
	    Ret = unicode:characters_to_list(list_to_binary([Rest|D2])),
	    {ok, Ret}
    end;
getc(_Fd, LookAhead) ->
    %% io:format("[getc ahead '~p']~n", [LookAhead]),
    {ok, LookAhead}.

-spec my_split(integer(), list(), list()) -> {list(), list()}.
my_split(0,Left,Acc) ->
    {lists:reverse(Acc),Left};
my_split(_,[],Acc) ->
    {lists:reverse(Acc),[]};
my_split(N,[H|T],Acc) ->
    my_split(N-1,T,[H|Acc]).

