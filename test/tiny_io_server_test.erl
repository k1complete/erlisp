-module(tiny_io_server_test).

-include_lib("eunit/include/eunit.hrl").

setupX(X) ->
    tiny_io_server:start_link(X).
instantiator() ->
    fun(R) ->
            {ok, {'(', {1,1}}, 1} = 
                io:request(R, {get_until, unicode, ">", scan, token, [1]})
    end.

io_test() ->
    {setup, 
     fun(X) -> setupX(X) end, 
     fun() -> instantiator() end}.

io2_test() ->
    tiny_io_server:start_link("(a b c)").


