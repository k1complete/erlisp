-module(els_item).
-include("els.hrl").
-export([from_erl/1]).
-export([from_erl/2]).

sexp_to_list(List, F) ->
    F(List).

from_erl(E) ->
    from_erl(E, fun(M) -> M end).

from_erl([H|T], F) when not is_list(T) ->
    [from_erl(H, F) | from_erl(T, F)];
from_erl(T, F) when is_list(T) ->
    S = try lists:all(fun(E) when is_integer(E) andalso 
                                  E =< 1114111 andalso 
                                  E >= 10 -> 
                              true;
                         (_)  -> 
                              false
                      end, T)
    catch _ ->
            false
    end,
    case S of
        true ->
            F(#item{type=string, value=io_lib:format("~s", [T])});
        false  ->
            lists:map(fun(E) ->
                              from_erl(E, F)
                      end, T)
    end;
from_erl({call, _, Func, Args}, F) ->
    Function = from_erl(Func, F),
    ArgList = lists:map(fun(E) ->from_erl(E, F) end, Args),
    R = [Function | ArgList],
    R;
from_erl({remote, L, M, FName}, F) ->
    Mod = from_erl(M),
    Fun = from_erl(FName),
    MF = Mod#item.value ++ ":" ++Fun#item.value,
    F(#item{type=atom, loc=L,  value= MF});
from_erl({cons, _L, H, T}, F) ->
    Head = from_erl(H, F),
    Tail = from_erl(T, F),
    sexp_to_list([Head|Tail], F);
from_erl({atom, L, V}, F) ->
    F(#item{type=atom, loc=L, value=atom_to_list(V)});
from_erl({integer, L, V}, F) ->
    F(#item{type=integer, loc=L, value=V});
from_erl({nil, _L}, F) ->
    F([]);
from_erl({var, _, Arg}, F) ->
    F(#item{value=atom_to_list(Arg), type=atom});
from_erl({ann_type, _, [Name| Args]}, F) ->
    sexp_to_list(lists:map(fun(E) ->
				   from_erl(E, F)
			   end, [Name, {atom,0, '::'} |Args]), F);
from_erl({type, _, 'product', Args}, F) ->
    ArgsM = [ hd(Args), {atom, 0, '::'}|tl(Args)],
    sexp_to_list(lists:map(fun(E) ->
				   from_erl(E, F)
			   end, ArgsM), F);
from_erl({type, _, 'bounded_fun', [Ft, Fc]}, F) ->
    Constraint = lists:map(fun(E) -> from_erl(E, F) end, Fc),
    Ftype = from_erl(Ft, F),
    sexp_to_list([Ftype,[from_erl('when') | Constraint]], F);
from_erl({type, _, 'bounded_fun', Args}, F) ->
    ArgsM = [ hd(Args), {atom, 0, '::'}|tl(Args)],
    sexp_to_list(lists:map(fun(E) ->
				  from_erl(E, F)
			   end, ArgsM), F);
from_erl({type, _, 'constraint', [{atom, _, 'is_subtype'}, [V, T]]}, F) ->
    M = case is_list(T) of 
	    true ->
		io:format("LIST ~p~n", [T]),
		lists:map(fun(E) -> from_erl(E, F) end, T);
	    false ->
		from_erl(T, F)
	end,
    sexp_to_list([from_erl(V), from_erl('::'), M], F);
from_erl({type, _, 'fun', [{type, _, product, Args}, Ret]}, F) ->
    %% io:format("FUNPRO: ~p~n", [Args]),
    Return = from_erl(Ret, F),
    A = lists:map(fun(E) ->
                          from_erl(E, F)
                  end, Args),
    %% io:format("FUNRET: ~p~n", [[A, Return]]),
    sexp_to_list([A, Return], F);
from_erl({type, _, 'fun', Args}, F) ->
    io:format("FUN: ~p~n", [Args]),
    [A, Return] = lists:map(fun(E) ->
				    from_erl(E, F)
			    end, Args),
    io:format("FUNRET: ~p~n", [[A, Return]]),
    sexp_to_list([A, Return], F);
from_erl({type, _, 'union', List}, F) when is_list(List) ->
    [from_erl('|', F) | lists:map(fun(E) ->  from_erl(E, F) end, List)];
from_erl({type, _, 'tuple', any}, F) ->
    [from_erl('tuple', F), [F('any')]];
from_erl({type, _, ContainerType, Args}, F) when is_list(Args) ->
    ArgsM = lists:map(fun(E) -> from_erl(E, F) end, Args),
    [from_erl(ContainerType, F) | ArgsM];
from_erl({type, _, UserType, Args}, F) when is_atom(UserType), is_list(Args)  ->
    ArgsM = lists:map(fun(E) -> from_erl(E, F) end, Args),
    %%[F(ContainerType) | ArgsM];
    %%F([F(UserType)]);
    [from_erl(UserType, F) | ArgsM ];
from_erl(T, F) when is_tuple(T) ->
    TList = lists:map(fun(E) -> 
                              from_erl(E, F) 
                      end, tuple_to_list(T)),
    [from_erl('tuple', F) | TList];
from_erl(T, F) when is_binary(T) ->
    TList = binary_to_list(T),
    [from_erl('binary', F) | TList];
from_erl(T, _F) when is_integer(T) ->
    T;
from_erl(T, _F) when is_float(T) ->
    T;
from_erl(T, F) when is_pid(T) ->
    [from_erl('pid', F), F(#item{type=string, value=pid_to_list(T)})];
from_erl(T, F) when is_map(T) ->
    TList = lists:foldl(
              fun({K, V}, A) ->
                      A++[from_erl(K, F), from_erl(V, F)]
              end, [], maps:to_list(T)),
    [from_erl('map', F) | TList] ;
from_erl(T, F) when is_function(T) ->
    F(#item{type=string, value=erlang:fun_to_list(T)});
from_erl(T, F) when is_atom(T) ->
    %%F(#item{type=atom, value=lists:flatten(io_lib:format("~p", [T]))}).
    F(#item{type=atom, value=atom_to_list(T)}).
