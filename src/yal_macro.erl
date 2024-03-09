-module(yal_macro).
-include_lib("els.hrl").
-export(['MACRO_backquote'/1]).
-export([bc_/1]).
-export([bc_item/2]).

-spec 'MACRO_backquote'(sexp()) -> sexp().
'MACRO_backquote'(E)  -> 
    %%io:format("bqquote L:~p~n", [E]),
    R = bc_([yal_util:make_symbol(backquote), E]),
    %%io:format("quote_ R: ~p~n", [R]),
    R.

%%% `basic -> 'basic (リストでもベクトルでもない任意の式)
bc_item([#item{value="backquote", loc=Loc}, Form], Env) when not is_list(Form)->
    transpile:form([ yal_util:make_symbol(quote, Loc), Form ], Env);
%%% `,form -> form (ただしformは@や.で始まらないかぎり)
bc_item([#item{value="backquote"}, [#item{value="unquote"}, [H|Form]]], Env)
  when  H#item.value =/= "dot", H#item.value =/="splice" ->
    transpile:form([H|Form], Env);
%%% `(a b c . atom) --> `(a b c (dot atom))--> (append a b c (quote atom))
%%% `(a b c . ,form) --> `(a b c (dot (unquote form)))--> (append a b c (quote atom))
bc_item([#item{value="backquote", loc=Loc}, Xn], Env) when is_list(Xn) ->
    %%io:format("bc_item=LIST <~p>~n", [Xn]),
    R = lists:map(fun 
                      %% . ,form -> form
                      ([#item{value="dot"}, [#item{value="unquote"}, F]])  ->
                          transpile:term(F, Env);
                      %% . atom -> quote atom 
                      ([#item{value="dot"}, #item{type=atom} = F])  ->
                          S = transpile:term([yal_util:make_symbol(quote, Loc), F], Env),
                          erl_syntax:set_pos(S, Loc);
                      %% ,@form -> form
                      ([#item{value="unquote_splice"}, F]) ->
                          M=transpile:term(F, Env),
                          M;
                      %% ,form -> (list form)
                      ([#item{value="unquote"}, F]) ->
                          S = erl_syntax:list([transpile:term(F, Env)]),
                          erl_syntax:set_pos(S, Loc);
                      %% form -> (list `form)
                      (F) ->
                          Lf = [bc_item([yal_util:make_symbol(backquote, Loc), F], Env)],
                          erl_syntax:set_pos(erl_syntax:list(Lf), Loc)
                  end, Xn),
    Args = erl_syntax:set_pos(erl_syntax:list(R), Loc),
    R2 = merl:qquote(Loc, "lists:append(_@r)", [{r, Args}]),
    R2.
    

%%% `basic -> 'basic (リストでもベクトルでもない任意の式)
bc_([#item{value="backquote", loc=Loc}, Form]) when not is_list(Form)->
    [ yal_util:make_symbol(quote, Loc), Form ];
%%% `,form -> form (ただしformは@や.で始まらないかぎり)
bc_([#item{value="backquote"}, [#item{value="unquote"}, [H|Form]]])
  when  H#item.value =/= "dot", H#item.value =/="splice" ->
    [H|Form];
%%% `(a b c . atom) --> `(a b c (dot atom))--> (append a b c (quote atom))
%%% `(a b c . ,form) --> `(a b c (dot (unquote form)))--> (append a b c (quote atom))
bc_([#item{value="backquote", loc=Loc}, Xn]) when is_list(Xn) ->
    %%io:format("bc_item=LIST <~p>~n", [Xn]),
    R = lists:map(fun 
                      %% . ,form -> form
                      ([#item{value="dot"}, [#item{value="unquote"}, F]])  ->
                          F;
                      %% . atom -> quote atom 
                      ([#item{value="dot"}, #item{type=atom} = F])  ->
                          [yal_util:make_symbol(quote, Loc), F];
                      %% ,@form -> form
                      ([#item{value="unquote_splice"}, F]) ->
                          F;
                      %% ,form -> (list form)
                      ([#item{value="unquote"}, F]) ->
                          [yal_util:make_symbol(list, Loc), F];
                      %% form -> (list `form)
                      (F) ->
                          [yal_util:make_symbol(list, Loc),
                           bc_([yal_util:make_symbol(backquote, Loc), F])]
                  end, Xn),
    R2 = [yal_util:make_symbol(list, Loc) | R],
%%    [#item{value={"lists", "append"}, type=module_function, loc=Loc}, R2].
    [#item{value="lists:append", type=module_function, loc=Loc}, R2].

%%    R2.
    

