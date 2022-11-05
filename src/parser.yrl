
Nonterminals

expressions expression sexpression elements literal
term hterm atom asymbol  lines.

Terminals
symbol module_function
integer float string 
underscore
'(' ')' ',' ',@' '\'' '#('.
%% variable 
Rootsymbol lines.
Endsymbol '$end'.

lines ->
    expressions : 
        '$1'.
expressions ->
    expression : 
        '$1'.
expressions ->
    expressions expression : 
        lists:append('$1' , ['$2']).

expression ->
    hterm : 
        io:format("Exp0 ~p~n", ['$1']), 
        '$1'.
expression ->
    '\'' hterm : 
        A=[#item{value="quote", type=atom, loc=getpos('$2')}, '$2'],
        io:format("ExTerm ~p~n", [A]),
        A.
    
sexpression ->
    '(' ')' : nil.
sexpression ->
    '(' elements ')' : 
        io:format("Exp ~p~n", ['$2']),
        '$2'.

elements ->
    hterm : ['$1'].
elements ->
    elements term : lists:append('$1', ['$2']).

term ->
    asymbol : 
        io:format("Term ~p~n", ['$1']), '$1'.
term ->
    literal : 
        io:format("Term ~p~n", ['$1']), '$1'.
term ->
    string : 
        io:format("Term ~p~n", ['$1']),'$1'.
term ->
    sexpression : 
        io:format("Term ~p~n", ['$1']),'$1'.

hterm ->
    asymbol : 
        io:format("HTerm ~p~n", ['$1']), '$1'.
hterm ->
    literal : 
        io:format("HTerm ~p~n", ['$1']), '$1'.
hterm ->
    sexpression : 
        io:format("HTerm ~p~n", ['$1']), '$1'.


asymbol ->
    module_function : setline(mf(tokenvalue('$1')), '$1').
asymbol ->
    symbol : 
        setline(tokenvalue('$1'), '$1').
asymbol ->
    underscore : 
        '$1'.

literal ->
    integer :
        tokenvalue('$1').
literal ->
    float : 
        tokenvalue('$1').  

Erlang code.

-include_lib("erlisp.hrl").

-export([setline/2]).


tokenvalue(T) ->
    element(3, T).

set_pos(Tree, Pos) ->
    #item{value=Tree, loc=Pos, type=atom}.
setline({Module, Function}, {Type, Line, _Value}) ->
    Pos=erl_anno:new(Line),
    #item{value={Module, Function}, loc=Pos, type=Type};
setline(Tree, {_t, Line}) ->
    Pos = erl_anno:new(Line),
    set_pos(Tree, Pos);
setline(Tree, {_t, Line, _v}) ->
    Pos = erl_anno:new(Line),
    set_pos(Tree, Pos).
getpos(Tree) ->
    Tree#item.loc.

mf(Text) ->
    [Module, Function] = string:split(Text, ":"),
    {Module, Function}.

