

Nonterminals

expressions sexpression elements
term hterm atom asymbol  lines.

Terminals
symbol module_function
integer float string variable underscore
'(' ')' ',' ',@' '\'' '#('.

Rootsymbol lines.
Endsymbol '$end'.

lines ->
    expressions : 
        '$1'.
expressions ->
    sexpression : 
        '$1'.
expressions ->
    expressions sexpression : 
        setline(erl_syntax:list('$1', ['$2']), '$2').
sexpression ->
    '(' ')' : nil.
sexpression ->
    '(' elements ')' : 
%        erl_syntax:list('$2').
        setline(erl_syntax:list('$2'), hd('$2')).

elements ->
    hterm : ['$1'].
elements ->
    elements term : lists:append('$1', ['$2']).


term ->
    atom : '$1'.
term ->
    asymbol : '$1'.
term ->
    string : setline(erl_syntax:string(tokenvalue('$1')), '$1').
term ->
    sexpression : '$1'.

hterm ->
    asymbol : '$1'.
hterm ->
    atom : '$1'.
hterm ->
    sexpression : '$1'.

asymbol ->
    module_function : setline(mf(tokenvalue('$1')), '$1').
asymbol ->
    symbol : 
        setline(erl_syntax:atom(tokenvalue('$1')), '$1').
asymbol ->
    variable : 
        setline(erl_syntax:variable(tokenvalue('$1')), '$1').
asymbol ->
    underscore : 
        setline(erl_syntax:underscore(), '$1').

atom ->
    integer :
        setline(erl_syntax:integer(tokenvalue('$1')), '$1').
atom ->
    float : 
        setline(erl_syntax:float(tokenvalue('$1')), '$1').
    
  

Erlang code.
-export([setline/2]).

-include_lib("scan.hrl").
tokenvalue(T) ->
    element(3, T).

setline(Tree, {_t, Line}) ->
    Pos = erl_anno:new(Line),
    erl_syntax:set_pos(Tree, Pos);
setline(Tree, {_t, Line, _v}) ->
    Pos = erl_anno:new(Line),
    erl_syntax:set_pos(Tree, Pos);
setline(Tree, {tree, _t, _a, _v} = L) ->
    erl_syntax:copy_pos(L, Tree).

mf(Text) ->
    [Module, Function] = string:split(Text, ":"),
    erl_syntax:module_qualifier(Module, Function).

