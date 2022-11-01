
Nonterminals

expressions sexpression elements literal
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
    sexpression : 
        '$1'.
expressions ->
    expressions sexpression : 
        lists:append('$1' , ['$2']).
sexpression ->
    '(' ')' : nil.
sexpression ->
    '(' elements ')' :
        '$2'.
%        erl_syntax:list('$2').
%        setline(erl_syntax:list('$2'), hd('$2')).

elements ->
    hterm : ['$1'].
elements ->
    elements term : lists:append('$1', ['$2']).


term ->
    literal : '$1'.
term ->
    asymbol : '$1'.
term ->
    string : '$1'.
term ->
    sexpression : '$1'.

hterm ->
    asymbol : '$1'.
hterm ->
    literal : '$1'.
hterm ->
    sexpression : '$1'.

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

mf(Text) ->
    [Module, Function] = string:split(Text, ":"),
    {Module, Function}.

