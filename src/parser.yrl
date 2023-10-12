
Nonterminals

expressions expression sexpression elements literal
term asymbol  lines.

Terminals
symbol module_function
integer float string 
underscore
'(' ')' 
'\'' 
'\.'.
%% ',' 
%%'#(' 
%% ',@' 
%% variable 
Rootsymbol lines.
Endsymbol '$end'.

lines ->
    expressions : 
        '$1'.
expressions ->
    expression : 
        %% io:format("ExpOK ~p~n", ['$1']),
        ['$1'].
expressions ->
    expressions expression : 
        lists:append('$1', ['$2']).

expression ->
    term : 
        %% io:format("Exp0 ~p~n", ['$1']), 
        '$1'.

sexpression ->
    '(' ')' : nil.
sexpression ->
    '(' elements ')' : 
        %% io:format("SexpFromElem ~p~n", ['$2']),
        '$2'.
sexpression ->
    '(' elements '\.' term ')' : 
        %% io:format("SexpFromElem ~p~n", ['$2']),
        lists:append('$2' , [[#item{value="dot", loc=element(2, '$3'),
                                    type=atom}, '$4']]).

elements ->
    term : ['$1'].
elements ->
    elements term : lists:append('$1', ['$2']).

term ->
    asymbol : 
        %%io:format("Term ~p~n", ['$1']), 
        '$1'.
term ->
    literal : 
        %% io:format("Term ~p~n", ['$1']), 
        '$1'.
term ->
    string : 
        %%io:format("Term ~p~n", ['$1']),
        #item{type=string, value=tokenvalue('$1'), loc=element(2, '$1')}.
term ->
    sexpression : 
        %%io:format("TermFromSexp ~p~n", ['$1']),
        '$1'.
term ->
    '\'' term : 
        A=[setline("quote", '$1'), '$2'],
        io:format("Term Quote ~p~n", [A]),
        A.


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
    #item{value=Module ++ ":" ++ Function, loc=Pos, type=atom};
%    #item{value={Module, Function}, loc=Pos, type=Type};
setline(Tree, {_t, Line}) ->
    Pos = erl_anno:new(Line),
    set_pos(Tree, Pos);
setline(Tree, {_t, Line, _v}) ->
    Pos = erl_anno:new(Line),
    set_pos(Tree, Pos).

mf(Text) ->
    [Module, Function] = string:split(Text, ":"),
    {Module, Function}.

