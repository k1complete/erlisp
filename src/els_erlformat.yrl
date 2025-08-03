
Nonterminals

term list map tuple termlist assoclist assoc
literal.

Terminals

string
objref
variable
integer
float
atom
'{' '}'
'[' ']'
'|' 
'#'
'=>'
'<'
'>'
'.'
'-'
'...'
','.

Rootsymbol term.
Endsymbol '$end'.

term ->
    list :
       '$1'.
term ->
    map :
       '$1'.
term ->
    tuple :
       '$1'.
term ->
    literal :
	'$1'.
term ->
    objref :
	makeitem('$1', objref).
term ->
    variable :
	makeitem('$1', atom).
term ->
    '...' : symbol("...").


list ->
    '[' termlist ']' : '$2' .
list ->
    '[' termlist '|' term ']' : lists:append('$2','$4').

termlist ->
    term : ['$1'].
termlist ->
    term ',' termlist : ['$1' | '$3' ].

map ->
    '#' '{' assoclist '}' : [symbol("map") | '$3' ].

assoclist ->
    assoc : ['$1'].
assoclist ->
    assoc ',' assoclist  : ['$1' | '$3'].

assoc ->
    term '=>' term : [symbol("=>"), '$1', '$3'].
assoc ->
    '...' : symbol("...").

tuple ->
    '{' termlist '}' : [symbol("tuple") | '$2'].

literal ->
    string : makeitem('$1', string).
literal ->
    integer : makeitem('$1', integer).
literal ->
    float : makeitem('$1', float).
literal ->
    atom : makeitem('$1', atom).
literal ->
    '-' float : 
	A=makeitem('$2', float),
        A#item{value= -A#item.value}.
literal ->
    '-' integer : 
	A=makeitem('$2', integer),
        A#item{value= -A#item.value}.

literal ->
    '<' float '.' integer '>' : 
	[symbol("pid"),
	 makeitem('$2', float),
	 makeitem('$4', integer)].

    


Erlang code.

-include_lib("els.hrl").

-spec tokenvalue(tuple()) -> string().
tokenvalue(T) ->
    case element(3, T) of
	X when is_atom(X) ->
	    atom_to_list(X);
	Y ->
	    Y
    end.
symbol(V) ->
    #item{type=atom, value=V}.

makeitem(T, Type) ->
    #item{type=Type, value=tokenvalue(T)}.

