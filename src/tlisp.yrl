

Nonterminals

expressions sexpression elements
term hterm atom asymbol  lines.

Terminals
symbol module_function
integer float string variable
'(' ')' ',' ',@' '\'' '#('.

Rootsymbol lines.
Endsymbol '$end'.

lines ->
    expressions : '$1'.
expressions ->
    sexpression : '$1'.
expressions ->
    expressions sexpression : erl_syntax:list('$1', ['$2']).
sexpression ->
    '(' ')' : nil.
sexpression ->
    '(' elements ')' : erl_syntax:list('$2').

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
    sexpression : '$1'.

asymbol ->
    module_function : setline(mf(tokenvalue('$1')), '$1').
asymbol ->
    symbol : 
        setline(erl_syntax:atom(tokenvalue('$1')), '$1').
asymbol ->
    variable : 
        setline(erl_syntax:variable(tokenvalue('$1')), '$1').

atom ->
    integer :
        io:format("integer: ~p~n", ['$1']),
        setline(erl_syntax:integer(tokenvalue('$1')), '$1').
atom ->
    float : 
        setline(erl_syntax:float(tokenvalue('$1')), '$1').
    
  

Erlang code.
-export([setline/2]).
-export([eval/1]).
-export([test/0]).

-include_lib("scan.hrl").
tokenvalue(T) ->
    element(3, T).

setline(Tree, {_t, Line}) ->
    Pos = erl_anno:new(Line),
    erl_syntax:set_pos(Tree, Pos);
setline(Tree, {_t, Line, _v}) ->
    Pos = erl_anno:new(Line),
    erl_syntax:set_pos(Tree, Pos).

mf(Text) ->
    [Module, Function] = string:split(Text, ":"),
    erl_syntax:module_qualifier(Module, Function).

eval(Tlist) ->
    case erl_syntax:type(Tlist) of
        nil ->
           [];
        _ ->
            io:format("eval: ~p~n", [Tlist]),
            case erl_syntax:type(Command = erl_syntax:list_head(Tlist)) of
                atom -> 
                    specialform(erl_syntax:atom_name(Command),
                                erl_syntax:list_tail(Tlist));
                list ->
                    R = eval(Command),
                    [R| eval(erl_syntax:list_tail(Tlist))];
                variable ->
                    Command;
                nil ->
                    []
            end
    end.
specialform("defun", Tlist) ->
    %% (name (a b) body)
    %% (name ((a b) (when list) body )
    %% (name ((match a) b) (when list) body ))
    Name = erl_syntax:list_head(Tlist),
    AB = erl_syntax:list_tail(Tlist),
    Args = erl_syntax:list_head(AB),
    case erl_syntax:type(erl_syntax:list_head(Args)) of
        variable ->
            Body = erl_syntax:list_tail(AB),
            
            BodyAst = lists:map(
                        fun(E) ->
                                eval(E)
                        end, erl_syntax:list_elements(Body)),
            Variables = erl_syntax:list_elements(Args),
            Clause = erl_syntax:clause(Variables, [], BodyAst),
            erl_syntax:function(Name, [Clause])
            
    end;
specialform("add", Tlist) ->
    %% (a b)
    %%
    [Head| Tail]= erl_syntax:list_elements(Tlist),
    Ret = erl_syntax:infix_expr(Head, erl_syntax:operator("+"), hd(Tail)),
    io:format("add = ~p~n", [Ret]),
    Ret
    .
-include_lib("syntax_tools/include/merl.hrl").
test() ->
    ?Q(["-module(m).",
        "-eport([f1/2]).",
        "f1(A, B) -> A + B."
        ]
      ).
