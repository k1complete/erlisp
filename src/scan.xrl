% header

Definitions.

%macro definitions
Digits = [0-9]+
Alphabet = [A-Za-z_]|[\x{80}-\x{10fff}]
Griph=[-+*/]
PostAlphabet = ({Alphabet}|{Digits}|{Griph})
Symbols = [-+/*a-z]{PostAlphabet}*
Op = (\+\+|\-\-|==|/=|=<|<|>=|>|=:=|=/=|\+|-|\*|/)
Variables = [A-Z_]{PostAlphabet}*
WhiteSpace = [\s\t]+
QString = \"([^\"]|\\\")+\"
LineFeed = \n
Rules.
%tokenrules
{Digits} :
  {token, {integer, ?LC(TokenLine, TokenLen), list_to_integer(TokenChars)}}.
{Digits}\.{Digits}((E|e)(\+|\-)?{Digits})? :
  {token, {float, ?LC(TokenLine, TokenLen), list_to_float(TokenChars)}}.
{Variables} : 
  {token, {symbol, ?LC(TokenLine, TokenLen), TokenChars}}.
{Op} : 
  {token, {symbol, ?LC(TokenLine, TokenLen), TokenChars}}.
{Symbols} : 
  {token, {symbol, ?LC(TokenLine, TokenLen), TokenChars}}.
{Symbols}:{Symbols} : 
  {token, {module_function, ?LC(TokenLine, TokenLen), TokenChars}}.
\( :
  {token, {'(', ?LC(TokenLine, TokenLen)}}.
\) :
  {token, {')', ?LC(TokenLine, TokenLen)}}.
\. : 
  {token, {'.', ?LC(TokenLine, TokenLen)}}.
\,\@ :
  {end_token, {read_macro, ?LC(TokenLine, TokenLen), 'unquote_splice'}}.
\, :
  {end_token, {read_macro, ?LC(TokenLine, TokenLen), 'unquote'}}.
\' :
  {end_token, {read_macro, ?LC(TokenLine, TokenLen), 'quote'}}.
\` :
  {end_token, {read_macro, ?LC(TokenLine, TokenLen), 'backquote'}}.
\#\( :
  {token, {'#(', ?LC(TokenLine, TokenLen)}}.
{QString} :
  [_|String] = lists:droplast(TokenChars),
  {token, {string, ?LC(TokenLine, TokenLen), String}}.
{WhiteSpace} :
  col(TokenLen),
  skip_token.
{LineFeed} :
  col(reset),
  {end_token, {'\n', ?LC(TokenLine, TokenLen)}}.
  %%skip_token.

Erlang code.

-include_lib("scan.hrl").
-define(LC(TokenLine,TokenLen), {TokenLine, col(TokenLen)}).
%%-export([tokenizer/2]).
-export([file/2]).
%%-export([read_balance/4]).
%%-export([read/3]).
%%-export([read/6]).
-export([reads/5]).
-export([from_string/2]).
-export([from_string/1]).
%%-export([replace/5]).
-export([read/5]).
-export([replace/5]).
-export([remove_nl/1]).

col(reset) ->
    put(col, 1),
    1;
col(TokenLen) ->
    Col = case get(col) of
              undefined -> 
                  put(col, 1),
                  put(col, TokenLen),
                  1+TokenLen;
              V -> V + TokenLen
          end,
    put(col, Col).

    
calclevel(IO, Prompt0, Tokens, GLevel, Line) ->
%%    io:format("calclevel [~p]~n", [Tokens]),
    R=do_calclevel(IO, Prompt0, Tokens, {[], GLevel}, Line),
%%    io:format("calclevelout [~p]~n", [R]),
    R.

do_calclevel(_IO, _Prompt0, [], {Acc, PreLevel}, Line) ->
%%    io:format("calc-out ): ~p ~p ~n", [PreLevel, Acc]),
    {Acc, PreLevel, [], Line};
do_calclevel(IO, Prompt0, [{'(', _Loc} =T | Tokens], {Acc, PreLevel}, Line) ->
    do_calclevel(IO, Prompt0, Tokens, {Acc ++ [T], PreLevel+1}, Line);
do_calclevel(_IO, _Prompt0, [{')', _Loc} =T | Tokens], {Acc, PrevLevel}, Line) when PrevLevel =< 1 ->
    {Acc ++ [T], PrevLevel-1, Tokens, Line};
do_calclevel(IO, Prompt0, [{')', _Loc} =T | Tokens], {Acc, PreLevel}, Line) ->
%%    io:format("calc-apply ): ~p ~p ~p ~n", [PreLevel, Acc, Tokens]),
    do_calclevel(IO, Prompt0, Tokens, {Acc ++ [T], PreLevel-1}, Line);
do_calclevel(IO, Prompt0, [{read_macro, Loc, MChar}], {Acc, PreLevel}, _Line) ->
    RM = #{quote => {scan, replace},
           backquote => {scan, replace},
           unquote => {scan, replace},
           unquote_splice => {scan, replace}
          },
    {MM, MF} = maps:get(MChar, RM, {scan, not_implemented}),
    io:format("calc-apply before: ~p ~p ~n", [Loc, Acc]),
    {ok, NewTokens, NewLoc, RestTokens} = apply(MM, MF, [{IO, Prompt0}, scan, read, 
                                                         Loc, MChar]),
    io:format("calc-apply after: NT ~p Rest ~p PL ~p ~n", [NewTokens, RestTokens, PreLevel]),
    do_calclevel(IO, Prompt0, RestTokens, {Acc ++ NewTokens, PreLevel}, NewLoc);
do_calclevel(IO, Prompt0, [{'\n', _Loc} | Tokens], {Acc, PreLevel}, Line) ->
    do_calclevel(IO, Prompt0, Tokens, {Acc, PreLevel}, Line);
do_calclevel(_IO, _Prompt0, [T | Tokens], {Acc, 0}, Line) ->
    {Acc++[T], 0, Tokens, Line};
do_calclevel(IO, Prompt0, [T | Tokens], {Acc, PreLevel}, Line) ->
    do_calclevel(IO, Prompt0, Tokens, {Acc ++ [T], PreLevel}, Line).
loctoline({Line, _Col}) ->
    Line;
loctoline(Line) ->
    Line.
    
replace({IO, Prompt0}, _M, _F, Loc, MChar) ->
    %Ret = read(IO, Prompt0, loctoline(Loc), [], 0),
    Ret = read(IO, "", loctoline(Loc), [], 0),
    io:format("replace-2read ~p~n", [Ret]),
    {ok, Tokens, NextLine, Rest} = Ret,
    NewTokens = [{'(', Loc}, 
                 {symbol, Loc, atom_to_list(MChar)} | 
                 Tokens ++ [{')', Loc}]],
    {ok, NewTokens, NextLine, Rest}.

make_prompt([], _Line, _PrevTokens) ->
    "";
make_prompt(Prompt, Line, []) ->
    io_lib:format(Prompt, [loctoline(Line)]);
make_prompt(_Prompt, _Line, PrevTokens) ->
    "".

read(IO, Prompt0, Line, PrevTokens, PrevLevel) when length(PrevTokens) > 0 andalso PrevLevel == 0 ->
    io:format("CalcLevel PreVTokens  ~p ~n PrevLevel ~p~n", [PrevTokens, PrevLevel]),
    {NNewTokens, NLevel, Rest, NewLine} = calclevel(IO, Prompt0, PrevTokens, PrevLevel, Line),
    Tokens = NNewTokens,
    case {Rest, NLevel} of
        {[], NLevel} when NLevel > 0 -> 
%%            io:format("Readmore ~p ~p~n", [NLevel, Rest]),
            read(IO, Prompt0, NewLine, Tokens++Rest, NLevel);
        {Rest, 0} ->
            io:format("Token ~p Rest ~p~n", [Tokens, Rest]),
            {ok, Tokens, NewLine, Rest};
        _  ->
            io:format("readRet ~p~n", [{ok, Tokens, NewLine, Rest}]),
            {ok, Tokens, NewLine, Rest} 
    end;
read(IO, Prompt0, Line, PrevTokens, PrevLevel) ->
    Prompt = make_prompt(Prompt0, Line, PrevTokens),
    case io:request(IO, {get_until, unicode, Prompt, scan, tokens, [Line]}) of
        {ok, NewTokens, NextLine} ->
            io:format("TokenCalcd L ~p Prev ~p ~nNT ~p~n", 
                      [PrevLevel, 
                       PrevTokens, NewTokens]),
            io:format("precalc ~p ~n", 
                      [PrevTokens++ NewTokens]),
            {NNewTokens, NLevel, Rest, NewLine} = calclevel(IO, Prompt0, PrevTokens++NewTokens, 
                                                            PrevLevel, NextLine),
            io:format("TokenCalcd Prev ~p ~nNN ~p ~nNT ~p~n Rest ~p ~n", 
                      [PrevTokens, NNewTokens, NewTokens, Rest]),
       %     Tokens = PrevTokens ++ NNewTokens,
            Tokens = NNewTokens,
            case {Rest, NLevel} of
                {[], NLevel} when NLevel > 0 -> 
                    io:format("Readmore ~p ~nToken ~p Rest ~p~n", [NLevel, Tokens, Rest]),
                    read(IO, Prompt0, NewLine, Tokens++Rest, NLevel);
                {Rest, NLevel} when NLevel =< 0 ->
                    io:format("Token ~p Rest ~p~n", [Tokens, Rest]),
                    {ok, Tokens, NewLine, Rest};
                _  ->
                    io:format("readRet ~p~n", [{ok, Tokens, NewLine, Rest}]),
                    {ok, Tokens, NewLine, Rest} 
            end;
        {eof, NextLine} ->
            
%%            {ok, NNewTokens, NextLine, []};
            io:format("PrevTokens ~p~n", [PrevTokens]),
            
            {ok, PrevTokens, NextLine, []};
        Error ->
            io:format("Error! ~p~n",[{Error, PrevTokens, PrevLevel}]),
            Error
    end.
from_string(String) ->
    from_string(String, 0).

from_string_rest(IO, Line, Rest, Acc) ->
    case read(IO, [], Line, Rest, 0) of
        {ok, Acc2, NewLine, []} ->
            io:format("from_string_rest ~p -> ~n ~p~n", [Rest, Acc2]),
            {ok, Acc ++ Acc2, NewLine};
        {ok, Acc2, NewLine, Rest2} ->
            from_string_rest(IO, Line, Rest2, Acc++Acc2);
        _ ->
            {ok, Acc, Line}
    end.

remove_nl(Tokens) ->    
    lists:filter(fun({'\n', _}) -> 
                         false;
                    (_) -> true 
                 end, Tokens).

from_string(String, Line) ->
    io:format(standard_error, "~p~n", [length(erlang:processes())]),
    IO = tiny_io_server:start_link(String),
    {ok, Acc , NewLine} = from_string_rest(IO, Line, [], []),
    tiny_io_server:stop(IO),
    io:format(standard_error, "~p~n", [length(erlang:processes())]),
    {ok, remove_nl(Acc), NewLine}.
            

reads(IO, File, Line, PrevTokens, Acc) ->
    case read(IO, [], Line, PrevTokens, 0) of
        {ok, [], _, RestTokens}  ->
            io:format("ReadsRET: ~p~n", [{Acc, RestTokens}]),
            {ok, Acc++RestTokens};
        {ok, Tokens, NextLine, RestTokens} ->
            io:format("Reads: ~p~n", [Tokens]),
            R = reads(IO, File, NextLine+1, RestTokens, Acc ++ Tokens),
            {ok, element(2, R)}
    end.
file(File, _Option) ->
    {ok, IO} = file:open(File, "r"),
    {ok, Acc} = reads(IO, File, 1, [], []),
    file:close(IO),
    {ok, Acc}.
