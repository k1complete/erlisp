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
    do_calclevel(IO, Prompt0, Tokens, {[], GLevel}, Line).

do_calclevel(_IO, _Prompt0, [], {Acc, PreLevel}, Line) ->
    {Acc, PreLevel, [], Line};
do_calclevel(IO, Prompt0, [{'(', _Loc} =T | Tokens], {Acc, PreLevel}, Line) ->
    do_calclevel(IO, Prompt0, Tokens, {Acc ++ [T], PreLevel+1}, Line);
do_calclevel(_IO, _Prompt0, [{')', _Loc} =T | Tokens], {Acc, PrevLevel}, Line) when PrevLevel =< 1 ->
    {Acc ++ [T], PrevLevel-1, Tokens, Line};
do_calclevel(IO, Prompt0, [{')', _Loc} =T | Tokens], {Acc, PreLevel}, Line) ->
    do_calclevel(IO, Prompt0, Tokens, {Acc ++ [T], PreLevel-1}, Line);
do_calclevel(IO, Prompt0, [{read_macro, Loc, MChar}], {Acc, PreLevel}, _Line) ->
    RM = #{quote => {scan, replace},
           backquote => {scan, replace},
           unquote => {scan, replace},
           unquote_splice => {scan, replace}
          },
    {MM, MF} = maps:get(MChar, RM, {scan, not_implemented}),
    {ok, NewTokens, NewLoc, RestTokens} = apply(MM, MF, [{IO, Prompt0}, scan, read, 
                                                         Loc, MChar]),
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
    io:format("replace-read ~p~n", [Ret]),
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

read(IO, Prompt0, Line, PrevTokens, PrevLevel) ->
    Prompt = make_prompt(Prompt0, Line, PrevTokens),
    case io:request(IO, {get_until, unicode, Prompt, scan, tokens, [Line]}) of
        {ok, NewTokens, NextLine} ->
            {NNewTokens, NLevel, Rest, NewLine} = calclevel(IO, Prompt0, NewTokens, PrevLevel, NextLine),
            Tokens = PrevTokens ++ NNewTokens,
            case {Rest, NLevel} of
                {[], NLevel} when NLevel > 0 -> 
                    read(IO, Prompt0, NewLine, Tokens, NLevel);
                {Rest, 0} ->
                    {ok, Tokens, NewLine, Rest};
                _  ->
                    io:format("readRet ~p~n", [{ok, Tokens, NewLine, Rest}]),
                    {ok, Tokens, NewLine, Rest} 
            end;
        {eof, NextLine} ->
            {ok, PrevTokens, NextLine, []};
        Error ->
            io:format("Error! ~p~n",[{Error, PrevTokens, PrevLevel}]),
            Error
    end.
from_string(String) ->
    from_string(String, 0).

from_string(String, Line) ->
    IO = tiny_io_server:start_link(String),
    {ok, Acc , NewLine, _Rest} =  read(IO, [], Line, [], 0),
    {ok, Acc, NewLine}.

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
    {ok, Acc} = reads(IO, File, 1, [], []).
