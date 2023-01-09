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
%%-export([file/1]).
%%-export([read_balance/4]).
%%-export([read/3]).
%%-export([read/6]).
%%-export([reads/4]).
-export([from_string/2]).
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

    
calclevel(IO, Prompt0, Tokens, GLevel) ->
    do_calclevel(IO, Prompt0, Tokens, {[], GLevel}).

do_calclevel(_IO, _Prompt0, [], {Acc, PreLevel}) ->
    {Acc, PreLevel, []};
do_calclevel(IO, Prompt0, [{'(', _Loc} =T | Tokens], {Acc, PreLevel}) ->
    do_calclevel(IO, Prompt0, Tokens, {Acc ++ [T], PreLevel+1});
do_calclevel(_IO, _Prompt0, [{')', _Loc} =T | Tokens], {Acc, PrevLevel}) when PrevLevel =< 1 ->
    {Acc ++ [T], PrevLevel-1, Tokens};
do_calclevel(IO, Prompt0, [{')', _Loc} =T | Tokens], {Acc, PreLevel}) ->
    do_calclevel(IO, Prompt0, Tokens, {Acc ++ [T], PreLevel-1});
do_calclevel(IO, Prompt0, [{read_macro, Loc, MChar}], {Acc, PreLevel}) ->
    RM = #{quote => {scan, replace},
           backquote => {scan, replace},
           unquote => {scan, replace},
           unquote_splice => {scan, replace}
          },
    {MM, MF} = maps:get(MChar, RM, {scan, not_implemented}),
    {ok, NewTokens, _NewLoc, RestTokens} = apply(MM, MF, [{IO, Prompt0}, scan, read, 
                                                         Loc, MChar]),
    do_calclevel(IO, Prompt0, RestTokens, {Acc ++ NewTokens, PreLevel});
do_calclevel(IO, Prompt0, [{'\n', _Loc} | Tokens], {Acc, PreLevel}) ->
    do_calclevel(IO, Prompt0, Tokens, {Acc, PreLevel});
do_calclevel(_IO, _Prompt0, [T | Tokens], {Acc, 0}) ->
    {Acc++[T], 0, Tokens};
do_calclevel(IO, Prompt0, [T | Tokens], {Acc, PreLevel}) ->
    do_calclevel(IO, Prompt0, Tokens, {Acc ++ [T], PreLevel}).
loctoline({Line, _Col}) ->
    Line;
loctoline(Line) ->
    Line.
    
replace({IO, Prompt0}, _M, _F, Loc, MChar) ->
    Ret = read(IO, Prompt0, loctoline(Loc), [], 0),
    {ok, Tokens, NextLine, Rest} = Ret,
    NewTokens = [{'(', Loc}, 
                 {symbol, Loc, atom_to_list(MChar)} | 
                 Tokens ++ [{')', NextLine}]],
    {ok, NewTokens, NextLine, Rest}.

read(IO, Prompt0, Line, PrevTokens, PrevLevel) ->
    Prompt = io_lib:format("~B~s", [loctoline(Line), Prompt0]),
    case io:request(IO, {get_until, unicode, Prompt, scan, tokens, [Line]}) of
        {ok, NewTokens, NextLine} ->
            {NNewTokens, NLevel, Rest} = calclevel(IO, Prompt0, NewTokens, PrevLevel),
            Tokens = PrevTokens ++ NNewTokens,
            case {Rest, NLevel} of
                {[], NLevel} when NLevel > 0 -> 
                    read(IO, Prompt0, NextLine, Tokens, NLevel);
                {Rest, 0} ->
                    {ok, Tokens, NextLine, Rest};
                _  ->
                    {ok, Tokens, NextLine, Rest} 
            end;
        {eof, NextLine} ->
            {ok, PrevTokens, NextLine, []};
        Error ->
            io:format("Error! ~p~n",[{Error, PrevTokens, PrevLevel}]),
            Error
    end.
                    

from_string(String, Line) ->
    IO = tiny_io_server:start_link(String),
%    {ok, Acc, NewLine} = reads(IO, "> ", Line, ""),

    {ok, Acc , NewLine, _Rest} =  read(IO, "> ", Line, [], 0),
    NAcc = lists:filter(fun({'\n', _}) ->
                         false;
                    (_)  ->
                         true
                 end, Acc),
    {ok, NAcc, NewLine}.
