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
  {token, {variable, ?LC(TokenLine, TokenLen), list_to_atom(TokenChars)}}.
{Op} : 
  {token, {symbol, ?LC(TokenLine, TokenLen), list_to_atom(TokenChars)}}.
{Symbols} : 
  {token, {symbol, ?LC(TokenLine, TokenLen), list_to_atom(TokenChars)}}.
{Symbols}:{Symbols} : 
  {token, {module_function, ?LC(TokenLine, TokenLen), list_to_atom(TokenChars)}}.
\( :
  {token, {'(', ?LC(TokenLine, TokenLen)}}.
\) :
  {token, {')', ?LC(TokenLine, TokenLen)}}.
\,\@ :
  {token, {',@', ?LC(TokenLine, TokenLen)}}.
\, :
  {token, {',', ?LC(TokenLine, TokenLen)}}.
\' :
  {token, {'\'', ?LC(TokenLine, TokenLen)}}.
\#\( :
  {token, {'#(', ?LC(TokenLine, TokenLen)}}.
  
{QString} :
  [_|String] = lists:droplast(TokenChars),
  {token, {string, ?LC(TokenLine, TokenLen), String}}.
{WhiteSpace} : 
  col(TokenLine),
  skip_token.
{LineFeed} :
  col(reset),
  skip_token.

Erlang code.

-include_lib("scan.hrl").
-define(LC(TokenLine,TokenLen), {TokenLine, col(TokenLen)}).
-export([tokenizer/2]).
-export([file/1]).

col(reset) ->
    put(col, 1),
    1;
col(TokenLen) ->
    Col = case get(col) of
              undefined -> 
                  put(col, 1),
                  TokenLen;
              V -> V + TokenLen
          end,
    put(col, Col).

tokenizer(F, Loc) ->
    io:request(F, {get_until, unicode, ">", scan, token, [Loc]}).
file(F) ->
    file(F, 1, []).
file(F, Loc, TokenList) ->
    case io:request(F, {get_until, unicode, ">", scan, tokens, [Loc]}) of
        {ok, Tokens, End} ->
            file(F, End, TokenList ++ Tokens);
        {eof, End} ->
            {ok, TokenList ++ [{'$end', {End+1,1}}]}
    end.

    
    
