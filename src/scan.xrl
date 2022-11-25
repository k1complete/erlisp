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
\,\@ :
  {token, {',@', ?LC(TokenLine, TokenLen)}}.
\, :
  {token, {',', ?LC(TokenLine, TokenLen)}}.
\' :
  {token, {read_macro, ?LC(TokenLine, TokenLen), 'quote'}}.
\#\( :
  {token, {'#(', ?LC(TokenLine, TokenLen)}}.
{QString} :
  [_|String] = lists:droplast(TokenChars),
  {token, {string, ?LC(TokenLine, TokenLen), String}}.
{WhiteSpace} : 
%  io:format(standard_error, "PreLoc: ~p, ~p~n", [get(col), TokenLen]),
  col(TokenLen),
%  io:format(standard_error, "TokenPos: ~p~n", [get(col)]),
  skip_token.
{LineFeed} :
  col(reset),
%  io:format(standard_error, "LineFeed: ~p~n", [get(col)]),
  skip_token.

Erlang code.

-include_lib("scan.hrl").
-define(LC(TokenLine,TokenLen), {TokenLine, col(TokenLen)}).
-export([tokenizer/2]).
-export([file/1]).
-export([ftokens/4]).
-export([read_balance/4]).
-export([read/3]).
-export([read/6]).
-export([quote/4]).
-export([from_string/2]).

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

ftokens({IO, Prompt}, Cont, Data, Arg) ->
    [H|T]= Data,
    tokens(Cont, Data, Arg).
%    {done, {token, {symbol, {1, 1}, {Cont, Data, Arg}}}, T}.
% ストリームを読み、トークンをバランス差せた状態まで読んでから返す。
% バランスは、"/", (/), [/], {/}の個数とする。
% バランスが壊れている場合はエラーとする。
read_balance({IO, Prompt}, {Cont, Level}, M, F, A) ->
    case io:request(IO, {get_until, unicode, Prompt, M, F, [A]}) of
        {ok, Token, EndLoc} ->
            case Token of
                {'(', _} ->
                    %io:format(standard_error, "L ~p, ~p~n", [Cont, Level]),
                    read(IO, Prompt, {[Token|Cont], Level+1}, M, F, A);
                {')', _} when Level > 1 ->
                    %io:format(standard_error, "L ~p, ~p~n", [Cont, Level]),
                    read(IO, Prompt, {[Token|Cont], Level-1}, M, F, A);
                {')', _} ->
                    %io:format(standard_error, "L ~p, ~p~n", [Cont, Level]),
                    {ok, lists:reverse([Token|Cont]), EndLoc};
                _ when Level == 0 ->
                    {ok, lists:reverse([Token|Cont]), EndLoc};
                _ ->
                    read(IO, Prompt, {[Token|Cont], Level}, M, F, A)
            end;
        {eof, EndLoc} ->
            %io:format(standard_error, "eof~n", []),
            {eof, EndLoc}
    end.
read_balance({IO, Prompt}, M, F, A) ->
    read_balance({IO, Prompt}, {[], 0}, M, F, A).

read(IO, Prompt, {[{read_macro, Loc, Value}|Cont], Level}, M, F, A) ->
    {ok, Tokens, EndLoc} = read_macro({IO, Prompt}, M, F, A, Value),
    read(IO, Prompt, {lists:reverse(Tokens) ++ Cont, Level}, M, F, EndLoc);
read(IO, Prompt, {Cont, Level}, M, F, A) ->
    case read_balance({IO, Prompt}, {Cont, Level}, scan, token, A) of
        {ok, [{read_macro, Loc, Value}], EndLoc} ->
            read_macro({IO, Prompt}, M, F, A, Value);
        {ok, Tokens, EndLoc} ->
            %io:format(standard_error, "balanced ~p ~n", [Tokens]),
            {ok, Tokens, EndLoc};
        Error ->
            Error
    end.
read(IO, Prompt, A) ->
    read(IO, Prompt, {[], 0}, scan, token, A).

read_macro({IO, Prompt}, M, F, A, Value) ->
    RM = #{quote => {scan, quote},
           backquote => {scan, backquote}},
    {MM, MF} = maps:get(Value, RM, {scan, not_implemented}),
    %io:format(standard_error, "RM ~p~n", [MF]),
    apply(MM, MF, [{IO, Prompt}, M, F, A]).

quote({IO, Prompt}, M, F, A) ->
    %io:format(standard_error, "E ~p~n", [M]),
    {ok, Tokens, EndLoc} = read(IO, Prompt, A),
    %io:format(standard_error, "E ~p~n", [Tokens]),
    NewTokens = [{'(', A},
                 {symbol, A, "quote"} |
                 Tokens] ++ [{')', EndLoc}],
    {ok, NewTokens, EndLoc}.
    
reads(IO, Prompt, Line, Acc) ->
    case read(IO, Prompt, Line) of
        {ok, Result, NewLine} ->
            reads(IO, Prompt, NewLine, Acc++Result);
        {eof, NewLine} ->
            {ok, Acc, NewLine}
    end.
from_string(String, Line) ->
    IO = tiny_io_server:start_link(String),
    reads(IO, "> ", Line, "").
