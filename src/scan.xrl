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
%  io:format(standard_error, "PreLoc: ~p, ~p~n", [get(col), TokenLen]),
  col(TokenLen),
%  io:format(standard_error, "TokenPos: ~p~n", [get(col)]),
  skip_token.
{LineFeed} :
  col(reset),
%  io:format(standard_error, "LineFeed: ~p~n", [get(col)]),
  {end_token, {'\n', ?LC(TokenLine, TokenLen)}}.
  %%skip_token.

Erlang code.

-include_lib("scan.hrl").
-define(LC(TokenLine,TokenLen), {TokenLine, col(TokenLen)}).
-export([tokenizer/2]).
-export([file/1]).
-export([read_balance/4]).
-export([read/3]).
-export([read/6]).
-export([reads/4]).
-export([from_string/2]).
-export([replace/5]).
-export([read2/5]).
-export([replace2/5]).

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

% ストリームを読み、トークンをバランス差せた状態まで読んでから返す。
% バランスは、"/", (/), [/], {/}の個数とする。
% バランスが壊れている場合はエラーとする。
read_balance({IO, Prompt}, {Cont, Level}, M, F, A) ->
    case io:request(IO, {get_until, unicode, Prompt, M, F, [A]}) of
        {ok, Token, EndLoc} ->
            case Token of
                {'(', _} ->
                    %io:format(standard_error, "L ~p, ~p~n", [Cont, Level]),
                    read(IO, Prompt, {[Token|Cont], Level+1}, M, F, EndLoc);
                {')', _} when Level > 1 ->
                    %io:format(standard_error, "L ~p, ~p~n", [Cont, Level]),
                    read(IO, Prompt, {[Token|Cont], Level-1}, M, F, EndLoc);
                {')', _} ->
                    %io:format(standard_error, "L ~p, ~p~n", [Cont, Level]),
                    {ok, lists:reverse([Token|Cont]), EndLoc};
                _ when Level == 0 ->
                    {ok, lists:reverse([Token|Cont]), EndLoc};
                _ ->
                    read(IO, Prompt, {[Token|Cont], Level}, M, F, EndLoc)
            end;

        {eof, EndLoc} ->
            %io:format(standard_error, "eof~n", []),
            {eof, EndLoc}
    end.
read_balance({IO, Prompt}, M, F, A) ->
    read_balance({IO, Prompt}, {[], 0}, M, F, A).

read(IO, Prompt, {[{read_macro, _Loc, Value}|Cont], Level}, M, F, A) ->
    {ok, Tokens, EndLoc} = read_macro({IO, Prompt}, M, F, A, Value),
    read(IO, Prompt, {lists:reverse(Tokens) ++ Cont, Level}, M, F, EndLoc);
read(IO, Prompt, {Cont, Level}, M, F, A) ->
    case read_balance({IO, Prompt}, {Cont, Level}, scan, token, A) of
        {ok, [{read_macro, _Loc, Value}], EndLoc} ->
            read_macro({IO, Prompt}, M, F, EndLoc, Value);
        {ok, Tokens, EndLoc} ->
            %io:format(standard_error, "balanced ~p ~n", [Tokens]),
            {ok, Tokens, EndLoc};
        Error ->
            Error
    end.
    
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
    RM = #{quote => {scan, replace2},
           backquote => {scan, replace2},
           unquote => {scan, replace2},
           unquote_splice => {scan, replace2}
          },
    io:format("OOOOO", []),
    {MM, MF} = maps:get(MChar, RM, {scan, not_implemented}),
    {ok, NewTokens, _NewLoc, RestTokens} = apply(MM, MF, [{IO, Prompt0}, scan, read2, 
                                                         Loc, MChar]),
    io:format("OOOO1 ~p~n", [{NewTokens, RestTokens, PreLevel}]),
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
    
replace2({IO, Prompt0}, M, F, Loc, MChar) ->
    io:format("READ2 ~p~n", [[{IO, Prompt0}, M, F, Loc, MChar]]),
    Ret = read2(IO, Prompt0, loctoline(Loc), [], 0),
    io:format("RET ~p~n", [Ret]),
    {ok, Tokens, NextLine, Rest} = Ret,
    NewTokens = [{'(', Loc}, 
                 {symbol, Loc, atom_to_list(MChar)} | 
                 Tokens ++ [{')', NextLine}]],
    {ok, NewTokens, NextLine, Rest}.

read2(IO, Prompt0, Line, PrevTokens, PrevLevel) ->
    Prompt = io_lib:format("~B~s", [loctoline(Line), Prompt0]),
    case io:request(IO, {get_until, unicode, Prompt, scan, tokens, [Line]}) of
        {ok, NewTokens, NextLine} ->
            {NNewTokens, NLevel, Rest} = calclevel(IO, Prompt0, NewTokens, PrevLevel),
            io:format("RES ~p~n", [{NNewTokens, NLevel, NewTokens, PrevLevel, Rest}]),
            Tokens = PrevTokens ++ NNewTokens,
            case {Rest, NLevel} of
                {[], NLevel} when NLevel > 0 -> 
                    read2(IO, Prompt0, NextLine, Tokens, NLevel);
                {Rest, 0} ->
                    io:format("READ2: ret ~p~n", [{Tokens, Rest, PrevLevel}]),
                    {ok, Tokens, NextLine, Rest};
                _  ->
                    %%  error, paren_level_error}
                    io:format("READ3: ret ~p~n", [{Tokens, Rest, NLevel, PrevLevel}]),
                    {ok, Tokens, NextLine, Rest} 
            end;
        {eof, NextLine} ->
            {ok, PrevTokens, NextLine, []};
        Error ->
            io:format("Error! ~p~n",[{Error, PrevTokens, PrevLevel}]),
            Error
    end.
                    
read(IO, Prompt, A) ->
    read(IO, Prompt, {[], 0}, scan, token, A).

read_macro({IO, Prompt}, M, F, A, Value) ->
    RM = #{quote => {scan, replace},
           backquote => {scan, replace},
           unquote => {scan, replace},
           unquote_splice => {scan, replace}
          },
    {MM, MF} = maps:get(Value, RM, {scan, not_implemented}),
    %io:format(standard_error, "RM ~p~n", [MF]),
    apply(MM, MF, [{IO, Prompt}, M, F, A, Value]).

replace({IO, Prompt}, _M, _F, A, Value) ->
    %io:format(standard_error, "E ~p~n", [M]),
    {ok, Tokens, EndLoc} = read(IO, Prompt, A),
    %io:format(standard_error, "E ~p~n", [Tokens]),
    NewTokens = [{'(', A},
                 {symbol, A, atom_to_list(Value)} |
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
%    {ok, Acc, NewLine} = reads(IO, "> ", Line, ""),

    {ok, Acc , NewLine, _Rest} =  read2(IO, "> ", Line, [], 0),
    NAcc = lists:filter(fun({'\n', _}) ->
                         false;
                    (_)  ->
                         true
                 end, Acc),
    {ok, NAcc, NewLine}.
