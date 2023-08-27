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
MQ = \"\"\"
%MString = {MQ}(\n|.)*{MQ}
QString = \"([^\"]|\\\")+\"

LineFeed = \n
Rules.
%tokenrules
{Digits} :
  {token, {integer, TokenLoc, list_to_integer(TokenChars)}}.
{Digits}\.{Digits}((E|e)(\+|\-)?{Digits})? :
  {token, {float, TokenLoc, list_to_float(TokenChars)}}.
{Variables} : 
  {token, {symbol, TokenLoc, TokenChars}}.
{Op} : 
  {token, {symbol, TokenLoc, TokenChars}}.
{Symbols} : 
  {token, {symbol, TokenLoc, TokenChars}}.
{Symbols}:{Symbols} : 
  {token, {module_function, TokenLoc, TokenChars}}.
\( :
  {token, {'(', TokenLoc}}.
\) :
  {token, {')', TokenLoc}}.
\. : 
  {token, {'.', TokenLoc}}.
\,\@ :
  {end_token, {read_macro, TokenLoc, 'unquote_splice'}}.
\, :
  {end_token, {read_macro, TokenLoc, 'unquote'}}.
\' :
  {end_token, {read_macro, TokenLoc, 'quote'}}.
\` :
  {end_token, {read_macro, TokenLoc, 'backquote'}}.
\#\( :
  {token, {'#(', TokenLoc}}.
{MQ} :
  {end_token, {'"""', TokenLoc}}.
{QString} :
  [_|String] = lists:droplast(TokenChars),
  {token, {string, TokenLoc, String}}.

{WhiteSpace} :
  skip_token.
{LineFeed} :
  {end_token, {'\n', TokenLoc}}.
  %%skip_token.

Erlang code.

-include_lib("scan.hrl").
-include_lib("erlisp.hrl").
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
    %io:format("calc-apply before: ~p ~p ~n", [Loc, Acc]),
    {ok, NewTokens, NewLoc, RestTokens} = apply(MM, MF, [{IO, Prompt0}, scan, read, 
                                                         Loc, MChar]),
    %io:format("calc-apply after: NT ~p Rest ~p PL ~p ~n", [NewTokens, RestTokens, PreLevel]),
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
    
replace({IO, _Prompt0}, _M, _F, Loc, MChar) ->
    %Ret = read(IO, Prompt0, loctoline(Loc), [], 0),
    Ret = read(IO, "", loctoline(Loc), [], 0),
    %io:format("replace-2read ~p~n", [Ret]),
    {ok, Tokens, NextLine, Rest} = Ret,
    {_L, ACol} = Loc,
    N2Tokens = lists:map(fun({T, {L, C}, V}) ->
                                 {T, {L, C+ACol}, V};
                            ({T, {L, C}}) ->
                                 {T, {L, C+ACol}};
                            (T) ->
                                 T
                         end, Tokens),
    N2Rest = lists:map(fun({T, {L, C}, V}) ->
                                 {T, {L, C+ACol}, V};
                          ({T, {L, C}}) ->
                                 {T, {L, C+ACol}};
                          (T) ->
                               T
                         end, Rest),
    NewTokens = [{'(', Loc}, 
                 {symbol, Loc, atom_to_list(MChar)} | 
                 N2Tokens ++ [{')', Loc}]],
    {ok, NewTokens, NextLine, N2Rest}.

make_prompt([], _Line, _PrevTokens) ->
    "";
make_prompt(Prompt, Line, []) ->
    io_lib:format(Prompt, [loctoline(Line)]);
make_prompt(_Prompt, _Line, _PrevTokens) ->
    "".

adjust_level(IO, Prompt0, PrevTokens, PrevLevel, Line) ->
    {NNewTokens, NLevel, Rest, NewLine} = calclevel(IO, Prompt0, PrevTokens, PrevLevel, Line),
    Tokens = NNewTokens,
    case {Rest, NLevel} of
        {[], NLevel} when NLevel > 0 -> 
            %io:format("Readmore ~p ~p~n", [NLevel, Rest]),
            read(IO, Prompt0, NewLine, Tokens++Rest, NLevel);
        {Rest, 0} ->
            %io:format("Token ~p Rest ~p~n", [Tokens, Rest]),
            ?LOG_DEBUG(#{ajust_level => [Tokens, Rest]}),
            {ok, Tokens, NewLine, Rest};
        _  ->
            io:format("readRet ~p~n", [{ok, Tokens, NewLine, Rest}]),
            {ok, Tokens, NewLine, Rest} 
    end.

read_multiline(IO, Line, Add, Stop) ->
    case io:get_line(IO, "") of
        {error, Error} ->
            {error, Error};
        eof ->
            eof;
        Stop ->
            {Add, Line+1};
        Data ->
            io:format("GetLine ~p vi ~p ~p~n", [Stop, Data, Stop=:=Data]),
            read_multiline(IO, Line+1, string:concat(Add ,Data), Stop)
    end.

multiline_quote(IO, Line, Tokens) ->
    case lists:last(Tokens) of
        {'"""', _} ->
            {MT, L} = read_multiline(IO, Line, "",
                                     "\"\"\"\n"),
            MM = {lists:append(lists:droplast(Tokens),
                               [{string, Line, MT}]),
                  L},
            io:format("REST: ~p n ~p~n", [Tokens,MM]),
            MM;
        _ ->
            {Tokens, Line}
    end.
    
            

read(IO, Prompt0, Line, PrevTokens, PrevLevel) when length(PrevTokens) > 0 andalso PrevLevel == 0 ->
    io:format("CalcLevel PreVTokens  ~p ~n PrevLevel ~p~n", [PrevTokens, PrevLevel]),
    adjust_level(IO, Prompt0, PrevTokens, PrevLevel, Line);
read(IO, Prompt0, Line, PrevTokens, PrevLevel) ->
    Prompt = make_prompt(Prompt0, Line, PrevTokens),
    case io:request(IO, {get_until, unicode, Prompt, scan, tokens, [Line]}) of
        {ok, NewTokens, NextLine} ->
            ?LOG_DEBUG(#{prevlevel => PrevLevel,
                         prevtokens => PrevTokens,
                         newtokens => NewTokens}),
            {NewTokens2, NextLine2} =  multiline_quote(IO, NextLine, NewTokens),
            %%?LOG_DEBUG(#{adjust_level => PrevTokens++NewTokens2}),
            %adjust_level(IO, Prompt0, PrevTokens++NewTokens2, PrevLevel, NextLine2);
            adjust_level(IO, Prompt0, PrevTokens++NewTokens2, 0, NextLine2);
        {eof, NextLine} ->
            io:format("PrevTokens ~p~n", [PrevTokens]),
            {eof, PrevTokens, NextLine, []};
        {error, terminated} ->
            io:format("PrevTokens ~p~n", [PrevTokens]),
            {eof, PrevTokens, Line, []};
        Error ->
            io:format(
                      "Error! sss ~p, ~p, ~p ~n",[Error, PrevTokens, PrevLevel]),
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
            %%from_string_rest(IO, Line, Rest2, Acc++Acc2);
            from_string_rest(IO, NewLine, Rest2, Acc++Acc2);
        {eof, Acc2, Line2, []} ->
            {eof, Acc2, Line2};
        _ ->
            {ok, Acc, Line}
    end.

remove_nl(Tokens) ->    
    lists:filter(fun({'\n', _}) -> 
                         false;
                    (_) -> true 
                 end, Tokens).

from_string(String, Line) ->
%%    io:format(standard_error, "~p~n", [length(erlang:processes())]),
    IO = tiny_io_server:start_link(String),
    {R, Acc , NewLine} = from_string_rest(IO, Line, [], []),
    tiny_io_server:stop(IO),
%%    io:format(standard_error, "~p~n", [length(erlang:processes())]),
    {R, remove_nl(Acc), NewLine}.
            

reads(IO, File, Line, PrevTokens, Acc) ->
    case read(IO, [], Line, PrevTokens, 0) of
        {ok, [], _, RestTokens}  ->
            io:format("ReadsRET: ~p~n", [{Acc, RestTokens}]),
            {ok, Acc++RestTokens};
        {ok, Tokens, NextLine, RestTokens} ->
            logger:degbug(#{reads=> Tokens}),
            R = reads(IO, File, NextLine+1, RestTokens, Acc ++ Tokens),
            {ok, element(2, R)}
    end.
file_rest(IO, Line, Acc0) ->
    case from_string_rest(IO, Line, [], []) of
        {ok, Acc, NewLine}  ->
            file_rest(IO, NewLine, Acc0++Acc);
        {eof, Acc, NewLine}  ->
            {ok, Acc0 ++ Acc, NewLine};
        Error ->
            Error
    end.
file(File, _Option) ->
    {ok, IO} = file:open(File, "r"),
    {ok, Acc, _Line} = file_rest(IO, 1, []),
    file:close(IO),
    {ok, Acc}.
