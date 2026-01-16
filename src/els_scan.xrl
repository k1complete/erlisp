% header

Definitions.

%macro definitions
Digits = [0-9]+
Alphabet = [A-Za-z_]|[\x{80}-\x{10fff}]
Griph=[-+*=#/\|:]
PostAlphabet = ({Alphabet}|{Digits}|{Griph})
Symbols = ([-+/*\|\?a-z]{PostAlphabet}*|:bf)
%Op = (\+\+|\-\-|==|/=|=<|<|>=|>|=:=|=/=|\+|-|\*|/|!|<-|<=|:=)
Op = (\+\+|\-\-|==|/=|=<|<|>=|>|=:=|=/=|\+|-|\*|/|!|<-|<=|:=|=>|\?=|=)
Variables = [A-Z_]{PostAlphabet}*
WhiteSpace = [\s\t]+
MQ = \"\"\"
MString = {MQ}[\n.]+{MQ}
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
#\. :
  {token, {symbol, TokenLoc, TokenChars}}.
#r :
  {token, {symbol, TokenLoc, TokenChars}}.
\[ : 
  {token, {'[', TokenLoc}}.
\] : 
  {token, {']', TokenLoc}}.
#\{ : 
  {token, {'#{', TokenLoc}}.
\{ : 
  {token, {'{', TokenLoc}}.
\} : 
  {token, {'}', TokenLoc}}.
\:\: : 
  {token, {symbol, TokenLoc, TokenChars}}.
\( :
  {token, {'(', TokenLoc}}.
\) :
  {token, {')', TokenLoc}}.
\.\. :
  {token, {symbol, TokenLoc, TokenChars}}.
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
\! :
  {token, {'!', TokenLoc}}.
#\\ :
  {end_token, {read_macro, TokenLoc, 'escape'}}.
# :
  {token, {symbol, TokenLoc, TokenChars}}.

{MQ} :
  {token, {'"""', TokenLoc}}.
{QString} :
  [_|String] = lists:droplast(TokenChars),
  {token, {string, TokenLoc, String}}.

{WhiteSpace} :
  skip_token.

{LineFeed} :
  {end_token, {'\n', TokenLoc}}.
%%  %%skip_token.

Erlang code.

-include_lib("els_scan.hrl").
-include_lib("els.hrl").
-export([file/2]).
%%-export([read_balance/4]).
%%-export([read/3]).
%%-export([read/6]).
-export([reads/5]).
-export([from_string/2]).
-export([from_string/1]).
-export([from_string_rest/4]).
%%-export([replace/5]).
-export([read/5]).
-export([replace/5]).
-export([escape/5]).

-define(IS_OPEN(X), is_map_key(X, #{'(' => 1, '{' => 1, '#{' => 1, '[' => 1})).
-define(IS_CLOSE(X), is_map_key(X, #{')' => 1, '}' => 1, ']' => 1})).

-export([tokens2/2, tokens2/3]).
-export([get_parens/3]).
  
calclevel(IO, Prompt0, Tokens, GLevel, Line) ->
%%    io:format("calclevel [~p]~n", [Tokens]),
    R=do_calclevel(IO, Prompt0, Tokens, {[], GLevel}, Line),
%%    io:format("calclevelout [~p]~n", [R]),
    R.

do_calclevel(_IO, _Prompt0, [], {Acc, PreLevel}, Line) ->
%%    io:format("calc-out ): ~p ~p ~n", [PreLevel, Acc]),
    {Acc, PreLevel, [], Line};
do_calclevel(IO, Prompt0, [{X, _Loc} =T | Tokens], {Acc, PreLevel}, Line) when ?IS_OPEN(X) ->
    do_calclevel(IO, Prompt0, Tokens, {Acc ++ [T], PreLevel+1}, Line);
do_calclevel(_IO, _Prompt0, [{X, _Loc} =T | Tokens], {Acc, PrevLevel}, Line) when PrevLevel =< 1, ?IS_CLOSE(X) ->
    {Acc ++ [T], PrevLevel-1, Tokens, Line};
do_calclevel(IO, Prompt0, [{X, _Loc} =T | Tokens], {Acc, PreLevel}, Line) when ?IS_CLOSE(X) ->
%%    io:format("calc-apply ): ~p ~p ~p ~n", [PreLevel, Acc, Tokens]),
    do_calclevel(IO, Prompt0, Tokens, {Acc ++ [T], PreLevel-1}, Line);
do_calclevel(IO, Prompt0, [{read_macro, Loc, MChar}], {Acc, PreLevel}, _Line) ->
    RM = #{quote => {?MODULE, replace},
           backquote => {?MODULE, replace},
           unquote => {?MODULE, replace},
           escape => {?MODULE, escape},
           unquote_splice => {?MODULE, replace}
          },
    {MM, MF} = maps:get(MChar, RM, {?MODULE, not_implemented}),
    %%PrevTokens = case put(prevtokens, Acc) of
    %%		     undefined ->
    %%			 [];
    %%		     X -> 
    %%			 X
    %%		 end,
    PrevTokens = Acc,
    %%io:format("calc-apply before: ~p~n", [Acc]),
    NPrompt = case Prompt0 of
		  {P, T} ->
		      {P, T++PrevTokens};
		  P when is_list(P) ->
		      {P, PrevTokens}
	      end,
    {ok, NewTokens, NewLoc, RestTokens} = apply(MM, MF, [{IO, NPrompt}, ?MODULE, read, 
                                                         Loc, MChar]),

    %%put(prevtokens, PrevTokens),
    %% io:format("calc-apply after: NT ~p ~n", [PrevTokens]),
    
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

set_col_offset(Line, _Offset) when is_integer(Line) ->
    Line;
set_col_offset({Line, Col}, {Line, Offset}) ->
    {Line, Col+Offset};
set_col_offset({Line, Col}, {_, _Offset}) ->
    {Line, Col};
set_col_offset(List, Offset) when is_list(List) ->
    lists:map(fun(Token) ->
		      case Token of 
			  {X, Loc} ->
			      {X, set_col_offset(Loc, Offset)};
			  {X, Loc, Y} ->
			      {X, set_col_offset(Loc, Offset), Y};
			  X ->
			      X
		      end
	      end, List).

escape({IO, _Prompt0}, _M, _F, Loc, _MChar) ->
    {Line, Row} = Loc,
    %% io:format("Loc: ~p, MChar: ~p~n", [Loc, MChar]),
    C = io:get_chars(IO, "", 1),
    {Line, Col} = Loc,
    NLoc = set_col_offset({Line, Col}, {Line, 2}),
    Ret = read(IO, _Prompt0, NLoc, [], 0),
    {ok, Tokens, NextLine, Rest} = Ret,
    {ok, [{string, {Line, Row+1}, C}], NextLine, Tokens++Rest}.


set_echo(IO, Bool) ->
    R= case get(term) of
	   undefined -> false;
	   S -> proplists:get_value(echo, S, false)
       end,
    Opts = case io:getopts(IO) of
	       undefined -> false;
	       S2 -> proplists:get_value(echo, S2, false)
	   end,
    %% io:format("on befgore Opts: ~p~n~p~n", [Opts, R]),
    case {R, Opts, Bool} of
	{true, O, B} when O =/= B ->
	    io:setopts(IO, [{echo, Bool}]);
	{true, O, O} ->
	    true;
	{false, _, _} ->
	    true
    end.
    
set_echo_on(IO) ->
    set_echo(IO, true).

set_echo_off(IO) ->
    set_echo(IO, false).

replace({IO, _Prompt0}, _M, _F, Loc, MChar) ->
    {NL, NC} = Loc,
    {ok, Tokens, NextLine, Rest} = read(IO, _Prompt0, {NL, NC}, [], 0),
    NewTokens = [{'(', Loc}, 
                 {symbol, Loc, atom_to_list(MChar)} | 
                 Tokens ++ [{')', Loc}]],
    {ok, NewTokens, NextLine, Rest}.

make_quote_prompt([], _Line, QuoteStr) ->
    "";
make_quote_prompt(Prompt, Line, QuoteStr) ->
    S = io_lib:format(Prompt, [loctoline(Line)]),
    P = string:pad(QuoteStr, length(S)-2, leading),
    P ++ ". ".

make_prompt(_IO, [], _Line, _PrevTokens, _PrevLevel) ->
    "";
make_prompt(IO, Prompt, Line, Tokens, PrevLevel) when is_list(Prompt) ->
    make_prompt(IO, {Prompt, []}, Line, Tokens, PrevLevel);
make_prompt(IO, {Prompt, PrevTokens}, Line, [], PrevLevel) ->
    Opt = io:getopts(IO),
    case proplists:get_value(terminal, Opt, false) of
	true ->
	    case PrevTokens of 
		[] ->
		    io_lib:format(Prompt, [loctoline(Line)]);
		_ ->
		    %% io:format("P: ~p~nTokens: ~p~n", [Prompt, PrevTokens]),
		    S = io_lib:format(Prompt, [loctoline(Line)]),
		    P = string:pad(get_parens(PrevTokens, [], PrevLevel), length(S)-2, leading),
		    P++". "
	    end;
	false ->
	    ""
    end;
make_prompt(IO, {Prompt, PPrevTokens}, Line, PrevTokens, PrevLevel) ->
    Opt = io:getopts(IO),
    case proplists:get_value(terminal, Opt, false) of
	true ->
	    %%io:format("l:~p~nt:~p~n", [PrevLevel, PrevTokens]),
	    %%S = io_lib:format(Prompt, [loctoline(Line)]),
	    S = io_lib:format(Prompt, [loctoline(Line)]),
	    P = string:pad(get_parens(PPrevTokens, PrevTokens, PrevLevel), length(S)-2, leading),
	    P++". ";
	false ->
	    ""
    end.

%% 

get_parens(PrevTokens, Tokens, Level) ->
    %%    PrevTokens = case get(prevtokens) of 
    %%undefined ->
    %%			 [];
    %%		     X ->
    %%			 X
    %%		 end,
    %%io:format("PrevTokens: ~p~n", [PrevTokens]),
    %%io:format("Tokens: ~p~n", [Tokens]),
    CTokens = case Tokens of
		  undefined ->
		      PrevTokens;
		  _ ->
		      PrevTokens++Tokens
	      end,
    R = lists:foldl(
	  fun({I, _}, A) when I == '('; I == '{'; I == '[' ->
		  [atom_to_list(I)|A];
	     ({I, _}, [_|A]) when I == ')'; I == '}'; I == ']' ->
		  A;
	     (_, A) ->
		  A
	  end, [], CTokens),
    lists:flatten(lists:reverse(R)).

adjust_level(IO, Prompt0, PrevTokens, PrevLevel, Line) ->
    {NNewTokens, NLevel, Rest, NewLine} = calclevel(IO, Prompt0, PrevTokens, PrevLevel, Line),
    %% io:format("calclevel ~p ~p~n", [NNewTokens, Rest]),
    Tokens = NNewTokens,
    case {Rest, NLevel} of
        {[], NLevel} when NLevel > 0 -> 
            %% io:format("Readmore ~p ~p~n", [NLevel, Rest]),
            S = read(IO, Prompt0, NewLine, Tokens++Rest, NLevel),
            %% io:format("After ~p~n", [S]),
	    S;
        {Rest, 0} ->
            %io:format("Token ~p Rest ~p~n", [Tokens, Rest]),
            %%?LOG_DEBUG(#{ajust_level => [Tokens, Rest]}),
            {ok, Tokens, NewLine, Rest};
        _  ->
            %io:format("readRet ~p~n", [{ok, Tokens, NewLine, Rest}]),
            {ok, Tokens, NewLine, Rest} 
    end.

read_multiline(IO, Prompt0, Line, Add, Stop, Col) ->
    Prompt = make_quote_prompt(Prompt0, Line, "\"\"\""),
    case io:get_line(IO, Prompt) of
        {error, Error} ->
            {error, Error};
        eof ->
            eof;
        Stop ->
            {Add, Line+1};
        Data ->
	    SS = string:slice(Data, Col-1),
	    %%%io:format("GetLine ~p FFF ~p ~p ~p ~p~n", [Stop, Data, Stop=:=Data, SS, Col]),
	    case SS of
		Stop ->
		    {Add, Line+1};
		Rest ->
		    %%%io:format("GetLine ~p vi ~p ~p~n", [Stop, Data, Stop=:=Data]),
		    read_multiline(IO, Prompt0, Line+1, string:concat(Add ,Rest), Stop, Col)
	    end
    end.

multiline_quote(IO, Prompt0, Line, Tokens) ->
    case hd(Tokens) of
        {'"""', {_, Col}} ->
	    %%%io:format("--------\n", []),
            {MT, L} = read_multiline(IO, Prompt0, Line, "",
                                     "\"\"\"\n", Col),
            MM = {[{string, Line, MT}],L},
	    %%% io:format("REST: ~p n ~p~n", [Tokens,MM]),
            MM;
        _ ->
            {Tokens, Line}
    end.

    
tokens2(Cont, Chars) -> 
    tokens2(Cont, Chars, 1).
tokens2(Cont, Chars, Line) ->
    %%io:format("----- ~p ~n ", [Chars]),
    R = tokens(Cont, Chars, Line),
    %%io:format("--out--- ~p ~n ", [R]),
    R.

read_do(IO, Prompt0, {Line, Col}, PrevTokens, PrevLevel) ->
    Prompt = case Col of
		 0 -> 
		     set_echo_on(IO),
		     make_prompt(IO, Prompt0, Line, PrevTokens, PrevLevel);
		 _ -> 
		     set_echo_off(IO),
		     ""
	     end,
    Ret =  io:request(IO, {get_until, unicode, Prompt, ?MODULE, tokens2, [Line]}),
    %% io:format("read ret ~p~n", [Ret]),
    case Ret of
        {ok, [{'\n',NL_Loc}], NextLine} ->
	    read(IO, Prompt0, {NextLine, 0}, PrevTokens, PrevLevel);
        {ok, [], NextLine} ->
	    read(IO, Prompt0, {NextLine, 0}, PrevTokens, PrevLevel);
        {ok, NewTokens, NextLine} ->
            %%?LOG_DEBUG(#{prevlevel => PrevLevel,
	    %%io:format("get tokens Col: ~p ~p ~p~n", [Col, NewTokens, NextLine]),
	    set_echo_on(IO),
	    N2NextLine=set_col_offset(NextLine, {Line, Col}),
	    N2NewTokens=set_col_offset(NewTokens, {Line, Col}),
            {NewTokens2, NextLine2} =  multiline_quote(IO, Prompt0, N2NextLine, N2NewTokens),
            %%?LOG_DEBUG(#{adjust_level => PrevTokens++NewTokens2}),
            adjust_level(IO, Prompt0, PrevTokens++NewTokens2, 0, NextLine2);
        {eof, NextLine} ->
	    %%io:format("PrevTokens ~p~n", [PrevTokens]),
            {eof, PrevTokens, NextLine, []};
        {error, terminated} ->
            %io:format("PrevTokens ~p~n", [PrevTokens]),
            {eof, PrevTokens, Line, []};
        Error ->
            io:format(
                      "Error! sss ~p, ~p, ~p ~n",[Error, PrevTokens, PrevLevel]),
            Error
    end;
read_do(IO, Prompt0, Line, PrevTokens, PrevLevel) when is_integer(Line) ->
    read_do(IO, Prompt0, {Line, 0}, PrevTokens, PrevLevel).

read(IO, Prompt0, Line, PrevTokens, PrevLevel) when length(PrevTokens) > 0 andalso PrevLevel == 0 ->
    adjust_level(IO, Prompt0, PrevTokens, PrevLevel, Line);
read(IO, Prompt0, Line, PrevTokens, PrevLevel) ->
    read_do(IO, Prompt0, Line, PrevTokens, PrevLevel).

from_string_rest(IO, Line, Rest, Acc) ->
    case read(IO, [], Line, Rest, 0) of
        {ok, Acc2, NewLine, Rest2} ->
	    %%io:format("Rest ~p~n", [Rest2]),
            from_string_rest(IO, NewLine, Rest2, Acc++Acc2);
        {eof, Acc2, Line2, []} ->
            {eof, Acc++Acc2, Line2};
        {X, Acc2, Line2, []} ->
	    %% io:format("rest ~p~n", [X]),
            {X, Acc2, Line2}
    end.
	    
from_string(String) ->
    from_string(String, 0).

from_string(String, Line) ->            
    IO = tiny_io_server:start_link(String),
    {R, Acc, NewLine} = from_string_rest(IO, Line, [], []),
    tiny_io_server:stop(IO),
    case R of 
	eof ->
	    {ok, Acc, NewLine};
	X ->
	    {X, Acc, NewLine}
    end.

reads(IO, File, Line, PrevTokens, Acc) ->
    case read(IO, [], Line, PrevTokens, 0) of
        {ok, [], _, RestTokens}  ->
            %%io:format("ReadsRET: ~p~n", [{Acc, RestTokens}]),
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
