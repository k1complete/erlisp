%%%-------------------------------------------------------------------
%% @doc erlisp public API
%% @end
%%%-------------------------------------------------------------------

-module(els_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlisp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
