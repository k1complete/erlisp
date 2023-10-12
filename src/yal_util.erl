-module(yal_util).
-include_lib("erlisp.hrl").
-export([make_symbol/2, make_symbol/1,
         required_macros/1,
         getmacros/1,
         make_macro_funcname/1
        ]).
make_macro_funcname(Name) ->
    list_to_atom("MACRO_" ++ Name).

make_symbol(S, Pos) ->
    #item{value=atom_to_list(S), loc=Pos, type=atom}.
make_symbol(S) ->
    #item{value=atom_to_list(S), loc=0, type=atom}.

required_macros(M) ->
    case code:ensure_loaded(M) of
        {module, M} ->
            Exports = M:module_info(exports),
            lists:filtermap(fun({F, A}) ->
                                    Fs = atom_to_list(F),
                                    case {Fs, A} of
                                        {"MACRO_"++Macro, A} ->
                                            {true, {{atom_to_list(M), Macro, A}, {M, F}}};
                                        _ ->
                                            false
                                    end
                            end, Exports);
        {error, _What} ->
            []
    end.

getmacros(Module) ->
    maps:from_list(yal_util:required_macros(Module)).

