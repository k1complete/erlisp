-module(els_dbg).
-export([i/3]).
i(Module,Src, BeamFile) ->
    {ok, BeamBin} = file:read_file(BeamFile),
    int:i([{Module, Src, BeamFile, BeamBin}]).

%% elis -> to els

     
