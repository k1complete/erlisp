
-type location() :: erl_anno:location().
-type atype() :: variable | function | atom | string.

-record(item, 
        {value :: atom(),
         loc :: location(),
         type :: atype()}).

-type sexp() :: list(any()).

-include_lib("kernel/include/logger.hrl").
