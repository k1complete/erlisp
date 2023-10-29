
-type location() :: erl_anno:location().
-type atype() :: variable | function | atom | string.

-record(item, 
        {value :: atom(),
         loc :: location(),
         type :: atype()}).

-type sexp() :: list(any()).
-type env() :: list().
-type option() :: atom() | {atom(), any()}.
-type options() :: list(option()).

-type tokenloc() :: {integer(), integer()} | integer().
-include_lib("kernel/include/logger.hrl").
