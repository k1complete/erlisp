
-type location() :: erl_anno:location().
-type atype() :: variable | function | atom | string.

-record(item, 
        {value :: atom(),
         loc :: location(),
         type :: atype()}).

-record(compile_info, 
	{loc:: location(),
	 msg:: string,
	 detail:: any()}).

-record(compile_result,
	{ok :: list(any()),
	 warning :: list(#compile_info{}),
	 error :: list(#compile_info{})}).


-type sexp() :: list(any()).
-type env() :: list().
-type option() :: atom() | {atom(), any()}.
-type options() :: list(option()).

-type tokenloc() :: {integer(), integer()} | integer().
-include_lib("kernel/include/logger.hrl").
