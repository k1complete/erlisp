
-type location() :: erl_anno:location().
-type atype() :: variable | function | atom | string.

-record(item, 
        {value :: atom(),
         loc :: location(),
         type :: atype()}).


