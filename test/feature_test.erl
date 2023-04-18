-module(feature_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE > 25).
-define(LEEX_LOC, 1).
-else.
-define(LEEX_LOC, 2).
-endif.
-else.
-define(?LEEX_LOC).
-endif.

