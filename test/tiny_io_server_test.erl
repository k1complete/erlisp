-module(tiny_io_server_test).

-include_lib("eunit/include/eunit.hrl").

slist_test() ->
    ?assertEqual({ok, [{'(',{0,1}},
                       {symbol, {0,2}, "q"},
                       {symbol, {0,4}, "b"},
                       {symbol, {0,6}, "c"},
                       {')', {0,7}}], 0},
                 scan:from_string("(q b c)", 0)).
quote_test() ->
    ?assertEqual({ok, [{'(',{0,1}},
                       {symbol, {0,2}, "q"},
                       {'(',0},
                       {symbol, 0, "quote"},
                       {symbol, {0,5}, "b"},
                       {')',0},
                       {symbol, {0,7}, "c"},
                       {')', {0,8}}], 0},
                 scan:from_string("(q 'b c)", 0)).
backquote_test() ->
    ?assertEqual({ok, [{'(',{0,1}},
                       {symbol, {0,2}, "q"},
                       {'(',0},
                       {symbol, 0, "backquote"},
                       {symbol, {0,5}, "b"},
                       {')',0},
                       {symbol, {0,7}, "c"},
                       {')', {0,8}}], 0},
                 scan:from_string("(q `b c)", 0)).
backquote_list_test() ->
    ?assertEqual({ok, [{'(',0},
                       {symbol,0,"backquote"},
                       {'(',{0,2}},
                       {symbol,{0,3},"q"},
                       {'(',0},
                       {symbol,0,"backquote"},
                       {symbol,{0,6},"b"},
                       {')',0},
                       {symbol,{0,8},"c"},
                       {')',{0,9}},
                       {')',0}], 0},
                 scan:from_string("`(q `b c)", 0)).
unquote_test() ->
    ?assertEqual({ok, [{'(',{0,1}},
                       {symbol, {0,2}, "q"},
                       {'(',0},
                       {symbol, 0, "unquote"},
                       {symbol, {0,5}, "b"},
                       {')',0},
                       {symbol, {0,7}, "c"},
                       {')', {0,8}}], 0},
                 scan:from_string("(q ,b c)", 0)),
    ?assertEqual({ok,[{'(',{0,1}},
                      {symbol,{0,2},"a"},
                      {'(',0},
                      {symbol,0,"backquote"},
                      {'(',{0,5}},
                      {symbol,{0,6},"q"},
                      {'(',0},
                      {symbol,0,"unquote"},
                      {symbol,{0,9},"b"},
                      {')',0},
                      {symbol,{0,11},"c"},
                      {')',{0,12}},
                      {')',0},
                      {')',{0,13}}],
                     0},
                 scan:from_string("(a `(q ,b c))", 0)).



    
