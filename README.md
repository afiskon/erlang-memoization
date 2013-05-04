Erlang Memoization Library
--------------------------

Usage:

    1> application:start(erlymemo).
    ok
    2> application:set_env(erlymemo, table_max_size, 5000).
    ok
    3> erlymemo:clean().
    ok
    4> List = [1,2,3].
    [1,2,3]
    5> erlymemo:call(erlang, length, [ List ]).
    3
    6> erlymemo:used_space().
    1
    7> erlymemo:free_space().
    4999
    8> [ erlymemo:call(erlang, length, [ [X] ]) || X <- lists:seq(1,99999) ].
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1|...]
    9> erlymemo:used_space().                                                
    5000
    10> erlymemo:free_space().                                                
    0
