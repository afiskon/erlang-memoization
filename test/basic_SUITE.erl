-module(basic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        call_test,
        table_size_test,
        two_calls_test,
        max_table_size_test,
        lru_test,
        clean_test,
        parallel_test,
        invalid_function_test,
        invalid_arguments_test
    ].

init_per_suite(Config) ->
    ok = application:start(erlymemo),
    Config.

init_per_testcase(_, Config) ->
    ets:new(?MODULE, [set, named_table, public]),
    zero_call_counter(),
    meck:new(foo),
    meck:expect(foo, bar, fun(X, Y) -> inc_call_counter(), X + Y end),
    meck:expect(foo, baz, fun(X, Y) -> X - Y end),
    meck:expect(foo, qux, fun(X, Y) -> X * Y end),
    erlymemo:clean(),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(foo),
    ets:delete(?MODULE),
    Config.

end_per_suite(Config) ->
    ok = application:stop(erlymemo),
    Config.

call_test(_Config) ->
    List = [1,2,3],
    Result = erlymemo:call(erlang, length, [ List ]),
    ?assertEqual(3, Result).

table_size_test(_Config) ->
    erlymemo:call(foo, bar, [ 4, 5 ]),
    UsedSpace = erlymemo:used_space(),
    erlymemo:call(foo, bar, [ 4, 5 ]),
    NewUsedSpace = erlymemo:used_space(),
    ?assertEqual(UsedSpace, NewUsedSpace).

two_calls_test(_Config) ->
    erlymemo:call(foo, bar, [ 4, 5 ]),
    erlymemo:call(foo, bar, [ 4, 5 ]),
    ?assertEqual(1, get_call_counter()).

max_table_size_test(_Config) ->
    make_a_lot_of_calls(),
    UsedSpace = erlymemo:used_space(),
    FreeSpace = erlymemo:free_space(),
    MaxSize = erlymemo:table_max_size(),
    ?assertEqual(0, FreeSpace),
    ?assertEqual(MaxSize, UsedSpace).

lru_test(_Config) ->
    erlymemo:call(foo, bar, [ 4, 5 ]),
    make_a_lot_of_calls(),
    erlymemo:call(foo, bar, [ 4, 5 ]),
    ?assertEqual(2, get_call_counter()).

clean_test(_Config) ->
    make_a_lot_of_calls(),
    UsedSpace = erlymemo:used_space(),
    ?assert(UsedSpace > 0),
    erlymemo:clean(),
    NewUsedSpace = erlymemo:used_space(),
    ?assertEqual(0, NewUsedSpace).

parallel_test(_Config) ->
    Parent = self(),
    PidList = [ spawn_link(
        fun() ->
            [ erlymemo:call(foo, Fun, [ Alpha, Beta ])
              ||  Fun   <- [ bar, baz, qux ],
                  Alpha <- [ random:uniform(10) || _J <- lists:seq(1, 50) ],
                  Beta  <- [ random:uniform(10) || _K <- lists:seq(1, 50) ]
            ],
            Parent ! { ok, self() }
        end)
        || _I <- lists:seq(1, 100) ],
    lists:foreach(
        fun(Pid) ->
            receive
                { ok, Pid } -> ok
            after
                30000 -> throw(timeout)
            end
        end,
        PidList).

invalid_function_test(_Config) ->
    ?assertError(undef, erlymemo:call(nomodule, nofunction, [1,2,3])).

invalid_arguments_test(_Config) ->
    ?assertError(badarith, erlymemo:call(lists, sum, [ [1,2,3,bang] ])).

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_a_lot_of_calls() ->
    MaxSize = erlymemo:table_max_size(),
    [ erlymemo:call(sets, from_list, [ [X] ]) || X <- lists:seq(1, MaxSize*2) ].

zero_call_counter() ->
    ets:insert(?MODULE, {calls, 0}).

inc_call_counter() ->
    ets:update_counter(?MODULE, calls, {2, 1}).

get_call_counter() ->
    [{_, Calls}] = ets:lookup(?MODULE, calls),
    Calls.
