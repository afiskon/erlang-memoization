-module(not_started_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        call_test,
        used_space_test,
        free_space_test,
        table_max_size_test
    ].

call_test(_Config) ->
    List = [1,2,3],
    ?assertError(badarg, erlymemo:call(erlang, length, [ List ])).

used_space_test(_Config) ->
    ?assertThrow({not_started, erlymemo}, erlymemo:used_space()).

free_space_test(_Config) ->
    ?assertExit({noproc, _} , erlymemo:free_space()).

table_max_size_test(_Config) ->
    ?assertExit({noproc, _} , erlymemo:table_max_size()).

