-module(memtissue_spec).

-include_lib("eunit/include/eunit.hrl").

%% 25 ms is enough time for the async request to finish.
-define(SLEEP(), timer:sleep(25)).

start_test() ->
    Capacity = 4,
    memtissue:start_link(Capacity),
    {ok, Size} = memtissue:count_cells(),
    ?assertEqual(1, Size),
    %% We have to stop the server in order to restart it in next
    %% test. This means the tests cannot be run in parallel as they
    %% all use the same server name `memtissue`.
    ok = gen_server:stop(memtissue).

get_empty_memtissue_test() ->
    memtissue:start_link(4),
    {ok, Callback} = memtissue:get(2),
    {Len, EmptyList} = Callback(),
    ?assertEqual(0, Len),
    ?assertEqual(0, length(EmptyList)),
    %% Asserts that callback can be called multiple times.
    {Len, EmptyList} = Callback(),
    ?assertEqual(0, Len),
    ?assertEqual(0, length(EmptyList)),
    ok = gen_server:stop(memtissue).

single_cell_test() ->
    Capacity = 4,
    memtissue:start_link(Capacity),
    {ok, Callback} = memtissue:get(2),
    ok = memtissue:insert("test"),
    ?SLEEP(),
    {Len, Elements} = Callback(),
    %% Assert that we can reuse callback after insert.
    ?assertEqual(2, Len),
    ?assertEqual(["test", "test"], Elements),
    ok = gen_server:stop(memtissue).

flush_test() ->
    memtissue:start_link(1),
    %% Asserts that flushing doesn't crash.
    ok = memtissue:flush(),
    ok = gen_server:stop(memtissue).

scaling_test() ->
    Capacity = 1,
    memtissue:start_link(Capacity),
    memtissue:insert("test"),
    memtissue:insert("test"),
    memtissue:insert("test"),
    %% Should be enough time to insert and scale.
    ?SLEEP(),
    {ok, Size} = memtissue:count_cells(),
    ?assert(Size > 1),
    ok = gen_server:stop(memtissue).

