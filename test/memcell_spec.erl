-module(memcell_spec).

-include_lib("eunit/include/eunit.hrl").

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-define(DEFAULT, first).

get_empty_memcell() ->
    Capacity = 2,
    N = 1,
    {ok, Pid} = memcell:start(Capacity),
    {0, []} = memcell:get(N, Pid).

get_no_elements_test() ->
    Capacity = 2,
    N = 0,
    {ok, Pid} = memcell:start(Capacity, [?DEFAULT]),
    {N, []} = memcell:get(N, Pid).

get_one_random_element_test() ->
    Capacity = 2,
    N = 1,
    {ok, Pid} = memcell:start(Capacity, [?DEFAULT]),
    {N, [?DEFAULT | Tail]} = memcell:get(N, Pid),
    ?assertEqual(N - 1, length(Tail)).

get_two_random_elements_test() ->
    Capacity = 10,
    N = 2,
    {ok, Pid} = memcell:start(Capacity, [?DEFAULT]),
    {N, [?DEFAULT | Tail]} = memcell:get(N, Pid),
    ?assertEqual(N - 1, length(Tail)).

get_capacity_test() ->
    Capacity = 10,
    N = Capacity,
    {ok, Pid} = memcell:start(Capacity, [?DEFAULT]),
    {N, [?DEFAULT | Tail]} = memcell:get(N, Pid),
    ?assertEqual(N - 1, length(Tail)).

get_more_than_capacity_test() ->
    Capacity = 10,
    N = 2 * Capacity,
    {ok, Pid} = memcell:start(Capacity, [?DEFAULT]),
    {N, [?DEFAULT | Tail]} = memcell:get(N, Pid),
    ?assertEqual(N - 1, length(Tail)).

insert_one_element_test() ->
    Capacity = 2,
    Insert = second,
    {ok, Pid} = memcell:start(Capacity, [?DEFAULT]),
    ok = memcell:insert(Insert, Pid),
    {Capacity, Response} = memcell:get(Capacity, Pid),
    ?PRINT(Response),
    HasNew = lists:any(fun(El) -> El == Insert end, Response),
    HasOld = lists:any(fun(El) -> El == ?DEFAULT end, Response),
    ?assertEqual(true, HasNew),
    ?assertEqual(true, HasOld).

insert_over_capacity_test() ->
    Capacity = 2,
    Second = second,
    Third = third,
    Forth = forth,
    {ok, Pid} = memcell:start(Capacity, [?DEFAULT]),
    ok = memcell:insert(Second, Pid),
    ok = memcell:insert(Third, Pid),
    ok = memcell:insert(Forth, Pid),
    {Capacity, Response} = memcell:get(Capacity, Pid),
    HasFirst = lists:any(fun(El) -> El == ?DEFAULT end, Response),
    HasSecond = lists:any(fun(El) -> El == Second end, Response),
    HasThird = lists:any(fun(El) -> El == Third end, Response),
    HasForth = lists:any(fun(El) -> El == Forth end, Response),
    ?assertEqual(false, HasFirst),
    ?assertEqual(false, HasSecond),
    ?assertEqual(true, HasThird),
    ?assertEqual(true, HasForth).
