-module(memcell_spec).

-include_lib("eunit/include/eunit.hrl").

-import(memcell, [start/2, get/2, insert/2]).

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-define(DEFAULT, first).

get_no_elements_test() ->
    Capacity = 2,
    N = 0,
    {ok, Pid} = start(Capacity, ?DEFAULT),
    {N, []} = get(N, Pid).

get_one_random_element_test() ->
    Capacity = 2,
    N = 1,
    {ok, Pid} = start(Capacity, ?DEFAULT),
    {N, [?DEFAULT | Tail]} = get(N, Pid),
    ?assertEqual(N - 1, length(Tail)).

get_two_random_elements_test() ->
    Capacity = 10,
    N = 2,
    {ok, Pid} = start(Capacity, ?DEFAULT),
    {N, [?DEFAULT | Tail]} = get(N, Pid),
    ?assertEqual(N - 1, length(Tail)).

get_capacity_test() ->
    Capacity = 10,
    N = Capacity,
    {ok, Pid} = start(Capacity, ?DEFAULT),
    {N, [?DEFAULT | Tail]} = get(N, Pid),
    ?assertEqual(N - 1, length(Tail)).

get_more_than_capacity_test() ->
    Capacity = 10,
    N = 2 * Capacity,
    {ok, Pid} = start(Capacity, ?DEFAULT),
    {N, [?DEFAULT | Tail]} = get(N, Pid),
    ?assertEqual(N - 1, length(Tail)).

insert_one_element_test() ->
    Capacity = 2,
    Insert = second,
    {ok, Pid} = start(Capacity, ?DEFAULT),
    ok = insert(Insert, Pid),
    {Capacity, Response} = get(Capacity, Pid),
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
    {ok, Pid} = start(Capacity, ?DEFAULT),
    ok = insert(Second, Pid),
    ok = insert(Third, Pid),
    ok = insert(Forth, Pid),
    {Capacity, Response} = get(Capacity, Pid),
    HasFirst = lists:any(fun(El) -> El == ?DEFAULT end, Response),
    HasSecond = lists:any(fun(El) -> El == Second end, Response),
    HasThird = lists:any(fun(El) -> El == Third end, Response),
    HasForth = lists:any(fun(El) -> El == Forth end, Response),
    ?assertEqual(false, HasFirst),
    ?assertEqual(false, HasSecond),
    ?assertEqual(true, HasThird),
    ?assertEqual(true, HasForth).
