-module(memcell_sup_spec).

-include_lib("eunit/include/eunit.hrl").

check_childspecs_test() ->
    ChildSpec = memcell_sup:child_spec(4),
    ?assertEqual(ok, supervisor:check_childspecs([ChildSpec])).

start_with_no_children_test() ->
    {ok, Pid} = memcell_sup:start_link(4),
    Children = memcell_sup:which_children(Pid),
    ?assertEqual(0, length(Children)).

has_four_children_test() ->
    {ok, Pid} = memcell_sup:start_link(4),
    {ok, Child1} = supervisor:start_child(Pid, []),
    {ok, Child2} = supervisor:start_child(Pid, []),
    {ok, Child3} = supervisor:start_child(Pid, []),
    {ok, Child4} = supervisor:start_child(Pid, []),
    Children = memcell_sup:which_children(Pid),
    ?assertEqual(4, length(Children)),
    ?assertEqual([Child1 | [Child2 | [Child3 | [Child4]]]], Children),
    true = is_process_alive(Child1),
    true = is_process_alive(Child2),
    true = is_process_alive(Child3),
    true = is_process_alive(Child4).
