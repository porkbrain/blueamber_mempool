-module(memtissue_sup_spec).

-include_lib("eunit/include/eunit.hrl").

check_childspecs_test() ->
    ChildSpec = memtissue_sup:child_spec(4),
    ?assertEqual(ok, supervisor:check_childspecs([ChildSpec])).

start_with_no_children_test() ->
    {ok, Pid} = memcell_sup:start_link(4),
    Children = supervisor:which_children(Pid),
    ?assertEqual(0, length(Children)).

has_four_children_test() ->
    {ok, Pid} = memtissue_sup:start_link(),
    {ok, MemtissuePid} = supervisor:start_child(Pid, [4]),
    Children = supervisor:which_children(Pid),
    ?assertEqual(1, length(Children)),
    [{_, ChildPid, _, _} | _ ] = Children,
    ?assertEqual(MemtissuePid, ChildPid),
    ?assertEqual(true, is_process_alive(MemtissuePid)).
