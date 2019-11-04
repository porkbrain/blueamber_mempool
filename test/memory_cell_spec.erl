-module(memory_cell_spec).

-include_lib("eunit/include/eunit.hrl").

% -import(memory_cell, [start/2]).

% return_random_element_test() ->
%     Capacity = 2,
%     Default = "a",
%     Pid = spawn(fun() -> start(Capacity, Default) end),
%     Pid ! {get, 1, self()},
%     receive [Head | Tail] ->
%         ?assertEqual(0, length(Tail)),
%         ?assertEqual(Default, Head)
%     end.

% insert_element_test() ->
%     Capacity = 2,
%     Default = "a",
%     Pid = spawn_link(fun() -> start(Capacity, Default) end),
%     Pid ! {insert, "b"},
%     Pid ! {get, 2, self()},
%     receive [Second | [First | Tail]] ->
%         ?assertEqual(0, length(Tail)),
%         ?assertEqual(Default, First),
%         ?assertEqual(Default, Second)

%     end,
%     io:format("well: ~p~n", ["a"]).


% it_receives_one_message() -> ok.
%   ?assertEqual({ok, "Hello World!"}, Greet).
