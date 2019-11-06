
-define(
    PRINT(Var),
    io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])
).

%%%-----------------------------------------------------------------------------
%%% @doc How many messages at most can each memory cell hold.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-define(MEMCELL_CAPACITY, 1024).
