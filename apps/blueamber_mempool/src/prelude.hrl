%%------------------------------------------------------------------------------
%% @doc Reexporting std array type.
%%
%% @end
%%------------------------------------------------------------------------------
-type array() :: term().

%%------------------------------------------------------------------------------
%% @doc Memory cell has to be larger than 0. It defines how many elements does
%%      the cell store. The capacity also effects accumulator which when hitting
%%      the capacity, discards old messages and uses new ones.
%%
%% @end
%%------------------------------------------------------------------------------
-type capacity() :: integer().

%%------------------------------------------------------------------------------
%% @doc The type of element that this cell stores. All elements in the
%%      accumulator and in the memory should be of the same type.
%%
%% @end
%%------------------------------------------------------------------------------
-type element() :: term().

%%------------------------------------------------------------------------------
%% @doc Number of messages to be retrieve or that has been retrieved.
%%
%% @end
%%------------------------------------------------------------------------------
-type number_of_elements() :: integer().

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

%%%-----------------------------------------------------------------------------
%%% @doc Mempool scales number of memcells automatically according to a simple
%%% linear rule. This value sets the maximum number of cells that can be spawned
%%% by the mempool
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-define(MAX_MEMCELLS, 256).
