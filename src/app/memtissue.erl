%%%-----------------------------------------------------------------------------
%%% @doc Mempool process group consists of `N` memcells instances `M`. They have
%%% all the same capacity `C`. We follow a simple rules for scaling and deciding
%%% which memcell should handle the next call.
%%%
%%% To pick a memcell which should handle the next call, we pick a random number
%%% number in interval <0;`N`). Memcell `M`[`N`] will be routed next request.
%%%
%%% To scale the mempool, we use probability. To fill the first instance, we
%%% need `C` insert messages. To fill the second instance, we need 2 * `C`
%%% insert messages as approximately half of those insert messages are going to
%%% be routed to the first cell (based on chance). Generally:
%%%
%%% NewInsertsUntilNewCell = `C` * `N`
%%%
%%% If a cell does not include requested number of messages, we query a random
%%% cell again until
%%% a) The number of messages hasn't changed
%%% b) We collected requested number of messages
%%% This is a mechanism on handling the issue of querying newly spawned node.
%%% Each mempool should have a reasonable env var which limits the number of
%%% messages a client can query in one request.
%%%
%%% @author Michael Bausano
%%% @end
%%%-----------------------------------------------------------------------------
-module(memtissue).
-behavior(gen_event).

-include("../prelude.hrl").

-export([init/1, handle_call/2]).

%%------------------------------------------------------------------------------
%% @doc Holds state for this gen server implementation.
%%
%% @end
%%------------------------------------------------------------------------------
-record(memtissue, {
    supervisor :: pid(),
    memcells :: array(),
    %% How many inserts until next cell.
    until_next_cell=?MEMCELL_CAPACITY :: integer()
}).

%%%
%%% Callback functions from gen_server
%%%

init(_Args) ->
    Supervisor = memcell_sup:start_link(),
    %% Supervisor starts with a single child.
    Pids = memcell_sup:which_children(Supervisor),
    ?PRINT("Starting memtissue with"),
    ?PRINT(Pids),
    State = #memtissue{
        memcells=array:from_list(Pids),
        supervisor=Supervisor
    },
    {ok, State}.

%% Scales the mempool.
handle_call(
    Call = {insert, _},
    State = #memtissue {until_next_cell=1, memcells=Cells}
) ->
    case array:size(Cells) =< ?MAX_MEMCELLS of
        true ->
            %% The ID of the new worker is going to be N as workers IDs start at 0.
            N = array:size(State#memtissue.memcells),
            NewChildSpec = memcell_sup:child_spec(N, ?MEMCELL_CAPACITY),
            supervisor:start_child(State#memtissue.supervisor, NewChildSpec),
            %% How many insert requests until a next new worker.
            UntilNextCell = (N + 1) * ?MEMCELL_CAPACITY,
            handle_call(Call, State#memtissue { until_next_cell=UntilNextCell });
        false ->
            handle_call(Call, State#memtissue { until_next_cell=0 })
    end;

%% If the tissue reached maximum number of cells, we don't do anything special
%% on inserts.
handle_call({insert, Element}, State = #memtissue {until_next_cell=0}) ->
    Pid = random_cell(State),
    ok = memcell:insert(Element, Pid),
    {reply, ok, State};

%% We decrement the state pointer for insert by 1.
handle_call({insert, Element}, State = #memtissue {until_next_cell=N}) ->
    Pid = random_cell(State),
    ok = memcell:insert(Element, Pid),
    {reply, ok, State#memtissue {until_next_cell=N - 1}};

handle_call({get, N}, State) ->
    Pid = random_cell(State),
    {reply, memcell:get(N, Pid), State}.

%% Reloads memcells by reading supervisor statuses and loadng the values into
%% an array.
handle_event(flush, State) ->
    %% WIP
    Pids = memcell_sup:which_children(State#memtissue.supervisor),
    {ok, State}.

%%%
%%% Local functions
%%%

-spec random_cell(#memtissue{}) -> pid().

random_cell(State) ->
    Children = array:size(State#memtissue.memcells),
    RandomCellIndex = erlang:system_time(milisecond) rem Children,
    array:get(RandomCellIndex, State#memtissue.memcells).
