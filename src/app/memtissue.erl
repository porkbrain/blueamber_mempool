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
%%% For this module, we picked behaviour of `gen_server`. Gen server provides
%%% useful functionality for handling async requests. Insert requests might
%%% scale the memtissue, which is unnecessary to wait for from client side of
%%% view.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(memtissue).
-behavior(gen_server).

-include("../prelude.hrl").

-export([insert/1, get/1, start_link/1, count_cells/0]).
-export([flush/0]).
-export([init/1, handle_call/3, handle_cast/2]).

%%------------------------------------------------------------------------------
%% @doc Holds state for this gen server implementation.
%%
%% @end
%%------------------------------------------------------------------------------
-record(memtissue, {
    supervisor :: pid(),
    memcells :: array(),
    %% How many inserts until next cell.
    until_next_cell :: integer(),
    %% How many elements does a single cell store at most.
    memcell_capacity :: integer()
}).

%%%
%%% Exported functions
%%%

%%------------------------------------------------------------------------------
%% @doc Initializes new memory tissue with given capacity.
%%
%% @end
%%------------------------------------------------------------------------------

-spec start_link(capacity()) -> ok.

start_link(Capacity) ->
    %% TODO: Save statistics/trace/log to file based on environment.
    %% See http://erlang.org/doc/man/gen_server.html#start_link-2 for more
    %% information on how this can be achieved with debug options.
    {ok, _} = gen_server:start_link({local, memtissue}, ?MODULE, Capacity, []).

%%------------------------------------------------------------------------------
%% @doc Inserts a new element to one of the memcells. Every now and then this
%% all results in scaling the memtissue.
%%
%% @end
%%------------------------------------------------------------------------------

-spec insert(element()) -> ok | {error, term()}.

insert(Element) -> gen_server:cast(memtissue, {insert, Element}).

%%------------------------------------------------------------------------------
%% @doc Returns a callback function which can be used as callback to get N
%% messages. The main reason for this function to be a callback is to avoid
%% blocking the memtissue process.
%%
%% Example:
%% ```
%% N = 10,
%% {ok, Callback} = memtissue:get(N),
%% {Len, Messages} = Callback().
%% ```
%%
%% @end
%%------------------------------------------------------------------------------

-spec get(number_of_elements()) -> {ok, fun(() -> [element()])}.

get(N) -> gen_server:call(memtissue, {get, N}).


%%------------------------------------------------------------------------------
%% @doc Counts how many memcells are there currently running (or being
%% restarted).
%%
%% @end
%%------------------------------------------------------------------------------

-spec count_cells() -> {ok, integer()}.

count_cells() -> gen_server:call(memtissue, count_cells).

%%------------------------------------------------------------------------------
%% @doc Memtissue keeps cached list of memcells pids in memory. Occasionally one
%% of the cells will be in restarting state when capturing its pid. When this
%% happens, the memtissue skips the restarting cell. A workaround for this is to
%% reload the array of pids every now and then.
%%
%% @end
%%------------------------------------------------------------------------------

-spec flush() -> ok.

flush() -> gen_server:cast(memtissue, flush).

%%%
%%% Callback functions from gen_server
%%%

init(Capacity) ->
    {ok, Supervisor} = memcell_sup:start_link(Capacity),
    {ok, _} = supervisor:start_child(Supervisor, []),
    %% Supervisor starts with a single child.
    Pids = memcell_sup:which_children(Supervisor),
    State = #memtissue{
        memcell_capacity=Capacity,
        memcells=array:from_list(Pids),
        supervisor=Supervisor,
        until_next_cell=Capacity
    },
    {ok, State}.

%% Scales the mempool.
handle_cast(
    Call = {insert, _},
    State = #memtissue {until_next_cell=1, memcells=Cells, memcell_capacity=Cap}
) ->
    case array:size(Cells) =< ?MAX_MEMCELLS of
        true ->
            %% The ID of the new worker is going to be N as workers IDs start at 0.
            N = array:size(State#memtissue.memcells),
            supervisor:start_child(State#memtissue.supervisor, []),
            %% How many insert requests until a next new worker.
            UntilNextCell = (N + 1) * Cap,
            ok = flush(),
            handle_cast(Call, State#memtissue { until_next_cell=UntilNextCell });
        false ->
            handle_cast(Call, State#memtissue { until_next_cell=0 })
    end;

%% If the tissue reached maximum number of cells, we don't do anything special
%% on inserts.
handle_cast({insert, Element}, State = #memtissue {until_next_cell=0}) ->
    Pid = random_cell(State),
    ok = memcell:insert(Element, Pid),
    {noreply, State};

%% We decrement the state pointer for insert by 1.
handle_cast({insert, Element}, State = #memtissue {until_next_cell=N}) ->
    Pid = random_cell(State),
    ok = memcell:insert(Element, Pid),
    {noreply, State#memtissue {until_next_cell=N - 1}};

%% Reloads memcells by reading supervisor's children and loadng the pids into
%% an array.
handle_cast(flush, State) ->
    Pids = memcell_sup:which_children(State#memtissue.supervisor),
    {noreply, State#memtissue { memcells=array:from_list(Pids) }}.

%% Picks a cell on random and creates a callback which the consumer can use to
%% get N random messages from.
handle_call({get, N}, _, State) ->
    Cell = random_cell(State),
    Callback = fun() -> memcell:get(N, Cell) end,
    {reply, {ok, Callback}, State};

%% Counts how many cells are there in the array at the moment.
handle_call(count_cells, _, State = #memtissue {memcells=Cells}) ->
    {reply, {ok, array:size(Cells)}, State}.

%%%
%%% Local functions
%%%

-spec random_cell(#memtissue{}) -> pid().

random_cell(State) ->
    Children = array:size(State#memtissue.memcells),
    RandomCellIndex = erlang:system_time(millisecond) rem Children,
    % Checks that the cell is not in restarting state.
    case array:get(RandomCellIndex, State#memtissue.memcells) of
        restarting -> random_cell(State);
        % Checks that the cell hasn't died.
        Pid -> case is_process_alive(Pid) of
            false -> random_cell(State);
            true -> Pid
        end
    end.
