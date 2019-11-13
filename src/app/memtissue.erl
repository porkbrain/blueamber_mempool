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
%%% @end
%%%-----------------------------------------------------------------------------
-module(memtissue).
-behavior(gen_event).

-include("../prelude.hrl").

-export([insert/1, get/1, flush/0, start_link/0]).
-export([init/1, handle_call/2, handle_event/2]).

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
%%% Exported functions
%%%

%%------------------------------------------------------------------------------
%% @doc Initializes new memory tissue which given capacity.
%%
%% @end
%%------------------------------------------------------------------------------

-spec start_link() -> {'ok', pid()} | {'error', any()}.

start_link() ->
    gen_server:start_link(?MODULE, {}, []).

%%------------------------------------------------------------------------------
%% @doc Inserts a new element to one of the memcells. Every now and then this
%% all results in scaling the memtissue.
%%
%% @end
%%------------------------------------------------------------------------------

-spec insert(element()) -> ok | {error, term()}.

insert(Element) -> gen_event:call(memtissue, ?MODULE, {insert, Element}).

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

get(N) -> gen_event:call(memtissue, ?MODULE, {get, N}).

%%------------------------------------------------------------------------------
%% @doc Memtissue keeps cached list of memcells pids in memory. Occasionally one
%% of the cells will be in restarting state when capturing its pid. When this
%% happens, the memtissue skips the restarting cell. A workaround for this is to
%% reload the array of pids every now and then.
%%
%% @end
%%------------------------------------------------------------------------------

-spec flush() -> ok.

flush() -> gen_event:notify(memtissue, ?MODULE, flush).

%%%
%%% Callback functions from gen_server
%%%

init(_Args) ->
    Supervisor = memcell_sup:start_link(?MEMCELL_CAPACITY),
    {ok, _} = supervisor:start_child(Supervisor, []),
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
            supervisor:start_child(State#memtissue.supervisor, []),
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

%% Picks a cell on random and creates a callback which the consumer can use to
%% get N random messages from.
handle_call({get, N}, State) ->
    Cell = random_cell(State),
    Callback = fun() -> memcell:get(N, Cell) end,
    {reply, {ok, Callback}, State}.

%% Reloads memcells by reading supervisor's children and loadng the pids into
%% an array.
handle_event(flush, State) ->
    Pids = memcell_sup:which_children(State#memtissue.supervisor),
    {ok, State#memtissue { memcells=Pids }}.

%%%
%%% Local functions
%%%

-spec random_cell(#memtissue{}) -> pid().

random_cell(State) ->
    Children = array:size(State#memtissue.memcells),
    RandomCellIndex = erlang:system_time(milisecond) rem Children,
    % Checks that the cell is not in restarting state.
    case array:get(RandomCellIndex, State#memtissue.memcells) of
        restarting -> random_cell(State);
        % Checks that the cell hasn't died.
        Pid -> case is_process_alive(Pid) of
            false -> random_cell(State);
            true -> Pid
        end
    end.
