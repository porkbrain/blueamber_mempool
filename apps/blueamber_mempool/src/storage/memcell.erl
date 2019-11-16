%%%-----------------------------------------------------------------------------
%%% @doc To store elements, mempool runs memory cell processes. Their main
%%% purpose is to separate concern between agents which check that the incoming
%%% requests are valid. A memory cell accepts two types of elements:
%%% - get ("read") elements retrieve one or more elements from the memory cell
%%%     at random
%%% - insert ("write") elements to the memory cell
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(memcell).
-behavior(gen_server).

-include("../prelude.hrl").

-export([start/1, start/2, start_link/1, start_link/2]).
-export([get/2, insert/2]).
-export([init/1, handle_call/3, handle_cast/2]).

%%------------------------------------------------------------------------------
%% @doc Represents a memory cell's state.
%% cap
%% - immutable integer which defines, throughout the whole life time of a cell
%%      how many elements can it hold at once
%% - a new memory cell will contain an array with cap elements, out of which all
%%      but the first one wil default element are undefined
%% mem
%% - an array of `cap` size, which is on initialization filled with undefined
%%      on all its indicies but the first
%% size
%% - an integer pointing to next message that will be overriden when an insert
%%       request hits the mailbox
%% - it increments from 0 to `cap` and once it reaches `cap`, it goes back to 0
%% - when `init` is "false", random message selection picks from the `mem` only
%%      elements which have lower index than `size`, as above size are undefined
%%      (invalid) elements
%% init
%% - defaults to "false" when a new memory cell is created
%% - boolean flag which indicates whether a cell can gone full circle at least
%%      once, meaning that even the last element holds a valid message
%% - main purpose it to help pattern match more effective implementations
%%
%% @end
%%------------------------------------------------------------------------------
-record(memcell, {
    cap :: capacity(),
    %% TODO: Replace `init` with `array:sparse_size`.
    init=false :: boolean(),
    mem :: array(),
    size :: integer()
}).

%%%
%%% Exported functions
%%%

%%------------------------------------------------------------------------------
%% @doc Initializes new memory cell which given capacity. The provided element
%% is put into the first index of the initial memory. This guarantees that a
%% memory cell is never empty
%%
%% @end
%%------------------------------------------------------------------------------
-spec start(capacity()) -> {'ok', pid()} | {'error', any()}.

start(Capacity) ->
    gen_server:start(?MODULE, {Capacity, []}, []).

-spec start(capacity(), [element()]) -> {'ok', pid()} | {'error', any()}.

start(Capacity, Elements) ->
    gen_server:start(?MODULE, {Capacity, Elements}, []).

-spec start_link(capacity()) -> {'ok', pid()} | {'error', any()}.

start_link(Capacity) ->
    gen_server:start_link(?MODULE, {Capacity, []}, []).

-spec start_link(capacity(), [element()]) -> {'ok', pid()} | {'error', any()}.

start_link(Capacity, Elements) ->
    gen_server:start_link(?MODULE, {Capacity, Elements}, []).

%%------------------------------------------------------------------------------
%% @doc Retrieves list of random elements. Since the requested amount of
%% can be greater than in memory elements (or even capacity) and the elements
%% are picked at random, it is possible that some elements are going to be
%% duplicates.
%%
%% @end
%%------------------------------------------------------------------------------

-spec get(number_of_elements(), pid()) -> {number_of_elements(), [element()]}.

get(N, Pid) -> gen_server:call(Pid, {get, N}).

%%------------------------------------------------------------------------------
%% @doc Inserts new element into the memory. Since the memory cell only has a
%% limited capacity, it will override some previous message that was contained
%% in the cell.
%%
%% @end
%%------------------------------------------------------------------------------
-spec insert(element(), pid()) -> ok.

insert(Element, Pid) -> gen_server:cast(Pid, {insert, Element}).

%%%
%%% Callback functions from gen_server
%%%

init({Capacity}) when Capacity > 0 ->
    Memory = array:new(),
    {ok, #memcell{cap=Capacity, size=0, mem=Memory}};

%% Starts a new memory cell with given capacity and welcome element.
init({Capacity, Elements}) when Capacity > 0 ->
    Memory = array:from_list(Elements),
    {ok, #memcell{cap=Capacity, size=array:size(Memory), mem=Memory}}.

%% If the requested number of elements is larger or equal to the capacity, the
%% memcell returns all elements in the memory. Since all messages are returned,
%% the memcell has to be fully initialized. That means all memory slots have to
%% be valid elements instead of default "undefined".
handle_call({get, N}, _, State = #memcell{cap=Capacity, mem=Memory, init=true})
    when N >= State#memcell.cap ->
    {reply, {Capacity, array:to_list(Memory)}, State};

%% If there are no elements in the cell, return 0 messages.
handle_call({get, _}, _, State = #memcell{size=0, init=false}) ->
    {reply, {0, []}, State};

%% If the memcell is not yet fully initialized (some elements are still
%% "undefined"), the random elements for the response are only selected from the
%% valid elements.
handle_call({get, N}, _, State = #memcell{size=Size, mem=Memory, init=false}) ->
    {reply, {N, random_elements(Memory, Size, N)}, State};

%% If all memcell slots are valid elements, it picks random N elements out of
%% its capacity.
handle_call({get, N}, _, State = #memcell{cap=Capacity, mem=Memory, init=true}) ->
    {reply, {N, random_elements(Memory, Capacity, N)}, State}.

%% Special case for memcell with capacity 1.
handle_cast({insert, Element}, State = #memcell{cap=1, mem=Memory}) ->
    NewState = State#memcell{
        init=true,
        mem=array:set(0, Element, Memory)
    },
    {noreply, NewState};

%% If the memcell is at the verge of its capacity, it resets its size pointer
%% back to 0 and starts writing new elements to the beginning of the array.
%% The element it has received with this call will be positioned last.
handle_cast(
    {insert, Element},
    State = #memcell{cap=Capacity, size=Size, mem=Memory}
) when Size == Capacity - 1 ->
    NewState = State#memcell{
        init=true,
        mem=array:set(Size, Element, Memory),
        size=0
    },
    {noreply, NewState};

%% Write received element over element at position of current size and
%% increments the size by one.
handle_cast({insert, Element}, State = #memcell{mem=Memory, size=Size}) ->
    NewState = State#memcell{
        mem=array:set(Size, Element, Memory),
        size=Size + 1
    },
    {noreply, NewState}.

%%%
%%% Local functions
%%%

%% Picks N elements from Source array at random.
random_elements(Source, Size, N) -> random_elements(Source, Size, N, []).

%% We the list is of expected size, return it.
random_elements(_Source, _Size, 0, List) -> List;

%% If we only have one process, we copy it N types.
random_elements(Source, 1, N, List) ->
    Head = array:get(0, Source),
    random_elements(Source, 1, N - 1, [Head | List]);

%% Get next random element, insert it into the list and call itself.
random_elements(Source, Size, N, List) ->
    %% Get a random integer in interval <0; Size - 1>. We will use this
    %% integer to access a random element.
    Index = rand:uniform(Size) - 1,
    Element = array:get(Index, Source),
    random_elements(Source, Size, N - 1, [Element | List]).
