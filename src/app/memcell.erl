%%%-----------------------------------------------------------------------------
%%% @doc To store elements, mempool runs memory cell processes. Their main
%%% purpose is to separate concern between agents which check that the incoming
%%% requests are valid. A memory cell accepts two types of messages:
%%% - get ("read") messages retrieve one or more elements from the memory cell
%%%     at random
%%% - insert ("write") messages to the accumulator that, once it reaches the
%%%     capacity of the cell in size, is used as the memory and an empty
%%%     accumulator is created
%%%
%%% @author Michael Bausano
%%% @end
%%%-----------------------------------------------------------------------------
-module(memcell).
-behavior(gen_server).

-include("../prelude.hrl").

-export([start/2, get/2, insert/2]).
-export([init/1, handle_call/3, handle_cast/2]).

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
-type element() :: any().

%%------------------------------------------------------------------------------
%% @doc Initializes new memory cell which given capacity.
%%
%% @param capacity() Max elements in memory
%% @param element() Default element whch will be used to fill the initial memory
%%
%% @end
%%------------------------------------------------------------------------------
-spec start(capacity(), element()) -> any().

-record(memcell, {
    cap,
    init=false,
    mem,
    size
}).

start(Capacity, Element) ->
    gen_server:start(?MODULE, {Capacity, Element}, []).

get(N, Pid) -> gen_server:call(Pid, {get, N}).

insert(Element, Pid) -> gen_server:cast(Pid, {insert, Element}).

%% Starts a new memory cell with given capacity and welcome element.
init({Capacity, Element}) when Capacity > 0 ->
    Memory = array:set(0, Element, array:new(Capacity, fixed)),
    {ok, #memcell{cap=Capacity, size=1, mem=Memory}}.

%% If the requested number of elements is larger or equal to the capacity, the
%% memcell returns all elements in the memory. Since all messages are returned,
%% the memcell has to be fully initialized. That means all memory slots have to
%% be valid elements instead of default "undefined".
handle_call({get, N}, _, State = #memcell{cap=Capacity, mem=Memory, init=true})
    when N >= State#memcell.cap ->
    ?PRINT("HMMM"),
    {reply, {Capacity, array:to_list(Memory)}, State};

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

%% Picks N elements from Source array at random.
random_elements(Source, Size, N) -> random_elements(Source, Size, N, []).

%% We the list is of expected size, return it.
random_elements(_Source, _Size, 0, List) -> List;

%% If we only have one process, we copy it N types.
random_elements(Source, 1, N, List) ->
    Head = array:get(0, Source),
    random_elements(Source, 1, N - 1, [ Head | List]);

%% Get next random element, insert it into the list and call itself.
random_elements(Source, Size, N, List) ->
    %% Get a random integer in interval <0; Size - 1>. We will use this
    %% integer to access a random element.
    Index = rand:uniform(Size) - 1,
    Element = array:get(Index, Source),
    random_elements(Source, Size, N - 1, [Element | List]).
