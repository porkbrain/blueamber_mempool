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
-module(memory_cell).
-behavior(gen_server).

-export([start/2]).
-export([handle_call/3]).


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

%% When a cell is initialized, is it filled with an instance of the provided
%% default element. It adds massive overhead for cell instantiation, however it
%% removes the need of checks for undefined elements.
start(Capacity, Default) when Capacity > 0 ->
    Elements = array:new(Capacity, [
        {default, Default},
        {fixed, true}
    ]),
    Acc = array:new(Capacity, fixed),
    loop(Elements, 0, Acc).

%% If the requested number of elements is larger or equal to the capacity, the
%% memcell returns all elements in the memory. Since all messages are returned,
%% the memcell has to be fully initialized. That means all memory slots have to
%% be valid elements instead of default "undefined".
handle_call({get, N}, _, State = #memcell{cap=Capacity, mem=Memory, init=true})
    when N >= State#memcell.cap ->
    {reply, {Capacity, Memory}, State};

%% If the memcell is not yet fully initialized (some elements are still
%% "undefined"), the random elements for the response are only selected from the
%% valid elements.
handle_call({get, N}, _, State = #memcell{size=Size, mem=Memory, init=false}) ->
    {reply, {N, random_elements(Memory, Size, N)}, State};

%% If all memcell slots are valid elements, it picks random N elements out of
%% its capacity.
handle_call({get, N}, _, State = #memcell{cap=Capacity, mem=Memory, init=true}) ->
    {reply, {N, random_elements(Memory, Capacity, N)}, State}.

%% If the memcell is at the verge of its capacity, it resets its size pointer
%% back to 0 and starts writing new elements to the beginning of the array.
handle_cast(
    {insert, Element},
    State = #memcell{cap=Capacity, size=Capacity, mem=Memory}
) ->
    NewState = State#memcell{
        init=true,
        mem=array:set(0, Element, Memory),
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

loop(Memory, AccSize, Acc) ->
    Capacity = array:size(Memory),
    receive
        %% If we have less elements in memory than the user requested elements,
        %% then we return all elements.
        {get, N, From} when N >= Capacity ->
            From ! {Capacity, Memory};
        %% We pick a random element and add it to the return list. This is
        %% repeated N times.
        {get, N, From} ->
            From ! random_elements(Memory, Capacity, N);
        %% If the accumulator reached the capacity, it is promoted to be the new
        %% memory and a new accumulator is created with the one received element.
        {insert, Element} when AccSize == Capacity ->
            NewAcc = array:new(Capacity, fixed),
            NewAccSize = 1,
            loop(Acc, NewAccSize, array:set(0, Element, NewAcc));
        %% Inserts new element to the memory pool,
        {insert, Element} ->
            loop(Memory, AccSize + 1, array:set(AccSize, Element, Acc))
    end,
    loop(Memory, AccSize, Acc).

%% Picks N elements from Source array at random.
random_elements(Source, Size, N) -> random_elements(Source, Size, N, []).

%% We the list is of expected size, return it.
random_elements(_Source, _Size, 0, List) -> List;

%% Get next random element, insert it into the list and call itself.
random_elements(Source, Size, N, List) ->
    %% Get a random integer in interval <0; Size - 1>. We will use this
    %% integer to access a random element.
    Index = rand:uniform(Size - 1),
    Element = array:get(Index, Source),
    random_elements(Source, Size, N - 1, [Element | List]).
