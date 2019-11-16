%%%-------------------------------------------------------------------
%% @doc blueamber_mempool public API
%% @end
%%%-------------------------------------------------------------------

-module(blueamber_mempool_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("~s~n", ["Hello"]),
    {ok, Pid} = blueamber_mempool_sup:start_link(),
    supervisor:start_child(Pid, memtissue:as_child(0)).

stop(_State) ->
    ok.

%% internal functions
