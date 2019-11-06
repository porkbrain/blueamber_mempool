%%%-----------------------------------------------------------------------------
%%% @doc Supervises memcells with `simple_one_for_one` strategy. New memcells
%%% are spawned empty with default capacity which can be configured in prelude.
%%%
%%% @author Michael Bausano
%%% @end
%%%-----------------------------------------------------------------------------
-module(memcell_sup).
-behaviour(supervisor).

-include("../prelude.hrl").

-export([start_link/0]).
-export([init/1]).

%%%-----------------------------------------------------------------------------
%%% @doc Reexports starting link functionality by supervisor behavior.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-type startlink_err() :: {already_started, pid()} | {shutdown, term()} | term().

-spec start_link() -> {ok, pid()} | ignore | {error, startlink_err()}.

start_link() ->
    supervisor:start_link(memcell_sup, []).

%%%
%%% Callback functions from gen_server
%%%

%% Boots a supervisor with default cell capacity.
init(_Args)->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpecs = [#{
        id => memcell,
        start => {memcell, start_link, [?MEMCELL_CAPACITY]},
        shutdown => brutal_kill
    }],
    {ok, {SupFlags, ChildSpecs}}.
