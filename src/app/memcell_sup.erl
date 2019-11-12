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

-export([start_link/1, child_spec/1, which_children/1]).
-export([init/1]).

%%%
%%% Exported functions
%%%

%%------------------------------------------------------------------------------
%% @doc Reexports starting link functionality by supervisor behavior.
%%
%% @end
%%------------------------------------------------------------------------------

-spec start_link(capacity()) -> {ok, pid()} | ignore | {error, supervisor:startlink_err()}.

start_link(Capacity) ->
    supervisor:start_link(?MODULE, [Capacity]).

%%------------------------------------------------------------------------------
%% @doc Exports child spec for supervisor.
%%
%% @end
%%------------------------------------------------------------------------------
-spec child_spec(integer()) -> supervisor:child_spec().

child_spec(Capacity) when Capacity > 0 -> #{
    id => cell,
    start => {memcell, start_link, [Capacity]},
    shutdown => brutal_kill,
    type => worker
}.

%%------------------------------------------------------------------------------
%% @doc Takes all supervisor children and puts them into a list.
%%
%% @end
%%------------------------------------------------------------------------------
-spec which_children(pid()) -> [pid() | restarting].

which_children(Supervisor) ->
    Children = supervisor:which_children(Supervisor),
    lists:map(fun({_, Pid, _, _}) -> Pid end, Children).

%%%
%%% Callback functions from supervisor
%%%

init(Capacity)->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpecs = [child_spec(Capacity)],
    {ok, {SupFlags, ChildSpecs}}.
