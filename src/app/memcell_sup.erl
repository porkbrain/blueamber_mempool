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

-export([start_link/0, child_spec/2, which_children/1]).
-export([init/1]).

%%%
%%% Exported functions
%%%

%%------------------------------------------------------------------------------
%% @doc Reexports starting link functionality by supervisor behavior.
%%
%% @end
%%------------------------------------------------------------------------------
% -type startlink_err() :: {already_started, pid()} | {shutdown, term()} | term().

-spec start_link() -> {ok, pid()} | ignore | {error, supervisor:startlink_err()}.

start_link() ->
    supervisor:start_link(memcell_sup, []).

%%------------------------------------------------------------------------------
%% @doc Exports child spec for a new child with given id.
%%
%% @end
%%------------------------------------------------------------------------------
-spec child_spec(term(), integer()) -> supervisor:child_spec().

child_spec(Id, Capacity) when Capacity > 0 -> #{
    id => Id,
    start => {memcell, start_link, [Capacity]},
    shutdown => brutal_kill
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

init({Capacity})->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpecs = [child_spec(0, Capacity)],
    {ok, {SupFlags, ChildSpecs}}.
