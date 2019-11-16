%%%-----------------------------------------------------------------------------
%%% @doc Supervises memtissue with `simple_one_for_one` strategy. There usually
%%% will only be one process this supervisor has to take care of.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(memtissue_sup).
-behaviour(supervisor).

-include("../prelude.hrl").

-export([start_link/0, child_spec/1, as_child/1]).
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
    supervisor:start_link(memtissue_sup, []).

%%------------------------------------------------------------------------------
%% @doc Exports itself as child spec for higher level supervisor.
%%
%% @end
%%------------------------------------------------------------------------------
-spec as_child(term()) -> supervisor:child_spec().

as_child(Id) -> #{
    id => Id,
    start => {memtissue_sup, start_link, []},
    type => supervisor
}.

%%------------------------------------------------------------------------------
%% @doc Exports child spec for a new child with given id.
%%
%% @end
%%------------------------------------------------------------------------------
-spec child_spec(term()) -> supervisor:child_spec().

child_spec(Id) -> #{
    id => Id,
    start => {memtissue, start_link, []}
}.

%%%
%%% Callback functions from supervisor
%%%

init(_Args)->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpecs = [child_spec(0)],
    {ok, {SupFlags, ChildSpecs}}.
