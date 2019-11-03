%% Main boot point of the program.
%%
%% @author:
-module(main).

%% Boot function has to be always exported with zero arity.
-export([start/0]).

-import(app_helloworld, [greet/0]).

%% This is where it all starts.
start() ->
  {ok, Greet} = app_helloworld:greet(),
  io:format("~p\n", [Greet]).
