%% app_helloworld This module doesn't do much so far.
%%
%% @author:
-module(app_helloworld).

-export([greet/0]).

%% Returns a tuple with an atom and a string.
-spec greet() -> tuple().

greet() -> {ok, "Hello World!"}.
