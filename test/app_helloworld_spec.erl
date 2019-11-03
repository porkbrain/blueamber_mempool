-module(app_helloworld_spec).

-include_lib("eunit/include/eunit.hrl").

-import(app_helloworld, [greet/0]).

it_greets_test() ->
  Greet = app_helloworld:greet(),
  ?assertEqual({ok, "Hello World!"}, Greet).
