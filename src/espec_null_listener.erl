-module(espec_null_listener).
-behaviour(espec_listener).
-export([start_example/2, end_example/3, pending_example/2, start_group/2, end_group/2, new/0]).



new() ->
  null_listener.

start_example(_Description, State) ->
  State.

end_example(_Description, _Result, State) ->
  State.

pending_example(_Description, State) ->
  State.

start_group(_GroupDescription, State) ->
  State.


end_group(_Description, State) ->
  State.

