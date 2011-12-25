-module(espec_null_listener).
-behaviour(espec_listener).
-export([start_spec/2,end_spec/2,start_example/2, end_example/3, pending_example/2, start_group/2, end_group/2, new/0]).

new() ->
  [].

start_spec(_Name, State) ->
  State.

end_spec(_Name, State) ->
  State.

start_example(_Description, State) ->
  State.

end_example(Description, Result, State) ->
  [{Description, Result} | State].

pending_example(Description, State) ->
  [{Description, pending} | State].

start_group(_GroupDescription, State) ->
  State.

end_group(_Description, State) ->
  State.

