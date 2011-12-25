-module(espec_listener).
-export([behaviour_info/1]).

behaviour_info(_) ->
   [{start_spec,2},
   {end_spec,2},
   {start_example, 2},
   {end_example, 3},
   {start_group, 2},
   {end_group, 2}].

