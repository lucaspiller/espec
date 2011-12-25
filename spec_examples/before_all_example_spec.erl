-module(before_all_example_spec).
-include("espec.hrl").

-export([spec/0]).

append(Atom) ->
  case get(before_all_example_spec) of
    undefined ->
      put(before_all_example_spec, [Atom]);
    List ->
      put(before_all_example_spec, List ++ [Atom])
  end.

spec() ->
  [
    ?_describe("before all spec", [
        ?_before(all, fun() ->
            append(ran_before_all)
        end),

        ?_it("should do stuff", fun() ->
            append(example)
        end),

        ?_describe("nestedspec", [
            ?_before(all, fun() ->
                  append(ran_before_all_nested)
            end),

            ?_it("should do nested stuff", fun() ->
                  append(nested_example)
            end)
         ])
    ])
].
