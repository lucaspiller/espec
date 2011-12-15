-module(before_each_example_spec).
-include("espec.hrl").

-export([spec/0]).

append(Atom) ->
  case get(before_each_example_spec) of
    undefined ->
      put(before_each_example_spec, [Atom]);
    List ->
      put(before_each_example_spec, List ++ [Atom])
  end.

spec() ->
  [
    ?_describe("beforespec", [
        ?_before(each, fun() ->
            append(ran_before)
        end),

        ?_it("should do stuff", fun() ->
            append(example)
        end),

        ?_describe("nestedspec", [
            ?_before(each, fun() ->
                  append(ran_before_nested)
            end),

            ?_it("should do nested stuff", fun() ->
                  append(nested_example)
            end)
         ])
    ])
].
