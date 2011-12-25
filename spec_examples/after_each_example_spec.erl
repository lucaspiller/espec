-module(after_each_example_spec).
-include("espec.hrl").

-export([spec/0]).

append(Atom) ->
  case get(after_each_example_spec) of
    undefined ->
      put(after_each_example_spec, [Atom]);
    List ->
      put(after_each_example_spec, List ++ [Atom])
  end.

spec() ->
  [
    ?_describe("after spec", [
        ?_after(each, fun() ->
            append(ran_after)
        end),

        ?_it("should do stuff", fun() ->
            append(example)
        end),

        ?_describe("nestedspec", [
            ?_after(each, fun() ->
                  append(ran_after_nested)
            end),

            ?_it("should do nested stuff", fun() ->
                  append(nested_example)
            end)
         ])
    ])
].
