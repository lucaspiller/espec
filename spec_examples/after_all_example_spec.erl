-module(after_all_example_spec).
-include("espec.hrl").

-export([spec/0]).

append(Atom) ->
  case get(after_all_example_spec) of
    undefined ->
      put(after_all_example_spec, [Atom]);
    List ->
      put(after_all_example_spec, List ++ [Atom])
  end.

spec() ->
  [
    ?_describe("after all spec", [
        ?_after(all, fun() ->
            append(ran_after_all)
        end),

        ?_it("should do stuff", fun() ->
            append(example)
        end),

        ?_describe("nestedspec", [
            ?_after(all, fun() ->
                  append(ran_after_all_nested)
            end),

            ?_it("should do nested stuff", fun() ->
                  append(nested_example)
            end)
         ])
    ])
].
