-module(nested_espec).
-include("espec.hrl").

-export([spec/0]).

spec() ->
  [
    ?_describe("beforespec", [
        ?_before(each, fun() ->
              io:format("before each~n", [])
        end),

        ?_it("should do stuff", fun() ->
              io:format("output from: it should do stuff~n", [])
        end),

        ?_describe("nestedspec", [

            ?_it("should do nested stuff", fun() ->
                  io:format("output from: it should do nested stuff~n", [])
            end)
         ]),

        ?_after(each, fun() ->
              io:format("after each~n", [])
        end)
    ])
].
