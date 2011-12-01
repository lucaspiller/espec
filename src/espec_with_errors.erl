-module(espec_with_errors).
-include("espec.hrl").

-export([spec/0]).

spec() ->
  [
    ?_describe("failbefore", [
        ?_before(each, fun() ->
              throw(fail_in_before)
        end),

        ?_it("should do stuff", fun() ->
              io:format("output from: it should do stuff~n", [])
        end),


        ?_after(each, fun() ->
              throw(fail_in_after)
        end)
    ]),

    ?_describe("failinafter", [
        ?_it("should do stuff", fun() ->
              io:format("output from: it should do stuff~n", [])
        end),

        ?_after(each, fun() ->
              throw(fail_in_after)
        end)
    ]),

   ?_describe("nestedfailing", [
      ?_before(each, fun() ->
            throw(fail_in_before)
      end),

      ?_describe("nested", [
          ?_before(each, fun() ->
                throw(fail_in_nested_before)
          end),

          ?_it("should do nested stuff", fun() ->
                io:format("output from: it should do nested stuff ~n", [])
          end)
      ])
  ]),

   ?_describe("nestedfailing", [
      ?_before(each, fun() ->
            io:format("in outer before", [])
      end),

      ?_describe("nested", [
          ?_before(each, fun() ->
                throw(fail_in_nested_before)
          end),

          ?_it("should do nested stuff", fun() ->
                io:format("output from: it should do nested stuff ~n", [])
          end)
      ])
  ])


].
