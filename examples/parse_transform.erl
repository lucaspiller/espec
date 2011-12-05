-module(parse_transform).
-include("espec.hrl").
-compile([{parse_transform, espec_transform}]).

-export([spec/0]).

myfun() ->
  [{foo, bar}].

spec() ->
  describe("beforespec", fun() ->
        before_all(fun() ->
              io:format("before all~n", [])
        end),

        before_each(fun() ->
              io:format("before each~n", [])
        end),

        it("should do stuff", fun() ->
              io:format("output from: it should do stuff~n", [])
        end),

        describe("nested spec", fun() ->
              it("should do nested stuff", fun() ->
                    io:format("output from: it should do nested stuff~n", [])
              end)
        end),

        after_each(fun() ->
              io:format("after each~n", [])
        end),

        after_all(fun() ->
              io:format("after all~n", [])
        end)
  end).
