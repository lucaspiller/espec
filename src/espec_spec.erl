-module(espec_spec).

-export([spec/0]).

-include("espec.hrl").

% example
spec() ->
    [
        ?_describe("square", [
                ?_before(each, fun() ->
                            ok
                    end),

                ?_it("should have four sides"),

                ?_it("should have sides of the same length"),

                ?_it("should do stuff", fun() ->
                            A = 1,
                            B = 2,
                            A = 2
                    end)
            ])
    ].
