-module(pending_example_spec).
-include("espec.hrl").

spec() ->
  describe("pending examples", fun() ->
        it("should be recorded", fun() ->
              State = espec:run_spec(pending_example_spec, pending_example_spec(), espec_null_listener:new(), espec_null_listener),
              ?assertEqual(pending, proplists:get_value("should do stuff", State))
          end)
    end).

%
% Example specs for testing
%

pending_example_spec() ->
  describe("pending spec", fun() ->
      it("should do stuff")
  end).
