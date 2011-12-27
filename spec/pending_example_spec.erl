-module(pending_example_spec).
-include("espec.hrl").

spec() ->
  describe("pending examples", fun() ->
        it("should be recorded", fun() ->
              State = espec:run_spec(pending_example_spec, pending_example_spec(), espec_null_listener:new(), espec_null_listener),
              pending = proplists:get_value("should do stuff", State),
              pending = proplists:get_value("should do other stuff", State)
          end)
    end),

  describe("pending example with body", fun() ->
        it("should be recorded", fun() ->
            State = espec:run_spec(pending_example_with_body_spec, pending_example_with_body_spec(), espec_null_listener:new(), espec_null_listener),
            pending = proplists:get_value("should do stuff", State)
        end)
  end).

%
% Example specs for testing
%

pending_example_spec() ->
  describe("pending spec", fun() ->
      it("should do stuff"),
      pending("should do other stuff")
  end).


pending_example_with_body_spec() ->
  describe("pending spec", fun() ->
      pending("should do stuff", fun() ->
          ok
      end)
  end).
