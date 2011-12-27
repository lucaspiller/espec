-module(pending_example_spec).
-include("espec.hrl").

spec() ->
  describe("pending examples", fun() ->
        it("should be recorded", fun() ->
              State = espec:run_spec(pending_example_spec, pending_example_spec(), espec_null_listener:new(), espec_null_listener),
              ?assertEqual(pending, proplists:get_value("should do stuff", State))
          end),
        it("should handle pending block", fun() ->
              State = espec:run_spec(pending_example_using_pending_block_spec, pending_example_using_pending_block_spec(), espec_null_listener:new(), espec_null_listener),
              ?assertEqual({pending, "get shit working"}, proplists:get_value("should do stuff", State))
         end),

         it("should print pending description to the console", fun() ->
              pending("need to have console listener with special io handling")
         end)
    end).


%
% Example specs for testing
%

pending_example_spec() ->
  describe("pending spec", fun() ->
      it("should do stuff")
  end).

pending_example_using_pending_block_spec() ->
  describe("pending spec", fun() ->
      it("should do stuff", fun() ->
          pending("get shit working")
      end)
  end).
