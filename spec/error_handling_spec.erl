-module(error_handling_spec).
-include("espec.hrl").

spec() ->
  describe("badmatch errors", fun() ->
        it("should be caught and returned", fun() ->
              State = espec:run_spec(error_example_spec, badmatch_error_example_spec(), espec_null_listener:new(), espec_null_listener),
              {error, {error, {badmatch,false}, _}} = proplists:get_value("should fail", State)
          end)
    end),

  describe("throws", fun() ->
        it("should be caught and returned", fun() ->
              State = espec:run_spec(error_example_spec, throw_error_example_spec(), espec_null_listener:new(), espec_null_listener),
              {error, {throw, something_went_wrong, _}} = proplists:get_value("should fail", State)
          end)
    end),

  describe("exits", fun() ->
        it("should be caught and returned", fun() ->
              State = espec:run_spec(error_example_spec, exit_error_example_spec(), espec_null_listener:new(), espec_null_listener),
              {error, {exit, goodbye, _}} = proplists:get_value("should fail", State)
          end)
    end).

%
% Example specs for testing
%

badmatch_error_example_spec() ->
  describe("error spec", fun() ->
      it("should fail", fun() ->
            A = true,
            B = false,
            A = B
      end)
  end).

throw_error_example_spec() ->
  describe("error spec", fun() ->
      it("should fail", fun() ->
            throw(something_went_wrong)
      end)
  end).

exit_error_example_spec() ->
  describe("error spec", fun() ->
      it("should fail", fun() ->
            exit(goodbye)
      end)
  end).
