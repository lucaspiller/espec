-module(helper_spec).
-include("espec.hrl").

spec() ->
  describe("spec helper", fun() ->
      describe("assertions", fun() ->
            describe("assert equal", fun() ->
                  it("should not raise an exception when matching", fun() ->
                        State = espec:run_spec(error_example_spec, assert_equal_example_spec(), espec_null_listener:new(), espec_null_listener),
                        ok = proplists:get_value("should return ok", State)
                    end),

                  it("should raise an exception when not matching", fun() ->
                        State = espec:run_spec(error_example_spec, assert_equal_example_spec(), espec_null_listener:new(), espec_null_listener),
                        {error, {error, {
                              assert_equal_failed,
                              {expected, true},
                              {got, false}
                            }, _}} = proplists:get_value("should fail", State)
                    end)
              end)
        end)
  end).

assert_equal_example_spec() ->
  describe("assert equal spec", fun() ->
      it("should return ok", fun() ->
            A = true,
            B = true,
            spec_helper:assert_equal(A, B)
      end),

      it("should fail", fun() ->
            A = true,
            B = false,
            spec_helper:assert_equal(A, B)
      end)
  end).
