-module(helper_spec).
-include("espec.hrl").

spec() ->
  describe("spec helper", fun() ->
      describe("assertions", fun() ->
            describe("assert equal", fun() ->
                  it("should not raise an exception when matching", fun() ->
                        State = espec:run_spec(error_example_spec, assert_equal_example_spec(), espec_null_listener:new(), espec_null_listener),
                        ?assertEqual(ok, proplists:get_value("should return ok", State))
                    end),

                  it("should raise an exception when not matching", fun() ->
                        State = espec:run_spec(error_example_spec, assert_equal_example_spec(), espec_null_listener:new(), espec_null_listener),
                        ?assertMatch({error, {error, {
                              assertEqual_failed,
                              [
                                {line, _},
                                {expression, _},
                                {expected, true},
                                {got, false}
                              ]
                            }, _}}, proplists:get_value("should fail", State))
                    end)
              end),

            describe("assert match", fun() ->
                  it("should not raise an exception when matching", fun() ->
                        State = espec:run_spec(error_example_spec, assert_match_example_spec(), espec_null_listener:new(), espec_null_listener),
                        ?assertEqual(ok, proplists:get_value("should return ok", State))
                    end),

                  it("should raise an exception when not matching", fun() ->
                        State = espec:run_spec(error_example_spec, assert_match_example_spec(), espec_null_listener:new(), espec_null_listener),
                        ?assertMatch({error, {error, {
                              assertMatch_failed,
                              [
                                {line, _},
                                {expression, "List"},
                                {expected, "[ a , _ ]"},
                                {got, [a, b, c]}
                              ]
                            }, _}}, proplists:get_value("should fail", State))
                    end)
              end)
        end)
  end).

assert_equal_example_spec() ->
  describe("assert equal spec", fun() ->
      it("should return ok", fun() ->
            A = true,
            B = true,
            ?assertEqual(A, B)
      end),

      it("should fail", fun() ->
            A = true,
            B = false,
            ?assertEqual(A, B)
      end)
  end).

assert_match_example_spec() ->
  describe("assert match spec", fun() ->
      it("should return ok", fun() ->
            List = [a, b, c],
            ?assertMatch([a, _, c], List)
      end),

      it("should fail", fun() ->
            List = [a, b, c],
            ?assertMatch([a, _], List)
      end)
  end).
