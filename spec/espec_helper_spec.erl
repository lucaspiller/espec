-module(espec_helper_spec).
-include("espec.hrl").

spec() ->
  describe("setting and getting values", fun() ->

    it("should return the last value that was set", fun() ->
          espec_helper:spec_set(key, "value1"),
          espec_helper:spec_set(key, "value2"),
          ?assertEqual("value2", espec_helper:spec_get(key))
    end),

    it("should return undefined if no value was set", fun() ->
          ?assertEqual(undefined, espec_helper:spec_get(key))
    end)
  end).
