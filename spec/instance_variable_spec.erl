-module(instance_variable_spec).
-include("espec.hrl").

increment(undefined) ->
  0;
increment(N) ->
  N + 1.

do_increment(Atom) ->
  Before = spec_get(bar),
  spec_set(bar, increment(Before)),
  spec_helper:append({Atom, Before}, instance_variable_spec).

spec() ->
  describe("instance variables", fun() ->
    it("should properly scope variables", fun() ->
      espec:run_spec(instance_variable_spec, instance_variable_spec(), espec_null_listener:new(), espec_null_listener),
      Output = get(instance_variable_spec),
      ?assertEqual(
        [{before_all, undefined},
         {before_each, 0},
         {example, 1},
         {after_each, 2},
         {nested_before_all, 0},
         {before_each, 1},
         {nested_before_each, 2},
         {nested_example, 3},
         {nested_after_each, 4},
         {after_each, 5},
         {nested_after_all, 1},
         {before_each, 0},
         {example2, 1},
         {after_each, 2},
         {after_all, 0}], Output)
    end)
  end).





instance_variable_spec() ->
  describe("foo", fun() ->
    before_all(fun() ->
      do_increment(before_all)
    end),

    before_each(fun() ->
      do_increment(before_each)
    end),

    it("should do stuff", fun() ->
      do_increment(example)
    end),

    describe("nested stuff", fun() ->
      before_all(fun() ->
        do_increment(nested_before_all)
      end),

      before_each(fun() ->
        do_increment(nested_before_each)
      end),

      it("should do nested stuff", fun() ->
        do_increment(nested_example)
      end),

      after_each(fun() ->
        do_increment(nested_after_each)
      end),

      after_all(fun() ->
        do_increment(nested_after_all)
      end)
    end),

    it("should do more stuff", fun() ->
      do_increment(example2)
    end),
  
    after_each(fun() ->
      do_increment(after_each)
    end),

    after_all(fun() ->
      do_increment(after_all)
    end)
  end).




      
