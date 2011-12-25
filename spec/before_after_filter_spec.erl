-module(before_after_filter_spec).
-include("espec.hrl").

spec() ->
  describe("each filters", fun() ->
        it("should run before each before examples in current group and nested groups", fun() ->
              espec:run_spec(before_each_example_spec, before_each_example_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_each, example, ran_before_each, ran_before_each_nested, nested_example] = get(before_each)
          end),

        it("should run after each after examples in current group and nested group", fun() ->
              espec:run_spec(after_each_example_spec, after_each_example_spec(), espec_null_listener:new(), espec_null_listener),
              [example, ran_after_each, nested_example, ran_after_each_nested, ran_after_each] = get(after_each)
          end)
    end),

  describe("all filters", fun() ->
        it("should run before all once before all examples in the current group and nested groups", fun() ->
              espec:run_spec(before_all_example_spec, before_all_example_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_all, example, ran_before_all_nested, nested_example] = get(before_all)
          end),

        it("should run after all once after all examples in the current group and nested groups", fun() ->
              espec:run_spec(after_all_example_spec, after_all_example_spec(), espec_null_listener:new(), espec_null_listener),
              [example, nested_example, ran_after_all_nested, ran_after_all] = get(after_all)
          end)
    end).

%
% Example specs for testing
%

% TODO something built in for this?
append(Atom, Name) ->
  case get(Name) of
    undefined ->
      put(Name, [Atom]);
    List ->
      put(Name, List ++ [Atom])
  end.

before_each_example_spec() ->
  describe("before_each spec", fun() ->
        before_each(fun() ->
              append(ran_before_each, before_each)
          end),

        it("should do stuff", fun() ->
              append(example, before_each)
          end),

        describe("nestedspec", fun() ->
              before_each(fun() ->
                    append(ran_before_each_nested, before_each)
                end),

              it("should do nested stuff", fun() ->
                    append(nested_example, before_each)
                end)
          end)
    end).

after_each_example_spec() ->
  describe("after_each spec", fun() ->
        after_each(fun() ->
              append(ran_after_each, after_each)
          end),

        it("should do stuff", fun() ->
              append(example, after_each)
          end),

        describe("nestedspec", fun() ->
              after_each(fun() ->
                    append(ran_after_each_nested, after_each)
                end),

              it("should do nested stuff", fun() ->
                    append(nested_example, after_each)
                end)
          end)
    end).

before_all_example_spec() ->
  describe("before_all spec", fun() ->
        before_all(fun() ->
              append(ran_before_all, before_all)
          end),

        it("should do stuff", fun() ->
              append(example, before_all)
          end),

        describe("nestedspec", fun() ->
              before_all(fun() ->
                    append(ran_before_all_nested, before_all)
                end),

              it("should do nested stuff", fun() ->
                    append(nested_example, before_all)
                end)
          end)
    end).

after_all_example_spec() ->
  describe("after_all spec", fun() ->
        after_all(fun() ->
              append(ran_after_all, after_all)
          end),

        it("should do stuff", fun() ->
              append(example, after_all)
          end),

        describe("nestedspec", fun() ->
              after_all(fun() ->
                    append(ran_after_all_nested, after_all)
                end),

              it("should do nested stuff", fun() ->
                    append(nested_example, after_all)
                end)
          end)
    end).
