-module(before_after_filter_spec).
-include("espec.hrl").

spec() ->
  describe("before each filters", fun() ->
        it("should run once when there is one example", fun() ->
              espec:run_spec(before_each_example_spec, before_each_example1_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_each, example1] = get(before_each1)
          end),

        it("should run twice when there are two examples", fun() ->
              espec:run_spec(before_each_example_spec, before_each_example2_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_each, example1, ran_before_each, example2] = get(before_each2)
          end),

        it("should run before examples in current group and nested groups", fun() ->
              espec:run_spec(before_each_example_spec, before_each_example3_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_each, example, ran_before_each, ran_before_each_nested, nested_example] = get(before_each3)
          end)
    end),

  describe("after each filters", fun() ->
        it("should run once when there is one example", fun() ->
              espec:run_spec(after_each_example_spec, after_each_example1_spec(), espec_null_listener:new(), espec_null_listener),
              [example1, ran_after_each] = get(after_each1)
          end),

        it("should run twice when there are two examples", fun() ->
              espec:run_spec(after_each_example_spec, after_each_example2_spec(), espec_null_listener:new(), espec_null_listener),
              [example1, ran_after_each, example2, ran_after_each] = get(after_each2)
          end),

        it("should run after examples in current group and nested groups", fun() ->
              espec:run_spec(after_each_example_spec, after_each_example3_spec(), espec_null_listener:new(), espec_null_listener),
              [example, ran_after_each, nested_example, ran_after_each_nested, ran_after_each] = get(after_each3)
          end)
    end),

  describe("before all filters", fun() ->
        it("should run once when there is one example", fun() ->
              espec:run_spec(before_all_example_spec, before_all_example1_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_all, example1] = get(before_all1)
          end),

        it("should run once when there are two examples", fun() ->
              espec:run_spec(before_all_example_spec, before_all_example2_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_all, example1, example2] = get(before_all2)
          end),

        it("should run before all once before all examples in the current group and nested groups", fun() ->
              espec:run_spec(before_all_example_spec, before_all_example3_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_all, example, ran_before_all_nested, nested_example] = get(before_all3)
          end)
    end),

  describe("after all filters", fun() ->
        it("should run once when there is one example", fun() ->
              espec:run_spec(after_all_example_spec, after_all_example1_spec(), espec_null_listener:new(), espec_null_listener),
              [example1, ran_after_all] = get(after_all1)
          end),

        it("should run once when there are two examples", fun() ->
              espec:run_spec(after_all_example_spec, after_all_example2_spec(), espec_null_listener:new(), espec_null_listener),
              [example1, example2, ran_after_all] = get(after_all2)
          end),

        it("should run after all once after all examples in the current group and nested groups", fun() ->
              espec:run_spec(after_all_example_spec, after_all_example_spec(), espec_null_listener:new(), espec_null_listener),
              [example, nested_example, ran_after_all_nested, ran_after_all] = get(after_all3)
          end)
    end).

%
% Example specs for testing
%

before_each_example1_spec() ->
  describe("before_each spec", fun() ->
        before_each(fun() ->
              spec_helper:append(ran_before_each, before_each1)
          end),

        it("should do stuff example1", fun() ->
              spec_helper:append(example1, before_each1)
          end)
  end).

before_each_example2_spec() ->
  describe("before_each spec", fun() ->
        before_each(fun() ->
              spec_helper:append(ran_before_each, before_each2)
          end),

        it("should do stuff example1", fun() ->
              spec_helper:append(example1, before_each2)
          end),

        it("should do stuff example2", fun() ->
              spec_helper:append(example2, before_each2)
          end)
  end).

before_each_example3_spec() ->
  describe("before_each spec", fun() ->
        before_each(fun() ->
              spec_helper:append(ran_before_each, before_each3)
          end),

        it("should do stuff", fun() ->
              spec_helper:append(example, before_each3)
          end),

        describe("nestedspec", fun() ->
              before_each(fun() ->
                    spec_helper:append(ran_before_each_nested, before_each3)
                end),

              it("should do nested stuff", fun() ->
                    spec_helper:append(nested_example, before_each3)
              end)

          end)
    end).

after_each_example1_spec() ->
  describe("after_each spec", fun() ->
        after_each(fun() ->
              spec_helper:append(ran_after_each, after_each1)
          end),

        it("should do stuff example1", fun() ->
              spec_helper:append(example1, after_each1)
          end)
  end).

after_each_example2_spec() ->
  describe("after_each spec", fun() ->
        after_each(fun() ->
              spec_helper:append(ran_after_each, after_each2)
          end),

        it("should do stuff example1", fun() ->
              spec_helper:append(example1, after_each2)
          end),

        it("should do stuff example2", fun() ->
              spec_helper:append(example2, after_each2)
          end)
  end).

after_each_example3_spec() ->
  describe("after_each spec", fun() ->
        after_each(fun() ->
              spec_helper:append(ran_after_each, after_each3)
          end),

        it("should do stuff", fun() ->
              spec_helper:append(example, after_each3)
          end),

        describe("nestedspec", fun() ->
              after_each(fun() ->
                    spec_helper:append(ran_after_each_nested, after_each3)
                end),

              it("should do nested stuff", fun() ->
                    spec_helper:append(nested_example, after_each3)
                end)
          end)
    end).

before_all_example1_spec() ->
  describe("before_all spec", fun() ->
        before_all(fun() ->
              spec_helper:append(ran_before_all, before_all1)
          end),

        it("should do stuff example1", fun() ->
              spec_helper:append(example1, before_all1)
          end)
  end).

before_all_example2_spec() ->
  describe("before_all spec", fun() ->
        before_all(fun() ->
              spec_helper:append(ran_before_all, before_all2)
          end),

        it("should do stuff example1", fun() ->
              spec_helper:append(example1, before_all2)
          end),

        it("should do stuff example2", fun() ->
              spec_helper:append(example2, before_all2)
          end)
  end).

before_all_example3_spec() ->
  describe("before_all spec", fun() ->
        before_all(fun() ->
              spec_helper:append(ran_before_all, before_all3)
          end),

        it("should do stuff", fun() ->
              spec_helper:append(example, before_all3)
          end),

        describe("nestedspec", fun() ->
              before_all(fun() ->
                    spec_helper:append(ran_before_all_nested, before_all3)
                end),

              it("should do nested stuff", fun() ->
                    spec_helper:append(nested_example, before_all3)
                end)
          end)
    end).

after_all_example1_spec() ->
  describe("after_all spec", fun() ->
        after_all(fun() ->
              spec_helper:append(ran_after_all, after_all1)
          end),

        it("should do stuff example1", fun() ->
              spec_helper:append(example1, after_all1)
          end)
  end).

after_all_example2_spec() ->
  describe("after_all spec", fun() ->
        after_all(fun() ->
              spec_helper:append(ran_after_all, after_all2)
          end),

        it("should do stuff example1", fun() ->
              spec_helper:append(example1, after_all2)
          end),

        it("should do stuff example2", fun() ->
              spec_helper:append(example2, after_all2)
          end)
  end).

after_all_example_spec() ->
  describe("after_all spec", fun() ->
        after_all(fun() ->
              spec_helper:append(ran_after_all, after_all3)
          end),

        it("should do stuff", fun() ->
              spec_helper:append(example, after_all3)
          end),

        describe("nestedspec", fun() ->
              after_all(fun() ->
                    spec_helper:append(ran_after_all_nested, after_all3)
                end),

              it("should do nested stuff", fun() ->
                    spec_helper:append(nested_example, after_all3)
                end)
          end)
    end).
