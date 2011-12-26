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
          end),

        it("should run 'outer before' before each example in the nested group", fun() ->
              espec:run_spec(before_each_example_spec, before_each_example4_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_each, example, ran_before_each, ran_before_each_nested, nested_example, ran_before_each, ran_before_each_nested, nested_example2] = get(before_each4)
          end),

        it("should run multiple before each filters", fun() ->
              espec:run_spec(before_each_multiple_before_filter_spec, before_each_multiple_before_filter_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_each, ran_before_each2, example] = get(before_each_multiple_before_filter)
        end),

        it("should run 'outer outer before' and 'outer before' before each example in the doubly nested group", fun() ->
              espec:run_spec(before_each_double_nested_filter_spec, before_each_double_nested_filter_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_each, ran_before_each2, ran_before_each3, example, ran_before_each, ran_before_each2, ran_before_each3, example2] = get(before_each_double_nested_filter)
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
          end),

        it("should run multiple after each filters", fun() ->
              espec:run_spec(after_each_multiple_after_filter_spec, after_each_multiple_after_filter_spec(), espec_null_listener:new(), espec_null_listener),
              [example, ran_after_each2, ran_after_each] = get(after_each_multiple_after_filter)
        end),

        it("should run 'outer after' after each example in the nested group", fun() ->
              espec:run_spec(after_each_nested_spec, after_each_nested_spec(), espec_null_listener:new(), espec_null_listener),
              [example,  ran_after_each2, ran_after_each,
               nested_example, ran_after_each_nested, ran_after_each2, ran_after_each, 
               nested_example2,  ran_after_each_nested, ran_after_each2, ran_after_each] = get(after_each_nested)
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
          end),

        it("should run multiple before all filters", fun() ->
              espec:run_spec(before_all_multiple_before_filter_spec, before_all_multiple_before_filter_spec(), espec_null_listener:new(), espec_null_listener),
              [ran_before_all, ran_before_all2, example] = get(before_all_multiple_before_filter)
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
          end),

        it("should run multiple after all filters", fun() ->
              espec:run_spec(after_all_multiple_after_filter_spec, after_all_multiple_after_filter_spec(), espec_null_listener:new(), espec_null_listener),
              [example, ran_after_all2, ran_after_all] = get(after_all_multiple_after_filter)
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

before_each_example1_spec() ->
  describe("before_each spec", fun() ->
        before_each(fun() ->
              append(ran_before_each, before_each1)
          end),

        it("should do stuff example1", fun() ->
              append(example1, before_each1)
          end)
  end).

before_each_example2_spec() ->
  describe("before_each spec", fun() ->
        before_each(fun() ->
              append(ran_before_each, before_each2)
          end),

        it("should do stuff example1", fun() ->
              append(example1, before_each2)
          end),

        it("should do stuff example2", fun() ->
              append(example2, before_each2)
          end)
  end).

before_each_example3_spec() ->
  describe("before_each spec", fun() ->
        before_each(fun() ->
              append(ran_before_each, before_each3)
          end),

        it("should do stuff", fun() ->
              append(example, before_each3)
          end),

        describe("nestedspec", fun() ->
              before_each(fun() ->
                    append(ran_before_each_nested, before_each3)
                end),

              it("should do nested stuff", fun() ->
                    append(nested_example, before_each3)
                end)

          end)
    end).

before_each_example4_spec() ->
  describe("before_each spec", fun() ->
        before_each(fun() ->
              append(ran_before_each, before_each4)
          end),

        it("should do stuff", fun() ->
              append(example, before_each4)
          end),

        describe("nestedspec", fun() ->
              before_each(fun() ->
                    append(ran_before_each_nested, before_each4)
                end),

              it("should do nested stuff", fun() ->
                    append(nested_example, before_each4)
                end),

              it("should do more nested stuff", fun() ->
                    append(nested_example2, before_each4)
              end)
          end)
    end).

before_each_multiple_before_filter_spec() ->
  describe("before_each spec", fun() ->
        before_each(fun() ->
              append(ran_before_each, before_each_multiple_before_filter)
          end),

        before_each(fun() ->
              append(ran_before_each2, before_each_multiple_before_filter)
        end),

        it("should do stuff", fun() ->
              append(example, before_each_multiple_before_filter)
          end)

    end).

before_each_double_nested_filter_spec() ->
  describe("before_each spec", fun() ->
        before_each(fun() ->
              append(ran_before_each, before_each_double_nested_filter)
          end),

        describe("nested", fun() ->
              before_each(fun() ->
                    append(ran_before_each2, before_each_double_nested_filter)
              end),

              describe("double nested", fun() ->
                    before_each(fun() ->
                          append(ran_before_each3, before_each_double_nested_filter)
                    end),

                  it("should do stuff", fun() ->
                      append(example, before_each_double_nested_filter)
                  end),

                  it("should do more stuff", fun() ->
                      append(example2, before_each_double_nested_filter)
                  end)
              end)
        end)

    end).


after_each_multiple_after_filter_spec() ->
  describe("after_each spec", fun() ->
        after_each(fun() ->
              append(ran_after_each, after_each_multiple_after_filter)
          end),

        after_each(fun() ->
              append(ran_after_each2, after_each_multiple_after_filter)
        end),

        it("should do stuff", fun() ->
              append(example, after_each_multiple_after_filter)
          end)

    end).

after_each_nested_spec() ->
  describe("after_each spec", fun() ->
        after_each(fun() ->
              append(ran_after_each, after_each_nested)
          end),

        after_each(fun() ->
              append(ran_after_each2, after_each_nested)
        end),

        it("should do stuff", fun() ->
              append(example, after_each_nested)
        end),

        describe("nested", fun() ->
              after_each(fun() ->
                    append(ran_after_each_nested, after_each_nested)
              end),

              it("should do nested stuff", fun() ->
                    append(nested_example, after_each_nested)
              end),

              it("should do more nested stuff", fun() ->
                    append(nested_example2, after_each_nested)
              end)
         end)

    end).


after_all_multiple_after_filter_spec() ->
  describe("after_all spec", fun() ->
        after_all(fun() ->
              append(ran_after_all, after_all_multiple_after_filter)
          end),

        after_all(fun() ->
              append(ran_after_all2, after_all_multiple_after_filter)
        end),

        it("should do stuff", fun() ->
              append(example, after_all_multiple_after_filter)
          end)

    end).

before_all_multiple_before_filter_spec() ->
  describe("before_all spec", fun() ->
        before_all(fun() ->
              append(ran_before_all, before_all_multiple_before_filter)
          end),

        before_all(fun() ->
              append(ran_before_all2, before_all_multiple_before_filter)
        end),

        it("should do stuff", fun() ->
              append(example, before_all_multiple_before_filter)
          end)

    end).



after_each_example1_spec() ->
  describe("after_each spec", fun() ->
        after_each(fun() ->
              append(ran_after_each, after_each1)
          end),

        it("should do stuff example1", fun() ->
              append(example1, after_each1)
          end)
  end).

after_each_example2_spec() ->
  describe("after_each spec", fun() ->
        after_each(fun() ->
              append(ran_after_each, after_each2)
          end),

        it("should do stuff example1", fun() ->
              append(example1, after_each2)
          end),

        it("should do stuff example2", fun() ->
              append(example2, after_each2)
          end)
  end).

after_each_example3_spec() ->
  describe("after_each spec", fun() ->
        after_each(fun() ->
              append(ran_after_each, after_each3)
          end),

        it("should do stuff", fun() ->
              append(example, after_each3)
          end),

        describe("nestedspec", fun() ->
              after_each(fun() ->
                    append(ran_after_each_nested, after_each3)
                end),

              it("should do nested stuff", fun() ->
                    append(nested_example, after_each3)
                end)
          end)
    end).

before_all_example1_spec() ->
  describe("before_all spec", fun() ->
        before_all(fun() ->
              append(ran_before_all, before_all1)
          end),

        it("should do stuff example1", fun() ->
              append(example1, before_all1)
          end)
  end).

before_all_example2_spec() ->
  describe("before_all spec", fun() ->
        before_all(fun() ->
              append(ran_before_all, before_all2)
          end),

        it("should do stuff example1", fun() ->
              append(example1, before_all2)
          end),

        it("should do stuff example2", fun() ->
              append(example2, before_all2)
          end)
  end).

before_all_example3_spec() ->
  describe("before_all spec", fun() ->
        before_all(fun() ->
              append(ran_before_all, before_all3)
          end),

        it("should do stuff", fun() ->
              append(example, before_all3)
          end),

        describe("nestedspec", fun() ->
              before_all(fun() ->
                    append(ran_before_all_nested, before_all3)
                end),

              it("should do nested stuff", fun() ->
                    append(nested_example, before_all3)
                end)
          end)
    end).

after_all_example1_spec() ->
  describe("after_all spec", fun() ->
        after_all(fun() ->
              append(ran_after_all, after_all1)
          end),

        it("should do stuff example1", fun() ->
              append(example1, after_all1)
          end)
  end).

after_all_example2_spec() ->
  describe("after_all spec", fun() ->
        after_all(fun() ->
              append(ran_after_all, after_all2)
          end),

        it("should do stuff example1", fun() ->
              append(example1, after_all2)
          end),

        it("should do stuff example2", fun() ->
              append(example2, after_all2)
          end)
  end).

after_all_example_spec() ->
  describe("after_all spec", fun() ->
        after_all(fun() ->
              append(ran_after_all, after_all3)
          end),

        it("should do stuff", fun() ->
              append(example, after_all3)
          end),

        describe("nestedspec", fun() ->
              after_all(fun() ->
                    append(ran_after_all_nested, after_all3)
                end),

              it("should do nested stuff", fun() ->
                    append(nested_example, after_all3)
                end)
          end)
    end).
