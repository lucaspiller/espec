-module(espec_spec).
-include("espec.hrl").
-export([spec/0]).

spec() ->
  [
    ?_describe("before and afters", [

        ?_it("should run before each before examples in current group and nested groups", fun() ->
              espec:run(before_each_example_spec, espec_null_listener:new(), espec_null_listener),
              [ran_before, example, ran_before, ran_before_nested, nested_example] = get(before_each_example_spec)
        end),

        ?_it("should run after each after examples in current group and nested group", fun() ->
              espec:run(after_each_example_spec, espec_null_listener:new(), espec_null_listener),
              [example, ran_after, nested_example, ran_after_nested, ran_after] = get(after_each_example_spec)
        end),

        ?_it("should run before all once before all examples in the current group and nested groups", fun() ->
              espec:run(before_all_example_spec, espec_null_listener:new(), espec_null_listener),
              [ran_before_all, example, ran_before_all_nested, nested_example] = get(before_all_example_spec) 
        end),

        ?_it("should run after all once after all examples in the current group and nested groups", fun() ->
              espec:run(after_all_example_spec, espec_null_listener:new(), espec_null_listener),
              [example, nested_example, ran_after_all_nested, ran_after_all] = get(after_all_example_spec)
        end)
    ])
].
