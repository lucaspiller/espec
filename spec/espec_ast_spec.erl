-module(espec_ast_spec).
-include("espec.hrl").

spec() ->
  describe("#convert_to_execution_tree", fun() ->
        it("should return example in execution tree format", fun() ->
              Fun = fun() -> ok end,
              AST = [
                {group, 1, "example spec", [
                  {example, 2, "example1", Fun}
                ]}
              ],
              [
                {start_group, 1, "example spec"},
                {start_example, 2, "example1"},
                {run, Fun},
                {end_example, 2, "example1"},
                {end_group, 1, "example spec"}
              ] = espec_ast:convert_to_execution_tree(AST)
        end),

        it("should return pending example in execution tree format", fun() ->
              AST = [
                {group, 1, "example spec", [
                  {pending, 2, "example1"}
                ]}
              ],
              [
                {start_group, 1, "example spec"},
                {pending_example, 2, "example1"},
                {end_group, 1, "example spec"}
              ] = espec_ast:convert_to_execution_tree(AST)
        end),

        it("should return multiple examples in execution tree format", fun() ->
              Fun1 = fun() -> ok end,
              Fun2 = fun() -> ok end,
              AST = [
                {group, 1, "example spec", [
                  {example, 2, "example1", Fun1},
                  {example, 3, "example2", Fun2}
                ]}
              ],
              [
                {start_group, 1, "example spec"},
                {start_example, 2, "example1"},
                {run, Fun1},
                {end_example, 2, "example1"},
                {start_example, 3, "example2"},
                {run, Fun2},
                {end_example, 3, "example2"},
                {end_group, 1, "example spec"}
              ] = espec_ast:convert_to_execution_tree(AST)
        end),

      describe("filters", fun() ->
            it("should return before each filters in execution tree format"),
            it("should return after each filters in execution tree format"),
            it("should return before all filters in execution tree format"),
            it("should return after all filters in execution tree format")
      end)
  end).
%   [{group,254,"after_all spec",
%       [{after_,255,all,#Fun<before_after_filter_spec.44.53736782>},
%         {example,259,"should do stuff",
%           #Fun<before_after_filter_spec.45.19509813>},
%         {group,263,"nestedspec",
%           [{after_,264,all,#Fun<before_after_filter_spec.46.81668172>},
%             {example,268,"should do nested stuff",
%               #Fun<before_after_filter_spec.47.50609279>}]}]}]
