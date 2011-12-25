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
              it("should return before each filters in execution tree format", fun() ->
                    Filter = fun() -> ok end,
                    Fun1 = fun() -> ok end,
                    Fun2 = fun() -> ok end,
                    AST = [
                      {group, 1, "example spec", [
                          {before_, 1, each, Filter},
                          {example, 2, "example1", Fun1},
                          {example, 3, "example2", Fun2}
                        ]}
                    ],
                    [
                      {start_group, 1, "example spec"},
                      {start_example, 2, "example1"},
                      {run, Filter},
                      {run, Fun1},
                      {end_example, 2, "example1"},
                      {start_example, 3, "example2"},
                      {run, Filter},
                      {run, Fun2},
                      {end_example, 3, "example2"},
                      {end_group, 1, "example spec"}
                    ] = espec_ast:convert_to_execution_tree(AST)
                end),

              it("should return after each filters in execution tree format", fun() ->
                    Filter = fun() -> ok end,
                    Fun1 = fun() -> ok end,
                    Fun2 = fun() -> ok end,
                    AST = [
                      {group, 1, "example spec", [
                          {after_, 1, each, Filter},
                          {example, 2, "example1", Fun1},
                          {example, 3, "example2", Fun2}
                        ]}
                    ],
                    [
                      {start_group, 1, "example spec"},
                      {start_example, 2, "example1"},
                      {run, Fun1},
                      {run, Filter},
                      {end_example, 2, "example1"},
                      {start_example, 3, "example2"},
                      {run, Fun2},
                      {run, Filter},
                      {end_example, 3, "example2"},
                      {end_group, 1, "example spec"}
                    ] = espec_ast:convert_to_execution_tree(AST)
                end),

              it("should return before all filters in execution tree format", fun() ->
                    Filter = fun() -> ok end,
                    Fun1 = fun() -> ok end,
                    Fun2 = fun() -> ok end,
                    AST = [
                      {group, 1, "example spec", [
                          {before_, 1, all, Filter},
                          {example, 2, "example1", Fun1},
                          {example, 3, "example2", Fun2}
                        ]}
                    ],
                    [
                      {start_group, 1, "example spec"},
                      {run, Filter},
                      {start_example, 2, "example1"},
                      {run, Fun1},
                      {end_example, 2, "example1"},
                      {start_example, 3, "example2"},
                      {run, Fun2},
                      {end_example, 3, "example2"},
                      {end_group, 1, "example spec"}
                    ] = espec_ast:convert_to_execution_tree(AST)
                end),

              it("should return after all filters in execution tree format", fun() ->
                    Filter = fun() -> ok end,
                    Fun1 = fun() -> ok end,
                    Fun2 = fun() -> ok end,
                    AST = [
                      {group, 1, "example spec", [
                          {after_, 1, all, Filter},
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
                      {run, Filter},
                      {end_group, 1, "example spec"}
                    ] = espec_ast:convert_to_execution_tree(AST)
                end)
          end)
    end).
