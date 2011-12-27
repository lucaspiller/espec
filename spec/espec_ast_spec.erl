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
              ?_assertEqual([
                {start_group, 1, "example spec"},
                {start_example, 2, "example1"},
                {run, Fun},
                {end_example, 2, "example1"},
                {end_group, 1, "example spec"}
              ], espec_ast:convert_to_execution_tree(AST))
          end),

        it("should return pending example in execution tree format", fun() ->
              AST = [
                {group, 1, "example spec", [
                    {pending, 2, "example1"}
                  ]}
              ],
              ?_assertEqual([
                {start_group, 1, "example spec"},
                {pending_example, 2, "example1"},
                {end_group, 1, "example spec"}
              ], espec_ast:convert_to_execution_tree(AST))
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
              ?_assertEqual([
                {start_group, 1, "example spec"},
                {start_example, 2, "example1"},
                {run, Fun1},
                {end_example, 2, "example1"},
                {start_example, 3, "example2"},
                {run, Fun2},
                {end_example, 3, "example2"},
                {end_group, 1, "example spec"}
              ], espec_ast:convert_to_execution_tree(AST))
          end),

        describe("filters", fun() ->
              it("should handle multiple before each filters", fun() ->
                    Filter1 = fun() -> ok end,
                    Filter2 = fun() -> ok end,
                    Fun1 = fun() -> ok end,
                    AST = [
                      {group, 1, "example spec", [
                          {before_, 1, each, Filter1},
                          {before_, 2, each, Filter2},
                          {example, 3, "example1", Fun1}
                      ]}
                  ],
                  ?_assertEqual([
                    {start_group, 1, "example spec"},
                    {start_example, 3, "example1"},
                    {run, Filter1},
                    {run, Filter2},
                    {run, Fun1},
                    {end_example, 3, "example1"},
                    {end_group, 1, "example spec"}
                  ], espec_ast:convert_to_execution_tree(AST))
              end),

              it("should handle multiple before all filters", fun() ->
                    Filter1 = fun() -> ok end,
                    Filter2 = fun() -> ok end,
                    Fun1 = fun() -> ok end,
                    AST = [
                      {group, 1, "example spec", [
                          {before_, 1, all, Filter1},
                          {before_, 2, all, Filter2},
                          {example, 3, "example1", Fun1}
                      ]}
                  ],
                  ?_assertEqual([
                    {start_group, 1, "example spec"},
                    {run, Filter1},
                    {run, Filter2},
                    {start_example, 3, "example1"},
                    {run, Fun1},
                    {end_example, 3, "example1"},
                    {end_group, 1, "example spec"}
                  ], espec_ast:convert_to_execution_tree(AST))
              end),

              it("should handle multiple after all filters", fun() ->
                    Filter1 = fun() -> ok end,
                    Filter2 = fun() -> ok end,
                    Fun1 = fun() -> ok end,
                    AST = [
                      {group, 1, "example spec", [
                          {after_, 1, all, Filter1},
                          {after_, 2, all, Filter2},
                          {example, 3, "example1", Fun1}
                      ]}
                    ],
                    ?_assertEqual([
                      {start_group, 1, "example spec"},
                      {start_example, 3, "example1"},
                      {run, Fun1},
                      {end_example, 3, "example1"},
                      {run, Filter2},
                      {run, Filter1},
                      {end_group, 1, "example spec"}
                    ], espec_ast:convert_to_execution_tree(AST))
              end),


              it("should handle multiple after each filters", fun() ->
                    Filter1 = fun() -> ok end,
                    Filter2 = fun() -> ok end,
                    Fun1 = fun() -> ok end,
                    AST = [
                      {group, 1, "example spec", [
                          {after_, 1, each, Filter1},
                          {after_, 2, each, Filter2},
                          {example, 3, "example1", Fun1}
                      ]}
                  ],
                  ?_assertEqual([
                    {start_group, 1, "example spec"},
                    {start_example, 3, "example1"},
                    {run, Fun1},
                    {run, Filter2},
                    {run, Filter1},
                    {end_example, 3, "example1"},
                    {end_group, 1, "example spec"}
                  ], espec_ast:convert_to_execution_tree(AST))
              end),

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
                    ?_assertEqual([
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
                    ], espec_ast:convert_to_execution_tree(AST))
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
                    ?_assertEqual([
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
                    ], espec_ast:convert_to_execution_tree(AST))
                end),

              it("should return before each filters with nested examples in execution tree format", fun() ->
                    Filter = fun() -> ok end,
                    Fun1 = fun() -> ok end,
                    Fun2 = fun() -> ok end,
                    AST = [
                      {group, 1, "example spec", [
                          {before_, 1, each, Filter},
                          {example, 2, "example1", Fun1},
                          {group, 3, "nested example spec", [
                            {example, 4, "example2", Fun2}
                          ]}
                        ]}
                    ],
                    ?_assertEqual([
                      {start_group, 1, "example spec"},
                      {start_example, 2, "example1"},
                      {run, Filter},
                      {run, Fun1},
                      {end_example, 2, "example1"},
                      {start_group, 3, "nested example spec"},
                      {start_example, 4, "example2"},
                      {run, Filter},
                      {run, Fun2},
                      {end_example, 4, "example2"},
                      {end_group, 3, "nested example spec"},
                      {end_group, 1, "example spec"}
                    ], espec_ast:convert_to_execution_tree(AST))
                end),

              it("should return after each filters with nested examples in execution tree format", fun() ->
                    Filter = fun() -> ok end,
                    Fun1 = fun() -> ok end,
                    Fun2 = fun() -> ok end,
                    AST = [
                      {group, 1, "example spec", [
                          {after_, 1, each, Filter},
                          {example, 2, "example1", Fun1},
                          {group, 3, "nested example spec", [
                            {example, 4, "example2", Fun2}
                          ]}
                        ]}
                    ],
                    ?_assertEqual([
                      {start_group, 1, "example spec"},
                      {start_example, 2, "example1"},
                      {run, Fun1},
                      {run, Filter},
                      {end_example, 2, "example1"},
                      {start_group, 3, "nested example spec"},
                      {start_example, 4, "example2"},
                      {run, Fun2},
                      {run, Filter},
                      {end_example, 4, "example2"},
                      {end_group, 3, "nested example spec"},
                      {end_group, 1, "example spec"}
                    ], espec_ast:convert_to_execution_tree(AST))
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
                    ?_assertEqual([
                      {start_group, 1, "example spec"},
                      {run, Filter},
                      {start_example, 2, "example1"},
                      {run, Fun1},
                      {end_example, 2, "example1"},
                      {start_example, 3, "example2"},
                      {run, Fun2},
                      {end_example, 3, "example2"},
                      {end_group, 1, "example spec"}
                    ], espec_ast:convert_to_execution_tree(AST))
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
                    ?_assertEqual([
                      {start_group, 1, "example spec"},
                      {start_example, 2, "example1"},
                      {run, Fun1},
                      {end_example, 2, "example1"},
                      {start_example, 3, "example2"},
                      {run, Fun2},
                      {end_example, 3, "example2"},
                      {run, Filter},
                      {end_group, 1, "example spec"}
                    ], espec_ast:convert_to_execution_tree(AST))
                end)
          end)
    end).
