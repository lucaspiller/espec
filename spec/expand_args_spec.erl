-module(expand_args_spec).
-include("espec.hrl").

spec() ->
    describe("processing command line arguments", fun() ->
        it("should recursively expand a directory", fun() ->
            Examples = espec_bin:expand_files_from_args(["examples_dir"]),
            SortedExpectation = lists:sort(["examples_dir/level1a_spec.erl", "examples_dir/level1b_spec.erl", "examples_dir/level2/level2a_spec.erl", "examples_dir/level2/level2b_spec.erl"]),
            SortedExpectation = lists:sort(Examples)
        end)
    end).