-module(expand_args_spec).
-include("espec.hrl").

spec() ->
    describe("processing command line arguments", fun() ->
        it("should recursively expand a directory", fun() ->
            Examples = espec_bin:expand_files_from_args(["spec"]),
            true = lists:any(fun(File) ->
                  File == "spec/expand_args_spec.erl"
            end, Examples)
        end)
    end).
