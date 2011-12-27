-module(expand_args_spec).
-include("espec.hrl").

spec() ->
    describe("processing command line arguments", fun() ->
        it("should recursively expand a directory", fun() ->
            Examples = espec_bin:expand_files_from_args(["spec"]),
            ?_assertEqual(true, lists:any(fun(File) ->
                  File == {"spec/expand_args_spec.erl", all}
              end, Examples))
        end),

      it("should process line number for files", fun() ->
            Examples = lists:sort(espec_bin:expand_files_from_args(["spec/expand_args_spec:45", "spec/before_after_filter_spec.erl"])),
            ?_assertEqual(lists:sort([{"spec/expand_args_spec", 45}, {"spec/before_after_filter_spec.erl", all}]), Examples)
        end)
    end).
