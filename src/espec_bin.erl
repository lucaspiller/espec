-module(espec_bin).
-export([run_spec_files_from_args/1, expand_files_from_args/1]).
-define(SPEC_FILE_REGEXP, ".*_spec.erl$").

-spec run_spec_files_from_args(list()) -> term().
run_spec_files_from_args(Args) ->
    SpecFiles = expand_files_from_args(Args),
    run_spec_files(SpecFiles).

-spec expand_files_from_args(list()) -> list({string(), integer() | all}).
expand_files_from_args(FilesOrDirs) ->
    lists:flatmap(fun(FileOrDir) ->
        case filelib:is_dir(FileOrDir) of
            true ->
                filelib:fold_files(FileOrDir, ?SPEC_FILE_REGEXP, true, fun(File, Acc) ->
                    [{File, all} | Acc]
                end, []);
            false ->
                [process_file_arg(FileOrDir)]
        end
    end, FilesOrDirs).

-spec process_file_arg(string()) -> {string(), integer() | all}.
process_file_arg(FileArg) ->
    case re:run(FileArg, "(.*):(\\d+)$", [{capture, all_but_first, list}]) of
        {match, [File, Line]} ->
            {File, list_to_integer(Line)};
        nomatch ->
            {FileArg, all}
    end.

-spec run_spec_files(list({string(), integer() | all})) -> term().
run_spec_files(SpecFiles) ->
    CompileOptions = [
        binary,
        {i, "ebin"},
        {i, "include"},
        report,
        warnings_as_errors
    ],
    Modules = lists:foldl(fun({SpecFile, Line}, Acc) ->
        case compile:file(SpecFile, CompileOptions) of
            {ok, Module, Binary} ->
                {module, Module} = code:load_binary(Module, SpecFile, Binary),
                [{Module, Line} | Acc];
            error ->
                % errors and warnings are automatically printed to stdout
                Acc
        end
    end, [], SpecFiles),

    %% Add ebin to the load path (when running as a standalone binary)
    %% TODO add a command line option to allow custom load paths
    code:add_patha("ebin"),

    espec:run(Modules).
