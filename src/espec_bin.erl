-module(espec_bin).
-export([run_spec_files_from_args/1, expand_files_from_args/1]).
-define(SPEC_FILE_REGEXP, ".*_spec.erl$").

-spec run_spec_files_from_args(list()) -> term().
run_spec_files_from_args(Args) ->
    SpecFiles = expand_files_from_args(Args),
    run_spec_files(SpecFiles).
    
-spec expand_files_from_args(list()) -> list().
expand_files_from_args(FilesOrDirs) ->
    lists:append(lists:map(fun(FileOrDir) ->
        case filelib:is_dir(FileOrDir) of
            true ->
                filelib:fold_files(FileOrDir, ?SPEC_FILE_REGEXP, true, fun(File, Acc) ->
                    [File | Acc]
                end, []);
            false ->
                [FileOrDir]
        end
    end, FilesOrDirs)).

-spec run_spec_files(list()) -> term().
run_spec_files(SpecFiles) ->
    CompileOptions = [
        binary,
        {i, "ebin"},
        {i, "include"},
        report,
        warnings_as_errors
    ],
    Modules = lists:foldl(fun(SpecFile, Acc) ->
        case compile:file(SpecFile, CompileOptions) of
            {ok, Module, Binary} ->
                {module, Module} = code:load_binary(Module, SpecFile, Binary),
                [Module | Acc];
            error ->
                % errors and warnings are automatically printed to stdout
                Acc
        end
    end, [], SpecFiles),
    espec:run(Modules).