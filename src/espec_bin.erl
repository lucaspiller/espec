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
    Modules = lists:foldl(fun({SpecFile, Line}, Acc) ->
        case compile_spec_file(SpecFile) of
            {ok, Module} ->
                [{Module, Line} | Acc];
            error ->
                % errors and warnings are automatically printed to stdout
                Acc
        end
    end, [], SpecFiles),

    %% Add ebin to the load path (when running as a standalone binary)
    %% TODO add a command line option to allow custom load paths
    code:add_patha("ebin"),

    case file:list_dir("deps") of
        {ok, Filenames} ->
            lists:foreach(fun(Name) -> code:add_patha("deps/" ++ Name ++ "/ebin") end, Filenames);
        _ ->
            no_deps_dir
    end,

    espec:run(Modules).

-spec espec_macros() -> list(term()).
espec_macros() ->
    [
        {
            'assertEqual',
            ['Expected', 'Expression'],
            "espec_helper:assert_equal(Expected, Expression, ?LINE, (??Expression))",
            complex
        },
        {
            'assertMatch',
            ['Guard', 'Expression'],
            "espec_helper:assert_match(Guard, Expression, ?LINE, (??Expression))",
            complex
        }
    ].

-spec compile_spec_file(string()) -> {ok, term()} | error.
compile_spec_file(SpecFile) ->
    ParseOptions = [
        {includes, ["ebin", "include"]},
        {macros, espec_macros()}
    ],
    CompileOptions = [
        {parse_transform, espec_transform},
        nowarn_unused_vars,
        verbose,
        report,
        export_all
    ],
    case epp2:parse_file(SpecFile, ParseOptions) of
        {ok, Forms} ->
            case compile:forms(Forms, CompileOptions) of
                {ok, Module, Binary} ->
                    {module, Module} = code:load_binary(Module, SpecFile, Binary),
                    {ok, Module};
                error ->
                    io:format("Aborted due to compile error!~n"),
                    %% errors and warnings are automatically printed to stdout
                    error
            end;
        error ->
            io:format("Aborted due to parse error!~n"),
            %% errors and warnings are automatically printed to stdout
            error
    end.
