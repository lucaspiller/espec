-module(espec).

-export([
        run/1
    ]).

run(Mods) when is_list(Mods) ->
    lists:foreach(fun(Mod) ->
                run(Mod)
        end, Mods);
run(Mod) when is_atom(Mod) ->
    Spec = Mod:spec(),
    lists:foreach(fun({Description, Children}) ->
        run_group(0, Description, [], [], Children)
    end, extract_groups(Spec)).

run_group(Indentation, GroupDescription, Befores, Afters, Children) ->
    io:format(user, "~s~s\n", [indentation(Indentation), GroupDescription]),
    BeforeEach = Befores ++ extract_before_each(Children),
    AfterEach = Afters ++ extract_after_each(Children),
    run_before(extract_before_all(Children)),
    lists:foreach(fun
            ({example, _Line, Description, Fun}) ->
                case run_test(Fun, BeforeEach, AfterEach) of
                    ok ->
                        io:format(user, "~s~s\n", [indentation(Indentation + 1), Description]);
                    {error, {Class, Reason}} ->
                        io:format(user, "~s~s (FAILED):\n~s~p ~p\n", [indentation(Indentation + 1), Description, indentation(Indentation + 2), Class, Reason])
                end;
            ({pending, _Line, Description}) ->
                io:format(user, "~s~s (PENDING)\n", [indentation(Indentation + 1), Description])
    end, extract_tests(Children)),

    lists:foreach(fun({Description, SubChildren}) ->
        run_group(Indentation + 1, Description, BeforeEach, AfterEach, SubChildren)
    end, extract_groups(Children)),
    run_after(extract_after_all(Children)).

extract_before_all([]) ->
    [];
extract_before_all([{before_, _Line, all, Fun} | _]) ->
    [Fun];
extract_before_all([_ | Rem]) ->
    extract_before_all(Rem).

extract_after_all([]) ->
    [];
extract_after_all([{after_, _Line, all, Fun} | _]) ->
    [Fun];
extract_after_all([_ | Rem]) ->
    extract_after_all(Rem).

extract_before_each([]) ->
    [];
extract_before_each([{before_, _Line, each, Fun} | _]) ->
    [Fun];
extract_before_each([_ | Rem]) ->
    extract_before_each(Rem).

extract_after_each([]) ->
    [];
extract_after_each([{after_, _Line, each, Fun} | _]) ->
    [Fun];
extract_after_each([_ | Rem]) ->
    extract_after_each(Rem).

extract_tests(Tests) ->
    extract_tests(Tests, []).
extract_tests([], Matched) ->
    lists:reverse(Matched);
extract_tests([{example, Line, Description, Fun} | Rem], Matched) ->
    extract_tests(Rem, [{example, Line, Description, Fun} | Matched]);
extract_tests([{pending, Line, Description} | Rem], Matched) ->
    extract_tests(Rem, [{pending, Line, Description} | Matched]);
extract_tests([_ | Rem], Matched) ->
    extract_tests(Rem, Matched).

extract_groups(Groups) ->
    extract_groups(Groups, []).
extract_groups([], Matched) ->
    lists:reverse(Matched);
extract_groups([{group, _Line, Description, Body} | Rem], Matched) ->
    extract_groups(Rem, [{Description, Body} | Matched]);
extract_groups([_ | Rem], Matched) ->
    extract_groups(Rem, Matched).

run_test(Fun, Before, After) ->
    try
        run_before(Before),
        Fun(),
        run_after(After),
        ok
    catch
        throw:{before_failed, {Class, Reason}} ->
            {error, {before_failed, {Class, Reason}}};
        throw:{after_failed, {Class, Reason}} ->
            {error, {after_failed, {Class, Reason}}};
        Class:Reason ->
            {error, {Class, Reason}}
    end.

run_before(Funs) ->
    run_functions(before_failed, Funs).

run_after(Funs) ->
    run_functions(after_failed, lists:reverse(Funs)).


run_functions(Error, Funs) ->
    try
        lists:map(fun(F) -> F() end, Funs)
    catch
        Class:Reason ->
            throw({Error, {Class, Reason}})
    end.

indentation(Depth) ->
    lists:duplicate(Depth * 2, " ").
