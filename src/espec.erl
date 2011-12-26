-module(espec).

-export([
        run/1,
        run/2,
        run/3,
        run_spec/2,
        run_spec/4,
        filter_groups_by_line/2
    ]).

run(Mods) when is_list(Mods) ->
    lists:foreach(fun(Mod) ->
                run(Mod)
        end, Mods);

run(Mod) when is_atom(Mod) ->
    Spec = Mod:spec(),
    run_spec(Mod, Spec).

run(Mod, ListenerState, ListenerModule) ->
    Spec = Mod:spec(),
    run_spec(Mod, Spec, ListenerState, ListenerModule).

run(Mod, LineNo) ->
    Spec = filter_groups_by_line(LineNo, Mod:spec()),
    run_spec(Mod, Spec).

run_spec(Mod, Spec) ->
    run_spec(Mod, Spec, espec_console_listener:new(), espec_console_listener).

run_spec(Mod, Spec, ListenerState0, ListenerModule) ->
    ExecutionTree = espec_ast:convert_to_execution_tree(Spec),
    ListenerState1 = ListenerModule:start_spec(Mod, ListenerState0),
    ListenerState2 = run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, ok),
    ListenerModule:end_spec(Mod, ListenerState2).

run_execution_tree([], ListenerState0, _, _) ->
    ListenerState0;
run_execution_tree([{start_group, _Line, GroupDescription} | ExecutionTree], ListenerState0, ListenerModule, _) ->
    ListenerState1 = ListenerModule:start_group(GroupDescription, ListenerState0),
    run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, ok);
run_execution_tree([{end_group, _Line, GroupDescription} | ExecutionTree], ListenerState0, ListenerModule, _) ->
    ListenerState1 = ListenerModule:end_group(GroupDescription, ListenerState0),
    run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, ok);

run_execution_tree([{pending_example, _Line, ExampleDescription} | ExecutionTree], ListenerState0, ListenerModule, _) ->
    ListenerState1 = ListenerModule:pending_example(ExampleDescription, ListenerState0),
    run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, ok);
run_execution_tree([{start_example, _Line, ExampleDescription} | ExecutionTree], ListenerState0, ListenerModule, _) ->
    ListenerState1 = ListenerModule:start_example(ExampleDescription, ListenerState0),
    run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, ok);
run_execution_tree([{end_example, _Line, ExampleDescription} | ExecutionTree], ListenerState0, ListenerModule, Result) ->
    ListenerState1 = ListenerModule:end_example(ExampleDescription, Result, ListenerState0),
    run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, ok);

run_execution_tree([{run, Fun} | ExecutionTree], ListenerState0, ListenerModule, ok) ->
    Result = execute_test(Fun),
    run_execution_tree(ExecutionTree, ListenerState0, ListenerModule, Result);
run_execution_tree([{run, _} | ExecutionTree], ListenerState0, ListenerModule, Error) ->
    % Don't run a fun if there are existing errors
    run_execution_tree(ExecutionTree, ListenerState0, ListenerModule, Error).

execute_test(Fun) ->
    try
        Fun(),
        ok
    catch
        Class:Reason ->
            {error, {Class, Reason, erlang:get_stacktrace()}}
    end.

is_setup(Part) ->
    Specifier = element(1, Part),
    Specifier =:= before_ orelse Specifier =:= after_.

filter_groups_by_line(Line, Groups) ->
    filter_by_line(Line, 999999999999999999999, Groups).

filter_element_by_line(Line, EndLine, {group, StartLine, Description, Body}) ->
    {group, StartLine, Description, filter_by_line(Line, EndLine, Body)};

filter_element_by_line(_Line, _EndLine, Part) ->
    Part.

filter_by_line(_Line, _EndLine, []) ->
    [];

filter_by_line(Line, EndLine, Body) ->
    PartsWithEndLines = lists:zip(Body, [ element(2, Part) || Part <- tl(Body) ] ++ [EndLine]),
    HasMatchingPart = lists:any(
        fun({Part, PartEndLine}) -> 
            not is_setup(Part) andalso element(2, Part) =< Line andalso PartEndLine > Line
        end, PartsWithEndLines),
    case HasMatchingPart of
        true ->
            FilteredParts = lists:filter(fun({Part, PartEndLine}) ->
                is_setup(Part) orelse (element(2, Part) =< Line andalso PartEndLine > Line)
            end, PartsWithEndLines),
            lists:map(fun({Part, PartEndLine}) ->
                filter_element_by_line(Line, PartEndLine, Part)
            end, FilteredParts);
        false ->
            lists:map(fun({Part, _EndLine}) -> Part end, PartsWithEndLines)
     end.
