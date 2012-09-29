-module(espec).

-export([
        run/1,
        run/3,
        run_spec/2,
        run_spec/4,
        filter_groups_by_line/2
    ]).

-record(spec_stats,{
    run = 0,
    failed = 0,
    succeeded = 0,
    pending = 0
  }).

run(Mods) when is_list(Mods) ->
    lists:map(fun(Mod) ->
          run(Mod)
      end, Mods),
    FinalStats = get(stats),
    io:format("~n~n"),
    io:format("Stats: run ~w examples. \e[0;32mSucceeded ~w \e[0;31mFailed ~w \e[0;33mPending  ~w ~n\e[m", [FinalStats#spec_stats.run, FinalStats#spec_stats.succeeded, FinalStats#spec_stats.failed, FinalStats#spec_stats.pending]);

run(Mod) when is_atom(Mod) ->
    Spec = Mod:spec(),
    run_spec(Mod, Spec);

run({Mod, all}) ->
    %% disturbingly we don't seem to need this case statement because
    %% of the way ordering of numerics and atoms work. but we should
    %% keep it because it is bound to create a bug in the future :)
    run(Mod);
run({Mod, LineNo}) ->
    Spec = filter_groups_by_line(LineNo, Mod:spec()),
    run_spec(Mod, Spec).
        
run(Mod, ListenerState, ListenerModule) ->
    Spec = Mod:spec(),
    run_spec(Mod, Spec, ListenerState, ListenerModule).

run_spec(Mod, Spec) ->
    run_spec(Mod, Spec, espec_console_listener:new(), espec_console_listener).

run_spec(Mod, Spec, ListenerState0, ListenerModule) ->
    ExecutionTree = espec_ast:convert_to_execution_tree(Spec),
    ListenerState1 = ListenerModule:start_spec(Mod, ListenerState0),
    pristine_context(),
    {ListenerState2, Stats} = run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, ok, 0, [[]], #spec_stats{}),
    update_stats(Stats),
    ListenerModule:end_spec(Mod, ListenerState2).

pop_error_stack(Result0, FailTestDepth0) ->
    case FailTestDepth0 of
      0 ->
        {ok, 0};
      _ ->
        {Result0, FailTestDepth0 - 1}
    end.
push_error_stack(Result0, FailTestDepth0) ->
    case {Result0, FailTestDepth0} of
       {ok, 0} -> 0;
       _ -> FailTestDepth0 + 1
    end.

run_execution_tree([], ListenerState0, _, _, _, _, SpecStats) ->
    {ListenerState0, SpecStats};
run_execution_tree([{start_group, _Line, GroupDescription} | ExecutionTree], ListenerState0, ListenerModule, Result, FailTestDepth0, ContextStack0, SpecStats) ->
    NewContext = save_context(),
    ListenerState1 = ListenerModule:start_group(GroupDescription, ListenerState0),
    FailTestDepth = push_error_stack(Result, FailTestDepth0),
    run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, Result, FailTestDepth,
      [NewContext | ContextStack0], SpecStats);
run_execution_tree([{end_group, _Line, GroupDescription} | ExecutionTree], ListenerState0, ListenerModule, Result0, FailTestDepth0, [ContextHead|ContextTail], SpecStats) ->
    restore_context(ContextHead),
    ListenerState1 = ListenerModule:end_group(GroupDescription, ListenerState0),
    {Result, FailTestDepth} = pop_error_stack(Result0, FailTestDepth0),
    run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, Result, FailTestDepth, ContextTail, SpecStats);

run_execution_tree([{pending_example, _Line, ExampleDescription} | ExecutionTree], ListenerState0, ListenerModule, Result, FailTestDepth, ContextStack, SpecStats) ->
    ListenerState1 = ListenerModule:pending_example(ExampleDescription, ListenerState0),
    Stats0 = SpecStats#spec_stats{
      run = SpecStats#spec_stats.run + 1,
      pending = SpecStats#spec_stats.pending + 1
    },
    run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, Result, FailTestDepth, ContextStack, Stats0);
run_execution_tree([{start_example, _Line, ExampleDescription} | ExecutionTree], ListenerState0, ListenerModule, Result, FailTestDepth0, ContextStack0, SpecStats) ->
    NewContext = save_context(),
    FailTestDepth = push_error_stack(Result, FailTestDepth0),
    ListenerState1 = ListenerModule:start_example(ExampleDescription, ListenerState0),
    run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, Result, FailTestDepth, [NewContext | ContextStack0], SpecStats);
run_execution_tree([{end_example, _Line, ExampleDescription} | ExecutionTree], ListenerState0, ListenerModule, Result0, FailTestDepth0, [ContextHead|ContextRest], SpecStats) ->
    restore_context(ContextHead), 
    ListenerState1 = ListenerModule:end_example(ExampleDescription, Result0, ListenerState0),
    {Result, FailTestDepth} = pop_error_stack(Result0, FailTestDepth0),
    run_execution_tree(ExecutionTree, ListenerState1, ListenerModule, Result, FailTestDepth, ContextRest, SpecStats);

run_execution_tree([{run, Fun} | ExecutionTree], ListenerState0, ListenerModule, ok, 0 = FailTestDepth, ContextStack, SpecStats) ->
    Stats0 = SpecStats#spec_stats{ run = SpecStats#spec_stats.run + 1},
    Result = execute_test(Fun),
    Stats2 = case Result of
      ok ->
        Stats0#spec_stats{ succeeded = Stats0#spec_stats.succeeded + 1};
      _ ->
        Stats0#spec_stats{ failed = Stats0#spec_stats.failed + 1}
    end,
    run_execution_tree(ExecutionTree, ListenerState0, ListenerModule, Result, FailTestDepth, ContextStack, Stats2);
run_execution_tree([{run, _} | ExecutionTree], ListenerState0, ListenerModule, Error, FailTestDepth, ContextStack, SpecStats) ->
    % Don't run a fun if there are existing errors
    run_execution_tree(ExecutionTree, ListenerState0, ListenerModule, Error, FailTestDepth, ContextStack, SpecStats).

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

pristine_context() ->
  put(espec_context, dict:new()).

save_context() ->
  get(espec_context).

restore_context(Context) ->
  put(espec_context, Context).

update_stats(Stats) ->
    PreviouStats = case get(stats) of
      undefined -> #spec_stats{};
      X -> X
    end,
    ModStats = #spec_stats{
                  run = Stats#spec_stats.run + PreviouStats#spec_stats.run,
                  failed = Stats#spec_stats.failed + PreviouStats#spec_stats.failed,
                  succeeded = Stats#spec_stats.succeeded + PreviouStats#spec_stats.succeeded
                  },
    put(stats, ModStats).
