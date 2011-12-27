-module(espec_console_listener).
-behaviour(espec_listener).
-export([start_spec/2, end_spec/2, start_example/2, end_example/3, pending_example/2, start_group/2, end_group/2, new/0]).

-record(state, {color = false, indentation = 0}).

new() ->
  Color = not is_running_in_shell(),
  #state{color = Color}.

start_spec(Name, #state{indentation = Indentation} = State) ->
  io:format(user, "~s~s~n", [indentation(Indentation), Name]),
  State#state{indentation = Indentation + 1}.

end_spec(_Name, #state{indentation = Indentation} = State) ->
  State#state{indentation = Indentation - 1}.

start_example(_Description, State) ->
  State.

end_example(Description, Result, #state{indentation = Indentation} = State) ->
  case Result of 
    ok ->
      io:format(user, "~s~s~s~s\n", [success_color(State), indentation(Indentation + 1), Description, no_color(State)]);
    {pending, PendingDescription} ->

      io:format(user, "~s~s~s (PENDING: ~s)~s\n", [pending_color(State), indentation(Indentation + 1), Description, PendingDescription, no_color(State)]);
    {error, {Class, Reason, Stacktrace}} ->
      io:format(user, "~s~s~s (FAILED):\n", [failure_color(State), indentation(Indentation + 1), Description]),
      print_error(Class, Reason, Stacktrace, Indentation),
      io:format(user, "~s~n", [no_color(State)])
  end,
  State.

print_error(error, {assertEqual_failed, Options}, _, Indentation) ->
  io:format(user, "~sassertEqual (line ~p) failed\n", [indentation(Indentation + 2), proplists:get_value(line, Options)]),
  io:format(user, "~sExpression:~n~s~s~n", [indentation(Indentation + 2), indentation(Indentation + 4), proplists:get_value(expression, Options)]),
  io:format(user, "~sExpected:~n~s~p~n", [indentation(Indentation + 2), indentation(Indentation + 4), proplists:get_value(expected, Options)]),
  io:format(user, "~sGot:~n~s~p~n", [indentation(Indentation + 2), indentation(Indentation + 4), proplists:get_value(got, Options)]);
print_error(error, {assertMatch_failed, Options}, _, Indentation) ->
  io:format(user, "~sassertMatch (line ~p) failed\n", [indentation(Indentation + 2), proplists:get_value(line, Options)]),
  io:format(user, "~sExpression:~n~s~s~n", [indentation(Indentation + 2), indentation(Indentation + 4), proplists:get_value(expression, Options)]),
  io:format(user, "~sExpected To Match:~n~s~s~n", [indentation(Indentation + 2), indentation(Indentation + 4), proplists:get_value(expected, Options)]),
  io:format(user, "~sGot:~n~s~p~n", [indentation(Indentation + 2), indentation(Indentation + 4), proplists:get_value(got, Options)]);
print_error(Class, Reason, Stacktrace, Indentation) ->
  io:format(user, "~s~p ~p\n", [indentation(Indentation + 2), Class, Reason]),
  io:format(user, "~s~p", [indentation(Indentation + 2), Stacktrace]).

pending_example(Description, #state{indentation = Indentation} = State) ->
  io:format(user, "~s~s~s (PENDING: Not Yet Implemented)~s\n", [pending_color(State), indentation(Indentation + 1), Description, no_color(State)]),
  State.

start_group(GroupDescription, #state{indentation = Indentation} = State) ->
  io:format(user, "~s~s~n", [indentation(Indentation), GroupDescription]),
  State#state{indentation = Indentation + 1}.


end_group(_Description, #state{indentation = Indentation} = State) ->
  State#state{indentation = Indentation - 1}.

indentation(Depth) ->
  lists:duplicate(Depth * 2, " ").

success_color(#state{color = false}) ->
  "";
success_color(_) ->
  green().

pending_color(#state{color = false}) ->
  "";
pending_color(_) ->
  yellow().

failure_color(#state{color = false}) ->
  "";

failure_color(_) ->
  red().

no_color(#state{color = false}) ->
  "";
no_color(_) ->
  "\e[m".

green() ->
  "\e[0;32m".

red() ->
  "\e[0;31m".

yellow() ->
  "\e[0;33m".

is_running_in_shell() ->
  ProcessInfo = process_info(self()),
  RootPid = list_to_pid("<0.0.0>"),
  case proplists:get_value(links, ProcessInfo) of
    [RootPid] ->
      false;
    _ ->
      true
  end.
