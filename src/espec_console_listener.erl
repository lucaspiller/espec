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
    {error, {Class, Reason, Stacktrace}} ->
      io:format(user, "~s~s~s (FAILED):\n~s~p ~p\n~s~p~s\n", [failure_color(State), indentation(Indentation + 1), Description, indentation(Indentation + 2), Class, Reason, indentation(Indentation + 2), Stacktrace, no_color(State)])
  end,

  State.

pending_example(Description, #state{indentation = Indentation} = State) ->
  io:format(user, "~s~s~s (PENDING)~s\n", [pending_color(State), indentation(Indentation + 1), Description, no_color(State)]),
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
