-module(espec_ast).
-export([
    convert_to_execution_tree/1
  ]).

convert_to_execution_tree(AST) ->
  lists:reverse(convert_to_execution_tree(AST, [])).

convert_to_execution_tree([], Parsed) ->
  lists:flatten(Parsed);
convert_to_execution_tree([{group, Line, Description, Body} | AST], Parsed) ->
  BodyExecutionTree = convert_to_execution_tree(Body, []),
  convert_to_execution_tree(AST, [
    {end_group, Line, Description},
    BodyExecutionTree,
    {start_group, Line, Description}
    | Parsed
  ]);
convert_to_execution_tree([{example, Line, Description, Fun} | AST], Parsed) ->
  convert_to_execution_tree(AST, [
    {end_example, Line, Description},
    {run, Fun},
    {start_example, Line, Description}
    | Parsed
  ]);
convert_to_execution_tree([{pending, Line, Description} | AST], Parsed) ->
  convert_to_execution_tree(AST, [
    {pending_example, Line, Description}
    | Parsed
  ]).
