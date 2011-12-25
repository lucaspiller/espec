-module(espec_ast).
-export([
    convert_to_execution_tree/1
  ]).

convert_to_execution_tree(AST) ->
  lists:reverse(convert_to_execution_tree(AST, [])).

convert_to_execution_tree(AST, []) ->
  {Filters, ASTWithoutFilters} = extract_filters_at_current_level(AST, [], []),
  convert_to_execution_tree(ASTWithoutFilters, Filters, []).

convert_to_execution_tree([], _, Parsed) ->
  lists:flatten(Parsed);
convert_to_execution_tree([{group, Line, Description, Body} | AST], Filters, Parsed) ->
  BodyExecutionTree = convert_to_execution_tree(Body, []),
  InstructionTree = [
    {end_group, Line, Description},
    BodyExecutionTree,
    {start_group, Line, Description}
  ],
  convert_to_execution_tree(AST, Filters, [InstructionTree | Parsed]);
convert_to_execution_tree([{example, Line, Description, Fun} | AST], Filters, Parsed) ->
  InstructionTree = [
    {end_example, Line, Description},
    {run, Fun},
    {start_example, Line, Description}
  ],
  convert_to_execution_tree(AST, Filters, [InstructionTree | Parsed]);
convert_to_execution_tree([{pending, Line, Description} | AST], Filters, Parsed) ->
  InstructionTree = [
    {pending_example, Line, Description}
  ],
  convert_to_execution_tree(AST, Filters, [InstructionTree | Parsed]).

extract_filters_at_current_level([], Filters, Parsed) ->
  {Filters, lists:reverse(Parsed)};
extract_filters_at_current_level([{before_, Line, each, Fun} | AST], Filters, Parsed) ->
  extract_filters_at_current_level(AST, [
      {before_each, Line, Fun}
      | Filters
    ], Parsed);
extract_filters_at_current_level([{after_, Line, each, Fun} | AST], Filters, Parsed) ->
  extract_filters_at_current_level(AST, [
      {after_each, Line, Fun}
      | Filters
    ], Parsed);
extract_filters_at_current_level([{before_, Line, all, Fun} | AST], Filters, Parsed) ->
  extract_filters_at_current_level(AST, [
      {before_all, Line, Fun}
      | Filters
    ], Parsed);
extract_filters_at_current_level([{after_, Line, all, Fun} | AST], Filters, Parsed) ->
  extract_filters_at_current_level(AST, [
      {after_all, Line, Fun}
      | Filters
    ], Parsed);
extract_filters_at_current_level([Instruction | AST], Filters, Parsed) ->
  extract_filters_at_current_level(AST, Filters, [Instruction | Parsed]).
