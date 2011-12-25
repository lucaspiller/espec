-module(espec_ast).
-export([
    convert_to_execution_tree/1
  ]).

convert_to_execution_tree(AST) ->
  lists:reverse(convert_to_execution_tree(AST, [])).

convert_to_execution_tree(AST, []) ->
  {Filters, ASTWithoutFilters} = extract_filters_at_current_level(AST, [], []),
  Parsed = [proplists:get_value(before_all, Filters, [])],
  ExecutionTree = convert_to_execution_tree(ASTWithoutFilters, Filters, Parsed),
  lists:flatten([proplists:get_value(after_all, Filters, []) | ExecutionTree]).

convert_to_execution_tree([], _, Parsed) ->
  lists:flatten(Parsed);
convert_to_execution_tree([{group, Line, Description, Body} | AST], Filters, Parsed) ->
  BodyExecutionTree = convert_to_execution_tree(Body, []),
  BeforeInstruction = proplists:get_value(before_each, Filters, []),
  AfterInstruction = proplists:get_value(after_each, Filters, []),
  InstructionTree = [
    {end_group, Line, Description},
    AfterInstruction,
    BodyExecutionTree,
    BeforeInstruction,
    {start_group, Line, Description}
  ],
  convert_to_execution_tree(AST, Filters, [InstructionTree | Parsed]);
convert_to_execution_tree([{example, Line, Description, Fun} | AST], Filters, Parsed) ->
  BeforeInstruction = proplists:get_value(before_each, Filters, []),
  AfterInstruction = proplists:get_value(after_each, Filters, []),
  InstructionTree = [
    {end_example, Line, Description},
    AfterInstruction,
    {run, Fun},
    BeforeInstruction,
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
  InstructionTree = [
    {run, Fun}
  ],
  extract_filters_at_current_level(AST, [{before_each, InstructionTree} | Filters], Parsed);
extract_filters_at_current_level([{after_, Line, each, Fun} | AST], Filters, Parsed) ->
  InstructionTree = [
    {run, Fun}
  ],
  extract_filters_at_current_level(AST, [{after_each, InstructionTree} | Filters], Parsed);
extract_filters_at_current_level([{before_, Line, all, Fun} | AST], Filters, Parsed) ->
  InstructionTree = [
    {run, Fun}
  ],
  extract_filters_at_current_level(AST, [{before_all, InstructionTree} | Filters], Parsed);
extract_filters_at_current_level([{after_, Line, all, Fun} | AST], Filters, Parsed) ->
  InstructionTree = [
    {run, Fun}
  ],
  extract_filters_at_current_level(AST, [{after_all, InstructionTree} | Filters], Parsed);
extract_filters_at_current_level([Instruction | AST], Filters, Parsed) ->
  extract_filters_at_current_level(AST, Filters, [Instruction | Parsed]).
