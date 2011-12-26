-module(espec_ast).
-export([
    convert_to_execution_tree/1
  ]).

%
% This module converts an abstract syntax tree representation of a spec
% into a linear execution tree representation. For example we go from:
%
%   [{group,254,"after_all spec",
%       [{after_,255,all,#Fun<before_after_filter_spec.44.53736782>},
%         {example,259,"should do stuff",
%           #Fun<before_after_filter_spec.45.19509813>},
%         {group,263,"nestedspec",
%           [{after_,264,all,#Fun<before_after_filter_spec.46.81668172>},
%             {example,268,"should do nested stuff",
%               #Fun<before_after_filter_spec.47.50609279>}]}]}]
%
% To:
%
%   [
%     {start_group,254,"after_all spec"},
%     {start_example,259,"should do stuff"},
%     {run,#Fun<before_after_filter_spec.45.19509813>},
%     {end_example,259,"should do stuff"},
%     {start_group,263,"nestedspec"},
%     {start_example,268,"should do nested stuff"},
%     {run,#Fun<before_after_filter_spec.47.50609279>},
%     {end_example,268,"should do nested stuff"},
%     {run,#Fun<before_after_filter_spec.46.81668172>},
%     {end_group,263,"nestedspec"},
%     {run,#Fun<before_after_filter_spec.44.53736782>},
%     {end_group,254,"after_all spec"}
%   ]
%

convert_to_execution_tree(AST) ->
  lists:reverse(convert_to_execution_tree(AST, [])).

convert_to_execution_tree(AST, []) ->
  {Filters, ASTWithoutFilters} = extract_filters_at_current_level(AST, [], []),
  Parsed = [proplists:get_all_values(before_all, Filters)],
  ExecutionTree = convert_to_execution_tree(ASTWithoutFilters, Filters, Parsed),
  lists:flatten([lists:reverse(proplists:get_all_values(after_all, Filters)) | ExecutionTree]).

convert_to_execution_tree([], _, Parsed) ->
  lists:flatten(Parsed);
convert_to_execution_tree([{group, Line, Description, Body} | AST], Filters, Parsed) ->
  BodyExecutionTree = convert_to_execution_tree(Body, []),
  BeforeInstruction = proplists:get_all_values(before_each, Filters),
  AfterInstruction = proplists:get_all_values(after_each, Filters),
  InstructionTree = [
    {end_group, Line, Description},
    AfterInstruction,
    BodyExecutionTree,
    BeforeInstruction,
    {start_group, Line, Description}
  ],
  convert_to_execution_tree(AST, Filters, [InstructionTree | Parsed]);
convert_to_execution_tree([{example, Line, Description, Fun} | AST], Filters, Parsed) ->
  BeforeInstruction = proplists:get_all_values(before_each, Filters),
  AfterInstruction = lists:reverse(proplists:get_all_values(after_each, Filters)),
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
extract_filters_at_current_level([{before_, _Line, each, Fun} | AST], Filters, Parsed) ->
  InstructionTree = [
    {run, Fun}
  ],
  extract_filters_at_current_level(AST, [{before_each, InstructionTree} | Filters], Parsed);
extract_filters_at_current_level([{after_, _Line, each, Fun} | AST], Filters, Parsed) ->
  InstructionTree = [
    {run, Fun}
  ],
  extract_filters_at_current_level(AST, [{after_each, InstructionTree} | Filters], Parsed);
extract_filters_at_current_level([{before_, _Line, all, Fun} | AST], Filters, Parsed) ->
  InstructionTree = [
    {run, Fun}
  ],
  extract_filters_at_current_level(AST, [{before_all, InstructionTree} | Filters], Parsed);
extract_filters_at_current_level([{after_, _Line, all, Fun} | AST], Filters, Parsed) ->
  InstructionTree = [
    {run, Fun}
  ],
  extract_filters_at_current_level(AST, [{after_all, InstructionTree} | Filters], Parsed);
extract_filters_at_current_level([Instruction | AST], Filters, Parsed) ->
  extract_filters_at_current_level(AST, Filters, [Instruction | Parsed]).
