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
  lists:reverse(convert_to_execution_tree(AST, [], [], [])).

convert_to_execution_tree(AST, [], Befores, Afters) ->
  {Filters, ASTWithoutFilters} = extract_filters_at_current_level(AST, [], []),
  Parsed = [proplists:get_all_values(before_all, Filters)],
  ExecutionTree = convert_to_execution_tree(ASTWithoutFilters, Filters, Parsed, Befores, Afters),
  lists:flatten([lists:reverse(proplists:get_all_values(after_all, Filters)) | ExecutionTree]).

convert_to_execution_tree([], _, Parsed, _, _) ->
  lists:flatten(Parsed);
convert_to_execution_tree([{group, Line, Description, Body} | AST], Filters, Parsed, Befores0, Afters0) ->
  Befores = proplists:get_all_values(before_each, Filters) ++ Befores0,
  Afters = proplists:get_all_values(after_each, Filters) ++ Afters0,
  BodyExecutionTree = convert_to_execution_tree(Body, [], Befores, Afters),
  InstructionTree = [
    {end_group, Line, Description},
    BodyExecutionTree,
    {start_group, Line, Description}
  ],
  convert_to_execution_tree(AST, Filters, [InstructionTree | Parsed], Befores0, Afters0);
convert_to_execution_tree([{example, Line, Description, Fun} | AST], Filters, Parsed, Befores0, Afters0) ->
  BeforeInstruction = proplists:get_all_values(before_each, Filters) ++ Befores0,
  AfterInstruction =   proplists:get_all_values(after_each, Filters) ++ Afters0,
  InstructionTree = [
    {end_example, Line, Description},
    lists:reverse(AfterInstruction),
    {run, Fun},
    BeforeInstruction,
    {start_example, Line, Description}
  ],
  convert_to_execution_tree(AST, Filters, [InstructionTree | Parsed], Befores0, Afters0);
convert_to_execution_tree([{pending, Line, Description} | AST], Filters, Parsed, Befores0, Afters0) ->
  InstructionTree = [
    {pending_example, Line, Description}
  ],
  convert_to_execution_tree(AST, Filters, [InstructionTree | Parsed], Befores0, Afters0).

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
