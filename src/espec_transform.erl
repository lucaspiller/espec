-module(espec_transform).

-export([parse_transform/2]).

parse_transform(AST, _Options) ->
  walk_head(AST).


walk_head(AST) ->
  lists:map(fun walk_head_element/1, AST).

walk_head_element({function, LineNo, Method, 0, Clauses}) ->
  DS = walk_single_clause(Clauses),
  {function, LineNo, Method, 0, [{clause, LineNo, [], [], [erl_syntax:revert(DS)]}]};

walk_head_element(El) ->
  El.

walk_single_clause([Clause]) ->
  walk_spec_clause(Clause).

walk_spec_clause({clause, _LineNo, _Arguments, _Guards, Body}) ->
  walk_spec_body(Body).

walk_spec_body(Body) ->
  List = lists:map(fun walk_spec_statement/1, Body),
  erl_syntax:list(List).

walk_spec_statement({call, LineNo, {atom, _LineNo2, it}, [{string, _LineNo3, Description}, {'fun', _LineNo4, _Clauses} = Fun]}) ->
  erl_syntax:tuple([erl_syntax:atom(example), erl_syntax:integer(LineNo), erl_syntax:string(Description), Fun]);

walk_spec_statement({call, LineNo, {atom, _LineNo2, before_each}, [{'fun', _LineNo4, _Clauses} = Fun]}) ->
  erl_syntax:tuple([erl_syntax:atom(before_), erl_syntax:integer(LineNo), erl_syntax:atom(each), Fun]);

walk_spec_statement({call, LineNo, {atom, _LineNo2, before_all}, [{'fun', _LineNo4, _Clauses} = Fun]}) ->
  erl_syntax:tuple([erl_syntax:atom(before_), erl_syntax:integer(LineNo), erl_syntax:atom(all), Fun]);

walk_spec_statement({call, LineNo, {atom, _LineNo2, after_each}, [{'fun', _LineNo4, _Clauses} = Fun]}) ->
  erl_syntax:tuple([erl_syntax:atom(after_), erl_syntax:integer(LineNo), erl_syntax:atom(each), Fun]);

walk_spec_statement({call, LineNo, {atom, _LineNo2, after_all}, [{'fun', _LineNo4, _Clauses} = Fun]}) ->
  erl_syntax:tuple([erl_syntax:atom(after_), erl_syntax:integer(LineNo), erl_syntax:atom(all), Fun]);

walk_spec_statement({call, LineNo, {atom, _LineNo2, describe}, [{string, _LineNo3, Description}, {'fun', _LineNo4, {clauses, Clauses}}]}) ->
  Body = walk_single_clause(Clauses),
  erl_syntax:tuple([erl_syntax:atom(group), erl_syntax:integer(LineNo), erl_syntax:string(Description), Body]).

