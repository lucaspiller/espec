-module(espec_helper).
-export([spec_get/1, spec_set/2, assert_equal/4, assert_match/4]).

spec_get(Atom) ->
  case get(espec_context) of
    undefined -> undefined;
    ContextDict ->
      get_value_or_undefined(Atom, ContextDict)
  end.

get_value_or_undefined(Key, Dict) ->
  case dict:is_key(Key, Dict) of
    true ->
      dict:fetch(Key, Dict);
    _ ->
      undefined
  end.

spec_set(Atom, Value) ->
  case get(espec_context) of
    undefined ->
      throw(espec_context_not_available);
    ContextDict ->
      % kind of space leaky if someone is repeatedly setting the same value
      % but the alternative 
      put(espec_context, dict:store(Atom, Value, ContextDict)),
      ok
  end.

assert_equal(Expected, Expression, Line, ExpressionLiteral) ->
    case Expression of
        Expected ->
          ok;
        Value ->
            erlang:error({assertEqual_failed,
                [
                    {line, Line},
                    {expression, ExpressionLiteral},
                    {expected, Expected},
                    {got, Value}
                ]})
    end.

assert_match(Guard, Expression, Line, ExpressionLiteral) ->
    case Expression of
        Guard ->
          ok;
        Value ->
            erlang:error({assertMatch_failed,
                [
                    {line, Line},
                    {expression, ExpressionLiteral},
                    {expected, Guard},
                    {got, Value}
                ]})
    end.
