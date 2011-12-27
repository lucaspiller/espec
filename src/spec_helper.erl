-module(spec_helper).
-export([
    append/2,
    assert_equal/2
  ]).

append(Atom, Name) ->
  case get(Name) of
    undefined ->
      put(Name, [Atom]);
    List ->
      put(Name, List ++ [Atom])
  end.

assert_equal(Expected, Result) ->
  case Result of
    Expected ->
      ok;
    _ ->
      erlang:error({assert_equal_failed, {expected, Expected}, {got, Result}})
  end.
