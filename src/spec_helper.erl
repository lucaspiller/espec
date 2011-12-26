-module(spec_helper).
-export([append/2]).

append(Atom, Name) ->
  case get(Name) of
    undefined ->
      put(Name, [Atom]);
    List ->
      put(Name, List ++ [Atom])
  end.
