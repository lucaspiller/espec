-module(espec_helper).
-export([pending/1]).

pending(Desc) ->
  throw({pending, Desc}).
