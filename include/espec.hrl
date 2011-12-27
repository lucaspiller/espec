-compile([
    {parse_transform, espec_transform}
  ]).
-export([
    spec/0
  ]).

-define(assertEqual(Expected, Expr),
  ((fun (__Expected) ->
          case (Expr) of
            __Expected ->
              ok;
            __Value ->
              erlang:error({assertEqual_failed,
                  [
                      {line, ?LINE},
                      {expression, (??Expr)},
                      {expected, __Expected},
                      {got, __Value}
                  ]})
        end
    end)(Expected))).

-define(assertMatch(Guard, Expr),
  ((fun () ->
          case (Expr) of
            Guard ->
              ok;
            __Value ->
              erlang:error({assertMatch_failed,
                  [
                      {line, ?LINE},
                      {expression, (??Expr)},
                      {expected, (??Guard)},
                      {got, __Value}
                  ]})
        end
    end)())).
