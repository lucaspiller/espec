-compile([
    {parse_transform, espec_transform}
  ]).
-export([
    spec/0
  ]).

-define(_assertEqual(Expected, Expr),
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
