-compile([
    {parse_transform, espec_transform}
  ]).
-export([
    spec/0
  ]).

-define(_assertEqual, spec_helper:assert_equal).
