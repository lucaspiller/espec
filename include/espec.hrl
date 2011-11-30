-define(_describe(Description, Body),
    {group, ?LINE, Description, Body}).

-define(_before(Type, Fun),
    {before_, ?LINE, Type, Fun}).

-define(_after(Type, Fun),
    {after_, ?LINE, Type, Fun}).

-define(_it(Description),
    {pending, ?LINE, Description}).

-define(_it(Description, Fun),
    {example, ?LINE, Description, Fun}).
