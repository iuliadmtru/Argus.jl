# Pattern Forms

We have come across pattern nodes preceeded by `~`, such as
`~var`. There are called _pattern forms_. A pattern form is a special
syntax form that performs actions on patterns or enables special
matching behaviour. (They [come from
`syntax/parse`](https://docs.racket-lang.org/syntax/stxparse-patterns.html)
as well.)

`~var` is an _active_ pattern form — apart from performing a match, it
also _binds_ pattern variables. `~rep`, which we've seen in its
sugared `...` form, is a _passive_ pattern form – it is only used for
matching, not for performing actions.

## Active Pattern Forms

There are currently four active pattern forms defined in Argus:
`~var`, `~when`, `~fail` and `~execute`. Among these, only `~var`
binds new pattern variables.

`~var` expects a pattern variable name and a syntax class name. If the
source matches the syntax class, it is bound to the pattern
variable. We have seen `~var` forms before quite a lot, but not
explicitly. The `:::` syntax is an implicit `~var` form. `{x:::vec}`
is expanded to `~var(:x, :vec)` and `{x}` to `~var(:x, :expr)`. This
is visible in the patterns' pretty printed form.

```julia
julia> x = @pattern ~var(:x, :expr)
Pattern:
x:::expr                                 :: ~var

julia> syntax_match(x, parsestmt(SyntaxNode, "a + b"))
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: (call-i a + b) @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries
```

`~when` expects a list of pattern variables (given as a list of
`Symbol`s) and a condition that should evaluate to a boolean. The
condition is evaluated during a pattern match using the match's
`BindingSet`. If it is satisfied, the matching succeeds. Otherwise, it
fails.

```julia
julia> id = @pattern begin
           {x:::identifier}
           @when [:x] x.name == "id"
       end
Pattern:
[~and]
  x:::identifier                         :: ~var
  [~when]
    [call-i]
      [.]
        x                                :: Identifier
        name                             :: Identifier
      ==                                 :: Identifier
      [string]
        "id"                             :: String

julia> syntax_match(id, parsestmt(SyntaxNode, "id"))
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: id @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries

julia> syntax_match(id, parsestmt(SyntaxNode, "not_id"))
MatchFail: no match @ :1:1
```

**Note:** The above `id` pattern is equivalent to an `~and` between it's
two statements, with the `~when` form written explicitly:

```julia
julia> id_and = @pattern ~and(
           {x:::identifier},
           ~when([:x], x.name == "id")
       )
Pattern:
[~and]
  x:::identifier                         :: ~var
  [~when]
    [call-i]
      [.]
        x                                :: Identifier
        name                             :: Identifier
      ==                                 :: Identifier
      [string]
        "id"                             :: String
```

`~fail` expects a list of pattern variables, a fail condition (given
as an expression) and a failure message. It has the opposite behaviour
of `~when`. If the fail condition is satisfied, the matching fails
with the given failure message. Otherwise, it succeeds.

```julia
julia> not_id = @pattern begin
           {x:::identifier}
           @fail [:x] x.name == "id" "expected something other than \"id\""
       end
Pattern:
[~and]
  x:::identifier                         :: ~var
  [~fail]
    [call-i]
      [.]
        x                                :: Identifier
        name                             :: Identifier
      ==                                 :: Identifier
      [string]
        "id"                             :: String
    "expected something other than \"id\"" :: String

julia> syntax_match(not_id, parsestmt(SyntaxNode, "id"))
MatchFail: expected something other than "id" @ :1:1

julia> syntax_match(not_id, parsestmt(SyntaxNode, "not_id"))
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: not_id @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries
```

`~execute` expects a list of pattern variables and an expression. The
expression is evaluated during a pattern match.

```julia
julia> log = @pattern ~and(
           {x},
           ~execute([:x], println("The pattern variable x is bound to: ", x.src))
       )
Pattern:
[~and]
  x:::expr                               :: ~var
  [~execute]
    [call]
      println                            :: Identifier
      [string]
        "The pattern variable x is bound to: " :: String
      [.]
        x                                :: Identifier
        src                              :: Identifier

julia> syntax_match(log, parsestmt(SyntaxNode, "1 + 2"))
The pattern variable x is bound to: (call-i 1 + 2)
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: (call-i 1 + 2) @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries
```

## Passive Pattern Forms

There are currently six passive pattern forms defined in Argus: `~or`,
`~and`, `~rep`, `~not`, `~inside` and `~contains`. These don't perform
any actions during pattern matching, apart from the matching itself.

`~or` and `~and` have a short-circuiting match behaviour:

```julia
julia> equals_x = @pattern ~or(
           x == {first},
           {second} == x
       )
Pattern:
[~or]
  [call-i]
    x                                    :: Identifier
    ==                                   :: Identifier
    first:::expr                         :: ~var
  [call-i]
    second:::expr                        :: ~var
    ==                                   :: Identifier
    x                                    :: Identifier

julia> syntax_match(equals_x, parsestmt(SyntaxNode, "x == 2"))
BindingSet @ 0:0 with 1 entry:
  :first => Binding:
              Name: :first
              Bound source: 2 @ 1:6
              Ellipsis depth: 0
              Sub-bindings:
                BindingSet @ 0:0 with 0 entries

julia> syntax_match(equals_x, parsestmt(SyntaxNode, "2 == x"))
BindingSet @ 0:0 with 1 entry:
  :second => Binding:
               Name: :second
               Bound source: 2 @ 1:1
               Ellipsis depth: 0
               Sub-bindings:
                 BindingSet @ 0:0 with 0 entries

julia> syntax_match(equals_x, parsestmt(SyntaxNode, "2 == y"))
MatchFail: no match @ :1:6

julia> conflicting_and = @pattern ~and({a} + 2, {a} + 3)
Pattern:
[~and]
  [call-i]
    a:::expr                             :: ~var
    +                                    :: Identifier
    2                                    :: Integer
  [call-i]
    a:::expr                             :: ~var
    +                                    :: Identifier
    3                                    :: Integer

julia> syntax_match(conflicting_and, parsestmt(SyntaxNode, "a + 2"))
MatchFail: no match @ :1:5
```

**Note:** We could be more specific with the fail message for
`equals_x`. Let's use an implicit `~fail` form:

```julia
julia> equals_x = @pattern begin
           ~or({x} == 2, 2 == {x})
           @fail [:x] x.name != "x" "not x"
       end
Pattern:
[~and]
  [~or]
    [call-i]
      x:::expr                           :: ~var
      ==                                 :: Identifier
      2                                  :: Integer
    [call-i]
      2                                  :: Integer
      ==                                 :: Identifier
      x:::expr                           :: ~var
  [~fail]
    [call-i]
      [.]
        x                                :: Identifier
        name                             :: Identifier
      !=                                 :: Identifier
      [string]
        "x"                              :: String
    "not x"                              :: String

julia> syntax_match(equals_x, parsestmt(SyntaxNode, "2 == y"))
MatchFail: not x @ :1:1
```

`~rep` expects an expression that is to be matched zero or more
times. `{x}...` is short for `~rep(~var(:x, :expr))`.

```julia
julia> arbitrary_rep = @pattern ~rep(({x} + 2))
Pattern:
[~rep]
  [call-i]
    x:::expr                             :: ~var
    +                                    :: Identifier
    2                                    :: Integer

julia> syntax_match(arbitrary_rep, parseall(SyntaxNode, """
                                                        1 + 2
                                                        2 + 2
                                                        """))
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound sources: [1 @ 1:1, 2 @ 2:1]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet @ 0:0 with 0 entries,
             BindingSet @ 0:0 with 0 entries
            ]
```

`~not` is the negation pattern form – if the enclosing pattern matches
the source node, the `~not` pattern doesn't match, and vice-versa.

```julia
julia> not_literal = @pattern ~not({lit:::literal})
Pattern:
[~not]
  lit:::literal                          :: ~var

julia> syntax_match(not_literal, parsestmt(SyntaxNode, "2"))
MatchFail: `~not` subpattern match succeeded @ :1:1

julia> syntax_match(not_literal, parsestmt(SyntaxNode, "a"))
BindingSet @ 0:0 with 0 entries
```

**Note:** The pattern enclosed in `~not` does not bind pattern variables
outside itself.

`~inside` expects a pattern expression and, optionally, a search
level. It is meant to be used as an `~and` branch pattern where
another branch is the main pattern. An `~inside` pattern signals that
the main pattern should be contained in the pattern enclosed in
`~inside`.

```julia
julia> inside_fundef = @pattern ~and(
           {a:::assign},
           ~inside({_:::fundef})
       )
Pattern:
[~and]
  a:::assign                             :: ~var
  [~inside]
    _:::fundef                           :: ~var

julia> assign_in_fundef = parsestmt(SyntaxNode, "f(x) = let x = 2 end")
SyntaxNode:
[function-=]
  [call]
    f                                    :: Identifier
    x                                    :: Identifier
  [let]
    [block]
      [=]
        x                                :: Identifier
        2                                :: Integer
    [block]


julia> syntax_match(inside_fundef, assign_in_fundef[2][1][1])
BindingSet @ 0:0 with 1 entry:
  :a => Binding:
          Name: :a
          Bound source: (= x 2) @ 1:11
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 2 entries:
              :rhs => Binding:
                        Name: :rhs
                        Bound source: 2 @ 1:16
                        Ellipsis depth: 0
                        Sub-bindings:
                          BindingSet @ 0:0 with 0 entries
              :lhs => Binding:
                        Name: :lhs
                        Bound source: x @ 1:12
                        Ellipsis depth: 0
                        Sub-bindings:
                          BindingSet @ 0:0 with 0 entries

julia> assign = parsestmt(SyntaxNode, "x = 2")
SyntaxNode:
[=]
  x                                      :: Identifier
  2                                      :: Integer


julia> syntax_match(inside_fundef, assign)
MatchFail: `~inside` pattern does not match @ :1:1
```

If a search level is given, the matching stops at the main pattern's
parent found at that level.

```julia
julia> inside_fundef_up_to_1 = @pattern ~and(
           {a:::assign},
           ~inside({_:::fundef}, 1)
       )
Pattern:
[~and]
  a:::assign                             :: ~var
  [~inside]
    _:::fundef                           :: ~var
    1                                    :: Integer

julia> syntax_match(inside_fundef_up_to_1, assign_in_fundef[2][1][1])
MatchFail: `~inside` pattern does not match: expected function definition @ :1:11
```

`~contains` signals that the main pattern should contain the pattern
enclosed in `~contains`. It can contain a search level as well,
signifying that the matching should stop at the main pattern's
children found at that level.

```julia
julia> contains_literal = @pattern ~and(
           {m:::macrocall},
           ~contains({l:::literal})
       )
Pattern:
[~and]
  m:::macrocall                          :: ~var
  [~contains]
    l:::literal                          :: ~var

julia> syntax_match(contains_literal, parsestmt(SyntaxNode, "@assert x == 2"))
BindingSet @ 1:14 with 1 entry:
  :l => Binding:
          Name: :l
          Bound source: 2 @ 1:14
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries

julia> contains_literal_up_to_1 = @pattern ~and(
           {m:::macrocall},
           ~contains({l:::literal}, 1)
       )
Pattern:
[~and]
  m:::macrocall                          :: ~var
  [~contains]
    l:::literal                          :: ~var
    1                                    :: Integer

julia> syntax_match(contains_literal_up_to_1, parsestmt(SyntaxNode, "@assert x == 2"))
MatchFail: `~contains` pattern does not match: expected literal @ :1:1

julia> contains_literal_up_to_2 = @pattern ~and(
           {m:::macrocall},
           ~contains({l:::literal}, 2)
       );

julia> syntax_match(contains_literal_up_to_2, parsestmt(SyntaxNode, "@assert x == 2"))
BindingSet @ 0:0 with 2 entries:
  :l => Binding:
          Name: :l
          Bound source: 2 @ 1:14
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries
  :m => Binding:
          Name: :m
          Bound source: (macrocall @assert (call-i x == 2)) @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries
```

**Note:** `~inside` and `~contains` may also be used by themselves,
without a main pattern:

```julia
julia> plain_inside = @pattern ~inside({i:::infix_call});

julia> src = parsestmt(SyntaxNode, "a + b")
SyntaxNode:
[call-i]
  a                                      :: Identifier
  +                                      :: Identifier
  b                                      :: Identifier


julia> syntax_match(plain_inside, src[1])
BindingSet @ 0:0 with 1 entry:
  :i => Binding:
          Name: :i
          Bound source: (call-i a + b) @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 2 entries:
              :rhs => Binding:
                        Name: :rhs
                        Bound source: b @ 1:5
                        Ellipsis depth: 0
                        Sub-bindings:
                          BindingSet @ 0:0 with 0 entries
              :lhs => Binding:
                        Name: :lhs
                        Bound source: a @ 1:1
                        Ellipsis depth: 0
                        Sub-bindings:
                          BindingSet @ 0:0 with 0 entries

julia> plain_contains = @pattern ~contains({id:::identifier});

julia> syntax_match(plain_contains, src)
BindingSet @ 1:1 with 1 entry:
  :id => Binding:
           Name: :id
           Bound source: a @ 1:1
           Ellipsis depth: 0
           Sub-bindings:
             BindingSet @ 0:0 with 0 entries
```
