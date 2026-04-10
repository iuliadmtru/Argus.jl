[![codecov](https://codecov.io/gh/iuliadmtru/Argus.jl/graph/badge.svg?token=Z79DY1TSL4)](https://codecov.io/gh/iuliadmtru/Argus.jl)

# Argus.jl

Matching syntax and writing static analysis rules for Julia. Heavily
inspired by
[`syntax/parse`](https://docs.racket-lang.org/syntax/stxparse.html) —
a library for writing and processing macros in Racket — and
[Resyntax](https://docs.racket-lang.org/resyntax/index.html) — a
refactoring tool for Racket based on `syntax/parse`.

## Overview

Argus implements a framework for writing static analysis rules on top
of a syntax matching mechanism. It is structured around several core
concepts:

  - Syntax patterns
  - Pattern variables
  - Syntax classes
  - Syntax templates
  - Rules

_Syntax patterns_ form the basis for syntax matching and closely
resemble Julia code. For example, `@pattern x = 2` matches an
assignment where the left-hand side is a variable named `x` and the
right-hand side is the literal `2`. On the other hand, `@pattern {x} =
2` matches any assignment or short-form function definition where the
right-hand side is the literal `2`; the expression on the left-hand
side is bound to the _pattern variable_ `x`.

A _pattern variable_ is one of several special forms permitted within
patterns. It can be seen as a "hole" that is filled by matching
syntax. For example, when matching `@pattern {x} = 2` against the
expression `f(a) = 2`, `x` is bound to `f(a)`. The result of a pattern
match is either a set of bindings corresponding to the syntax matched
by each pattern variable, or an error explaining why the matching
failed.

A pattern variable can be constrained by a _syntax class_. In the
example above, `@pattern {x} = 2` is equivalent to `@pattern
{x:::expr} = 2`, where `expr` is the syntax class that matches any
expression. Syntax classes are defined through patterns and can
reference other syntax classes. For example, a syntax class matching
any function definition may be defined as such:

```julia
fundef = @syntax_class "function definition" begin
    @pattern {call:::funcall} = {body}
    @pattern function ({call:::funcall})
        {body}...
    end
end
```

Argus provides a set of pre-defined syntax classes, including `expr`,
`funcall` and `fundef`.

_Syntax templates_ are expanded to produce Julia code. They contain
variables that are replaced with information gathered during pattern
matching.

A _rule_ contains a description, a pattern and, optionally, a
template. In the case of rules, matching recursively traverses a given
unit of source code (e.g. a file) and collects the sub-expressions
that match the rule's pattern. Pattern variables bound by these
identifications are returned in corresponding _binding sets_.

Rules may be organised into _rule groups_. For example, it may be
useful to group all rules related to Julia usage in a `lang` group:

```julia
lang_rules = RuleGroup("lang")

@define_rule_in_group lang_rules "compare-nothing" begin
    description = """
    Comparisons to `nothing` should use ===, !== or isnothing().
    """

    pattern = @pattern begin
        ~or(
            nothing == {_},
            {_} == nothing,
            nothing != {_},
            {_} != nothing
        )
    end
end
```

## Getting Started

### Instalation

Argus is not yet registered in the Julia package system. It can be
installed with `Pkg`:

```julia
using Pkg
Pkg.add("https://github.com/iuliadmtru/Argus.jl/")
```

### Tutorial

#### Basics

The essential structures in Argus are `Pattern`s, `CommentPattern`s,
`SyntaxClass`es and `Template`s.

```julia
julia> using Argus

help?> Pattern
search: Pattern @pattern CommentPattern

  Pattern

  Syntax pattern used for matching syntax.

help?> CommentPattern
search: CommentPattern CommentDisabler AbstractPattern Pattern

  CommentPattern

  Trivia pattern used for matching comments.

help?> SyntaxClass
search: SyntaxClass @syntax_class SyntaxClassRegistry SyntaxError syntax_match

  SyntaxClass

  Syntax classes provide the basis for a syntax matching mechanism. A syntax class specifies a syntactic "shape" and provides a description for that shape.

  Syntax class bodies can contain one or more patterns. If there are multiple patterns, the search for a syntax match stops at the first matching pattern. A pattern variable can be constrained by a
  syntax class using the syntax {<pattern_var_name>:::<syntax_class_name>}. Unconstrained pattern variables are constrained by default to :::expr.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> binary_funcall = @syntax_class "binary function call" begin
             @pattern {_}({_}, {_})
         end
  SyntaxClass: binary function call
    Pattern alternative #1:
      [call]
        _:::expr                           :: ~var
        _:::expr                           :: ~var
        _:::expr                           :: ~var

  julia> fundef = @syntax_class "function definition" begin
             @pattern {f:::funcall} = {_}...
             @pattern function ({f:::funcall}) {_}... end
         end
  SyntaxClass: function definition
    Pattern alternative #1:
      [function-=]
        f:::funcall                        :: ~var
        [~rep]
          _:::expr                         :: ~var
    Pattern alternative #2:
      [function]
        f:::funcall                        :: ~var
        [block]
          [~rep]
            _:::expr                       :: ~var

help?> Template
search: Template @template replace tempname keepat! relpath realpath Tuple repeat accumulate replace! empty splat

  Template

  Syntax template, to be filled using a BindingSet obtained after pattern matching. Alias for SyntaxPatternNode.
```

A simple pattern to experiment with is the match-all pattern:

```julia
julia> expr = @pattern {x}
Pattern:
x:::expr                                 :: ~var
```

This pattern matches any Julia expression. It has one pattern
variable: `x`. The pretty print tells us that the pattern consists of
a `~var` node which constraints `x` to be an `expr`. `~var` and `expr`
are explained later.

Patterns can be matched against Julia expressions.

```julia
julia> using JuliaSyntax: parsestmt, SyntaxNode

julia> syntax_match(expr, parsestmt(SyntaxNode, "1 + 2"))
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: (call-i 1 + 2) @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries

julia> syntax_match(expr, parsestmt(SyntaxNode, "f(x::Int) = x * 2"))
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: (function-= (call f (::-i x Int)) (call-i x * 2)) @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries
```

The result of a successful match is a `BindingSet` — a dictionary with
bound pattern variables (`Binding`s). A `Binding` contains information
about a pattern variable that matched (part of) the source, as well as
the matched source node.

In the second example above, the pretty print tells us that the
pattern variable `x` was bound to the function definition found at
line 1, column 1 in the source. The fifth line of the pretty print
tells us that `x` has ellipsis depth 0. This is because `x` is not
surrounded by any ellipses (`...`).

#### Ellipses

A pattern followed by an ellipsis means that the pattern repeats zero
or more times. 

```julia
julia> exprs = @pattern {x}...
Pattern:
[~rep]
  x:::expr                               :: ~var

julia> using JuliaSyntax: parseall

julia> syntax_match(exprs, parseall(SyntaxNode, """
                                                a = 1
                                                b = 2
                                                a + b
                                                """))
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound sources: [(= a 1) @ 1:1, (= b 2) @ 2:1, (call-i a + b) @ 3:1]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet @ 0:0 with 0 entries,
             BindingSet @ 0:0 with 0 entries,
             BindingSet @ 0:0 with 0 entries
            ]
```

This is useful in many situations. For example, when defining a
pattern that matches a function definition:

```julia
julia> function_def = @pattern begin
           function {f}({args}...)
               {body}...
           end
       end
Pattern:
[function]
  [call]
    f:::expr                             :: ~var
    [~rep]
      args:::expr                        :: ~var
  [block]
    [~rep]
      body:::expr                        :: ~var

julia> syntax_match(function_def, parsestmt(SyntaxNode, """
                                                        function my_fun()
                                                            println("My fun!")
                                                            return nothing
                                                        end
                                                        """))
BindingSet @ 0:0 with 3 entries:
  :f => Binding:
          Name: :f
          Bound source: my_fun @ 1:10
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries
  :args => Binding:
             Name: :args
             Bound sources: []
             Ellipsis depth: 1
             Sub-bindings:
               []
  :body => Binding:
             Name: :body
             Bound sources: [(call println (string "My fun!")) @ 2:5, (return nothing) @ 3:5]
             Ellipsis depth: 1
             Sub-bindings:
               [
                BindingSet @ 0:0 with 0 entries,
                BindingSet @ 0:0 with 0 entries
               ]
```

`args` and `body` both bind to sequences of expressions. You can see
this by looking at their ellipsis depth, which is 1, or by noting that
they are bound to vectors of source nodes instead of just source
nodes, like `f` is.

Ellipses can be nested to match sequences of any depth.

```julia
julia> vec_of_vecs = @pattern [[{el}...]...]
Pattern:
[vect]
  [~rep]
    [vect]
      [~rep]
        el:::expr                        :: ~var

julia> syntax_match(vec_of_vecs, parsestmt(SyntaxNode, "[[1, 2], [[3]]]"))
BindingSet @ 0:0 with 1 entry:
  :el => Binding:
           Name: :el
           Bound sources: [[1 @ 1:3, 2 @ 1:6], [(vect 3) @ 1:11]]
           Ellipsis depth: 2
           Sub-bindings:
             [
              [
               BindingSet @ 0:0 with 0 entries,
               BindingSet @ 0:0 with 0 entries
              ],
              [
               BindingSet @ 0:0 with 0 entries
              ]
             ]
```

`el` has ellipsis depth 2, so it binds to a sequence of sequences of
expressions. In the example above, it binds to a sequence containing
two sequences: the sequence with `1` and `2` and the sequence with the
vector `[3]`.

You can read the pattern `vec_of_vecs` more intuitively as: _A vector
that contains any number of elements. Its elements need to be vectors
that have any number of elements of any kind._

By default, the matching algorithm for ellipses is "greedy".

```julia
julia> ones_vec = @pattern [1, {ones1}..., 1, {ones2}...];

julia> syntax_match(ones_vec, parsestmt(SyntaxNode, "[1, 1, 1, 1]"))
BindingSet @ 0:0 with 2 entries:
  :ones2 => Binding:
              Name: :ones2
              Bound sources: []
              Ellipsis depth: 1
              Sub-bindings:
                []
  :ones1 => Binding:
              Name: :ones1
              Bound sources: [1 @ 1:5, 1 @ 1:8]
              Ellipsis depth: 1
              Sub-bindings:
                [
                 BindingSet @ 0:0 with 0 entries,
                 BindingSet @ 0:0 with 0 entries
                ]
```

`ones1` consumes all the expressions it can, leaving none for
`ones2`. We can choose to match with a non-"greedy" algorithm:

```julia
julia> syntax_match(ones_vec, parsestmt(SyntaxNode, "[1, 1, 1, 1]"); greedy=false)
BindingSet @ 0:0 with 2 entries:
  :ones2 => Binding:
              Name: :ones2
              Bound sources: [1 @ 1:8, 1 @ 1:11]
              Ellipsis depth: 1
              Sub-bindings:
                [
                 BindingSet @ 0:0 with 0 entries,
                 BindingSet @ 0:0 with 0 entries
                ]
  :ones1 => Binding:
              Name: :ones1
              Bound sources: []
              Ellipsis depth: 1
              Sub-bindings:
                []
```

This is useful if we expect a non-greedy approach to be more efficient
in a particular case.

Argus hijacks Julia's [splat
operator](https://docs.julialang.org/en/v1/base/base/#...). If we want
to use `...` with the regular splat meaning, we need to escape it:

```julia
julia> syntax_match((@pattern v...), parsestmt(SyntaxNode, "v..."))
MatchFail: no match @ :1:1

julia> syntax_match((@pattern @esc(v...)), parsestmt(SyntaxNode, "v..."))
BindingSet @ 0:0 with 0 entries
```

We can also escape an expression only up to a certain depth.

```julia
julia> syntax_match((@pattern @esc([{elems}...]..., 1)), parsestmt(SyntaxNode, "[1, 2, 3]..."))
BindingSet @ 0:0 with 1 entry:
  :elems => Binding:
              Name: :elems
              Bound sources: [1 @ 1:2, 2 @ 1:5, 3 @ 1:8]
              Ellipsis depth: 1
              Sub-bindings:
                [
                 BindingSet @ 0:0 with 0 entries,
                 BindingSet @ 0:0 with 0 entries,
                 BindingSet @ 0:0 with 0 entries
                ]
```

Without specifying an escape depth, `@esc` would have escaped the
entire expression:

```julia
julia> @pattern @esc([{elems}...])
Pattern:
[vect]
  [...]
    [braces]
      elems                              :: Identifier
```


#### Syntax classes

We saw that a pattern variable is printed as `x:::expr`. This means
that `x` is constrained by the _syntax class_ `expr`.

A syntax class is a syntax matching construct useful for defining
syntactic "categories". You can see it as an abstraction for patterns.
(You can read more about them in the [`syntax/parse`
documentation](https://docs.racket-lang.org/syntax/stxparse-specifying.html).)

`expr` is the "category" of all Julia expressions. It is a syntax
class provided by Argus. We can define our own syntax classes and use
them in patterns. Let's write a syntax class for vectors:

```julia
julia> vec = @syntax_class "vector" begin
           @pattern [{_}...]
       end
SyntaxClass: vector
  Pattern alternative #1:
    [vect]
      [~rep]
        _:::expr                         :: ~var
```

Syntax class bodies consist of a sequence of patterns. The match
behaviour is that of a short-circuiting `or` — the result is either
the first successful pattern match, or a failure if no patterns match.

Let's test `vec`:

```julia
julia> syntax_match(vec, parsestmt(SyntaxNode, "[]"))
BindingSet @ 0:0 with 0 entries

julia> syntax_match(vec, parsestmt(SyntaxNode, "[1, [2]]"))
BindingSet @ 0:0 with 0 entries
```

The match results are empty binding sets. This is because we used an
_anonymous pattern variable_ in the syntax class's pattern.

If we want to use our syntax class in a pattern with the `:::` syntax
we need to register it first.

```julia
julia> register_syntax_class!(:my_vec, vec)
SyntaxClass: vector
  Pattern alternative #1:
    [vect]
      [~rep]
        _:::expr                         :: ~var
```

Now we can find it in the syntax class registry under the name `:my_vec`.

```julia
julia> Argus.SYNTAX_CLASS_REGISTRY[:my_vec]
SyntaxClass: vector
  Pattern alternative #1:
    [vect]
      [~rep]
        _:::expr                         :: ~var
```

Let's use it to rewrite the `vec_of_vecs` pattern:

```julia
julia> vec_of_vecs2 = @pattern [{v:::vec}...]
Pattern:
[vect]
  [~rep]
    v:::vec                              :: ~var

julia> syntax_match(vec_of_vecs2, parsestmt(SyntaxNode, "[[1, 2], [[3]]]"))
BindingSet @ 0:0 with 1 entry:
  :v => Binding:
          Name: :v
          Bound sources: [(vect 1 2) @ 1:2, (vect (vect 3)) @ 1:10]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet @ 0:0 with 0 entries,
             BindingSet @ 0:0 with 0 entries
            ]

julia> syntax_match(vec_of_vecs2, parsestmt(SyntaxNode, "[1]"))
MatchFail: no match @ :1:2
```

Argus's built-in implementation of the `vec` syntax class uses
`@define_syntax_class` to define and register `vec` and the same time:

```julia
julia> @define_syntax_class :vec "vector" begin
           @pattern [{_}...]
       end
SyntaxClass: vector
  Pattern alternative #1:
    [vect]
      [~rep]
        _:::expr                         :: ~var
```

#### Pattern forms

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

##### Active pattern forms

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

##### Passive pattern forms

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

#### Templates

The `BindingSet` resulting from a successful pattern match can be used
to fill in a _template_ in order to generate new code.

Let's say we want to replace a call to `append!` with one to
`vcat`. First, we write the pattern and match it with something:

```julia
julia> pattern = @pattern append!({vs}...)
Pattern:
[call]
  append!                                :: Identifier
  [~rep]
    vs:::expr                            :: ~var

julia> bindings = syntax_match(pattern, parsestmt(SyntaxNode, "append!([1, 2, 3], 4)"))
BindingSet with 1 entry:
  :vs => Binding:
           Name: :vs
           Bound sources: [(vect 1 2 3) @ 1:9, 4 @ 1:20]
           Ellipsis depth: 1
           Sub-bindings:
             [
              BindingSet with 0 entries,
              BindingSet with 0 entries
             ]
```

Then, we write the template and expand it using the bindings obtained
after matching:

```julia
julia> expand(template, bindings)
SyntaxNode:
[call]
  vcat                                   :: Identifier
  [vect]
    1                                    :: Integer
    2                                    :: Integer
    3                                    :: Integer
  4                                      :: Integer
```

Template variables should have the same ellipsis depth as the
corresponding pattern variables.

```julia
julia> expand((@template vcat({vs})), bindings)
ERROR: template variable vs has inconsistent ellipsis depth
Template variables should have the same depth as the corresponding pattern variables.

...
```

#### Rules

Now we're ready to write some rules!

A _rule_ is useful for matching all apparitions of a pattern in a
source.

```julia
julia> assignments = @rule "assignments" begin
           description = "Assignment here."
           pattern = @pattern {a:::assign}
       end
assignments:
Assignment here.

Pattern:
a:::assign                               :: ~var

julia> src = """
       x = 2
       y = x + 1
       if y == x
           z = true
       end
       f() = 4
       """;

julia> match_result = rule_match(assignments, parseall(SyntaxNode, src))
RuleMatchResult with 3 matches and 0 failures:
Matches:
  BindingSet(:a => Binding(:a, (= x 2) @ 1:1, BindingSet(:rhs => Binding(:rhs, 2 @ 1:5, BindingSet()), :lhs => Binding(:lhs, x @ 1:1, BindingSet(:_id => Binding(:_id, x @ 1:1, BindingSet()))))))
  nothing

  BindingSet(:a => Binding(:a, (= y (call-i x + 1)) @ 2:1, BindingSet(:rhs => Binding(:rhs, (call-i x + 1) @ 2:5, BindingSet()), :lhs => Binding(:lhs, y @ 2:1, BindingSet(:_id => Binding(:_id, y @ 2:1, BindingSet()))))))
  nothing

  BindingSet(:a => Binding(:a, (= z true) @ 4:5, BindingSet(:rhs => Binding(:rhs, true @ 4:9, BindingSet()), :lhs => Binding(:lhs, z @ 4:5, BindingSet(:_id => Binding(:_id, z @ 4:5, BindingSet()))))))
  nothing
```

The `assignments` rule above matches all the assignments in the
source. The `assign` built-in syntax class uses the pattern variables
`lhs` and `rhs`, which appear as sub-bindings for the rule pattern
variable `a`. We can access sub-bindings of a bound pattern variable
inside the pattern. Let's say we want to only match assignments with
literals on the rhs:

```julia
julia> lit_assignments = @rule "assignments" begin
           description = "Assignment here."
           pattern = @pattern begin
               {a:::assign}
               @fail begin
                   using JuliaSyntax: is_literal
                   !is_literal(a.rhs.src)
               end "rhs not a literal"
           end
       end;

julia> match_result = rule_match(lit_assignments, parseall(SyntaxNode, src))
RuleMatchResult with 2 matches and 0 failures:
Matches:
  BindingSet(:a => Binding(:a, (= x 2) @ 1:1, BindingSet(:rhs => Binding(:rhs, 2 @ 1:5, BindingSet()), :lhs => Binding(:lhs, x @ 1:1, BindingSet(:_id => Binding(:_id, x @ 1:1, BindingSet()))))))
  nothing

  BindingSet(:a => Binding(:a, (= z true) @ 4:5, BindingSet(:rhs => Binding(:rhs, true @ 4:9, BindingSet()), :lhs => Binding(:lhs, z @ 4:5, BindingSet(:_id => Binding(:_id, z @ 4:5, BindingSet()))))))
  nothing
```

`rule_match` can keep track of all non-trivial failed matches as well
by setting the `only_matches` keyword argument to `false`. This can be
useful for debugging a rule.

```julia
julia> rule_match(lit_assignments, parseall(SyntaxNode, src); only_matches=false).failures
21-element Vector{MatchFail}:
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("rhs not a literal")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
 MatchFail("expected assignment")
```

At least it would be useful if the failure would contain other
information such as the source location of the failure... This is in
plan for the future :).

The result of a rule match consists of two vectors: one with all
non-trivial failures and one with all matches with their associated
template expansions, if applicable. The above rules don't have
templates, so they have `nothing` as template expansions. A rule with
a template is created using the `template` argument in the `@rule`
macro.

Say we want to replace all apparitions of `rand() < 0.5` with the call
`rand(Bool)`.

```julia
julia> rand_bool = @rule "rand-bool" begin
           description = """
           To get a random Boolean, use `rand(Bool)`.
           """

           pattern = @pattern begin
               {randf}() < 0.5
               @fail match(r"^(Base.)?rand$", randf.name) === nothing "not `rand` call"
           end

           template = @template rand(Bool)
       end;
```

We could match this rule against a file called `"rand-bool.jl"` with
the following content:

```
# Match.
rand() < 0.5

function some_rand_function(x)
    # Match.
    if rand() < 0.5
        println("Random")
    end
end

# Match.
if some_flag && rand() < 0.5 || other_flag
    println("Random")
end
```

The result would be:

```julia
julia> rule_match(rand_bool, "rand-bool.jl")
RuleMatchResult with 3 matches and 0 failures:
Matches:
  BindingSet(:randf => Binding(:randf, rand @ 2:1, BindingSet()))
  (call rand Bool)

  BindingSet(:randf => Binding(:randf, rand @ 6:8, BindingSet()))
  (call rand Bool)

  BindingSet(:randf => Binding(:randf, rand @ 12:17, BindingSet()))
  (call rand Bool)
```

Sometimes it is useful to group rules by category. We can define a
rule group and store rules inside it:

```julia
julia> style_rules = RuleGroup("style")
RuleGroup("style")

julia> @define_rule_in_group style_rules "useless-equals" begin
           description = """
           Comparing an object with itself always returns `true`.
           """

           pattern = @pattern begin
               ~or(
                   {x} ==  {x},
                   {x} !=  {x},
                   {x} === {x},
                   {x} !== {x}
               )
           end
       end;

julia> @define_rule_in_group style_rules "lowercase-const" begin
           description = """
           Prefer writing `const` variables in all-uppercase.
           """

           pattern = @pattern begin
               const {x:::identifier} = {_}
               @fail all(isuppercase, x.name) "`const` variable with all-uppercase name"
           end
       end;
```

To check if the rules are correct, let's try them on a source file:

```julia
julia> f = tempname();

julia> write(f, """
       a == a
       const low = 2
       const OK = true
       """);

julia> rule_group_match(style_rules, f; only_matches=false)
RuleGroupMatchResults with 2 entries:
  "useless-equals"  => RuleMatchResult(Tuple{BindingSet, Union{Nothing, SyntaxNode}}[(BindingSet(:x=>Binding(:x, a @ 1:6, BindingSet())), nothing)], MatchFail[])
  "lowercase-const" => RuleMatchResult(Tuple{BindingSet, Union{Nothing, SyntaxNode}}[(BindingSet(:x=>Binding(:x, low @ 2:7, BindingSet(:_id => Binding(:_id, low @ 2:7, BindingSet())))), nothing)], MatchF…
```

## Notes

Argus is the final project for my Computer Science and Engineering
degree at University Politehnica of Bucharest. The paper is attached
in the repo (Thesis.pdf).

## Acknowledgements

[JuliaHub](https://juliahub.com) in general and Avik Sengupta in
particular for allowing me to partly work on this project during my
internship and for guiding me in the process.

[Andrei Duma](https://github.com/AndreiDuma) for being providing me
with reading recommendations, feedback and constant motivation. Thank
you for telling me about the amazing `syntax/parse` library and for
spending so many hours with me discussing software design,
metaprogramming and more.

## References

- [Fortifying
  macros](https://www2.ccs.neu.edu/racket/pubs/c-jfp12.pdf), a 2012
  paper by Ryan Culpepper on Racket's macro system
- [`syntax/parse`](https://docs.racket-lang.org/syntax/stxparse.html)
- [Resyntax](https://docs.racket-lang.org/resyntax/index.html)
- [Semgrep](https://semgrep.dev/docs/)
