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

#### Patterns

Patterns are defined with the `@pattern` macro.

```julia
help?> @pattern
  @pattern(expr)

  Create a Pattern from the given expression. Special syntax can be escaped by wrapping it in an @esc macro. Match and fail conditions are defined with the @when and @fail macros respectively.

  @esc usage:

    •  @esc(ex): Escape everything inside ex.
    •  @esc(ex, depth): Escape ex only up to depth.

  @when usage:

    •  @when(pattern_vars, condition): Match only if condition is satisfied. The pattern variables found in condition must be given as a vector of Symbols.

  @fail usage:

    •  @fail(pattern_vars, condition, msg): Fail with the message msg if condition is satisfied. The pattern variables found in condition must be given as a vector of Symbols.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> @pattern a + b  # Simple expression with no pattern variables.
  Pattern:
  [call-i]
    a                                      :: Identifier
    +                                      :: Identifier
    b                                      :: Identifier

  julia> assign_to_x = @pattern begin
             {x:::assign}                                    # Pattern variable matching an assignment.
             @fail [:x] x.lhs.name == "x" "assignment to x"  # The matching fails if the rhs variable's name is "x".
         end
  Pattern:
  [~and]
    x:::assign                             :: ~var
    [~fail]
      [vect]
        [quote-:]
          x                                :: Identifier
      [call-i]
        [.]
          [.]
            x                              :: Identifier
            lhs                            :: Identifier
          name                             :: Identifier
        ==                                 :: Identifier
        [string]
          "x"                              :: String

  julia> syntax_match(assign_to_x, JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "x = 2"))
  MatchFail("assignment to x")

  julia> const_reassign = @pattern begin
             const {a:::identifier} = {_}
             {_}...
             const {a:::identifier} = {_}  # Reassignment of `const`.
         end
  Pattern:
  [toplevel]
    [const]
      [=]
        a:::identifier                     :: ~var
        _:::expr                           :: ~var
    [~rep]
      _:::expr                             :: ~var
    [const]
      [=]
        a:::identifier                     :: ~var
        _:::expr                           :: ~var

  julia> syntax_match(const_reassign, parseall(SyntaxNode,
                                               """
                                               const x = 2
                                               other_ex
                                               const x = 3
                                               """))
  BindingSet @ 0:0 with 1 entries:
    :a => Binding:
            Name: :a
            Bound source: x @ 3:7
            Ellipsis depth: 0
            Sub-bindings:
              BindingSet @ 0:0 with 1 entries:
                :_id => Binding:
                          Name: :_id
                          Bound source: x @ 3:7
                          Ellipsis depth: 0
                          Sub-bindings:
                            BindingSet @ 0:0 with 0 entries

  julia> esc_pattern = @pattern @esc(x...)
  Pattern:
  [...]
    x                                      :: Identifier

  julia> syntax_match(esc_pattern, parsestmt(SyntaxNode, "x..."))
  BindingSet @ 0:0 with 0 entries

  julia> esc1_pattern = @pattern @esc({x}..., 1)
  Pattern:
  [...]
    x:::expr                               :: ~var

  julia> syntax_match(esc1_pattern, parsestmt(SyntaxNode, "x..."))
  BindingSet @ 0:0 with 1 entries:
    :x => Binding:
            Name: :x
            Bound source: x @ 1:1
            Ellipsis depth: 0
            Sub-bindings:
              BindingSet @ 0:0 with 0 entries

  Note: @when, @fail and @esc macros only exist inside @pattern bodies.

  julia> @when [] :false
  ERROR: LoadError: UndefVarError: `@when` not defined in `Main`
  Suggestion: check for spelling errors or missing imports.
  in expression starting at ...

  julia> @fail [] :false ""
  ERROR: LoadError: UndefVarError: `@fail` not defined in `Main`
  Suggestion: check for spelling errors or missing imports.
  in expression starting at ...

  julia> @esc
  ERROR: LoadError: UndefVarError: `@esc` not defined in `Main`
  Suggestion: check for spelling errors or missing imports.
  Hint: a global variable of this name also exists in MacroTools.
  in expression starting at ...
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
are explained later (see [Pattern
forms](https://github.com/iuliadmtru/Argus.jl?tab=readme-ov-file#pattern-forms)
and [Syntax
classes](https://github.com/iuliadmtru/Argus.jl?tab=readme-ov-file#syntax-classes)).

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
surrounded by any ellipses (`...` – see
[Ellipses](https://github.com/iuliadmtru/Argus.jl?tab=readme-ov-file#ellipses)).

`syntax_match` matches a pattern with a source node exactly. It may
also be useful to extract all the pattern's occurences in a source
node. `syntax_match_all` does just that:

```julia
julia> syntax_match_all(expr, parsestmt(SyntaxNode, "1 + 2"))
MatchResults with 4 matches and 0 failures:
Matches:
  BindingSet(:x => Binding(:x, (call-i 1 + 2) @ 1:1, BindingSet()))
  BindingSet(:x => Binding(:x, 1 @ 1:1, BindingSet()))
  BindingSet(:x => Binding(:x, + @ 1:3, BindingSet()))
  BindingSet(:x => Binding(:x, 2 @ 1:5, BindingSet()))
```

#### Comment Patterns

Regular patterns match `SyntaxNode`s, but ignore trivia nodes such as
comments. The `@comment` macro allows defining patterns that match
comments. These patterns consist of strings or regular expressions.

```julia
help?> @comment
  @comment(expr)

  Create a CommentPattern from the given String or Regex.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> @comment "This is a comment"
  CommentPattern:
  # This is a comment

  julia> @comment """
             This is a
             multiline comment"""
  CommentPattern:
  #= This is a
  multiline comment =#

  julia> @comment r"TODO[\S\s]*"
  CommentPattern:
  # r"TODO[\S\s]*"
```

A `CommentPattern` doesn't have an equivalent for `syntax_match`; it
only has one for `syntax_match_all` – `comment_match_all`:

```julia
julia> src = parseall(SyntaxNode, """
                                  # x
                                  x = 1
                                  x = 2  # TODO: Remove reassignment
                                  function f(x)
                                      # TODO: Change `x` to `a`
                                      return x
                                  end
                                  """);

julia> todo_comment = @comment r"TODO[\S\s]*"
CommentPattern:
# r"TODO[\S\s]*"

julia> comment_match_all(todo_comment, src).matches
2-element Vector{BindingSet}:
 BindingSet @ 3:8 with 0 entries
 BindingSet @ 5:5 with 0 entries
```

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


#### Syntax Classes

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

#### Pattern Forms

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

##### Active Pattern Forms

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

##### Passive Pattern Forms

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
BindingSet @ 0:0 with 1 entry:
  :vs => Binding:
           Name: :vs
           Bound sources: [(vect 1 2 3) @ 1:9, 4 @ 1:20]
           Ellipsis depth: 1
           Sub-bindings:
             [
              BindingSet @ 0:0 with 0 entries,
              BindingSet @ 0:0 with 0 entries
             ]
```

Then, we write the template and expand it using the bindings obtained
after matching:

```julia
julia> template = @template vcat({vs}...)
SyntaxPatternNode:
[call]
  vcat                                   :: Identifier
  [~rep]
    [~var]
      [quote-:]
        vs                               :: Identifier
      [quote-:]
        expr                             :: Identifier

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

Template:
<no template>

Hooks:
<no hooks>

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
  @ 1:1
  BindingSet(:a => Binding(:a, (= x 2) @ 1:1, BindingSet(:rhs => Binding(:rhs, 2 @ 1:5, BindingSet()), :lhs => Binding(:lhs, x @ 1:1, BindingSet()))))

  @ 2:1
  BindingSet(:a => Binding(:a, (= y (call-i x + 1)) @ 2:1, BindingSet(:rhs => Binding(:rhs, (call-i x + 1) @ 2:5, BindingSet()), :lhs => Binding(:lhs, y @ 2:1, BindingSet()))))

  @ 4:5
  BindingSet(:a => Binding(:a, (= z true) @ 4:5, BindingSet(:rhs => Binding(:rhs, true @ 4:9, BindingSet()), :lhs => Binding(:lhs, z @ 4:5, BindingSet()))))
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
               @fail [:a] begin
                   !JuliaSyntax.is_literal(a.rhs.src)
               end "expected literal rhs"
           end
       end;

julia> match_result = rule_match(lit_assignments, parseall(SyntaxNode, src))
RuleMatchResult with 2 matches and 0 failures:
Matches:
  @ 1:1
  BindingSet(:a => Binding(:a, (= x 2) @ 1:1, BindingSet(:rhs => Binding(:rhs, 2 @ 1:5, BindingSet()), :lhs => Binding(:lhs, x @ 1:1, BindingSet()))))

  @ 4:5
  BindingSet(:a => Binding(:a, (= z true) @ 4:5, BindingSet(:rhs => Binding(:rhs, true @ 4:9, BindingSet()), :lhs => Binding(:lhs, z @ 4:5, BindingSet()))))
```

`rule_match` can keep track of all non-trivial failed matches as well
by setting the `only_matches` keyword argument to `false`. This can be
useful for debugging a rule.

```julia
julia> rule_match(lit_assignments, parseall(SyntaxNode, src); only_matches=false).failures
21-element Vector{MatchFail}:
 MatchFail: expected assignment @ :1:1
 MatchFail: expected assignment @ :1:1
 MatchFail: expected assignment @ :1:5
 MatchFail: expected literal rhs @ :2:1
 MatchFail: expected assignment @ :2:1
 MatchFail: expected assignment @ :2:5
 MatchFail: expected assignment @ :2:5
 MatchFail: expected assignment @ :2:7
 MatchFail: expected assignment @ :2:9
 MatchFail: expected assignment @ :3:1
 MatchFail: expected assignment @ :3:3
 MatchFail: expected assignment @ :3:4
 MatchFail: expected assignment @ :3:6
 MatchFail: expected assignment @ :3:9
 MatchFail: expected assignment @ :3:10
 MatchFail: expected assignment @ :4:5
 MatchFail: expected assignment @ :4:9
 MatchFail: expected assignment @ :6:1
 MatchFail: expected assignment @ :6:1
 MatchFail: expected assignment @ :6:1
 MatchFail: expected assignment @ :6:7
```

The result of a rule match consists of two vectors: one with all
non-trivial failures and one with all matches and their associated
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
               @when [:randf] match(r"^(Base.)?rand$", randf.name) !== nothing
           end

           template = @template rand(Bool)
       end;
```

We could match this rule against a file:

```julia
julia> f = tempname();

julia> src = """
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
           """;

julia> write(f, src);

julia> rule_match(rand_bool, f)
RuleMatchResult with 3 matches and 0 failures:
Matches:
  @ /var/folders/4p/xtm72jnx4654xybjwm1mpd0h0000gn/T/jl_OBsPMZSfGH:2:1
  BindingSet(:randf => Binding(:randf, rand @ 2:1, BindingSet()))
  (call rand Bool)

  @ /var/folders/4p/xtm72jnx4654xybjwm1mpd0h0000gn/T/jl_OBsPMZSfGH:6:7
  BindingSet(:randf => Binding(:randf, rand @ 6:8, BindingSet()))
  (call rand Bool)

  @ /var/folders/4p/xtm72jnx4654xybjwm1mpd0h0000gn/T/jl_OBsPMZSfGH:12:16
  BindingSet(:randf => Binding(:randf, rand @ 12:17, BindingSet()))
  (call rand Bool)
```

Rules can also be bulk-matched using `rules_match`:

```julia
julia> compare_nothing = @rule "compare-nothing" begin
           description = """
           Comparisons of `nothing` should be made with === or !== or with isnothing().
           """

           pattern = @pattern begin
               ~or(
                   nothing == {_},
                   {_} == nothing,
                   nothing != {_},
                   {_} != nothing
               )
           end
       end;

julia> useless_equals = @rule "useless-equals" begin
           description = """
           Comparing the same object in the RHS and LHS is pointless.
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

julia> rules_match([compare_nothing, useless_equals], parsestmt(SyntaxNode, "nothing == nothing"))
RuleGroupMatchResult with 2 entries:
  "useless-equals"  => RuleMatchResult(Tuple{BindingSet, Union{Nothing, SyntaxNode}}[(BindingSet(:x=>Binding(:x, nothing @ 1:12, BindingSet())), nothing)], MatchFail[])
  "compare-nothing" => RuleMatchResult(Tuple{BindingSet, Union{Nothing, SyntaxNode}}[(BindingSet(), nothing), (BindingSet(), nothing)], MatchFail[])
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
               @when [:x] any(islowercase, x.name)
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
RuleGroupMatchResult with 2 entries:
  "useless-equals"  => RuleMatchResult(Tuple{BindingSet, Union{Nothing, SyntaxNode}}[(BindingSet(:x=>Binding(:x, a @ 1:6, BindingSet())), nothing)], MatchFail[])
  "lowercase-const" => RuleMatchResult(Tuple{BindingSet, Union{Nothing, SyntaxNode}}[(BindingSet(:x=>Binding(:x, low @ 2:7, BindingSet())), nothing)], MatchFail[])
```

##### Disabling Rules

Linters provide a set of built-in linting rules. For a given code
base, not all of them may be useful in every case. That is why linters
generally also provide a mechanism for disabling rules. Argus'
consists of a user-defined `RuleDisabler`.

```julia
help?> RuleDisabler
search: RuleDisabler CommentDisabler

  RuleDisabler <: Function

  Supertype for all rule disablers.

  RuleDisablers other than CommentDisablers must define the following methods:

  """
      disabler(src::JuliaSyntax.SyntaxNode)::Bool

  Disable all rules for the given three.
  """
  disabler(src::JuliaSyntax.SyntaxNode)


  """
      disabler(rule::Rule, src::JuliaSyntax.SyntaxNode)::Bool

  Disable the given rule for the given three.
  """
  disabler(rule::Rule, src::JuliaSyntax.SyntaxNode)
```

Argus defines `CommentDisabler` as an abstract subtype of
`RuleDisabler`, and `DefaultDisabler` as the concrete type for the
built-in disabler.

```julia
help?> Argus.default_disabler
  │ Warning
  │
  │  The following bindings may be internal; they may change or be removed in future versions:
  │
  │    •  Argus.default_disabler

  default_disabler([rule::Rule,] line::AbstractString)

  The default rule disabler. Allows disabling rules in source code via comments of the form # lint-disable[: [<rule-name>, ]+]?. The rules are disabled for the annotated node.

  Examples:
  ≡≡≡≡≡≡≡≡≡

  julia> src = """
         f(x) = x

         # lint-disable
         f(x, y)

         # lint-disable: disabled_rule
         function g(x)
             f(x + 1)
         end

         # lint-disable: another_rule
         function g(x)
             f(x + 1)
         end
         """;

  julia> rule = @rule "disabled_rule" begin
             description = ""
             pattern = @pattern f({_}...)
         end;

  julia> rule_match(rule, parseall(SyntaxNode, src))
  RuleMatchResult with 2 matches and 0 failures:
  Matches:
    @ :1:1
    BindingSet()

    @ :13:5
    BindingSet()
```

The default disabler works on entire AST nodes rather than on lines of
code. A disabling annotation disables the specified rules (or all
rules if no rule name is given) for the AST node that follows it. This
is the behaviour of all `CommentDisabler`s.

##### Rule Hooks

> [!WARNING]
> Rule hooks will most likely change both behaviour and structure in
> the future.

It may be useful to define pre- and post-match hooks for certain
rules. For example, it might be necessary for some rules to only run
in certain directories or not to run in some files. For these cases,
it is possible to define `RuleHook`s:

```julia
julia> @define_rule_hook :only_in_dirs begin
    args = @pattern [{dirs}...]

    pre_check = @check [:dirs] begin
        dir_names = map(s -> s.children[1].val, dirs.src)
        if !any(contains.(current_file(), dir_names))
            skip_match()
        end
    end

    post_check = nothing
end;

julia> is_nothing = @rule "isnothing" begin
           description = "Don't use `isnothing` in performance-critical code."

           pattern = @pattern isnothing({x})

           template = @template {x} === nothing

           hooks = Dict(
               :only_in_dirs => ["performance/", "critical/"]
           )
       end
isnothing:
Don't use `isnothing` in performance-critical code.

Pattern:
[call]
  isnothing                              :: Identifier
  x:::expr                               :: ~var

Template:
SyntaxPatternNode:
[call-i]
  [~var]
    [quote-:]
      x                                  :: Identifier
    [quote-:]
      expr                               :: Identifier
  ===                                    :: Identifier
  nothing                                :: Identifier

Hooks:
  :only_in_dirs => ["performance/", "critical/"]


julia> rule_match(is_nothing, parsestmt(SyntaxNode, "isnothing(x)"))
RuleMatchResult with 0 matches and 0 failures

julia> rule_match(is_nothing, parsestmt(SyntaxNode, "isnothing(x)"; filename="performance/f.jl"))
RuleMatchResult with 1 matches and 0 failures:
Matches:
  @ performance/f.jl:1:1
  BindingSet(:x => Binding(:x, x @ 1:11, BindingSet()))
  (call-i x === nothing)
```

## Further Reading

### Matching Utils

Matching utils can be used by action pattern forms that execute code,
such as `~when`, `~fail` and `~execute`. For now, only three such
utils exist: `previous_line`, `inside_parens` and `comments`. Users
can define their own matching utils and use them in patterns.

```julia
help?> previous_line
search: previous_line

  previous_line(src::SyntaxNode)

  Return the line previous to src as a string. If src is at the first line, return the empty string.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> src = parsestmt(SyntaxNode, """
                                     function f(x)
                                         x += 1
                                         return x
                                     end
                                     """)
  SyntaxNode:
  [function]
    [call]
      f                                    :: Identifier
      x                                    :: Identifier
    [block]
      [op=]
        x                                  :: Identifier
        +                                  :: Identifier
        1                                  :: Integer
      [return]
        x                                  :: Identifier


  julia> src[2][1]
  SyntaxNode:
  [op=]
    x                                      :: Identifier
    +                                      :: Identifier
    1                                      :: Integer


  julia> previous_line(src[2][1])
  "function f(x)"

  julia> src[2][2]
  SyntaxNode:
  [return]
    x                                      :: Identifier


  julia> previous_line(src[2][2])
  "x += 1"

help?> inside_parens
search: inside_parens include_string

  inside_parens(src::SyntaxNode)

  Returns true if src is enclosed in parentheses and false otherwise.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> src = parsestmt(SyntaxNode, """ "interpolated $x" """)
  SyntaxNode:
  [string]
    "interpolated "                        :: String
    x                                      :: Identifier


  julia> inside_parens(src[2])
  false

  julia> src = parsestmt(SyntaxNode, """ "interpolated $(x)" """)
  SyntaxNode:
  [string]
    "interpolated "                        :: String
    x                                      :: Identifier


  julia> inside_parens(src[2])
  true

  julia> string_interpolation = @pattern ~and(
             "$({_}...)$({x:::identifier})$({_}...)",
             ~when([:x], !inside_parens(x.src))
         );

  julia> syntax_match(string_interpolation, parsestmt(SyntaxNode, """ "interpolated $x" """))
  BindingSet @ 0:0 with 1 entry:
    :x => Binding:
            Name: :x
            Bound source: x @ 1:17
            Ellipsis depth: 0
            Sub-bindings:
              BindingSet @ 0:0 with 0 entries

  julia> syntax_match(string_interpolation, parsestmt(SyntaxNode, """ "interpolated $(x)" """))
  MatchFail: no match @ :1:2

help?> comments
search: comments @comment codeunits convert count ncodeunits const collect count!

  comments(src::SyntaxNode)

  Returns a list of comments found in src with their associated byte ranges.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> src = parsestmt(SyntaxNode, "x #=commented=# + 1");

  julia> comments(src)
  1-element Vector{Tuple{AbstractString, UnitRange{Int64}}}:
   ("#=commented=#", 3:15)

  julia> no_comments = @pattern ~and(
             {s},
             ~when([:s], isempty(comments(s.src)))
         );

  julia> syntax_match(no_comments, parsestmt(SyntaxNode, "x #=commented=# + 1"))
  MatchFail: no match @ :1:1

  julia> syntax_match(no_comments, parsestmt(SyntaxNode, "x + 1"))
  BindingSet @ 0:0 with 1 entry:
    :s => Binding:
            Name: :s
            Bound source: (call-i x + 1) @ 1:1
            Ellipsis depth: 0
            Sub-bindings:
              BindingSet @ 0:0 with 0 entries
```

### Replacing Macros With Function Calls

Argus defines a domain-specific language for working with patterns,
rules and other syntactic constructs. This is by design, to aid with
readability and expressivity. However, almost all Argus structures can
be defined using constructors and function calls.

`@pattern expr` is equivalent to `Pattern(:( expr ))`.

```
@define_syntax_class :name "description" begin
    pattern1
    pattern2
end
```
is equivalent to
```
sc = SyntaxClass("description", [pattern1, pattern2])
register_syntax_class!(:name, sc)
```

```
@rule "rule-name" begin
    description = descr,
    pattern = patt,
    template = templ,
    hooks = Dict(...)
end
```
is equivalent to
`Rule("rule-name", descr, patt, templ, Dict(...))`

```
@define_rule_in_group group "rule-name" begin
    description = descr,
    pattern = patt,
    template = templ,
    hooks = Dict(...)
end
```
is equivalent to
```
group["rule-name"] = Rule("rule-name", descr, patt, templ, Dict(...))
```

The only exception is `@define_rule_hook`. Pre- and post-match checks
are runtime-generated functions that introduce special inner functions
such as `skip_match()`. These are automatically generated by
`@define_rule_hook`.

The user could define their own runtime-generated functions and pass
them to the `RuleHook` constructor. Then, they can use
`register_rule_hook!` to make their hook available in Argus. The
functions should define `current_file()` and `skip_match()` and should
have the following signature: `<check>(rule::Rule,
current_file_name::AbstractString, bindings::BindingSet)`.

## Notes

Argus started as the final project for my Computer Science and
Engineering degree at University Politehnica of Bucharest. The paper
is attached in the repo (Thesis.pdf).

## Acknowledgements

[Andrei Duma](https://github.com/AndreiDuma) for being providing me
with reading recommendations, feedback and constant motivation. Thank
you for telling me about the amazing `syntax/parse` library and for
spending so many hours with me discussing software design,
metaprogramming and more.

[JuliaHub](https://juliahub.com) in general and Avik Sengupta in
particular for allowing me to partly work on this project during my
internship and for guiding me in the process.

[RelationalAI](https://www.relational.ai) in general and Niko Göbel
and Alexandre Bergel in particular for pushing forward the project and
its integration with
[ReLint.jl](https://github.com/RelationalAI-oss/ReLint.jl).

## References

- [Fortifying
  macros](https://www2.ccs.neu.edu/racket/pubs/c-jfp12.pdf), a 2012
  paper by Ryan Culpepper on Racket's macro system
- [`syntax/parse`](https://docs.racket-lang.org/syntax/stxparse.html)
- [Resyntax](https://docs.racket-lang.org/resyntax/index.html)
- [Semgrep](https://semgrep.dev/docs/)
