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
    @pattern ~fail(:true, "not a function definition")
end
```

Argus provides a set of pre-defined syntax classes, including `expr`,
`funcall` and `fundef`.

A _rule_ contains a description and a pattern. In the case of rules,
matching recursively traverses a given unit of source code (e.g. a
file) and collects the sub-expressions that match the rule's
pattern. Pattern variables bound by these identifications are returned
in corresponding _binding sets_.

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

First, we need to import Argus.

```julia
julia> using Argus
```

Now we can create some patterns. Let's start with the simplest
pattern.

```julia
julia> expr = @pattern {x}
Pattern:
x:::expr                                 :: ~var
```

This pattern matches any Julia expression. It has one pattern
variable: `x`. The pretty print tells us that the pattern consists of
a `~var` node which constraints `x` to be an `expr`. We'll get to
`~var` and `expr` later.

Let's try to match this pattern with some Julia expressions.

```julia
julia> using JuliaSyntax: parsestmt, SyntaxNode

julia> syntax_match(expr, parsestmt(SyntaxNode, "1 + 2"))
BindingSet with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: (call-i 1 + 2) @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet with 0 entries

julia> syntax_match(expr, parsestmt(SyntaxNode, "f(x::Int) = x * 2"))
BindingSet with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: (function-= (call f (::-i x Int)) (call-i x * 2)) @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet with 0 entries
```

The result of a successful match is a `BindingSet` — a dictionary with
bound pattern variables (`Binding`s). A `Binding` contains information
about a pattern variable that matched (part of) the source, as well as
the source node matched

In the second example above, the pretty print tells us that the
pattern variable `x` was bound to the function definition found at
line 1, column 1 in the source. The third line of the pretty print
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

julia> syntax_match(exprs, parseall(SyntaxNode, """
                                                a = 1
                                                b = 2
                                                a + b
                                                """))
BindingSet with 1 entry:
  :x => Binding:
          Name: :x
          Bound sources: [(= a 1) @ 1:1, (= b 2) @ 2:1, (call-i a + b) @ 3:1]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet with 0 entries,
             BindingSet with 0 entries,
             BindingSet with 0 entries
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
BindingSet with 3 entries:
  :f => Binding:
          Name: :f
          Bound source: my_fun @ 1:10
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet with 0 entries
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
                BindingSet with 0 entries,
                BindingSet with 0 entries
               ]
```

`args` and `body` both bind to sequences of expressions. You can see
this by looking at their ellipsis depth, which is 1, or by noting that
they are bound to vectors of source nodes instead of just source
nodes, like `x` in the `expr` pattern.

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
BindingSet with 1 entry:
  :el => Binding:
           Name: :el
           Bound sources: [[1 @ 1:3, 2 @ 1:6], [(vect 3) @ 1:11]]
           Ellipsis depth: 2
           Sub-bindings:
             [
              [
               BindingSet with 0 entries,
               BindingSet with 0 entries
              ],
              [
               BindingSet with 0 entries
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

Argus highjacks Julia's [splat
operator](https://docs.julialang.org/en/v1/base/base/#...). If we want
to use `...` with the regular splat meaning, we need to escape it:

```julia
julia> syntax_match((@pattern v...), parsestmt(SyntaxNode, "v..."))
MatchFail("no match")

julia> syntax_match((@pattern @esc(v...)), parsestmt(SyntaxNode, "v..."))
BindingSet with 0 entries
```

We can also escape an expression only up to a certain depth.

```julia
julia> syntax_match((@pattern @esc([{elems}...]..., 1)), parsestmt(SyntaxNode, "[1, 2, 3]..."))
BindingSet with 1 entry:
  :elems => Binding:
              Name: :elems
              Bound sources: [1 @ 1:2, 2 @ 1:5, 3 @ 1:8]
              Ellipsis depth: 1
              Sub-bindings:
                [
                 BindingSet with 0 entries,
                 BindingSet with 0 entries,
                 BindingSet with 0 entries
                ]
```

Without the specifying an escape depth, `@esc` would have escaped the
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

`expr` is the "category" of all Julia expressions. It is a syntax
class provided by Argus. We can define our own syntax classes and use
them in patterns. Let's write a syntax class for vectors:

```julia
julia> vec = @syntax_class "vector" begin
    @pattern [{_}...]
end
```

Syntax class bodies consist of a sequence of patterns. The match
behaviour is that of a short-circuiting `or` — the result is either
the first successful pattern match, or a failure if no patterns match.

Let's test `vec`:

```julia
julia> syntax_match(vec, parsestmt(SyntaxNode, "[]"))
BindingSet with 0 entries

julia> syntax_match(vec, parsestmt(SyntaxNode, "[1, [2]]"))
BindingSet with 0 entries
```

The match results are empty binding sets. This is because we used an
_anonymous pattern variable_ in the syntax class's pattern.

If we want to use our syntax class in a pattern with the `:::` syntax
we need to register it first.

```julia
julia> register_syntax_class!(:vec, vec)
SyntaxClass: vector
  Pattern alternative #1:
    [vect]
      [~rep]
        _:::expr                         :: ~var
```

Now we can find it in the syntax class registry under the name `:vec`.

```julia
julia> Argus.SYNTAX_CLASS_REGISTRY[:vec]
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
BindingSet with 1 entry:
  :v => Binding:
          Name: :v
          Bound sources: [(vect 1 2) @ 1:2, (vect (vect 3)) @ 1:10]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet with 0 entries,
             BindingSet with 0 entries
            ]

julia> syntax_match(vec_of_vecs2, parsestmt(SyntaxNode, "[1]"))
MatchFail("no match")
```

Argus's built-in implementation of the `vec` syntax class uses
`@define_syntax_class` to define and register `vec` and the same time:

```julia
julia> @define_syntax_class :vec "vector" begin
           @pattern [{_}...]
           @pattern ~fail(true, "not a vector")
       end
SyntaxClass: vector
  Pattern alternative #1:
    [vect]
      [~rep]
        _:::expr                         :: ~var
  Pattern alternative #2:
    [~fail]
      true                               :: Bool
      "not a vector"                     :: String
```

This implementation is longer than ours, but it has the advantage that
it can return a more specific failure message:

```julia
julia> syntax_match(vec_of_vecs2, parsestmt(SyntaxNode, "a"))
MatchFail("no match")

julia> syntax_match((@pattern {v:::vec}), parsestmt(SyntaxNode, "a"))
MatchFail("not a vector")
```

#### Pattern forms

We have come across pattern nodes preceeded by `~`, such as `~var` or
`~fail`. There are called _pattern forms_. A pattern form is a special
syntax form that performs actions on patterns or enables special
matching behaviour.

`~var` and `~fail` are both _action forms_ — `~var` binds pattern
variables and `~fail` causes the matching to fail if a given condition
is satisfied. We have seen `~fail` explicitly written in the body of
the built-in `vec` syntax class, in the pattern `@pattern ~fail(true,
"not a vector")`. This pattern means: _if the condition `true` is
satified, fail with the message "not a vector"_. Since `true` is
always satified, the syntax class will always fail with the message
"not a vector" if its first pattern fails.

`~var` expects a pattern variable name and a syntax class name. If the
source matches the syntax class, it is bound to the pattern
variable. We have seen `~var` forms before quite a lot, but not
explicitly. The `:::` syntax is an implicit `~var` form. `{x:::vec}`
is expanded to `~var(:x, :vec)` and `{x}` to `~var(:x, :expr)`. This
is visible in the patterns' pretty printed form.

`...` is another hidden pattern form called `~rep`. `~rep` take as
argument an expression that is to be matched zero or more
times. `{x}...` is short for `~rep(~var(:x, :expr))`.

Two other pattern forms exist: `~or` and `~and`. These only have
explicit forms. Both have a short-circuiting match behaviour.

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
BindingSet with 1 entry:
  :first => Binding:
              Name: :first
              Bound source: 2 @ 1:6
              Ellipsis depth: 0
              Sub-bindings:
                BindingSet with 0 entries

julia> syntax_match(equals_x, parsestmt(SyntaxNode, "2 == x"))
BindingSet with 1 entry:
  :second => Binding:
               Name: :second
               Bound source: 2 @ 1:1
               Ellipsis depth: 0
               Sub-bindings:
                 BindingSet with 0 entries

julia> syntax_match(equals_x, parsestmt(SyntaxNode, "2 == y"))
MatchFail("no match")

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
MatchFail("no match")
```

We could be more specific with the fail message for `equals_x`. Let's
use an implicit `~fail` form:

```julia
julia> equals_x = @pattern begin
           ~or({x} == 2, 2 == {x})
           @fail x.name != "x" "not x"
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
MatchFail("not x")
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
  BindingSet(:a => Binding(:a, (= y (call-i x + 1)) @ 2:1, BindingSet(:rhs => Binding(:rhs, (call-i x + 1) @ 2:5, BindingSet()), :lhs => Binding(:lhs, y @ 2:1, BindingSet(:_id => Binding(:_id, y @ 2:1, BindingSet()))))))
  BindingSet(:a => Binding(:a, (= z true) @ 4:5, BindingSet(:rhs => Binding(:rhs, true @ 4:9, BindingSet()), :lhs => Binding(:lhs, z @ 4:5, BindingSet(:_id => Binding(:_id, z @ 4:5, BindingSet()))))))
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
  BindingSet(:a => Binding(:a, (= z true) @ 4:5, BindingSet(:rhs => Binding(:rhs, true @ 4:9, BindingSet()), :lhs => Binding(:lhs, z @ 4:5, BindingSet(:_id => Binding(:_id, z @ 4:5, BindingSet()))))))
```

`rule_match` can keep track of all failed matches as well by setting
the `only_matches` keyword argument to `false`. This can be useful for
debugging a rule.

```julia
julia> rule_match(lit_assignments, parseall(SyntaxNode, src); only_matches=false).failures
21-element Vector{MatchFail}:
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("rhs not a literal")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
 MatchFail("no match")
```

At least it would be useful if the failure would contain other
information such as the source location of the failure... This is plan
for the future :).


## Matching Syntax

!! Everything from here onwards is outdated.

Argus defines a syntax matching language (_pattern language_) capable
to match Julia syntax and provide relevant information in case of
match failure. The pattern language has three types of building
blocks:

  - Regular Julia syntax
  - Pattern variables
  - Syntax classes

These three components allow the writing of patterns which can be used
to match code that "fits" them.

Here is a pattern that matches binary function calls:

```julia
julia> using Argus, JuliaSyntax

julia> binary_funcall_pattern = @pattern (_f:::identifier)(_arg1, _arg2)
Pattern:
[call]
  _f:::identifier                        :: ~var
  _arg1:::expr                           :: ~var
  _arg2:::expr                           :: ~var


julia> syntax_match(binary_funcall_pattern, parsestmt(SyntaxNode, "f(x, y)"))
BindingSet with 3 entries:
  :_arg1 => Binding(:_arg1, x, BindingSet())
  :_f    => Binding(:_f, f, BindingSet(:__id=>Binding(:__id, f, BindingSet())))
  :_arg2 => Binding(:_arg2, y, BindingSet())

julia> syntax_match(binary_funcall_pattern, parsestmt(SyntaxNode, "f(x, 1 + 2)"))
BindingSet with 3 entries:
  :_arg1 => Binding(:_arg1, x, BindingSet())
  :_f    => Binding(:_f, f, BindingSet(:__id=>Binding(:__id, f, BindingSet())))
  :_arg2 => Binding(:_arg2, (call-i 1 + 2), BindingSet())

julia> syntax_match(binary_funcall_pattern, parsestmt(SyntaxNode, "f(x)"))
MatchFail("no match")
```

### Pattern variables

A pattern variable is an identifier pattern node whose name starts
with `_`. Pattern variables can be constrained by syntax classes to
only match certain "classes" of syntax. An unconstrained pattern
variable is implicitly constrained to the `expr` syntax class, which
means it can match any expression.

`binary_funcall_pattern` contains three pattern variables: `_f`,
`_arg1` and `_arg2`. `_f` is constrained to match an
identifier. `_arg1` and `_arg2` are not explicitly constrained, which
means they are implicitly constrained to match expressions.

A successful pattern match returns a binding set with a binding for
each pattern variable. Each binding contains the pattern variable
name, the matched AST and potentially sub-bindings.

A binding can have sub-bindings if the syntax class that constrains it
contains pattern variables. In the `binary_funcall_pattern` example,
`_f` has a sub-binding `__id` because the `identifier` syntax class is
defined using the `__id` pattern variable. In the future, pattern
variables whose names start with `__` will be treated as _unexported_,
making them invisible outside the pattern they are used in.

The usefulness of sub-bindings will become clear later.

Pattern construction fails if a pattern variable constraint if the
`:::` "operator"[^1] is preceded by a non-pattern variable node (for
example: `x:::identifier`).
```julia
julia> @pattern x:::identifier
ERROR: Invalid pattern variable name x.
Pattern variable names should start with _.
Stacktrace:
 [1] error(::String, ::String)
   @ Base ./error.jl:54
 [2] Argus.VarSyntaxData(id::Symbol, syntax_class_name::Symbol)
   @ Argus ~/.../Argus.jl/src/special-syntax-data.jl:97
 [3] _parse_pattern_form(node::SyntaxNode)
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:139
 [4] parse_pattern_forms(node::SyntaxNode)
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:111
 [5] SyntaxPatternNode(ex::Expr)
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:63
 [6] Pattern(ex::Expr)
   @ Argus ~/.../Argus.jl/src/pattern.jl:8
 [7] top-level scope
   @ REPL[43]:1
```

### Syntax classes

Syntax classes allow specifying common "types" of ASTs. See the Racket
`syntax/parse` documentation on [syntax
classes](https://docs.racket-lang.org/syntax/stxparse-specifying.html)
for a more in-depth description.

TODO: Provide a more in-depth description myself...

Here is a syntax class that matches an assignment:
```julia
julia> assign =  @syntax_class "assignment" begin
           @pattern __lhs:::identifier = __rhs:::expr
       end
SyntaxClass("assignment", Pattern[(= (~var (quote-: __lhs) (quote-: identifier)) (~var (quote-: __rhs) (quote-: expr)))])
```

This is one of the built-in syntax classes and it uses two other
built-in syntax classes, `identifier` and `expr`. The most "primitive"
syntax class is `expr`. It is defined as:
```julia
@syntax_class "expr" begin
    @pattern ~fail(:false, "")
end
```

Its body means:
> Fail with the message "" when the the condition `false` evaluates to `true`.

Since `false` never evaluates to `true`, the fail condition is never
satisfied so the pattern never fails to match.

`~fail` is a pattern form. All identifiers that have a preceding `~`
are parsed as pattern forms, which are described in the following
section. They are built-in and the only way to add more pattern forms
is by modifying and extending Argus.

The `identifier` syntax class is more interesing, because it has a
more complex fail condition:
```julia
@syntax_class "identifier" begin
    @pattern begin
        ~and(__id,
             ~fail(begin
                       using JuliaSyntax: is_identifier
                       !is_identifier(__id.src)
                   end,
                   "not an identifier"))
    end
end
```

The `~and` pattern form binds `__id` to an expression AST and `~fail`
checks whether the bound AST is not an identifier using JuliaSyntax's
`is_identifier` predicate. If the fail check succeeds, the match
fails with the message "not an identifier".
```julia
julia> syntax_match((@pattern _x:::identifier), parsestmt(SyntaxNode, "1 + 2"))
MatchFail("not an identifier")
```

Syntax classes need to be registered in order to use them in
patterns. This is done through `register_syntax_class!`:
```julia
register_syntax_class!(:assign, @syntax_class "assignment" begin
                           @pattern __lhs:::identifier = __rhs:::expr
                       end)
```

### Pattern forms

Argus provides a set of special syntax forms called _pattern
forms_. These usually perform special actions such as pattern variable
binding and alternative matching. They are used to define patterns and
can be composed. See the section on [syntax
patterns](https://docs.racket-lang.org/syntax/stxparse-patterns.html)
in Racket's `syntax/parse` documentation for more pattern forms and
more details on pattern forms in general.

#### `~var`

`~var(<pattern_variable>, <syntax_class_name>)`

Binds a pattern variable to a syntax class.

```julia
julia> @pattern ~var(:_ex, :expr)
Pattern:
_ex:::expr                               :: ~var


julia> @pattern ~var(x, :identifier)
ERROR: Invalid pattern form argument `x` at (1, 7).
`~var` pattern form arguments should be `Symbol`s.
Stacktrace:
 [1] error(::String, ::String)
   @ Base ./error.jl:54
 [2] _var_arg_names(args::Vector{SyntaxNode})
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:383
 [3] _pattern_form_args(node::SyntaxNode)
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:293
 [4] _parse_pattern_form(node::SyntaxNode)
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:136
 [5] parse_pattern_forms(node::SyntaxNode)
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:111
 [6] SyntaxPatternNode(ex::Expr)
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:63
 [7] Pattern(ex::Expr)
   @ Argus ~/.../Argus.jl/src/pattern.jl:9
 [8] top-level scope
   @ REPL[40]:1

julia> @pattern ~var(:x, :identifier)
ERROR: Invalid pattern variable name x.
Pattern variable names should start with _.
Stacktrace:
 [1] error(::String, ::String)
   @ Base ./error.jl:54
 [2] Argus.VarSyntaxData(id::Symbol, syntax_class_name::Symbol)
   @ Argus ~/.../Argus.jl/src/special-syntax-data.jl:97
 [3] _parse_pattern_form(node::SyntaxNode)
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:139
 [4] parse_pattern_forms(node::SyntaxNode)
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:111
 [5] SyntaxPatternNode(ex::Expr)
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:63
 [6] Pattern(ex::Expr)
   @ Argus ~/.../Argus.jl/src/pattern.jl:9
 [7] top-level scope
   @ REPL[41]:1
```

#### `~or`

`~or(<alternatives>...)`

Short-circuits a match (success) at the first matching pattern alternative.

```julia
julia> fundef = @syntax_class "function definition" begin
           @pattern ~or(_f:::funcall = _, function (_g:::funcall) _ end)
       end
SyntaxClass("function definition", Pattern[(~or (function-= (~var (quote-: _f) (quote-: funcall)) (~var (quote-: _) (quote-: expr))) (function (~var (quote-: _g) (quote-: funcall)) (block (~var (quote-: _) (quote-: expr)))))])

julia> syntax_match(fundef, parsestmt(SyntaxNode, "f() = begin 2 end"))
BindingSet with 1 entry:
  :_f => Binding(:_f, (call f), BindingSet(:__id=>Binding(:__id, f, BindingSet(…

julia> syntax_match(fundef, parsestmt(SyntaxNode, "function f() 2 end"))
BindingSet with 1 entry:
  :_g => Binding(:_g, (call f), BindingSet(:__id=>Binding(:__id, f, BindingSet(…
```

Note that this is not actually a correct pattern for function
definitions. The `funcall` syntax class only matches function calls
with no arguments and the long form branch of the `~or` form above
only matches function definitions with one expression in the body. The
`funcall` and `fundef` syntax classes can be defined only after
defining the concept of _repetition_.

#### `~and`

`~and(<branches>...)`

Short-circuits a match (fail) at the first non-matching pattern
branch. Pattern variables from a branch are bound in succeeding
branches.

```julia
julia> conflicting = @pattern ~and(_x + 2, _x + 3)
Pattern:
[~and]
  [call-i]
    _x:::expr                            :: ~var
    +                                    :: Identifier
    2                                    :: Integer
  [call-i]
    _x                                   :: Identifier
    +                                    :: Identifier
    3                                    :: Integer


julia> syntax_match(conflicting, parsestmt(SyntaxNode, "1 + 2"))
MatchFail("no match")
```

#### `~fail`

`~fail(<condition>, <message>)`

Contains a fail condition and a message to be shown if the fail
condition evaluates to `true`.

```julia
julia> @pattern begin
          ~and(__id,
               ~fail(begin
                         using JuliaSyntax: is_identifier
                         !is_identifier(__id.src)
                     end,
                     "not an identifier"))
       end
Pattern:
[~and]
  __id:::expr                            :: ~var
  [~fail]
    [block]
      [using]
        [:]
          [importpath]
            JuliaSyntax                  :: Identifier
          [importpath]
            is_identifier                :: Identifier
      [call-pre]
        !                                :: Identifier
        [call]
          is_identifier                  :: Identifier
          [.]
            __id                         :: Identifier
            ast                          :: Identifier
    "not an identifier"                  :: String
```

The matching of a `~fail` form will error if the fail condition
references a pattern variable that was not previously bound or if the
fail condition evaluates to a non-Boolean value.

```julia
julia> pattern = @pattern ~fail(_x, "")
Pattern:
[~fail]
  _x:::expr                              :: ~var
  ""                                     :: String


julia> syntax_match(pattern, parsestmt(SyntaxNode, "dummy"))
ERROR: Binding context does not contain a binding for _x.
Stacktrace:
 [1] error(s::String)
   @ Base ./error.jl:44
 [2] (::Argus.var"#2#3"{Expr, Vector{Symbol}})(binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/special-syntax-data.jl:61
 [3] syntax_match_fail(fail_node::SyntaxPatternNode, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:94
 [4] syntax_match_pattern_form(pattern_node::SyntaxPatternNode, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:76
 [5] syntax_match(pattern_node::SyntaxPatternNode, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:44
 [6] syntax_match(pattern::Pattern, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:23
 [7] top-level scope
   @ REPL[53]:1

caused by: KeyError: key :_x not found
Stacktrace:
 [1] getindex
   @ ./abstractdict.jl:552 [inlined]
 [2] (::Argus.var"#2#3"{Expr, Vector{Symbol}})(binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/special-syntax-data.jl:57
 [3] syntax_match_fail(fail_node::SyntaxPatternNode, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:94
 [4] syntax_match_pattern_form(pattern_node::SyntaxPatternNode, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:76
 [5] syntax_match(pattern_node::SyntaxPatternNode, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:44
 [6] syntax_match(pattern::Pattern, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:23
 [7] top-level scope
   @ REPL[53]:1

julia> pattern = @pattern ~fail(:(x + 1), "")
Pattern:
[~fail]
  [call-i]
    x                                    :: Identifier
    +                                    :: Identifier
    1                                    :: Integer
  ""                                     :: String


julia> syntax_match(pattern, parsestmt(SyntaxNode, "dummy"))
ERROR: Fail condition evaluated to non-Boolean value: x + 1 (::Expr)
Stacktrace:
 [1] error(::String, ::String)
   @ Base ./error.jl:54
 [2] (::Argus.var"#30#31"{Expr, Vector{Symbol}})(binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/special-syntax-data.jl:75
 [3] syntax_match_fail(fail_node::SyntaxPatternNode, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:94
 [4] syntax_match_pattern_form(pattern_node::SyntaxPatternNode, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:76
 [5] syntax_match(pattern_node::SyntaxPatternNode, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:44
 [6] syntax_match(pattern::Pattern, src::SyntaxNode, binding_context::BindingSet)
   @ Argus ~/.../Argus.jl/src/syntax-match.jl:23
 [7] top-level scope
   @ REPL[63]:1
```

`~fail` pattern forms appear implicitly in pattern fail
conditions. For example, the `identifier` syntax class can also be
defined using the following syntax:
```julia
@syntax_class "identifier" begin
    @pattern begin
        __id
        @fail begin
            using JuliaSyntax: is_identifier
            !is_identifier(__id.src)
        end "not an identifier"
    end
end
```

The first expression in a `@pattern` body is the pattern
expression. The following expressions are pattern fail
conditions. `@pattern begin <expr>; @fail <condition> <message>; end`
is equivalent to `@pattern ~and(<expr>, ~fail(<condition>,
<message>))`.

The value of sub-bindings becomes clear when thinking of what could be
expressed in a fail condition. It would be useful to be able to access
the components bound within a pattern variable in order to do more
sophisticated checks.

For example, in some cases in might be necessary to match an
assignment where the right hand side satisfies some
constraint. Allowing a syntax similar to the following would be
desirable (and is in plan for the future -- I am currently working on
it):
```julia
julia> @pattern ~and(_x:::assign, ~fail(!is_literal(_x.rhs), "rhs not a literal"))
Pattern:
[~and]
  _x:::assign                            :: ~var
  [~fail]
    [call-pre]
      !                                  :: Identifier
      [call]
        is_literal                       :: Identifier
        [.]
          _x                             :: Identifier
          rhs                            :: Identifier
    "rhs not a literal"                  :: String
```

## Rule writing

Argus allows the writing on rules based on the syntax matching system
described so far.

Rules are described by a pattern and a rule description.

```julia
julia> chained_const_assignment = @rule "chained-const-assignment" begin
           description = """
           Do not chain assignments with const. The right hand side is not constant here.
           """

           pattern = @pattern begin
               const _a:::identifier = _b:::identifier = _
           end
       end
chained-const-assignment: Do not chain assignments with const. The right hand side is not constant here.
Pattern:
[const]
  [=]
    _a:::identifier                      :: ~var
    [=]
      _b:::identifier                    :: ~var
      _:::expr                           :: ~var


julia> rule_match(chained_const_assignment, parsestmt(SyntaxNode, "f(a, b) = const a = b = 1 + 2"))
RuleMatchResult(BindingSet[BindingSet(:_a => Binding(:_a, a, BindingSet(:__id => Binding(:__id, a, BindingSet()))), :_b => Binding(:_b, b, BindingSet(:__id => Binding(:__id, b, BindingSet()))))], MatchFail[])
```

`rule_match` can either return the set of matches, like above, or it
can return both matches and failures.

```julia
julia> rule_match(chained_const_assignment, parsestmt(SyntaxNode, "const a = b"); only_matches=false)
RuleMatchResult(BindingSet[], MatchFail[MatchFail("no match"), MatchFail("no match"), MatchFail("no match"), MatchFail("no match")])
```

There are four failures and no matches, accounting for four AST
comparisons:
  - `(const (= a b))`
  - `(= a b)`
  - `a`
  - `b`

Rules can also be matched against files:

```julia
julia> compare_nothing = @rule "compare-nothing" begin
           description = """
           Comparisons of `nothing` should be made with === or !== or with isnothing().
           """

           pattern = @pattern begin
               ~or(
                   nothing == _,
                   _ == nothing,
                   nothing != _,
                   _ != nothing
			   )
           end
       end;

julia> rule_match(compare_nothing, "semgrep-to-argus/compare-nothing.jl")
6-element Vector{BindingSet}:
 BindingSet()
 BindingSet()
 BindingSet()
 BindingSet()
 BindingSet()
 BindingSet()
```

As usual, anonymous pattern variables don't bind.

Rules can be grouped in `RuleGroup`s.

```julia
julia> style_rules = RuleGroup("style")
RuleGroup("style")

julia> @define_rule_in_group style_rules "useless-bool" begin
           description = "Useless boolean in if condition."
           pattern = @pattern begin
               if true
                   _
               end
           end
       end
useless-bool: Useless boolean in if condition.
Pattern:
[if]
  true                                   :: Bool
  [block]
    _:::expr                             :: ~var


julia> style_rules
RuleGroup("style") with 1 entry:
  "useless-bool" => useless-bool: Useless boolean in if condition.…

julia> test_src_match = """
       if true
           do_something()
       end
       """;

julia> test_src_no_match = """
       if cond
           do_something()
       end
       """;

julia> rule_match(style_rules["useless-bool"], parsestmt(SyntaxNode, test_src_match))
RuleMatchResult(BindingSet[BindingSet()], MatchFail[])

julia> rule_match(style_rules["useless-bool"], parsestmt(SyntaxNode, test_src_no_match))
RuleMatchResult(BindingSet[], MatchFail[])
```

These are examples of rules that exist in the
[semgrep-rules-julia](https://github.com/JuliaComputing/semgrep-rules-julia)
repository and that can be ported to Argus. All ported rules can be
found inside `semgrep-to-argus/`, grouped according to their category.

## Notes

Argus is the final project for my Computer Science and Engineering
degree at University Politehnica of Bucharest.

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


[^1]: It is not really an operator. `_x:::identifier` is parsed in
    Julia as `(::-i _x (quote-: identifier))`. Argus interprets this
	kind of node (a call to `::` with a `quote` as rhs) as a short
	form for `~var(:_x, :identifier)`.
