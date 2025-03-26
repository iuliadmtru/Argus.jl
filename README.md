# Argus.jl

This package aims to be a static analysis tool for Julia. It is
inspired by [Semgrep](https://github.com/semgrep/semgrep),
[Clippy](https://github.com/rust-lang/rust-clippy) and
[Resyntax](https://docs.racket-lang.org/resyntax/).


## Status

I am working on this project as part of my thesis for the Computer
Science and Engineering Bachelor's at Politehnica University of
Bucharest.

Status: _barely beginning_. Working on metavariables.

Currently the package is able to:

```julia
julia> using Argus

julia> using JuliaSyntax

julia> rule = Pattern("f(x) = 2")
Pattern((= (call f x) 2))

julia> rule.ast
line:col│ tree
   -:-  |[=]
   -:-  |  [call]
   -:-  |    f
   -:-  |    x
   -:-  |  2

julia> rule = Pattern("f(Metavariable(:X)) = 2 + Metavariable(:Y)")
Pattern((= (call f M"X") (call-i 2 + M"Y")))

julia> rule.ast
line:col│ tree
   -:-  |[=]
   -:-  |  [call]
   -:-  |    f
   -:-  |    M"X"
   -:-  |  [call-i]
   -:-  |    2
   -:-  |    +
   -:-  |    M"Y"


julia> Argus.check!(rule, "test/test-file.jl")
1-element RuleMatches:
 RuleMatch((= (call f x) (call-i 2 + x)), (11, 1))
```

The AST comparison mechanism used by `Argus.check!` is currently a mock
implementation (in `src/utils.jl`).


## Design choices

`Argus` is built on top of
[`JuliaSyntax.jl`](https://github.com/JuliaLang/JuliaSyntax.jl). The
goal is to integrate it as much as possible with `JuliaSyntax` and
maybe also with
[`JuliaLowering`](https://github.com/c42f/JuliaLowering.jl).

### Rules

Most inspiration for rule design comes from Semgrep for now because I
am more familiar with it than with other similar tools. The basic
element of a rule should be `Pattern`, which allows matching code
corresponding to a given expression.

`Pattern` is defined in `src/rule_syntax.jl`. It contains the
rule-specific AST, `RuleSyntaxNode`, defined in `src/Argus.jl`. This
AST is built `JuliaSyntax`'s AST interface (i.e. it is a
`JuliaSyntax.TreeNode{RuleSyntaxData}`). `RuleSyntaxData` is a custom
syntax data type which can either mimic a regular
[`JuliaSyntax.SyntaxNode`](https://julialang.github.io/JuliaSyntax.jl/dev/api/#JuliaSyntax.SyntaxNode)
by storing data as `JuliaSyntax.SyntaxData`, or it can contain some
special syntax such as metavariables or ellipsis (inspired from
Semprep's
[metavariables](https://semgrep.dev/docs/writing-rules/pattern-syntax#metavariables)
and [ellipsis
operator](https://semgrep.dev/docs/writing-rules/pattern-syntax#ellipsis-operator))

There should be a different, custom representation for
`RuleSyntaxNode`s.

### Patterns

#### Metavariables

Metavariables currently only bind to whatever
`JuliaSyntax.is_valid_identifier` returns `true` for.

### AST comparison

This is probably the most difficult part of the project. For now I am
using a mock implementation for testing purposes. I keep adding to
it/changing it as I go because I need to constantly ask myself what
design choices to make in order to be able to compare the rules' AST
to the source code AST.

**Current questions/decisions to make:**

- HOW DO I COMPARE THE ASTS??
- How should metavariables be represented?
- How do source code expressions bind to metavariables?
- How should the ellipsis operator be implemented? Is `JuliaLowering`
  necessary for implementing it? (This is likely much harder than
  metavariables so I'll leave it for later.)
