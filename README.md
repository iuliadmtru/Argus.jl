# Argus.jl

This package aims to be a static analysis tool for Julia. It is
inspired by [Semgrep](https://github.com/semgrep/semgrep),
[Clippy](https://github.com/rust-lang/rust-clippy) and
[Resyntax](https://docs.racket-lang.org/resyntax/).


## Status

I am working on this project as part of my thesis for the Computer
Science and Engineering Bachelor's at Politehnica University of
Bucharest.

Status: _barely beginning_

Currently the package is able to:

```julia
julia> using Argus

julia> using JuliaSyntax

julia> text = "f(x) = 2";

julia> rule = parsestmt(RuleSyntaxNode, text)
line:col│ tree                                   │ file_name
   1:1  │[=]
   1:1  │  [call]
   1:1  │    f
   1:3  │    x
   1:8  │  2

julia> p = Pattern(text)
Pattern((= (call f x) 2))
```


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

**Current problems:**

- What should `Pattern` look like?  My idea so far is to make use of
  [`JuliaSyntax`'s AST
  interface](https://github.com/JuliaLang/JuliaSyntax.jl/blob/main/src/syntax_tree.jl)
  in order to make AST comparison easier later. (That is, the
  comparison between the rule's AST and the source code's AST.)

- What should a `RuleSyntaxNode` look like? I would like a result similar to this:
	- `f(x)` -> `(:call f::SyntaxNode x::SyntaxNode)`
	- `f(...)` -> `(:call f::SyntaxNode ellipsis::SpecialSyntaxNode)`
	- `f(M"x")` -> `(:call f::SyntaxNode metavar::SpecialSyntaxNode)`

  The `SyntaxNode`s and `SpecialSyntaxNode`s would actually all be
  `RuleSyntaxNode`s. I only wrote them this way for clarity. The
  ellipsis should probably be wrapped around `M""` as well?

**Related questions:**

- What should the node data in `Argus` look like? I'm thinking it should
  have a
  [`GreenNode`](https://julialang.github.io/JuliaSyntax.jl/dev/api/#JuliaSyntax.GreenNode)
  and a value. (Similar to `JuliaSyntax`'s `SyntaxData`, but without
  the source and position.)

- If it has a `GreenNode` field, should it parametrized (something
  like `GreenNode{RuleSyntaxHead}`) or should I keep it as
  `GreenNode{SyntaxHead}` and add custom
  [`Kind`](https://julialang.github.io/JuliaSyntax.jl/dev/api/#JuliaSyntax.Kind)s?
  I am inclined to choose the latter.
