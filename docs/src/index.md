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
