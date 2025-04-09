[![codecov](https://codecov.io/gh/iuliadmtru/Argus.jl/graph/badge.svg?token=Z79DY1TSL4)](https://codecov.io/gh/iuliadmtru/Argus.jl)

# Argus.jl

This package aims to be a static analysis tool for Julia. It takes
inspiration from [Semgrep](https://github.com/semgrep/semgrep),
[Clippy](https://github.com/rust-lang/rust-clippy) and
[Resyntax](https://docs.racket-lang.org/resyntax/) (mostly the
latter).


## Ideal result

I would like the user to be able to write syntax very similar to
actual Julia syntax for the rules. Some ideas:

```julia
@define_rule "my_rule" begin
	description = "Don't print integers!"
	template = :(
		println(%X)
	)
	conditions = [
		:(%X isa Int)
	]
```

```julia
@define_rule "my_rule2" begin
	description = "Don't print an integer and a negative number!"
	template = :(
		println(%X, %Y)
	) where [
		:(%X isa Int),
		:(%Y < 0)
	]
```

I am currently working on implementing the first one. The second one
could be added later as syntactic sugar over the first.


## Status

This project is part of my thesis for the Computer Science and
Engineering Bachelor's at Politehnica University of Bucharest.

Status: Thinking/Rethinking...

Currently the package is able to define rules, upload them to a
registry (the default is at `./rules-registry/`) and run them against
some source code/file.

```julia
julia> using Argus

julia> test_rule = @define_rule "my_rule" begin
           description = "My description"
           template = """
               f(Metavariable(:x)) = 2
           """
       end
[ Info: Rule stored at .../Argus.jl/rules-registry/my_rule.
line:col│ tree                                   │ metadata
   -:-  |[function-=]                            |
   -:-  |  [call]                                |
   -:-  |    f                                   |
   -:-  |    M"x"                                | nothing
   -:-  |  2                                     |

julia> using JuliaSyntax

julia> src = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "f(y) = 2")
SyntaxNode:
[function-=]
  [call]
    f                                    :: Identifier
    y                                    :: Identifier
  2

julia> matches = rule_match!(test_rule, src)
1-element SyntaxMatches:
 SyntaxMatch((function-= (call f y) 2), AbstractSyntaxPlaceholder[Metavariable(:x, JuliaSyntax.SyntaxData(SourceFile("f(y) = 2", 0, nothing, 1, [1, 9]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), 3, :y))])

julia> matches[1].ast
SyntaxNode:
[function-=]
  [call]
    f                                    :: Identifier
    y                                    :: Identifier
  2                                      :: Integer

julia> matches[1].placeholders
1-element Vector{AbstractSyntaxPlaceholder}:
 Metavariable(:x, JuliaSyntax.SyntaxData(SourceFile("f(y) = 2", 0, nothing, 1, [1, 9]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), 3, :y))

julia> matches[1].placeholders[1].name
:x

julia> matches[1].placeholders[1].binding
JuliaSyntax.SyntaxData(SourceFile("f(y) = 2", 0, nothing, 1, [1, 9]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), 3, :y)
```

Run against file:

```julia
julia> matches = rule_match!("my_rule", "test/test-file.jl")
1-element SyntaxMatches:
 SyntaxMatch((function-= (call f y) 2), AbstractSyntaxPlaceholder[Metavariable(:x, JuliaSyntax.SyntaxData(SourceFile("function f(a, b)\n    y = a + b\n    return y\nend\n\na + b\na + b + c # (call-i a + b c); 4 children, all leaves; Semgrep finds this\nc + a + b # (call-i c + a b); 4 children, all leaves; Semgrep doesn't find this\n\nf(x) = \"a\"\nf(x) = 2 + x\ng(x) = 2 + x\nf(y) = 2\n", 0, "test/test-file.jl", 1, [1, 18, 32, 45, 49, 50, 56, 129, 209, 210, 221, 234, 247, 256]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), 249, :y))])

julia> JuliaSyntax.source_location(matches[1].ast)
(13, 1)
```


## Design choices

`Argus` is built on top of
[`JuliaSyntax.jl`](https://github.com/JuliaLang/JuliaSyntax.jl). The
goal is to integrate it as much as possible with `JuliaSyntax` and
maybe also with
[`JuliaLowering`](https://github.com/c42f/JuliaLowering.jl).

### Rules

Rules can be defined in a similar way to Resyntax's
[`define-refactoring-rule`](https://docs.racket-lang.org/resyntax/Refactoring_Rules_and_Suites.html#%28form._%28%28lib._resyntax%2Fbase..rkt%29._define-refactoring-rule%29%29). The
goal is to make rule writing as intuitive as possible for Julia users
and also as extendable and configurable as possible.

`@define_rule` registers a new rule in a rule registry, or updates an
existing rule with the same name. It also returns the rule as a
`SyntaxTemplateNode` (defined in `src/syntax_template_tree.jl`). This
is an AST built on `JuliaSyntax`'s AST interface (i.e. it is a
`JuliaSyntax.TreeNode{SyntaxTemplateData}`). `SyntaxTemplateData` is a
custom syntax data type which can either mimic a regular
[`JuliaSyntax.SyntaxNode`](https://julialang.github.io/JuliaSyntax.jl/dev/api/#JuliaSyntax.SyntaxNode)
by storing data as `JuliaSyntax.SyntaxData`, or it can contain some
special syntax such as metavariables or ellipses (inspired from
Semprep's
[metavariables](https://semgrep.dev/docs/writing-rules/pattern-syntax#metavariables)
and [ellipsis
operator](https://semgrep.dev/docs/writing-rules/pattern-syntax#ellipsis-operator);
the ellipsis operator's meaning might change though in order to be
more intuitive for Julia users -- specifically, I would like it to
have a meaning similar to that of the [splat
operator](https://docs.julialang.org/en/v1/base/base/#...) or to
Racket's ellipsis used for [syntax
matching](https://docs.racket-lang.org/reference/stx-patterns.html)).


### Special syntax

The goal is to permit special syntax for non-trivial matching
(e.g. match a function call with an arbitrary number of arguments). As
a start, I would like to have some rudimentary implementation of
metavariables and ellipses.

#### Metavariables

Metavariables should bind to expressions. I have not tested this
yet. The ultimate goal is to have metavariables bind to something that
can be chosen by the user. This could be accomplished by providing
types to metavariables (either Julia built-in types or custom types
that have some kind of corresponding predicate, or something like
Racket's [syntax
classes](https://docs.racket-lang.org/syntax/stxparse-specifying.html))]).

### AST comparison

I implemented a rudimentary AST matching mechanism in
`template_match!`. There are some existing pattern matching packages
in Julia that I could have used, such as
[Match.jl](https://github.com/JuliaServices/Match.jl/tree/cb25c8c686cbd94c46f05b91a6870dbba19e0acc)
or [MLStyle.jl](https://github.com/thautwarm/MLStyle.jl). However,
they seem to be focused on smaller patterns, not on full
`SyntaxNode`s. I have only tried Match.jl and noticed that it does not
work well with large ASTs, I have not tried MLStyle.jl yet. Another
reason why I chose to implement it from scratch is that I would need
special treatment for special syntax nodes and it seemed difficult to
configure the packages I mentioned. I will look more into this though.


## Future ideas

- I really like Racket's [pattern based
  macros](https://docs.racket-lang.org/guide/pattern-macros.html),
  maybe this tool can go more towards that? Rules can be seen as
  templates which should match source code.


## Inspiration

(In random order.)

- [Semgrep](https://semgrep.dev/docs/writing-rules/overview) as a
  generic tool.
- [Resyntax](https://docs.racket-lang.org/resyntax/index.html) for
  Racket.
- [Clippy](https://doc.rust-lang.org/clippy/) for Rust.
- [Fortifying
  macros](https://www2.ccs.neu.edu/racket/pubs/c-jfp12.pdf), a 2012
  paper by Ryan Culpepper on Racket's macro system. (Thank you
  [@AndreiDuma](https://github.com/AndreiDuma)!!!)
