[![codecov](https://codecov.io/gh/iuliadmtru/Argus.jl/graph/badge.svg?token=Z79DY1TSL4)](https://codecov.io/gh/iuliadmtru/Argus.jl)

# Argus.jl

This package aims to be a static analysis tool for Julia. It takes
inspiration from [Semgrep](https://github.com/semgrep/semgrep),
[Clippy](https://github.com/rust-lang/rust-clippy) and
[Resyntax](https://docs.racket-lang.org/resyntax/) (mostly the
latter).


## Ideal result

### Rules

I would like the user to be able to write syntax very similar to
actual Julia syntax for the rules. Some ideas:

```julia
@rule "my_rule" begin
	description = "Don't print integers!"
	pattern = :(
		println(%X)
	)
	conditions = [
		:(%X isa Int)
	]
end
```

```julia
@rule "my_rule2" begin
	description = "Don't print an integer and a negative number!"
	pattern = :(
		println(%X, %Y)
	) where [
		:(%X isa Int),
		:(%Y < 0)
	]
end
```

I am currently working on implementing the first one. The second one
could be added later as syntactic sugar over the first.

Sadly it is quite difficult to manage patterns passed as expressions,
as much as I would like it. The reason is that I could't find a way to
correctly transform an `Expr` into a `SyntaxNode`, without
losing/adding any information to the AST. As an example, consider the
simple function definition `f(x) = 2`:

```julia
julia> JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "f(x) = 2")
SyntaxNode:
[function-=]
  [call]
    f                                    :: Identifier
    x                                    :: Identifier
  2                                      :: Integer


julia> :(f(x) = 2)
:(f(x) = begin
          #= REPL[3]:1 =#
          2
      end)
```

The `Expr` transforms the function body into a block, while the
`SyntaxNode` doesn't.

This is why I settled for pattern strings.

TODO: Meta.unblock/Meta.unescape?

### Rule groups

The user should be able to group rules together.

```julia
julia> style_rules = RuleGroup("style")

julia> @define_rule_in_group style_rules "useless-bool" begin
           description = "Useless boolean in if condition"
           pattern = """
               if true Metavariable(:body) end
           """
       end

julia> style_rules
RuleGroup "style" with 1 entry:
  "useless-bool" => (if true (block M"body"))
```

## Status

This project is part of my thesis for the Computer Science and
Engineering Bachelor's at Politehnica University of Bucharest.

Status: Thinking/Rethinking...

Currently the package is able to define rules, define rule groups,
store rules in groups and run rules against some source code/file. The
rules can contain metavariables, but the implementation is still in
the very early stages.

```julia
julia> using Argus

julia> useless_bool = @rule "useless-bool" begin
           description = "Useless boolean in if condition"
           pattern = """
               if true Metavariable(:body) end
           """
       end
line:col│ tree                                   │ metadata
   -:-  |[if]                                    |
   -:-  |  true                                  |
   -:-  |  [block]                               |
   -:-  |    M"body"                             | nothing

julia> using JuliaSyntax

julia> test_src_match = """
       if true
           do_something()
       end
       """;

julia> test_expr_match = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, test_src_match);

julia> matches = rule_match!(useless_bool, test_expr_match)
1-element SyntaxMatches:
 SyntaxMatch((if true (block (call do_something))), AbstractSyntaxPlaceholder[Metavariable(:body, JuliaSyntax.SyntaxData(SourceFile("if true\n    do_something()\nend", 0, nothing, 1, [1, 9, 28, 31]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"call", 0x0000), 0x0000000e, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x0000000c, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"(", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K")", 0x0001), 0x00000001, nothing)]), 13, nothing))])

julia> matches[1].ast
SyntaxNode:
[if]
  true                                   :: Bool
  [block]
    [call]
      do_something                       :: Identifier


julia> matches[1].placeholders
1-element Vector{AbstractSyntaxPlaceholder}:
 Metavariable(:body, JuliaSyntax.SyntaxData(SourceFile("if true\n    do_something()\nend", 0, nothing, 1, [1, 9, 28, 31]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"call", 0x0000), 0x0000000e, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x0000000c, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"(", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K")", 0x0001), 0x00000001, nothing)]), 13, nothing))

julia> matches[1].placeholders[1].name
:body

julia> matches[1].placeholders[1].binding
JuliaSyntax.SyntaxData(SourceFile("if true\n    do_something()\nend", 0, nothing, 1, [1, 9, 28, 31]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"call", 0x0000), 0x0000000e, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x0000000c, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"(", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K")", 0x0001), 0x00000001, nothing)]), 13, nothing)
```

After this, the rule's placeholders are bound and it wouldn't match
again. (I need to fix this.) So we need to create the rule again to
test it with some other source code. This time we will load it in a
group.

```julia
julia> test_src_no_match = """
       if cond
           do_something()
       end
       """;

julia> test_expr_no_match = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, test_src_no_match);

julia> style_rules = RuleGroup("style")
RuleGroup()

julia> @define_rule_in_group style_rules "useless-bool" begin
                  description = "Useless boolean in if condition"
                  pattern = """
                      if true Metavariable(:body) end
                  """
              end
line:col│ tree                                   │ metadata
   -:-  |[if]                                    |
   -:-  |  true                                  |
   -:-  |  [block]                               |
   -:-  |    M"body"                             | nothing

julia> style_rules
RuleGroup style with 1 entry:
  "useless-bool" => (if true (block M"body"))

julia> rule_match!(style_rules["useless-bool"], test_expr_no_match)
SyntaxMatch[]
```

This time we get no match.

Run against file:

```julia
julia> matches = rule_match!(style_rules["useless-bool"], "test/test-file.jl")
SyntaxMatch[]
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

`@rule` creates a rule as a `SyntaxPatternNode` (defined in
`src/syntax_pattern_tree.jl`). This is an AST built on
`JuliaSyntax`'s AST interface (i.e. it is a
`JuliaSyntax.TreeNode{SyntaxPatternData}`). `SyntaxPatternData` is a
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

Rules can also be created with `create_rule`. They can also be created
inside `RuleGroup`s with `@define_rule_in_group` (or
`define_rule_in_group`).

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
`pattern_match!`. There are some existing pattern matching packages
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
  patterns which should match source code.


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
