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

### Rule groups

The user should be able to group rules together.

```julia
julia> style_rules = RuleGroup("style")

julia> @define_rule_in_group style_rules "useless-bool" begin
           description = "Useless boolean in if condition"
           pattern = :(
               if true %body end
           )
       end

julia> style_rules
RuleGroup "style" with 1 entry:
  "useless-bool" => (if true (block %body))
```

## Status

This project is part of my thesis for the Computer Science and
Engineering Bachelor's at Politehnica University of Bucharest.

Status: Adding `and` and `or` pattern directives...

For now the package is able to define rules, define rule groups,
store rules in groups and run rules against some source code/file. The
rules can contain metavariables which bind to expressions in the
source code if the pattern matches.

```julia
julia> using Argus

julia> useless_bool = @rule "useless-bool" begin
           description = "Useless boolean in if condition."
           pattern = :(
           if true
               m"body"
           end
           )
       end
useless-bool: Useless boolean in if condition.
line:col│ tree                                   │ metadata
   -:-  |[if]                                    |
   -:-  |  true                                  |
   -:-  |  [block]                               |
   -:-  |    %body                               | nothing

julia> using JuliaSyntax

julia> test_src_match = """
       if true
           do_something()
       end
       """;

julia> test_expr_match = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, test_src_match);

julia> matches = rule_match!(useless_bool, test_expr_match)
1-element SyntaxMatches:
 SyntaxMatch((if true (block (call do_something))), AbstractSyntaxPlaceholder[Metavariable(body, call@13)])

julia> matches[1].ast
SyntaxNode:
[if]
  true                                   :: Bool
  [block]
    [call]
      do_something                       :: Identifier


julia> matches[1].placeholders
1-element Vector{AbstractSyntaxPlaceholder}:
 Metavariable(body, call@13)

julia> matches[1].placeholders[1].name
:body

julia> matches[1].placeholders[1].binding
JuliaSyntax.SyntaxData(SourceFile("if true\n    do_something()\nend", 0, nothing, 1, [1, 9, 28, 31]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"call", 0x0000), 0x0000000e, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x0000000c, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"(", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K")", 0x0001), 0x00000001, nothing)]), 13, nothing)
```

After this, the rule's placeholders are bound and it wouldn't match
again. So we need to create the rule again to test it with some other
source code. This time we will load it in a group.

```julia
julia> test_src_no_match = """
       if cond
           do_something()
       end
       """;

julia> test_expr_no_match = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, test_src_no_match);

julia> style_rules = RuleGroup("style")
RuleGroup("style")

julia> @define_rule_in_group style_rules "useless-bool" begin
           description = "Useless boolean in if condition."
           pattern = :(
           if true
               m"body"
           end
           )
       end
useless-bool: Useless boolean in if condition.
line:col│ tree                                   │ metadata
   -:-  |[if]                                    |
   -:-  |  true                                  |
   -:-  |  [block]                               |
   -:-  |    %body                               | nothing

julia> style_rules
RuleGroup("style") with 1 entry:
  "useless-bool" => useless-bool: Useless boolean in if condition…

julia> rule_match!(style_rules["useless-bool"], test_expr_no_match)
SyntaxMatch[]
```

This time we get no match.

You could also create composite patterns, for now using the directives
`and` and `or`. Here's a useful example using `or`
([here](https://github.com/JuliaComputing/semgrep-rules-julia/blob/main/rules/lang/correctness/compare-nothing.yaml)'s)
the Semgrep alternative for reference):

```julia
julia> compare_nothing = @rule "compare-nothing" begin
           description = """
           Comparisons of `nothing` should be made with === or !== or with isnothing().
           """
           pattern = or(
           :(nothing == m"_"),
           :(m"_" == nothing),
           :(nothing != m"_"),
           :(m"_" != nothing)
           )
       end
compare-nothing: Comparisons of `nothing` should be made with === or !== or with isnothing().
line:col│ tree                                   │ metadata
   -:-  |[directive-or]                          |
   -:-  |  [call-i]                              |
   -:-  |    nothing                             |
   -:-  |    ==                                  |
   -:-  |    %_                                  | nothing
   -:-  |  [call-i]                              |
   -:-  |    %_                                  | nothing
   -:-  |    ==                                  |
   -:-  |    nothing                             |
   -:-  |  [call-i]                              |
   -:-  |    nothing                             |
   -:-  |    !=                                  |
   -:-  |    %_                                  | nothing
   -:-  |  [call-i]                              |
   -:-  |    %_                                  | nothing
   -:-  |    !=                                  |
   -:-  |    nothing                             |


julia> src = """
       x = nothing

       if y == nothing end

       if cond1 && z != nothing
           do_something()
       else
           do_something_else()
       end

       q === nothing ? ok() : not_ok()
       """;

julia> matches = rule_match!(compare_nothing, JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src))
2-element SyntaxMatches:
 SyntaxMatch((call-i y == nothing), AbstractSyntaxPlaceholder[Metavariable(_, y@17)])
 SyntaxMatch((call-i z != nothing), AbstractSyntaxPlaceholder[Metavariable(_, z@47)])
```

And here are some not-so-useful examples using `and`:

```julia
julia> conflicting_branches = SyntaxPatternNode(:(and(:(m"x" = 2), :(m"x" = 3))))
line:col│ tree                                   │ metadata
   -:-  |[directive-and]                         |
   -:-  |  [=]                                   |
   -:-  |    %x                                  | nothing
   -:-  |    2                                   |
   -:-  |  [=]                                   |
   -:-  |    %x                                  | nothing
   -:-  |    3                                   |


julia> matches = pattern_match!(conflicting_branches, JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, "abc = 3"))
┌ Warning: Conflicting `and` branches:
│ (= %x 2)
│ (= %x 3)
└ @ Argus ~/.../Argus.jl/src/matching.jl:141
SyntaxMatch[]

julia> rule = @rule "and-rule" begin
           description = "Note that metavariables with the same name from different branches are unified."
           pattern = :(
           and(
               :(m"x" = y),
               :(m"y" = m"x")
           ))
       end;

julia> src = """
       y = y
       x = y
       f(x) = y
       """;

julia> matches = rule_match!(rule, JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src))
2-element SyntaxMatches:
 SyntaxMatch((= y y), AbstractSyntaxPlaceholder[Metavariable(x, y@1)])
 SyntaxMatch((= y y), AbstractSyntaxPlaceholder[Metavariable(y, y@1), Metavariable(x, y@5)])
```

In the second example, only one AST node is matched: `y = y`. There
are two matches because both subpatterns generate matches. Actually,
every `and` composite pattern will generate 2x the number of "true"
matches. `or` patterns also return all the matches from all
subpatterns, even though they correspond the the same AST in the
source code. The duplicate information is useful when wanting to see
exactly what each subpattern matches. I will find a better way to
provide the extra info sometime.

Currently I'm working on reorganising the code to make it more
"algebra-driven" (inspired by Sandy Maguire's "Algebra-Driven Design";
I've only read the introduction though...).

### Demo

Inside `test/` there's a `demo/` directory where I try to rewrite some
rules from the [`semgrep-rules-julia`
repo](https://github.com/JuliaComputing/semgrep-rules-julia). Once
`Argus` has support for repetitions (similar to Semgrep's [ellipsis
operator](https://semgrep.dev/docs/writing-rules/pattern-syntax#ellipsis-operator))
it will be possible to rewrite most rules. For now, I rewrote the
following:
- [chained-const-assignment](https://github.com/JuliaComputing/semgrep-rules-julia/blob/main/rules/lang/correctness/chained-const-assignment.yaml)
- [compare-nothing](https://github.com/JuliaComputing/semgrep-rules-julia/blob/main/rules/lang/correctness/compare-nothing.yaml)
- [useless-equals](https://github.com/JuliaComputing/semgrep-rules-julia/blob/main/rules/lang/best-practice/useless-equals.yaml)

You can test the rules in the demo:

```julia
julia> include("test/demo/lang_rules.jl");

julia> chained_const_assignment = lang_rules["chained-const-assignment"]
chained-const-assignment: Do not chain assignments with const. The right hand side is not constant here.
line:col│ tree                                   │ metadata
   -:-  |[const]                                 |
   -:-  |  [=]                                   |
   -:-  |    %x                                  | nothing
   -:-  |    [=]                                 |
   -:-  |      %y                                | nothing
   -:-  |      %_                                | nothing


julia> rule_match!(chained_const_assignment, "test/demo/chained_const_assignment.jl")
4-element SyntaxMatches:
 SyntaxMatch((const (= a (= b 1))), AbstractSyntaxPlaceholder[Metavariable(x, a@23), Metavariable(y, b@27), Metavariable(_, 1@31)])
 SyntaxMatch((const (= a (= b c))), AbstractSyntaxPlaceholder[Metavariable(x, a@56), Metavariable(y, b@60), Metavariable(_, c@64)])
 SyntaxMatch((const (= a (= b (= c 1)))), AbstractSyntaxPlaceholder[Metavariable(x, a@89), Metavariable(y, b@93), Metavariable(_, =@97)])
 SyntaxMatch((const (= a (= b (string "abc")))), AbstractSyntaxPlaceholder[Metavariable(x, a@126), Metavariable(y, b@130), Metavariable(_, string@134)])

julia> useless_equals = lang_rules["useless-equals"]
useless-equals: Comparing the same object in the RHS and LHS is pointless.
line:col│ tree                                   │ metadata
   -:-  |[directive-or]                          |
   -:-  |  [call-i]                              |
   -:-  |    %x                                  | nothing
   -:-  |    ==                                  |
   -:-  |    %x                                  | nothing
   -:-  |  [call-i]                              |
   -:-  |    %x                                  | nothing
   -:-  |    !=                                  |
   -:-  |    %x                                  | nothing
   -:-  |  [call-i]                              |
   -:-  |    %x                                  | nothing
   -:-  |    ===                                 |
   -:-  |    %x                                  | nothing
   -:-  |  [call-i]                              |
   -:-  |    %x                                  | nothing
   -:-  |    !==                                 |
   -:-  |    %x                                  | nothing


julia> rule_match!(useless_equals, "test/demo/useless_equals.jl")
4-element SyntaxMatches:
 SyntaxMatch((call-i x == x), AbstractSyntaxPlaceholder[Metavariable(x, x@20)])
 SyntaxMatch((call-i x != x), AbstractSyntaxPlaceholder[Metavariable(x, x@67)])
 SyntaxMatch((call-i x === x), AbstractSyntaxPlaceholder[Metavariable(x, x@114)])
 SyntaxMatch((call-i x !== x), AbstractSyntaxPlaceholder[Metavariable(x, x@162)])
```

Sometime I'll implement automatic checking of test files, similar to
Semgrep's approach with paired .jl and .yaml files.


## Design choices and ideas

TODO: Rewrite this.

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

Metavariables should bind to expressions. The ultimate goal is to have
metavariables bind to something that can be chosen by the user. This
could be accomplished by providing types to metavariables (either
Julia built-in types or custom types that have some kind of
corresponding predicate, or something like Racket's [syntax
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
