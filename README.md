# Argus.jl

This package aims to be a static analysis tool for Julia. It takes
inspiration from [Semgrep](https://github.com/semgrep/semgrep),
[Clippy](https://github.com/rust-lang/rust-clippy) and
[Resyntax](https://docs.racket-lang.org/resyntax/).


## Ideal result

I would like the user to be able to write syntax very similar to
actual Julia syntax.

~~~julia
@syntax_template ```
	function %F(%X, %Y::Int)
		%_...
	end
```
~~~

`@syntax_template` should create a kind of `SyntaxNode` that can have
"holes". These "holes" are pieces of syntax that can be "filled" by
actual Julia syntax that matches the template. Something like
```julia
function my_func(arg1, arg2::Int)
	println(arg1)
	arg2 + 1
end
```
should match the above template and should bind `my_func` to the
metavariable `%F`, `arg1` to `%X`, `arg2` to `%Y` and the expressions
in the function body to the anonymous metavariable `%_` (which would
actually discard the binding).

The result should be a first-class object wich can later be used in
other templates.

~~~julia
func_template = @syntax_template ```
	function %F(%X, %Y::Int)
		%_...
	end
```

func_with_docs_template(t::SyntaxTemplate) = @syntax_template ```
	"""%_..."""
	t
```
~~~

Rules should be created using templates.

```julia
simple_rule = Pattern(func_template)
either_rule = PatternEither(func_template, func_with_docs_template(func_template))
```


## Status

I am working on this project as part of my thesis for the Computer
Science and Engineering Bachelor's at Politehnica University of
Bucharest.

Status: _barely beginning_. Working on metavariables.

Currently the package is able to:

- **Create a rule based on a `Pattern`;**

```julia
julia> using Argus

julia> using JuliaSyntax

julia> simple_rule = Pattern("f(x) = 2")
Pattern((= (call f x) 2))

julia> simple_rule.ast
line:col│ tree                                   │ metadata
   -:-  |[=]                                     |
   -:-  |  [call]                                |
   -:-  |    f                                   |
   -:-  |    x                                   |
   -:-  |  2                                     |
```

_Note_: You need to load `JuliaSyntax` as well.

- **Create patterns that contain metavariables that bind to the match;**

_Note_: This is the intended behaviour, but not completely
implemented. I am currently working on attaching `Metavariable`s to
each match.

```julia
julia> two_metavar_rule = Pattern("f(Metavariable(:X)) = 2 + Metavariable(:Y)")
Pattern((= (call f M"X") (call-i 2 + M"Y")))

julia> two_metavar_rule.ast
line:col│ tree                                   │ metadata
   -:-  |[=]                                     |
   -:-  |  [call]                                |
   -:-  |    f                                   |
   -:-  |    M"X"                                | nothing
   -:-  |  [call-i]                              |
   -:-  |    2                                   |
   -:-  |    +                                   |
   -:-  |    M"Y"                                | nothing
```

- **Search for rule matches inside a source AST;**

_Note_: Special behaviour is implemented for `Pattern`s with
`Metavariable`s. Both the special implementation and the regular
implementation are very rudimentary.

```julia
julia> src = """
       function f(a, b)
           y = a + b
           return y
       end

       a = 2
       b = 3
       f(a + b, b)

       let x = 2, y = 3
           z = x + b
           a = 1
           b = 2
           b + a
       end
       """;

julia> src_ast = parseall(JuliaSyntax.SyntaxNode, src);

julia> single_metavar_rule = Pattern("Metavariable(:A) + b")
Pattern((call-i M"A" + b))

julia> Argus.search_ast!(single_metavar_rule.ast, src_ast)
3-element RuleMatches:
 RuleMatch((call-i a + b), Metavariable[Metavariable(:A, JuliaSyntax.SyntaxData(SourceFile("function f(a, b)\n    y = a + b\n    return y\nend\n\na = 2\nb = 3\nf(a + b, b)\n\nlet x = 2, y = 3\n    z = x + b\n    a = 1\n    b = 2\n    b + a\nend\n", 0, nothing, 1, [1, 18, 32, 45, 49, 50, 56, 62, 74, 75, 92, 106, 116, 126, 136, 140]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, ()), 26, :a))])
 RuleMatch((call-i a + b), Metavariable[Metavariable(:A, JuliaSyntax.SyntaxData(SourceFile("function f(a, b)\n    y = a + b\n    return y\nend\n\na = 2\nb = 3\nf(a + b, b)\n\nlet x = 2, y = 3\n    z = x + b\n    a = 1\n    b = 2\n    b + a\nend\n", 0, nothing, 1, [1, 18, 32, 45, 49, 50, 56, 62, 74, 75, 92, 106, 116, 126, 136, 140]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, ()), 64, :a))])
 RuleMatch((call-i x + b), Metavariable[Metavariable(:A, JuliaSyntax.SyntaxData(SourceFile("function f(a, b)\n    y = a + b\n    return y\nend\n\na = 2\nb = 3\nf(a + b, b)\n\nlet x = 2, y = 3\n    z = x + b\n    a = 1\n    b = 2\n    b + a\nend\n", 0, nothing, 1, [1, 18, 32, 45, 49, 50, 56, 62, 74, 75, 92, 106, 116, 126, 136, 140]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, ()), 100, :x))])
```

- **Run a rule against a file to get `RuleMatches`.**

```julia
julia> Argus.check!(two_metavar_rule, "test/test-file.jl")
1-element RuleMatches:
 RuleMatch((= (call f x) (call-i 2 + x)), Metavariable[])
```

_Note_: I don't know why there is no metavariable info here. I'm still
working on metavariables.

_Note_: All of this was built before getting the ideas written in the
[Ideal result](#ideal-result) section. I will probably change
everything.


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

#### Special syntax

The goal is to permit special syntax for non-trivial matching
(e.g. match a function call with an arbitrary number of arguments). As
a start, I would like to have some rudimentary implementation of
metavariables and ellipsis.

#### Metavariables

Metavariables currently only (should) bind to whatever
`JuliaSyntax.is_valid_identifier` returns `true` for. This is NOT
working yet.

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


## Future ideas

- Instead of using ellipsis as a match-all operator like Semgrep does,
  I'd like to use it as a repeater of homogeneous syntax ("Kleene
  star") like Racket does. Ellipsis nesting is also interesting.
- It would be cool to make this tool highly configurable. For example,
  let metavariables match any kind of syntactic behaviour. (They
  currently match for `n -> is_valid_identifier(n)` but why not let
  them match for any predicate that checks a property for a syntax
  node?)
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
