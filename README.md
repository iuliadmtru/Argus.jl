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

Status: Working on metavariables.

Currently the package is able to:

- **Create a rule based on a `Pattern`;**

```julia
julia> using Argus

julia> using JuliaSyntax

julia> simple_pattern = Pattern("f(x) = 2")
Pattern((= (call f x) 2))

julia> simple_pattern.template
line:col│ tree                                   │ metadata
   -:-  |[=]                                     |
   -:-  |  [call]                                |
   -:-  |    f                                   |
   -:-  |    x                                   |
   -:-  |  2                                     |
```

_Note_: You need to load `JuliaSyntax` as well.

- **Create patterns that contain metavariables that bind to the match;**

_Note_: The intended behaviour is for metavariables to bind to
expressions. I have not tested this yet.

```julia
julia> two_metavar_pattern = Pattern("f(Metavariable(:X)) = 2 + Metavariable(:Y)")
Pattern((= (call f M"X") (call-i 2 + M"Y")))

julia> two_metavar_pattern.template
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

julia> src_ast = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src);

julia> single_metavar_pattern = Pattern("Metavariable(:A) + b")
Pattern((call-i M"A" + b))

julia> pattern_match!(single_metavar_pattern, src_ast)
3-element SyntaxMatches:
 SyntaxMatch((call-i a + b), AbstractSyntaxPlaceholder[Metavariable(:A, JuliaSyntax.SyntaxData(SourceFile("function f(a, b)\n    y = a + b\n    return y\nend\n\na = 2\nb = 3\nf(a + b, b)\n\nlet x = 2, y = 3\n    z = x + b\n    a = 1\n    b = 2\n    b + a\nend\n", 0, nothing, 1, [1, 18, 32, 45, 49, 50, 56, 62, 74, 75, 92, 106, 116, 126, 136, 140]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, ()), 26, :a))])
 SyntaxMatch((call-i a + b), AbstractSyntaxPlaceholder[Metavariable(:A, JuliaSyntax.SyntaxData(SourceFile("function f(a, b)\n    y = a + b\n    return y\nend\n\na = 2\nb = 3\nf(a + b, b)\n\nlet x = 2, y = 3\n    z = x + b\n    a = 1\n    b = 2\n    b + a\nend\n", 0, nothing, 1, [1, 18, 32, 45, 49, 50, 56, 62, 74, 75, 92, 106, 116, 126, 136, 140]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, ()), 64, :a))])
 SyntaxMatch((call-i x + b), AbstractSyntaxPlaceholder[Metavariable(:A, JuliaSyntax.SyntaxData(SourceFile("function f(a, b)\n    y = a + b\n    return y\nend\n\na = 2\nb = 3\nf(a + b, b)\n\nlet x = 2, y = 3\n    z = x + b\n    a = 1\n    b = 2\n    b + a\nend\n", 0, nothing, 1, [1, 18, 32, 45, 49, 50, 56, 62, 74, 75, 92, 106, 116, 126, 136, 140]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, ()), 100, :x))])
```

- **Run a rule against a file to get `SyntaxMatches`.**

```julia
julia> pattern_match!(two_metavar_pattern, "test/test-file.jl")
1-element SyntaxMatches:
 SyntaxMatch((= (call f x) (call-i 2 + x)), AbstractSyntaxPlaceholder[Metavariable(:X, JuliaSyntax.SyntaxData(SourceFile("function f(a, b)\n    y = a + b\n    return y\nend\n\na + b\na + b + c # (call-i a + b c); 4 children, all leaves; Semgrep finds this\nc + a + b # (call-i c + a b); 4 children, all leaves; Semgrep doesn't find this\n\nf(x) = \"a\"\nf(x) = 2 + x\ng(x) = 2 + x\n", 0, "test/test-file.jl", 1, [1, 18, 32, 45, 49, 50, 56, 129, 209, 210, 221, 234, 247]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, ()), 223, :x)), Metavariable(:Y, JuliaSyntax.SyntaxData(SourceFile("function f(a, b)\n    y = a + b\n    return y\nend\n\na + b\na + b + c # (call-i a + b c); 4 children, all leaves; Semgrep finds this\nc + a + b # (call-i c + a b); 4 children, all leaves; Semgrep doesn't find this\n\nf(x) = \"a\"\nf(x) = 2 + x\ng(x) = 2 + x\n", 0, "test/test-file.jl", 1, [1, 18, 32, 45, 49, 50, 56, 129, 209, 210, 221, 234, 247]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, ()), 232, :x))])
```

_Note_: Clearly I need to work on my `show`.


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

`Pattern` is defined in `src/syntax_pattern.jl`. It contains the
template AST, `SyntaxTemplateNode`, defined in `src/Argus.jl`. This
AST is built on `JuliaSyntax`'s AST interface (i.e. it is a
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

There should be a different, custom representation for
`RuleSyntaxNode`s.

### Patterns

#### Special syntax

The goal is to permit special syntax for non-trivial matching
(e.g. match a function call with an arbitrary number of arguments). As
a start, I would like to have some rudimentary implementation of
metavariables and ellipses.

#### Metavariables

Metavariables should bind to expressions. I have not tested this
yet. The ultimate goal is to have metavariables bind to something that
can be chosen by the user. This could be accomplished by providing
types to metavariables (either Julia built-in types or custom types
that have some kind of corresponding predicate).

### AST comparison

This is probably the most difficult part of the project. For now I am
using a mock implementation for testing purposes. I keep adding to
it/changing it as I go because I need to constantly ask myself what
design choices to make in order to be able to compare the patterns'
AST to the source code AST.


## Future ideas

- Instead of using ellipsis as a match-all operator like Semgrep does,
  I'd like to use it as a repeater of homogeneous syntax ("Kleene
  star") like Racket does. Ellipsis nesting is also interesting.
- It would be cool to make this tool highly configurable. For example,
  let metavariables match any kind of syntactic behaviour. (They
  currently (should) match for expressions but why not let them match
  for any predicate that checks a property for a syntax node?)
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
