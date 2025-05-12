[![codecov](https://codecov.io/gh/iuliadmtru/Argus.jl/graph/badge.svg?token=Z79DY1TSL4)](https://codecov.io/gh/iuliadmtru/Argus.jl)

# Argus.jl

Syntax matching and static analysis rule writing for Julia. Heavily
inspired by
[`syntax/parse`](https://docs.racket-lang.org/syntax/stxparse.html), a
library for writing and processing macros in Racket, and
[Resyntax](https://docs.racket-lang.org/resyntax/index.html), a
refactoring tool for Racket based on `syntax/parse`.

Argus provides a mechanism for matching Julia syntax through patterns
that resemble Julia syntax but contain special forms: pattern
variables which can be annotated by syntax classes. This mechanism
serves as a base for Argus' rule writing framework, a static analysis
tool similar to [Clippy](https://doc.rust-lang.org/clippy/) for Rust,
[Resyntax](https://docs.racket-lang.org/resyntax/index.html) for
Racket or [Semgrep](https://semgrep.dev/docs/) for multiple languages.

## Syntax matching

The first part of Argus defines a syntax matching language (_pattern
language_) capable to match Julia syntax and provide relevant
information in case of match failure. The pattern language has three
types of building blocks:
  - regular Julia syntax
  - pattern variables
  - syntax classes

These three components allow the writing of patterns which can be used
to match code that "fits" them.

Here is a pattern that matches binary function calls:

```julia
julia> using Argus, JuliaSyntax

julia> binary_funcall_pattern = @pattern :( (_f:::identifier)(_arg1, _arg2) )
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

Pattern construction fails if a pattern variable constraint syntax is
invalid (for example: `_x::identifier`) or if the `:::` "operator"[^1]
if preceded by a non-pattern variable node (for example:
`x:::identifier`).
```julia
julia> @pattern :( _x::identifier )
ERROR: Invalid constraint syntax: _x::identifier
Pattern variable constraints should follow the syntax: <pattern_variable>:::<syntax_class>
Stacktrace:
 [1] error(::String, ::String, ::String)
   @ Base ./error.jl:54
 [2] _desugar_expr(ex::Expr; is_and_context::Bool, bindings::Vector{Symbol})
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:79
 [3] _desugar_expr
   @ ~/.../Argus.jl/src/syntax-pattern-node.jl:78 [inlined]
 [4] desugar_expr
   @ ~/.../Argus.jl/src/syntax-pattern-node.jl:75 [inlined]
 [5] SyntaxPatternNode(ex::Expr)
   @ Argus ~/.../Argus.jl/src/syntax-pattern-node.jl:62
 [6] Pattern(ex::Expr)
   @ Argus ~/.../Argus.jl/src/pattern.jl:8
 [7] top-level scope
   @ REPL[42]:1

julia> @pattern :( x:::identifier )
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
julia> assign =  @syntax_class "assignment" quote
           __lhs:::identifier = __rhs:::expr
       end
SyntaxClass("assignment", Pattern[(= (~var (quote-: __lhs) (quote-: identifier)) (~var (quote-: __rhs) (quote-: expr)))])
```

This is one of the built-in syntax classes and it uses two other
built-in syntax classes, `identifier` and `expr`. The most "primitive"
syntax class is `expr`. It is defined as:
```julia
@syntax_class "expr" quote
    ~fail(:false, "")
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
@syntax_class "identifier" quote
    ~and(__id,
         ~fail(begin
                   using JuliaSyntax: is_identifier
                   !is_identifier(__id.ast)
               end,
               "not an identifier"))
end
```

The `~and` pattern form binds `__id` to an expression AST and `~fail`
checks whether the bound AST is not an identifier using JuliaSyntax's
`is_identifier` predicate. If the fail check succeeds, the match
fails with the message "not an identifier".
```julia
julia> syntax_match(Pattern(:( _x:::identifier )), parsestmt(SyntaxNode, "1 + 2"))
MatchFail("not an identifier")
```

Syntax classes need to be registered in order to use them in
patterns. This is done through `register_syntax_class!`:
```julia
register_syntax_class!(:assign, @syntax_class "assignment" quote
                           __lhs:::identifier = __rhs:::expr
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
julia> @pattern :( ~var(:_ex, :expr) )
Pattern:
_ex:::expr                               :: ~var


julia> @pattern :( ~var(x, :identifier) )
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

julia> @pattern :( ~var(:x, :identifier) )
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
julia> fundef = @syntax_class "function definition" quote
           ~or(_f:::funcall = _, function (_g:::funcall) _ end)
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
julia> conflicting = @pattern :( ~and(_x + 2, _x + 3) )
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
julia> @pattern :(
    ~and(__id,
         ~fail(begin
                   using JuliaSyntax: is_identifier
                   !is_identifier(__id.ast)
               end,
               "not an identifier"))
)
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
julia> pattern = @pattern :( ~fail(_x, "") )
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

julia> pattern = @pattern :( ~fail(:(x + 1), "") )
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

The value of sub-bindings becomes clear when thinking of what could be
expressed in a fail condition. It would be useful to be able to access
the components bound within a pattern variable in order to do more
sophisticated checks.

For example, in some cases in might be necessary to match an
assignment where the right hand side satisfies some
constraint. Allowing a syntax similar to the following would be
desirable (and is in plan for the future):
```julia
julia> @pattern :( ~and(_x:::assign, ~fail(!is_literal(_x.rhs), "rhs not a literal")) )
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

The second part of Argus allows the writing on rules based on the
syntax matching system described so far.

Rules are described by a pattern and a rule description.

```julia
julia> chained_const_assignment = @rule "chained-const-assignment" quote
           description = """
           Do not chain assignments with const. The right hand side is not constant here.
           """

           pattern = :(
               const _a:::identifier = _b:::identifier = _
           )
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
1-element Vector{BindingSet}:
 BindingSet(:_a => Binding(:_a, a, BindingSet(:__id => Binding(:__id, a, BindingSet()))), :_b => Binding(:_b, b, BindingSet(:__id => Binding(:__id, b, BindingSet()))))
```

`rule_match` can either return the set of matches, like above, or it
can return both matches and failures.

```julia
julia> rule_match(chained_const_assignment, parsestmt(SyntaxNode, "const a = b"); only_matches=false)
Argus.RuleMatchResult(MatchFail[MatchFail("no match"), MatchFail("no match"), MatchFail("no match"), MatchFail("no match")], BindingSet[])
```

There are four failures and no matches, accounting for four AST
comparisons:
  - `(const (= a b))`
  - `(= a b)`
  - `a`
  - `b`

Rules can also be matched against files:

```julia
julia> compare_nothing = @rule "compare-nothing" quote
           description = """
           Comparisons of `nothing` should be made with === or !== or with isnothing().
           """

           pattern = :(
               ~or(
                   nothing == _,
                   _ == nothing,
                   nothing != _,
                   _ != nothing
			   )
           )
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

julia> @define_rule_in_group style_rules "useless-bool" quote
           description = "Useless boolean in if condition."
           pattern = :(
           if true
               _
           end
           )
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
1-element Vector{BindingSet}:
 BindingSet()

julia> rule_match(style_rules["useless-bool"], parsestmt(SyntaxNode, test_src_no_match))
BindingSet[]
```

These are examples of rules that exist in the
[semgrep-rules-julia](https://github.com/JuliaComputing/semgrep-rules-julia)
repository and that can be ported to Argus. All ported rules can be
found inside `semgrep-to-argus/`, grouped according to their category.

## Notes

Argus is the final project for my Computer Science and Engineering
degree at University Politehnica of Bucharest.

## Acknowledgements

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
