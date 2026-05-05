# Patterns

Patterns are defined with the `@pattern` macro.

```julia
help?> @pattern
  @pattern(expr)

  Create a Pattern from the given expression. Special syntax can be escaped by wrapping it in an @esc macro. Match and fail conditions are defined with the @when and @fail macros respectively.

  @esc usage:

    •  @esc(ex): Escape everything inside ex.
    •  @esc(ex, depth): Escape ex only up to depth.

  @when usage:

    •  @when(pattern_vars, condition): Match only if condition is satisfied. The pattern variables found in condition must be given as a vector of Symbols.

  @fail usage:

    •  @fail(pattern_vars, condition, msg): Fail with the message msg if condition is satisfied. The pattern variables found in condition must be given as a vector of Symbols.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> @pattern a + b  # Simple expression with no pattern variables.
  Pattern:
  [call-i]
    a                                      :: Identifier
    +                                      :: Identifier
    b                                      :: Identifier

  julia> assign_to_x = @pattern begin
             {x:::assign}                                    # Pattern variable matching an assignment.
             @fail [:x] x.lhs.name == "x" "assignment to x"  # The matching fails if the rhs variable's name is "x".
         end
  Pattern:
  [~and]
    x:::assign                             :: ~var
    [~fail]
      [vect]
        [quote-:]
          x                                :: Identifier
      [call-i]
        [.]
          [.]
            x                              :: Identifier
            lhs                            :: Identifier
          name                             :: Identifier
        ==                                 :: Identifier
        [string]
          "x"                              :: String

  julia> syntax_match(assign_to_x, JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "x = 2"))
  MatchFail("assignment to x")

  julia> const_reassign = @pattern begin
             const {a:::identifier} = {_}
             {_}...
             const {a:::identifier} = {_}  # Reassignment of `const`.
         end
  Pattern:
  [toplevel]
    [const]
      [=]
        a:::identifier                     :: ~var
        _:::expr                           :: ~var
    [~rep]
      _:::expr                             :: ~var
    [const]
      [=]
        a:::identifier                     :: ~var
        _:::expr                           :: ~var

  julia> syntax_match(const_reassign, parseall(SyntaxNode,
                                               """
                                               const x = 2
                                               other_ex
                                               const x = 3
                                               """))
  BindingSet @ 0:0 with 1 entries:
    :a => Binding:
            Name: :a
            Bound source: x @ 3:7
            Ellipsis depth: 0
            Sub-bindings:
              BindingSet @ 0:0 with 1 entries:
                :_id => Binding:
                          Name: :_id
                          Bound source: x @ 3:7
                          Ellipsis depth: 0
                          Sub-bindings:
                            BindingSet @ 0:0 with 0 entries

  julia> esc_pattern = @pattern @esc(x...)
  Pattern:
  [...]
    x                                      :: Identifier

  julia> syntax_match(esc_pattern, parsestmt(SyntaxNode, "x..."))
  BindingSet @ 0:0 with 0 entries

  julia> esc1_pattern = @pattern @esc({x}..., 1)
  Pattern:
  [...]
    x:::expr                               :: ~var

  julia> syntax_match(esc1_pattern, parsestmt(SyntaxNode, "x..."))
  BindingSet @ 0:0 with 1 entries:
    :x => Binding:
            Name: :x
            Bound source: x @ 1:1
            Ellipsis depth: 0
            Sub-bindings:
              BindingSet @ 0:0 with 0 entries

  Note: @when, @fail and @esc macros only exist inside @pattern bodies.

  julia> @when [] :false
  ERROR: LoadError: UndefVarError: `@when` not defined in `Main`
  Suggestion: check for spelling errors or missing imports.
  in expression starting at ...

  julia> @fail [] :false ""
  ERROR: LoadError: UndefVarError: `@fail` not defined in `Main`
  Suggestion: check for spelling errors or missing imports.
  in expression starting at ...

  julia> @esc
  ERROR: LoadError: UndefVarError: `@esc` not defined in `Main`
  Suggestion: check for spelling errors or missing imports.
  Hint: a global variable of this name also exists in MacroTools.
  in expression starting at ...
```

A simple pattern to experiment with is the match-all pattern:

```julia
julia> expr = @pattern {x}
Pattern:
x:::expr                                 :: ~var
```

This pattern matches any Julia expression. It has one pattern
variable: `x`. The pretty print tells us that the pattern consists of
a `~var` node which constraints `x` to be an `expr`. `~var` and `expr`
are explained later (see [Pattern
forms](https://github.com/iuliadmtru/Argus.jl?tab=readme-ov-file#pattern-forms)
and [Syntax
classes](https://github.com/iuliadmtru/Argus.jl?tab=readme-ov-file#syntax-classes)).

Patterns can be matched against Julia expressions.

```julia
julia> using JuliaSyntax: parsestmt, SyntaxNode

julia> syntax_match(expr, parsestmt(SyntaxNode, "1 + 2"))
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: (call-i 1 + 2) @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries

julia> syntax_match(expr, parsestmt(SyntaxNode, "f(x::Int) = x * 2"))
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: (function-= (call f (::-i x Int)) (call-i x * 2)) @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries
```

The result of a successful match is a `BindingSet` — a dictionary with
bound pattern variables (`Binding`s). A `Binding` contains information
about a pattern variable that matched (part of) the source, as well as
the matched source node.

In the second example above, the pretty print tells us that the
pattern variable `x` was bound to the function definition found at
line 1, column 1 in the source. The fifth line of the pretty print
tells us that `x` has ellipsis depth 0. This is because `x` is not
surrounded by any ellipses (`...` – see
[Ellipses](https://github.com/iuliadmtru/Argus.jl?tab=readme-ov-file#ellipses)).

`syntax_match` matches a pattern with a source node exactly. It may
also be useful to extract all the pattern's occurences in a source
node. `syntax_match_all` does just that:

```julia
julia> syntax_match_all(expr, parsestmt(SyntaxNode, "1 + 2"))
MatchResults with 4 matches and 0 failures:
Matches:
  BindingSet(:x => Binding(:x, (call-i 1 + 2) @ 1:1, BindingSet()))
  BindingSet(:x => Binding(:x, 1 @ 1:1, BindingSet()))
  BindingSet(:x => Binding(:x, + @ 1:3, BindingSet()))
  BindingSet(:x => Binding(:x, 2 @ 1:5, BindingSet()))
```
