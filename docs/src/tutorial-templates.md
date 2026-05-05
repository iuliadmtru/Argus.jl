# Templates

The `BindingSet` resulting from a successful pattern match can be used
to fill in a _template_ in order to generate new code.

Let's say we want to replace a call to `append!` with one to
`vcat`. First, we write the pattern and match it with something:

```julia
julia> pattern = @pattern append!({vs}...)
Pattern:
[call]
  append!                                :: Identifier
  [~rep]
    vs:::expr                            :: ~var

julia> bindings = syntax_match(pattern, parsestmt(SyntaxNode, "append!([1, 2, 3], 4)"))
BindingSet @ 0:0 with 1 entry:
  :vs => Binding:
           Name: :vs
           Bound sources: [(vect 1 2 3) @ 1:9, 4 @ 1:20]
           Ellipsis depth: 1
           Sub-bindings:
             [
              BindingSet @ 0:0 with 0 entries,
              BindingSet @ 0:0 with 0 entries
             ]
```

Then, we write the template and expand it using the bindings obtained
after matching:

```julia
julia> template = @template vcat({vs}...)
SyntaxPatternNode:
[call]
  vcat                                   :: Identifier
  [~rep]
    [~var]
      [quote-:]
        vs                               :: Identifier
      [quote-:]
        expr                             :: Identifier

julia> expand(template, bindings)
SyntaxNode:
[call]
  vcat                                   :: Identifier
  [vect]
    1                                    :: Integer
    2                                    :: Integer
    3                                    :: Integer
  4                                      :: Integer
```

Template variables should have the same ellipsis depth as the
corresponding pattern variables.

```julia
julia> expand((@template vcat({vs})), bindings)
ERROR: template variable vs has inconsistent ellipsis depth
Template variables should have the same depth as the corresponding pattern variables.

...
```
