# Ellipses

A pattern followed by an ellipsis means that the pattern repeats zero
or more times. 

```julia
julia> exprs = @pattern {x}...
Pattern:
[~rep]
  x:::expr                               :: ~var

julia> using JuliaSyntax: parseall

julia> syntax_match(exprs, parseall(SyntaxNode, """
                                                a = 1
                                                b = 2
                                                a + b
                                                """))
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound sources: [(= a 1) @ 1:1, (= b 2) @ 2:1, (call-i a + b) @ 3:1]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet @ 0:0 with 0 entries,
             BindingSet @ 0:0 with 0 entries,
             BindingSet @ 0:0 with 0 entries
            ]
```

This is useful in many situations. For example, when defining a
pattern that matches a function definition:

```julia
julia> function_def = @pattern begin
           function {f}({args}...)
               {body}...
           end
       end
Pattern:
[function]
  [call]
    f:::expr                             :: ~var
    [~rep]
      args:::expr                        :: ~var
  [block]
    [~rep]
      body:::expr                        :: ~var

julia> syntax_match(function_def, parsestmt(SyntaxNode, """
                                                        function my_fun()
                                                            println("My fun!")
                                                            return nothing
                                                        end
                                                        """))
BindingSet @ 0:0 with 3 entries:
  :f => Binding:
          Name: :f
          Bound source: my_fun @ 1:10
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries
  :args => Binding:
             Name: :args
             Bound sources: []
             Ellipsis depth: 1
             Sub-bindings:
               []
  :body => Binding:
             Name: :body
             Bound sources: [(call println (string "My fun!")) @ 2:5, (return nothing) @ 3:5]
             Ellipsis depth: 1
             Sub-bindings:
               [
                BindingSet @ 0:0 with 0 entries,
                BindingSet @ 0:0 with 0 entries
               ]
```

`args` and `body` both bind to sequences of expressions. You can see
this by looking at their ellipsis depth, which is 1, or by noting that
they are bound to vectors of source nodes instead of just source
nodes, like `f` is.

Ellipses can be nested to match sequences of any depth.

```julia
julia> vec_of_vecs = @pattern [[{el}...]...]
Pattern:
[vect]
  [~rep]
    [vect]
      [~rep]
        el:::expr                        :: ~var

julia> syntax_match(vec_of_vecs, parsestmt(SyntaxNode, "[[1, 2], [[3]]]"))
BindingSet @ 0:0 with 1 entry:
  :el => Binding:
           Name: :el
           Bound sources: [[1 @ 1:3, 2 @ 1:6], [(vect 3) @ 1:11]]
           Ellipsis depth: 2
           Sub-bindings:
             [
              [
               BindingSet @ 0:0 with 0 entries,
               BindingSet @ 0:0 with 0 entries
              ],
              [
               BindingSet @ 0:0 with 0 entries
              ]
             ]
```

`el` has ellipsis depth 2, so it binds to a sequence of sequences of
expressions. In the example above, it binds to a sequence containing
two sequences: the sequence with `1` and `2` and the sequence with the
vector `[3]`.

You can read the pattern `vec_of_vecs` more intuitively as: _A vector
that contains any number of elements. Its elements need to be vectors
that have any number of elements of any kind._

By default, the matching algorithm for ellipses is "greedy".

```julia
julia> ones_vec = @pattern [1, {ones1}..., 1, {ones2}...];

julia> syntax_match(ones_vec, parsestmt(SyntaxNode, "[1, 1, 1, 1]"))
BindingSet @ 0:0 with 2 entries:
  :ones2 => Binding:
              Name: :ones2
              Bound sources: []
              Ellipsis depth: 1
              Sub-bindings:
                []
  :ones1 => Binding:
              Name: :ones1
              Bound sources: [1 @ 1:5, 1 @ 1:8]
              Ellipsis depth: 1
              Sub-bindings:
                [
                 BindingSet @ 0:0 with 0 entries,
                 BindingSet @ 0:0 with 0 entries
                ]
```

`ones1` consumes all the expressions it can, leaving none for
`ones2`. We can choose to match with a non-"greedy" algorithm:

```julia
julia> syntax_match(ones_vec, parsestmt(SyntaxNode, "[1, 1, 1, 1]"); greedy=false)
BindingSet @ 0:0 with 2 entries:
  :ones2 => Binding:
              Name: :ones2
              Bound sources: [1 @ 1:8, 1 @ 1:11]
              Ellipsis depth: 1
              Sub-bindings:
                [
                 BindingSet @ 0:0 with 0 entries,
                 BindingSet @ 0:0 with 0 entries
                ]
  :ones1 => Binding:
              Name: :ones1
              Bound sources: []
              Ellipsis depth: 1
              Sub-bindings:
                []
```

This is useful if we expect a non-greedy approach to be more efficient
in a particular case.

Argus hijacks Julia's [splat
operator](https://docs.julialang.org/en/v1/base/base/#...). If we want
to use `...` with the regular splat meaning, we need to escape it:

```julia
julia> syntax_match((@pattern v...), parsestmt(SyntaxNode, "v..."))
MatchFail: no match @ :1:1

julia> syntax_match((@pattern @esc(v...)), parsestmt(SyntaxNode, "v..."))
BindingSet @ 0:0 with 0 entries
```

We can also escape an expression only up to a certain depth.

```julia
julia> syntax_match((@pattern @esc([{elems}...]..., 1)), parsestmt(SyntaxNode, "[1, 2, 3]..."))
BindingSet @ 0:0 with 1 entry:
  :elems => Binding:
              Name: :elems
              Bound sources: [1 @ 1:2, 2 @ 1:5, 3 @ 1:8]
              Ellipsis depth: 1
              Sub-bindings:
                [
                 BindingSet @ 0:0 with 0 entries,
                 BindingSet @ 0:0 with 0 entries,
                 BindingSet @ 0:0 with 0 entries
                ]
```

Without specifying an escape depth, `@esc` would have escaped the
entire expression:

```julia
julia> @pattern @esc([{elems}...])
Pattern:
[vect]
  [...]
    [braces]
      elems                              :: Identifier
```
