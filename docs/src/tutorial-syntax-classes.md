# Syntax Classes

We saw that a pattern variable is printed as `x:::expr`. This means
that `x` is constrained by the _syntax class_ `expr`.

A syntax class is a syntax matching construct useful for defining
syntactic "categories". You can see it as an abstraction for patterns.
(You can read more about them in the [`syntax/parse`
documentation](https://docs.racket-lang.org/syntax/stxparse-specifying.html).)

`expr` is the "category" of all Julia expressions. It is a syntax
class provided by Argus. We can define our own syntax classes and use
them in patterns. Let's write a syntax class for vectors:

```julia
julia> vec = @syntax_class "vector" begin
           @pattern [{_}...]
       end
SyntaxClass: vector
  Pattern alternative #1:
    [vect]
      [~rep]
        _:::expr                         :: ~var
```

Syntax class bodies consist of a sequence of patterns. The match
behaviour is that of a short-circuiting `or` — the result is either
the first successful pattern match, or a failure if no patterns match.

Let's test `vec`:

```julia
julia> syntax_match(vec, parsestmt(SyntaxNode, "[]"))
BindingSet @ 0:0 with 0 entries

julia> syntax_match(vec, parsestmt(SyntaxNode, "[1, [2]]"))
BindingSet @ 0:0 with 0 entries
```

The match results are empty binding sets. This is because we used an
_anonymous pattern variable_ in the syntax class's pattern.

If we want to use our syntax class in a pattern with the `:::` syntax
we need to register it first.

```julia
julia> register_syntax_class!(:my_vec, vec)
SyntaxClass: vector
  Pattern alternative #1:
    [vect]
      [~rep]
        _:::expr                         :: ~var
```

Now we can find it in the syntax class registry under the name `:my_vec`.

```julia
julia> Argus.SYNTAX_CLASS_REGISTRY[:my_vec]
SyntaxClass: vector
  Pattern alternative #1:
    [vect]
      [~rep]
        _:::expr                         :: ~var
```

Let's use it to rewrite the `vec_of_vecs` pattern:

```julia
julia> vec_of_vecs2 = @pattern [{v:::vec}...]
Pattern:
[vect]
  [~rep]
    v:::vec                              :: ~var

julia> syntax_match(vec_of_vecs2, parsestmt(SyntaxNode, "[[1, 2], [[3]]]"))
BindingSet @ 0:0 with 1 entry:
  :v => Binding:
          Name: :v
          Bound sources: [(vect 1 2) @ 1:2, (vect (vect 3)) @ 1:10]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet @ 0:0 with 0 entries,
             BindingSet @ 0:0 with 0 entries
            ]

julia> syntax_match(vec_of_vecs2, parsestmt(SyntaxNode, "[1]"))
MatchFail: no match @ :1:2
```

Argus's built-in implementation of the `vec` syntax class uses
`@define_syntax_class` to define and register `vec` and the same time:

```julia
julia> @define_syntax_class :vec "vector" begin
           @pattern [{_}...]
       end
SyntaxClass: vector
  Pattern alternative #1:
    [vect]
      [~rep]
        _:::expr                         :: ~var
```
