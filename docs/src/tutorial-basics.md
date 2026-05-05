# Basics

The essential structures in Argus are `Pattern`s, `CommentPattern`s,
`SyntaxClass`es and `Template`s.

```julia
julia> using Argus

help?> Pattern
search: Pattern @pattern CommentPattern

  Pattern

  Syntax pattern used for matching syntax.

help?> CommentPattern
search: CommentPattern CommentDisabler AbstractPattern Pattern

  CommentPattern

  Trivia pattern used for matching comments.

help?> SyntaxClass
search: SyntaxClass @syntax_class SyntaxClassRegistry SyntaxError syntax_match

  SyntaxClass

  Syntax classes provide the basis for a syntax matching mechanism. A syntax class specifies a syntactic "shape" and provides a description for that shape.

  Syntax class bodies can contain one or more patterns. If there are multiple patterns, the search for a syntax match stops at the first matching pattern. A pattern variable can be constrained by a
  syntax class using the syntax {<pattern_var_name>:::<syntax_class_name>}. Unconstrained pattern variables are constrained by default to :::expr.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> binary_funcall = @syntax_class "binary function call" begin
             @pattern {_}({_}, {_})
         end
  SyntaxClass: binary function call
    Pattern alternative #1:
      [call]
        _:::expr                           :: ~var
        _:::expr                           :: ~var
        _:::expr                           :: ~var

  julia> fundef = @syntax_class "function definition" begin
             @pattern {f:::funcall} = {_}...
             @pattern function ({f:::funcall}) {_}... end
         end
  SyntaxClass: function definition
    Pattern alternative #1:
      [function-=]
        f:::funcall                        :: ~var
        [~rep]
          _:::expr                         :: ~var
    Pattern alternative #2:
      [function]
        f:::funcall                        :: ~var
        [block]
          [~rep]
            _:::expr                       :: ~var

help?> Template
search: Template @template replace tempname keepat! relpath realpath Tuple repeat accumulate replace! empty splat

  Template

  Syntax template, to be filled using a BindingSet obtained after pattern matching. Alias for SyntaxPatternNode.
```
