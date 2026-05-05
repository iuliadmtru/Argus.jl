# Comment Patterns

Regular patterns match `SyntaxNode`s, but ignore trivia nodes such as
comments. The `@comment` macro allows defining patterns that match
comments. These patterns consist of strings or regular expressions.

```julia
help?> @comment
  @comment(expr)

  Create a CommentPattern from the given String or Regex.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> @comment "This is a comment"
  CommentPattern:
  # This is a comment

  julia> @comment """
             This is a
             multiline comment"""
  CommentPattern:
  #= This is a
  multiline comment =#

  julia> @comment r"TODO[\S\s]*"
  CommentPattern:
  # r"TODO[\S\s]*"
```

A `CommentPattern` doesn't have an equivalent for `syntax_match`; it
only has one for `syntax_match_all` – `comment_match_all`:

```julia
julia> src = parseall(SyntaxNode, """
                                  # x
                                  x = 1
                                  x = 2  # TODO: Remove reassignment
                                  function f(x)
                                      # TODO: Change `x` to `a`
                                      return x
                                  end
                                  """);

julia> todo_comment = @comment r"TODO[\S\s]*"
CommentPattern:
# r"TODO[\S\s]*"

julia> comment_match_all(todo_comment, src).matches
2-element Vector{BindingSet}:
 BindingSet @ 3:8 with 0 entries
 BindingSet @ 5:5 with 0 entries
```
