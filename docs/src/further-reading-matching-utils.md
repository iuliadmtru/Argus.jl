# Matching Utils

Matching utils can be used by action pattern forms that execute code,
such as `~when`, `~fail` and `~execute`. For now, only three such
utils exist: `previous_line`, `inside_parens` and `comments`.

```julia
help?> previous_line
search: previous_line

  previous_line(src::SyntaxNode)

  Return the line previous to src as a string. If src is at the first line, return the empty string.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> src = parsestmt(SyntaxNode, """
                                     function f(x)
                                         x += 1
                                         return x
                                     end
                                     """)
  SyntaxNode:
  [function]
    [call]
      f                                    :: Identifier
      x                                    :: Identifier
    [block]
      [op=]
        x                                  :: Identifier
        +                                  :: Identifier
        1                                  :: Integer
      [return]
        x                                  :: Identifier


  julia> src[2][1]
  SyntaxNode:
  [op=]
    x                                      :: Identifier
    +                                      :: Identifier
    1                                      :: Integer


  julia> previous_line(src[2][1])
  "function f(x)"

  julia> src[2][2]
  SyntaxNode:
  [return]
    x                                      :: Identifier


  julia> previous_line(src[2][2])
  "x += 1"

help?> inside_parens
search: inside_parens include_string

  inside_parens(src::SyntaxNode)

  Returns true if src is enclosed in parentheses and false otherwise.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> src = parsestmt(SyntaxNode, """ "interpolated $x" """)
  SyntaxNode:
  [string]
    "interpolated "                        :: String
    x                                      :: Identifier


  julia> inside_parens(src[2])
  false

  julia> src = parsestmt(SyntaxNode, """ "interpolated $(x)" """)
  SyntaxNode:
  [string]
    "interpolated "                        :: String
    x                                      :: Identifier


  julia> inside_parens(src[2])
  true

  julia> string_interpolation = @pattern ~and(
             "$({_}...)$({x:::identifier})$({_}...)",
             ~when([:x], !inside_parens(x.src))
         );

  julia> syntax_match(string_interpolation, parsestmt(SyntaxNode, """ "interpolated $x" """))
  BindingSet @ 0:0 with 1 entry:
    :x => Binding:
            Name: :x
            Bound source: x @ 1:17
            Ellipsis depth: 0
            Sub-bindings:
              BindingSet @ 0:0 with 0 entries

  julia> syntax_match(string_interpolation, parsestmt(SyntaxNode, """ "interpolated $(x)" """))
  MatchFail: no match @ :1:2

help?> comments
search: comments @comment codeunits convert count ncodeunits const collect count!

  comments(src::SyntaxNode)

  Returns a list of comments found in src with their associated byte ranges.

  Examples
  ≡≡≡≡≡≡≡≡

  julia> src = parsestmt(SyntaxNode, "x #=commented=# + 1");

  julia> comments(src)
  1-element Vector{Tuple{AbstractString, UnitRange{Int64}}}:
   ("#=commented=#", 3:15)

  julia> no_comments = @pattern ~and(
             {s},
             ~when([:s], isempty(comments(s.src)))
         );

  julia> syntax_match(no_comments, parsestmt(SyntaxNode, "x #=commented=# + 1"))
  MatchFail: no match @ :1:1

  julia> syntax_match(no_comments, parsestmt(SyntaxNode, "x + 1"))
  BindingSet @ 0:0 with 1 entry:
    :s => Binding:
            Name: :s
            Bound source: (call-i x + 1) @ 1:1
            Ellipsis depth: 0
            Sub-bindings:
              BindingSet @ 0:0 with 0 entries
```
