# Matching utils
# ==============

"""
    previous_line(src::SyntaxNode)

Return the line previous to `src` as a string. If `src` is at the first line, return the
empty string.

# Examples

```julia
julia> src = parsestmt(SyntaxNode, \"\"\"
                                   function f(x)
                                       x += 1
                                       return x
                                   end
                                   \"\"\")
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
```
"""
function previous_line(src::JS.SyntaxNode)
    source_file = JS.sourcefile(src)
    source_line = JS.source_location(src)[1]
    # Check if there is a previous line.
    source_line == JS.sourcefile(src).line_starts[1] &&
        return ""
    prev_line_first_byte = JS.sourcefile(src).line_starts[source_line - 1]
    prev_line_byte_range = JS.source_line_range(JS.sourcefile(src), prev_line_first_byte)
    return strip(view(source_file, prev_line_byte_range[1]:prev_line_byte_range[2]))
end

"""
    inside_parens(src::SyntaxNode)

Returns `true` if `src` is enclosed in parentheses and `false` otherwise.

# Examples

```julia
julia> src = parsestmt(SyntaxNode, \"\"\" "interpolated \$x" \"\"\")
SyntaxNode:
[string]
  "interpolated "                        :: String
  x                                      :: Identifier


julia> inside_parens(src[2])
false

julia> src = parsestmt(SyntaxNode, \"\"\" "interpolated \$(x)" \"\"\")
SyntaxNode:
[string]
  "interpolated "                        :: String
  x                                      :: Identifier


julia> inside_parens(src[2])
true

julia> string_interpolation = @pattern ~and(
           "\$({_}...)\$({x:::identifier})\$({_}...)",
           ~when([:x], !inside_parens(x.src))
       );

julia> syntax_match(string_interpolation, parsestmt(SyntaxNode, \"\"\" "interpolated \$x" \"\"\"))
BindingSet @ 0:0 with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: x @ 1:17
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet @ 0:0 with 0 entries

julia> syntax_match(string_interpolation, parsestmt(SyntaxNode, \"\"\" "interpolated \$(x)" \"\"\"))
MatchFail: no match @ :1:2
```
"""
function inside_parens(src::JS.SyntaxNode)
    source_file = JS.sourcefile(src)
    source_byte_range = JS.byte_range(src)
    prev_byte = source_byte_range[1] - 1
    next_byte = source_byte_range[end] + 1
    # Check that we have an opening parenthesis at `prev_byte` and a closing one at
    # `next_byte`.
    view(source_file, prev_byte:prev_byte) == "(" || return false
    view(source_file, next_byte:next_byte) == ")" || return false
    return true
end

"""
    comments(src::SyntaxNode)

Returns a list of comments found in `src` with their associated byte ranges.

# Examples

```julia
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
"""
comments(src::JS.SyntaxNode) = _comments(src.data.raw, JS.sourcetext(src))
function _comments(node::Union{Nothing, JS.GreenNode},
                   str::AbstractString;
                   result::Vector{Tuple{AbstractString, UnitRange{Int}}}=
                       Tuple{AbstractString, UnitRange{Int}}[],
                   pos::Int64=1)
    isnothing(node) && return result
    if kind(node) != K"Comment"
        is_leaf(node) && return result
        p = pos
        for c in children(node)
            _comments(c, str; result, pos=p)
            p += c.span
        end
        return result
    end
    # The node is a comment. Add the node and its byte range to the results list.
    byte_range = pos:prevind(str, pos + node.span)
    push!(result, (view(str, byte_range), byte_range))
    is_leaf(node) && return result
    p = pos
    for c in children(node)
        _comments(c, str; result, pos=p)
        p += c.span
    end
    return result
end
