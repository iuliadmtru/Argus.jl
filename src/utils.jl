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
