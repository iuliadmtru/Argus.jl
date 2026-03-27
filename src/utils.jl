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
