function inside_parens(src::JS.SyntaxNode)
    source_file = JS.sourcefile(src)
    source_byte_range = JS.byte_range(src)
    view(source_file, source_byte_range .- 1) == "(" || return false
    view(source_file, source_byte_range .+ 1) == ")" || return false
    return true
end
