struct Pattern
    ast::SyntaxPatternNode
    fail_conditions::Vector{Function}
end

# TODO: Support for fail conditions.
function Pattern(ex)
    try
        ast = SyntaxPatternNode(ex)
        return Pattern(ast, Function[])
    catch
        error("Invalid pattern syntax")
    end
end
