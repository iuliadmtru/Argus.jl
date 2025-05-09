struct Pattern
    ast::SyntaxPatternNode
    fail_conditions::Vector{Function}
end

# TODO: Support for fail conditions.
Pattern(ex::Expr) = Pattern(SyntaxPatternNode(ex), [])
