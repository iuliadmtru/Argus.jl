struct Pattern
    ast::SyntaxPatternNode
    fail_conditions::Vector{Function}
end

Pattern(ex::Expr) = Pattern(SyntaxPatternNode(ex), [])
