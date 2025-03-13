# TODO: Actually implement equality.
# TODO: Look for an efficient method.
ast_equals(ast1::SyntaxNode, ast2::SyntaxNode) = string(ast1) === string(ast2)

# TODO: Make this tail-recursive?
# TODO: Is it possible to remove recursivity?
function search_ast(rule_ast::SyntaxNode, src_ast::SyntaxNode)::RuleMatches
    if ast_equals(rule_ast, src_ast)
        # @info "equal"
        return RuleMatches([RuleMatch(src_ast)])
    end

    if isnothing(src_ast.children)
        # @info "here" src_ast
        return RuleMatches()
    end

    # @info "also here"
    matched_nodes = RuleMatches()

    for child in src_ast.children
        # @info child
        append!(matched_nodes, search_ast(rule_ast, child))
    end

    # @info matched_nodes
    return matched_nodes
end
