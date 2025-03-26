function ast_match(rule::RuleSyntaxNode, source::JuliaSyntax.SyntaxNode)
    # No match if the ASTs have different heads or if one has children
    # and the other doesn't.
    head(rule) != head(source) && return false
    xor(haschildren(rule), haschildren(source)) && return false # Is this ever necessary?

    # TODO

    return true
end

# function _ast_match(rule_node::)

function search_ast(rule_ast::RuleSyntaxNode, src_ast::JuliaSyntax.SyntaxNode)::RuleMatches
    if ast_match(rule_ast, src_ast)
        return RuleMatches([RuleMatch(src_ast)])
    end

    if isnothing(src_ast.children)
        return RuleMatches()
    end

    matched_nodes = RuleMatches()

    for child in src_ast.children
        append!(matched_nodes, search_ast(rule_ast, child))
    end

    return matched_nodes
end
