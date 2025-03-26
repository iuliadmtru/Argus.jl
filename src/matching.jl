
#=
    Matches
=#

struct RuleMatch
    # text::String
    ast::JuliaSyntax.SyntaxNode
    source_location # Change to byte range? Keep both? Is this field necessary? A: It depends on what the `ast` above is.
end
RuleMatch(ast::SyntaxNode) = RuleMatch(ast, source_location(ast))

struct RuleMatches <: AbstractVector{RuleMatch}
    matches::AbstractVector{RuleMatch}
end
RuleMatches() = RuleMatches(RuleMatch[])

Base.size(v::RuleMatches) = size(v.matches)
Base.getindex(v::RuleMatches, i::Int) = v.matches[i]
Base.getindex(v::RuleMatches, r::UnitRange) = RuleMatches(view(v.matches, r))
Base.setindex!(v::RuleMatches, el::RuleMatch, i::Int) = RuleMatches(setindex!(v.matches, el, i))
Base.push!(v::RuleMatches, el::RuleMatch) = RuleMatches(push!(v.matches, el))
Base.pushfirst!(v::RuleMatches, el::RuleMatch) = RuleMatches(pushfirst!(v.matches, el))


#=
    AST search and comparison.
=#

function ast_compare(rule::RuleSyntaxNode, source::JuliaSyntax.SyntaxNode)
    # No match if the ASTs have different heads or if one has children
    # and the other doesn't.
    head(rule) != head(source) && return false
    xor(haschildren(rule), haschildren(source)) && return false # Is this ever necessary?

    # TODO

    return true
end

# TODO: Rename.
function search_ast(rule_ast::RuleSyntaxNode, src_ast::JuliaSyntax.SyntaxNode)::RuleMatches
    if ast_compare(rule_ast, src_ast)
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

# TODO: Rename.
function check(rule::Pattern, filename::String)::RuleMatches
    src = read(filename, String)

    # Obtain ASTs.
    rule_ast = rule.ast
    source_ast = parseall(SyntaxNode, src; filename=filename)

    # Compare ASTs.
    matches = search_ast(rule_ast, source_ast)

    return matches
end

