#=
    Matches
=#

struct RuleMatch
    # text::String
    ast::JuliaSyntax.SyntaxNode
    metavariables::Union{Nothing, Vector{Metavariable}}
end
RuleMatch(ast::SyntaxNode) = RuleMatch(ast, nothing)

function set_metavariable!(match::RuleMatch, metavar::Metavariable)
    metavars = match.metavariables
    if isnothing(metavars)
        match.metavariables = [metavar]
        return true
    else
        existing_metavar = findfirst(m -> m.name === metavar.name, metavars)
        if isnothing(existing_metavar)
            push!(match.metavariables, metavar)
            return true
        elseif metavars_match(existing_metavar, metavar)
            # The metavariable is bound already and there is no mismatch.
            return true
        end
    end

    # If we're here, the match tried to bind `metavar` to two different bindings
    # which would mean the match is wrong.
    return false
end

function Base.getproperty(m::RuleMatch, name::Symbol)
    name === :metavariables && return getfield(m, :metavariables)
    ast = getfield(m, :ast)
    name === :ast && return ast
    return getproperty(ast, name)
end

JuliaSyntax.source_location(m::RuleMatch) = source_location(m.ast)


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

function ast_compare!(rule::RuleSyntaxNode, source::JuliaSyntax.SyntaxNode)
    if is_special_syntax(rule.data)
        return ast_compare_special!(rule.data, source)
    end

    if has_special_syntax(rule)
        # The node itself is not a special node, but it has a successor
        # with some special syntax.
        head(rule) != head(source) && return false

        if length(children(rule)) == length(children(source))
            zipped_children = zip(children(rule), children(source))
            return all(p -> ast_compare!(p[1], p[2]), zipped_children)
        else
            # The rule might have an ellipsis.
            # TODO.
            return false
        end
    end

    # No special syntax.
    head(rule) != head(source) && return false
    rule.data.val != source.data.val && return false
    length(children(rule)) != length(children(source)) && return false
    zipped_children = zip(children(rule), children(source))

    # TODO.

    return all(p -> ast_compare!(p[1], p[2]), zipped_children)
end

function ast_compare_special!(data::RuleSyntaxData{Metavariable}, source_ast::JuliaSyntax.SyntaxNode)
    # TODO: Change to `is_identifier`?
    !is_valid_identifier(source_ast) && return false
    return set_binding!(data.special_syntax, source_ast.data)
end


# TODO: Rename.
function search_ast!(rule_ast::RuleSyntaxNode, src_ast::JuliaSyntax.SyntaxNode)::RuleMatches
    if ast_compare!(rule_ast, src_ast)
        return RuleMatches([RuleMatch(src_ast, all_special_syntax(rule_ast))])
    end
    if isnothing(src_ast.children)
        return RuleMatches()
    end
    # Clean up bound metavariables.
    clean_up_special_syntax!(rule_ast)

    matched_nodes = RuleMatches()
    for child in src_ast.children
        append!(matched_nodes, search_ast!(rule_ast, child))
    end

    return matched_nodes
end

# TODO: Rename.
function check!(rule::Pattern, filename::String)::RuleMatches
    src = read(filename, String)
    # Obtain ASTs.
    rule_ast = rule.ast
    source_ast = parseall(SyntaxNode, src; filename=filename)
    # Compare ASTs.
    matches = search_ast!(rule_ast, source_ast)

    return matches
end

