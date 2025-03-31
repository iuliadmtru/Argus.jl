## Template comparison.

# TODO: Needs more work.
"""
    template_compare!(templ::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)

Compare a given template to a source AST. If the template contains placeholders, fill them.
The comparison fails if the source doesn't fit the template or if the placeholders' bindings
don't agree with each other.

Return `true` if the template matches the source AST, `false` otherwise.
"""
function template_compare!(templ::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)
    # The node is a placeholder that needs to be filled.
    # TODO: Take into account repetitions with ellipses.
    is_placeholder(templ) && return placeholder_fill!(templ.data, src)

    # The node itself is not a special node, but it has a successor
    # with some special syntax.
    if contains_placeholder(templ)
        head(templ) != head(src) && return false
        if length(children(templ)) == length(children(src))
            zipped_children = zip(children(templ), children(src))
            return all(p -> template_compare!(p[1], p[2]), zipped_children)
        else
            # The rule might have ellipses.
            # TODO.
            return false
        end
    end

    # No special syntax.
    head(templ) != head(src) && return false
    templ.data.val != src.data.val && return false
    length(children(templ)) != length(children(src)) && return false
    zipped_children = zip(children(templ), children(src))
    # TODO: This doesn't seem finished.
    return all(p -> template_compare!(p[1], p[2]), zipped_children)
end

## -------------------------------------------

## Template matching.

"""
    template_match!(templ::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)

Try to match the given template with a source AST and all its children. If a match is found,
bind the placeholders in the template, if any. Return an array of `RuleMatch`es.
"""
function template_match!(templ::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)
    if template_compare!(templ, src)
        return SyntaxMatches([SyntaxMatch(src, placeholders(templ))])
    end
    if isnothing(children(src))
        return SyntaxMatches()
    end
    # Clean up bound placeholders.
    # TODO: Should this be done here?
    unbind_placeholders!(templ)
    # Search for matches within children.
    matches = SyntaxMatches()
    for c in children(src)
        append!(matched_nodes, template_match!(templ, c))
    end

    return matches
end
