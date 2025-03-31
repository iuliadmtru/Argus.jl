## Template comparison.

# TODO: Needs more work.
"""
    template_compare!(template::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)

Compare a given template to a source AST. If the template contains placeholders, fill them.
The comparison fails if the source doesn't fit the template or if the placeholders' bindings
don't agree with each other.

Return `true` if the template matches the source AST, `false` otherwise.
"""
function template_compare!(template::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)
    # The node is a placeholder that needs to be filled.
    # TODO: Take into account repetitions with ellipses.
    is_placeholder(template) && return placeholder_fill!(template.data, src)

    # The node itself is not a special node, but it has a successor
    # with some special syntax.
    if contains_placeholders(template)
        head(template) != head(src) && return false
        if length(children(template)) == length(children(src))
            zipped_children = zip(children(template), children(src))
            return all(p -> template_compare!(p[1], p[2]), zipped_children)
        else
            # The rule might have ellipses.
            # TODO.
            return false
        end
    end

    # No special syntax.
    head(template) != head(src) && return false
    template.data.val != src.data.val && return false
    length(children(template)) != length(children(src)) && return false
    zipped_children = zip(children(template), children(src))
    # TODO: This doesn't seem finished.
    return all(p -> template_compare!(p[1], p[2]), zipped_children)
end

## -------------------------------------------

## Template matching.

"""
    template_match!(template::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)

Try to match the given template with a source AST and all its children. When a match is
found bind the placeholders in the template, if any. Return an array of `SyntaxMatch`es.
"""
function template_match!(tp::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)::SyntaxMatches
    if template_compare!(tp, src)
        return SyntaxMatches([SyntaxMatch(src, placeholders(tp))])
    end
    if isnothing(children(src))
        return SyntaxMatches()
    end
    # Clean up bound placeholders.
    # TODO: Should this be done elsewhere?
    placeholders_unbind!(tp)
    # Search for matches within children.
    matches = SyntaxMatches()
    for c in children(src)
        append!(matches, template_match!(tp, c))
    end

    return matches
end
