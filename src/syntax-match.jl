struct MatchFail
    message::String
end
MatchFail() = MatchFail("no match")

function check(fail_conditions::Vector{Function},
               bindings::BindingSet)::Union{MatchFail, BindingSet}
    # TODO.
    return bindings
end
check(::Vector{Function}, failure::MatchFail)::MatchFail = failure

function syntax_match(pattern::Pattern,
                      src::JuliaSyntax.SyntaxNode)::Union{MatchFail, BindingSet}
    bindings = syntax_match(pattern.ast, src)
    return check(pattern.fail_conditions, bindings)
end
function syntax_match(syntax_class::SyntaxClass,
                      src::JuliaSyntax.SyntaxNode)::Union{MatchFail, BindingSet}
    for pattern in syntax_class.pattern_alternatives
        match_result = syntax_match(pattern, src)
        # Return the first successful match.
        isa(match_result, BindingSet) && return match_result
        # TODO: Track the failures and return the most relevant one.
    end
    # If neither of the pattern alternatives matched, `src` does not match the syntax_class.
    return MatchFail()
end
function syntax_match(pattern_node::SyntaxPatternNode,
                      src::JuliaSyntax.SyntaxNode)::Union{MatchFail, BindingSet}
    # Special syntax.
    is_pattern_form(pattern_node) && return syntax_match_pattern_form(pattern_node, src)
    # Regular syntax.
    success = BindingSet()
    if head(pattern_node) != head(src)
        # A `[=]` node in the pattern can match a short form function definition.
        if kind(pattern_node) === K"=" && kind(src) === K"function"
            src_flags = JuliaSyntax.flags(src)
            src_flags === JuliaSyntax.SHORT_FORM_FUNCTION_FLAG || return MatchFail()
        else
            return MatchFail()
        end
    end
    pattern_node.data.val != src.data.val && return MatchFail()
    xor(is_leaf(pattern_node), is_leaf(src)) && return MatchFail()
    # Recurse on children if there are any.
    is_leaf(src) && return success
    length(children(pattern_node)) != length(children(src)) && return MatchFail()
    zipped_children = zip(children(pattern_node), children(src))
    for c_pair in zipped_children
        match_result = syntax_match(c_pair[1], c_pair[2])
        if isa(match_result, MatchFail)
            return match_result
        else
            merge!(success, match_result)
        end
    end
    return success
end

# --------------------------------------------
# Pattern form matching.

"""
Try to match a pattern form node. Dispatch the matching to the specific pattern form
match function.
"""
function syntax_match_pattern_form(pattern_node::SyntaxPatternNode,
                                   src::JuliaSyntax.SyntaxNode)::Union{MatchFail, BindingSet}
    isa(pattern_node.data, VarSyntaxData) && return syntax_match_var(pattern_node, src)
    isa(pattern_node.data, OrSyntaxData) && return syntax_match_or(pattern_node, src)
    return MatchFail("unknown pattern form")
end

"""
Try to match a `var` pattern form. If there's a match, bind the pattern variable.
"""
function syntax_match_var(var_node::SyntaxPatternNode,
                          src::JuliaSyntax.SyntaxNode)::Union{MatchFail, BindingSet}
    pattern_var_name = var_node.data.id
    syntax_class_name = var_node.data.syntax_class_name
    syntax_class =
        try
            SYNTAX_CLASS_REGISTRY[syntax_class_name]
        catch e
            if isa(e, KeyError)
                return MatchFail("unregistered syntax class :$syntax_class_name")
            else
                rethrow(e)
            end
        end
    # Try to match the pattern syntax class to the AST.
    match_result = syntax_match(syntax_class, src)
    isa(match_result, MatchFail) && return match_result
    # If there's a match, bind the pattern variable and return the binding as a
    # `BindingSet`.
    binding = Binding(pattern_var_name, src, match_result)
    # TODO: Check for already existing binding. The new binding must be "compatible" with
    #       the old one.
    return BindingSet(pattern_var_name => binding)
end

"""
Try to match an `or` pattern form. Return the match for the first pattern alternative.
Fail if no alternative matches.
"""
function syntax_match_or(or_node::SyntaxPatternNode,
                         src::JuliaSyntax.SyntaxNode)::Union{MatchFail, BindingSet}
    for p in children(or_node)
        match_result = syntax_match(p, src)
        isa(match_result, BindingSet) && return match_result
    end
    # TODO: Return the most specific error.
    return MatchFail("no alternative match")
end
