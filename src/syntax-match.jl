# TODO: Add location information.
struct MatchFail
    message::String
end
MatchFail() = MatchFail("no match")

const MatchResult = Union{MatchFail, BindingSet}

function syntax_match(pattern::Pattern,
                      src::JuliaSyntax.SyntaxNode)::MatchResult
    return syntax_match(pattern.src, src)
end
function syntax_match(syntax_class::SyntaxClass,
                      src::JuliaSyntax.SyntaxNode)::MatchResult
    failure = MatchFail()
    for pattern in syntax_class.pattern_alternatives
        match_result = syntax_match(pattern, src)
        # Return the first successful match.
        isa(match_result, BindingSet) && return match_result
        # TODO: Track the failures and return the most relevant one.
        failure = match_result
    end
    # If neither of the pattern alternatives matched, `src` does not match `syntax_class`.
    return failure
end
# TODO: Split in two separate methods.
function syntax_match(pattern::Union{SyntaxPatternNode,
                                     Vector{SyntaxPatternNode}},
                      src::Union{JuliaSyntax.SyntaxNode,
                                 Vector{JuliaSyntax.SyntaxNode}})::MatchResult
    match_result = _syntax_match(pattern, src)
    isa(match_result, MatchFail) && return match_result
    return remove_invalid_bindings(match_result)
end

"""
Returns a `BindingSet{AbstractBinding}` which may contain `InvalidBinding`s.
"""
function _syntax_match(pattern_node::SyntaxPatternNode,
                       src::JuliaSyntax.SyntaxNode,
                       bindings::BindingSet=BindingSet())::MatchResult
    # Special syntax.
    is_pattern_form(pattern_node) &&
        return syntax_match_pattern_form(pattern_node, src, bindings)
    # Regular syntax.
    head(pattern_node) != head(src) && return MatchFail()
    pattern_node.data.val != src.data.val && return MatchFail()
    xor(is_leaf(pattern_node), is_leaf(src)) && return MatchFail()
    # Recurse on children if there are any.
    is_leaf(src) && return bindings
    return _syntax_match(children(pattern_node), children(src), bindings)
end
function _syntax_match(pattern_nodes::Vector{SyntaxPatternNode},
                       srcs::Vector{JuliaSyntax.SyntaxNode},
                       bindings::BindingSet=BindingSet())::MatchResult
    length(pattern_nodes) != length(srcs) && return MatchFail()
    for node_pair in zip(pattern_nodes, srcs)
        match_result = _syntax_match(node_pair[1], node_pair[2], bindings)
        # If the match failed, return the corresponding failure. Otherwise, update the
        # bindings (`~var` pattern forms may have added bindings to the binding set).
        isa(match_result, MatchFail) && return match_result
        bindings = match_result
    end
    return bindings
end

# --------------------------------------------
# Pattern form matching.

"""
Try to match a pattern form node. Dispatch the matching to the specific pattern form
match function.
"""
function syntax_match_pattern_form(pattern_node::SyntaxPatternNode,
                                   src::JuliaSyntax.SyntaxNode,
                                   bindings::BindingSet)::MatchResult
    args = (pattern_node, src, bindings)
    node_data = pattern_node.data
    # Dispatch on form type.
    isa(node_data, FailSyntaxData) && return syntax_match_fail(args...)
    isa(node_data, VarSyntaxData)  && return syntax_match_var(args...)
    isa(node_data, OrSyntaxData)   && return syntax_match_or(pattern_node, src)
    isa(node_data, AndSyntaxData)  && return syntax_match_and(args...)
    return MatchFail("unknown pattern form")
end

"""
Try to match a `fail` pattern form. If the fail condition is satisfied, return a
`MatchFail` with the fail message. Otherwise, return an empty `BindingSet`.
"""
function syntax_match_fail(fail_node::SyntaxPatternNode,
                           src::JuliaSyntax.SyntaxNode,
                           bindings::BindingSet)::MatchResult
    condition = _get_fail_condition(fail_node)
    message = _get_fail_message(fail_node)
    # Evaluate the fail condition.
    fail = try
        condition(bindings)
    catch err
        if isa(err, BindingFieldError)
            message = sprint(showerror, err)
            true
        else
            rethrow(err)
        end
    end
    return fail ? MatchFail(message) : bindings
end

"""
Try to match a `var` pattern form. If there's a match, bind the pattern variable.
"""
function syntax_match_var(var_node::SyntaxPatternNode,
                          src::JuliaSyntax.SyntaxNode,
                          bindings::BindingSet)::MatchResult
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
    # If there's a match and the pattern variable is not anonymous, bind the pattern
    # variable and add it to the `BindingSet`.
    is_anonymous_pattern_variable(pattern_var_name) && return bindings
    # If the binding already exists, check if the new binding is compatible with the
    # old one.
    if haskey(bindings, pattern_var_name)
        b = bindings[pattern_var_name]
        isa(b, InvalidBinding) && return MatchFail(b.msg)
        # If the bindings are not compatible, mark the binding so that it won't bind
        # further.
        if !compatible(b.src, src)
            fail_msg = "conflicting bindings for pattern variable $pattern_var_name"
            bindings[pattern_var_name] = InvalidBinding(fail_msg)
            return MatchFail(fail_msg)
        end
    end
    bindings[pattern_var_name] = Binding(pattern_var_name, src, match_result)
    return bindings
end

"""
Try to match an `or` pattern form. Return the match for the first pattern alternative.
Fail if no alternative matches.
"""
function syntax_match_or(or_node::SyntaxPatternNode,
                         src::JuliaSyntax.SyntaxNode)::MatchResult
    failure = MatchFail("no matching alternative")
    for p in children(or_node)
        match_result = _syntax_match(p, src, BindingSet())
        isa(match_result, BindingSet) && return match_result
        failure = match_result
    end
    # TODO: Return the most specific error.
    return failure
end

"""
Try to match an `and` pattern form. Return the bindings resulted from all branches, or
the `MatchFail` of the first failing branch.
"""
function syntax_match_and(and_node::SyntaxPatternNode,
                          src::JuliaSyntax.SyntaxNode,
                          bindings::BindingSet)::MatchResult
    for p in children(and_node)
        match_result = _syntax_match(p, src, bindings)
        isa(match_result, MatchFail) && return match_result
        bindings = match_result
    end
    return bindings
end

## Utils.

function compatible(ex1::JuliaSyntax.SyntaxNode, ex2::JuliaSyntax.SyntaxNode)
    head(ex1) == head(ex2) || return false
    ex1.data.val == ex2.data.val || return false
    xor(is_leaf(ex1), is_leaf(ex2)) && return false
    is_leaf(ex1) && return true
    return all(map(p -> p[1] == p[2], zip(children(ex1), children(ex2))))
end

remove_invalid_bindings(bs::BindingSet)::BindingSet{Binding} =
    filter(p -> isa(p.second, Binding), bs)
