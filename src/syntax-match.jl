# TODO: Add location information.
struct MatchFail
    message::String
end
MatchFail() = MatchFail("no match")

const MatchResult = Union{MatchFail, BindingSet}

"""
    check(fail_conditions::Vector{Function}, match_result)

If the fail conditions are satisfied or if `match_result` is a `MatchFail` return a
corresponding `MatchFail`. Otherwise, return `match_result::BindingSet`.
"""
function check(fail_conditions::Vector{Function}, bindings::BindingSet)::MatchResult
    # TODO.
    return bindings
end
check(::Vector{Function}, failure::MatchFail)::MatchFail = failure

function syntax_match(pattern::Pattern,
                      src::JuliaSyntax.SyntaxNode,
                      bindings::BindingSet = BindingSet())::MatchResult
    bindings = syntax_match(pattern.ast, src, bindings)
    return check(pattern.fail_conditions, bindings)
end
function syntax_match(syntax_class::SyntaxClass,
                      src::JuliaSyntax.SyntaxNode,
                      bindings::BindingSet = BindingSet())::MatchResult
    failure = MatchFail()
    for pattern in syntax_class.pattern_alternatives
        match_result = syntax_match(pattern, src, bindings)
        # Return the first successful match.
        isa(match_result, BindingSet) && return match_result
        # TODO: Track the failures and return the most relevant one.
        failure = match_result
    end
    # If neither of the pattern alternatives matched, `src` does not match the syntax_class.
    return failure
end
function syntax_match(pattern_node::SyntaxPatternNode,
                      src::JuliaSyntax.SyntaxNode,
                      bindings::BindingSet = BindingSet())::MatchResult
    # Special syntax.
    is_pattern_form(pattern_node) &&
        return syntax_match_pattern_form(pattern_node, src, bindings)
    # Regular syntax.
    # TODO: Replace `success` with just `bindings`?
    success = bindings
    head(pattern_node) != head(src) && return MatchFail()
    pattern_node.data.val != src.data.val && return MatchFail()
    xor(is_leaf(pattern_node), is_leaf(src)) && return MatchFail()
    # Recurse on children if there are any.
    is_leaf(src) && return success
    length(children(pattern_node)) != length(children(src)) && return MatchFail()
    zipped_children = zip(children(pattern_node), children(src))
    for c_pair in zipped_children
        match_result = syntax_match(c_pair[1], c_pair[2], success)
        if isa(match_result, MatchFail)
            return match_result
        end
        # TODO: Don't mutate the result here. Only mutate for `var` form.
        #       `bindings = match_result`
        merge!(success, match_result)
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
                                   src::JuliaSyntax.SyntaxNode,
                                   bindings::BindingSet)::MatchResult
    nodes = (pattern_node, src)
    isa(pattern_node.data, FailSyntaxData) && return syntax_match_fail(nodes...,
                                                                       bindings)
    isa(pattern_node.data, VarSyntaxData) && return syntax_match_var(nodes...)
    isa(pattern_node.data, OrSyntaxData) && return syntax_match_or(nodes...)
    isa(pattern_node.data, AndSyntaxData) && return syntax_match_and(nodes...)
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
        if isa(err, BindingSetKeyError) || isa(err, MatchError)
            rethrow(err)
        else
            message = sprint(showerror, err)
            true
        end
    end
    return fail ? MatchFail(message) : BindingSet()
end

"""
Try to match a `var` pattern form. If there's a match, bind the pattern variable.
"""
function syntax_match_var(var_node::SyntaxPatternNode,
                          src::JuliaSyntax.SyntaxNode)::MatchResult
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
    # @info "syntax class" syntax_class
    # Try to match the pattern syntax class to the AST.
    match_result = syntax_match(syntax_class, src)
    isa(match_result, MatchFail) && return match_result
    # If there's a match and the pattern variable is not anonymous, bind the pattern
    # variable and return the binding as a `BindingSet`.
    is_anonymous_pattern_variable(pattern_var_name) &&
        return BindingSet()
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
                         src::JuliaSyntax.SyntaxNode)::MatchResult
    failure = MatchFail("no matching alternative")
    for p in children(or_node)
        match_result = syntax_match(p, src)
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
                          src::JuliaSyntax.SyntaxNode)::MatchResult
    bindings = BindingSet()
    for p in children(and_node)
        match_result = syntax_match(p, src, bindings)
        isa(match_result, MatchFail) && return match_result
        merge!(bindings, match_result)
    end
    return bindings
end
