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
# TODO: Don't duplicate code.
function syntax_match(pattern_node::SyntaxPatternNode,
                      src::JuliaSyntax.SyntaxNode)::MatchResult
    match_result = _syntax_match(pattern_node, src)
    isa(match_result, MatchFail) && return match_result
    return remove_invalid_bindings(match_result)
end
function syntax_match(pattern_nodes::Vector{SyntaxPatternNode},
                      srcs::Vector{JuliaSyntax.SyntaxNode})::MatchResult
    match_result = _syntax_match(pattern_nodes, srcs)
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
                       bindings::BindingSet=BindingSet();
                       recover_stack=[])::MatchResult
    if isempty(srcs)
        # Reasons to be here:
        #
        # 1. This is the only match possibility left. (This is the case if we have no state
        #    in `recover_stack` to return to.)
        #
        #    1.a. Only repetitions remain among the pattern nodes. These can all match
        #         0 source nodes.
        #    1.b. Non-repetition patterns remain unmatched. Since there was no other way to
        #         get here, there is no match.
        #
        # 2. There are other states to try if this one fails. (The `recovery_stack` is not
        #    empty.)
        #
        #    2.a. This path results in a match, so there's no need to try other states.
        #    2.b. There is no possible match on this path. We need to try another state
        #         (the last one stored in the `recover_stack`.)
        all(is_rep.(pattern_nodes)) && return permanentise_bindings(bindings)  # 1.a, 2.a
        isempty(recover_stack) && return MatchFail()                           # 1.b
        pattern_nodes, srcs, bindings = pop!(recover_stack)
        return _syntax_match(pattern_nodes, srcs, bindings; recover_stack)     # 2.b
    end
    # If we're here, there still are unmatched source nodes.
    #
    # If there are no more pattern nodes to match the source nodes, there can be no match.
    isempty(pattern_nodes) && return MatchFail()
    # Here we know we have at least one pattern node and at least one source node.
    p = pattern_nodes[1]
    s = srcs[1]
    match_result = _syntax_match(p, s, bindings)
    if !is_rep(p)
        # The first pattern node is not a repetition so it needs to match the first source
        # node exactly.
        if isa(match_result, MatchFail)
            # If the two don't match, we might be able to try a previous state. If there is
            # no other state to return to, the overall match fails.
            isempty(recover_stack) && return match_result
            # There are still states we can go back to.
            pattern_nodes, srcs, bindings = pop!(recover_stack)
            return _syntax_match(pattern_nodes, srcs, bindings; recover_stack)
        end
        # If the pattern and source nodes match, we can continue matching the remainders of
        # the node lists.
        return _syntax_match(rest(pattern_nodes), rest(srcs), match_result; recover_stack)
    end
    # If we're here, the first pattern node in the list is a repetition.
    if isa(match_result, MatchFail)
        # No match for a repetition node means the repetition consumed all the source nodes
        # it could. We can continue matching with the next pattern in the list.
        #
        # While consuming source nodes, a repetition node creates a temporary entry in the
        # bindings set where source nodes get added as they are consumed. Once the
        # repetition ends the entry is "permanentised". This means it is either given the
        # appropriate name (the name of the pattern variable in the repetition node) or it
        # is discarded if the repetition pattern variable is anonymous.
        bindings = permanentise_bindings(bindings)
        return _syntax_match(rest(pattern_nodes), srcs, bindings; recover_stack)
    end
    # The last possibility is that the repetition is a match. We need to do two things:
    #
    # 1. Mark a recover state. Repetitions can match any number of source nodes (including
    #    0). Whenever a repetition pattern node fits a source node, it can either "consume"
    #    it or leave it for the next pattern node to match. Therefore, a decision is made.
    #    We can't tell in advance which decision is right, so we continue on one path and
    #    we save the other possible path in order to get back to it if the chosen path
    #    fails.
    #
    #    In the recover state, the repetition is finished.
    push!(recover_stack, (rest(pattern_nodes), srcs, permanentise_bindings(bindings)))
    # 2. Continue on the preferred path. This path is eager, meaning that a repetition node
    #    consumes as many source nodes as possible.
    return _syntax_match(pattern_nodes, rest(srcs), match_result; recover_stack)
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
    isa(node_data, RepSyntaxData)  && return syntax_match_rep(args...)
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
                          bindings::BindingSet;
                          tmp=false)::MatchResult
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
    # If there's a match and the pattern variable is not anonymous or is `keep_anonymous`
    # is `true`, bind the pattern variable and add it to the `BindingSet`.
    is_anonymous_pattern_variable(pattern_var_name) && !tmp &&
        return bindings
    # If the binding already exists, check if the new binding is compatible with the
    # old one.
    if haskey(bindings, pattern_var_name)
        b = bindings[pattern_var_name]
        isa(b, InvalidBinding) && return MatchFail(b.msg)
        if isa(b, TemporaryBinding)
            push!(bindings[pattern_var_name].src, src)
            return bindings
        end
        # If the bindings are not compatible, mark the binding so that it won't bind
        # further.
        if !compatible(b.src, src)
            fail_msg = "conflicting bindings for pattern variable $pattern_var_name"
            bindings[pattern_var_name] = InvalidBinding(fail_msg)
            return MatchFail(fail_msg)
        end
    end
    if tmp
        bindings[pattern_var_name] = TemporaryBinding(pattern_var_name, [src], match_result)
    else
        # TODO: Multiple pattern variable appearances should be bound to multiple source
        #       nodes.
        bindings[pattern_var_name] = Binding(pattern_var_name, src, match_result)
    end
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

"""
Try to match a `rep` pattern form. If the `var` node contained within the `rep` matches,
bind the pattern variable as a `TemporaryBinding`.
"""
function syntax_match_rep(rep_node::SyntaxPatternNode,
                          src::JuliaSyntax.SyntaxNode,
                          bindings::BindingSet)::MatchResult
    return syntax_match_var(_get_rep_var(rep_node), src, bindings; tmp=true)
end

## Utils.

function compatible(ex1::JuliaSyntax.SyntaxNode, ex2::JuliaSyntax.SyntaxNode)
    head(ex1) == head(ex2) || return false
    ex1.data.val == ex2.data.val || return false
    xor(is_leaf(ex1), is_leaf(ex2)) && return false
    is_leaf(ex1) && return true
    return all(map(p -> p[1] == p[2], zip(children(ex1), children(ex2))))
end

(rest(v::Vector{JuliaSyntax.TreeNode{T}})::Vector{JuliaSyntax.TreeNode{T}}) where T =
    isempty(v) || length(v) == 1 ? [] : v[2:end]

remove_invalid_bindings(bs::BindingSet)::BindingSet{Binding} =
    filter(p -> isa(p.second, Binding), bs)

function permanentise_bindings(bs::BindingSet)
    without_temp = BindingSet()
    for (k, v) in bs
        if isa(v, TemporaryBinding)
            # Only permanentise non-anonymous bindings.
            if !is_anonymous_pattern_variable(k)
                without_temp[k] = Binding(v)
            end
        else
            without_temp[k] = v
        end
    end
    return without_temp
end
