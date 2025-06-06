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
    # Remove invalid bindings and permanentise temporary bindings (there can be one
    # temporary binding if the last matching pattern was a repetition).
    match_result = remove_invalid_bindings(match_result)
    return make_permanent(match_result)
end
function syntax_match(pattern_nodes::Vector{SyntaxPatternNode},
                      srcs::Vector{JuliaSyntax.SyntaxNode})::MatchResult
    match_result = _syntax_match(pattern_nodes, srcs)
    isa(match_result, MatchFail) && return match_result
    match_result = remove_invalid_bindings(match_result)
    return make_permanent(match_result)
end

"""
Returns a `BindingSet{AbstractBinding}` which may contain `InvalidBinding`s or
`TemporaryBinding`s.
"""
function _syntax_match(pattern_node::SyntaxPatternNode,
                       src::JuliaSyntax.SyntaxNode,
                       bindings::BindingSet=BindingSet();
                       recovery_stack=[],
                       tmp=false)::MatchResult
    # Special syntax.
    if is_pattern_form(pattern_node)
        match_result =
            syntax_match_pattern_form(pattern_node, src, bindings; recovery_stack, tmp)
        isa(match_result, MatchFail) &&
            # Try another path if possible.
            return recover(recovery_stack, _syntax_match; fail_ret=match_result, tmp)
        # If there is a match, return it.
        return match_result
    end
    # Regular syntax.
    head(pattern_node) != head(src) &&
        return recover(recovery_stack, _syntax_match; fail_ret=MatchFail(), tmp)
    pattern_node.data.val != src.data.val &&
        return recover(recovery_stack, _syntax_match; fail_ret=MatchFail(), tmp)
    xor(is_leaf(pattern_node), is_leaf(src)) &&
        return recover(recovery_stack, _syntax_match; fail_ret=MatchFail(), tmp)
    # Recurse on children if there are any.
    is_leaf(src) && return bindings
    return _syntax_match(children(pattern_node), children(src), bindings; recovery_stack, tmp)
end
function _syntax_match(pattern_nodes::Vector{SyntaxPatternNode},
                       srcs::Vector{JuliaSyntax.SyntaxNode},
                       bindings::BindingSet=BindingSet();
                       recovery_stack=[],
                       tmp=false)::MatchResult
    match_result, srcs = _partial_syntax_match(pattern_nodes, srcs, bindings; tmp)
    isa(match_result, MatchFail) &&
        return recover(recovery_stack, _syntax_match; fail_ret=match_result, tmp)
    isempty(srcs) ||
        return recover(recovery_stack, _syntax_match; fail_ret=MatchFail(), tmp)
    return match_result
end
"""
TODO: Rewrite this.

Main matching function. Tries to match a sequence of pattern nodes to a sequence of source
nodes exactly once. If any of the pattern nodes fails to match, returns a `MatchFail`. A
successful partial match returns the generated bindings as a `BindingSet` (can contain
`InvalidBinding`s) and the remaining unmatched source nodes.
"""
function _partial_syntax_match(pattern_nodes::Vector{SyntaxPatternNode},
                               srcs::Vector{JuliaSyntax.SyntaxNode},
                               bindings::BindingSet=BindingSet();
                               recovery_stack=[],
                               tmp=false)::Tuple{MatchResult, Vector{JuliaSyntax.SyntaxNode}}
    if isempty(srcs)
        # Reasons to be here:
        #
        # 1. This is the only match possibility left. (This is the case if we have no state
        #    in `recovery_stack` to return to.)
        #
        #    1.a. Only repetitions remain among the pattern nodes, or no patterns at all.
        #         These can all match 0 source nodes.
        #    1.b. Non-repetition patterns remain unmatched. Since there was no other way to
        #         get here, there is no match.
        #
        # 2. There are other states to try if this one fails. (The `recovery_stack` is not
        #    empty.)
        #
        #    2.a. This path results in a match, so there's no need to try other states.
        #    2.b. There is no possible match on this path. We need to try another state
        #         (the last one stored in the `recovery_stack`.)
        isempty(pattern_nodes) && return (bindings, srcs)          # 1.a,2.a
        # Try the first pattern. There can only be a match if it's a repetition.
        p = pattern_nodes[1]
        if is_rep(p)
            match_result = syntax_match_rep(p, srcs, bindings)
            # This could never fail, but it's good to be exhaustive.
            isa(match_result, MatchFail) && return (match_result, srcs)
            return _partial_syntax_match(rest(pattern_nodes),                       # 1.a,
                                         srcs,                                      # 1.b,
                                         match_result;                              # 2.a,
                                         recovery_stack,                            # 2.b
                                         tmp)
        end
        # The first pattern node is not a repetition, so there can be no match on this path.
        return recover(recovery_stack,                      # `recovery_stack` empty => 1.b
                       _partial_syntax_match;               # else                   => 2.b
                       fail_ret=(MatchFail(), srcs),
                       tmp)
    end
    # If we're here, there still are unmatched source nodes.
    #
    # No pattern nodes left marks the end of the partial match.
    isempty(pattern_nodes) && return (bindings, srcs)
    # Here we know we have at least one pattern node and at least one source node.
    p = pattern_nodes[1]
    s = srcs[1]
    match_result = _syntax_match(p, s, bindings; tmp)
    if !is_rep(p)
        # The first pattern node is not a repetition so it needs to match the first source
        # node exactly.
        if isa(match_result, MatchFail)
            # If the two don't match, we might be able to try a previous state. If there is
            # no other state to return to, the overall match fails.
            return recover(recovery_stack,
                           _partial_syntax_match;
                           fail_ret=(match_result, srcs),
                           tmp)
        end
        # If the pattern and source nodes match, we can continue matching the remainders of
        # the node lists.
        return _partial_syntax_match(rest(pattern_nodes),
                                     rest(srcs),
                                     match_result;
                                     recovery_stack,
                                     tmp)
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
        bindings = make_permanent(bindings)
        return _partial_syntax_match(rest(pattern_nodes),
                                     srcs,
                                     bindings;
                                     recovery_stack,
                                     tmp)
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
    #    In the recover state, the repetition is finished and the repetition node did not
    #    match the current source node.
    push!(recovery_stack, (rest(pattern_nodes), srcs, make_permanent(bindings)))
    # 2. Continue on the preferred path. This path is eager, meaning that a repetition node
    #    consumes as many source nodes as possible.
    return _partial_syntax_match(pattern_nodes,
                                 rest(srcs),
                                 match_result;
                                 recovery_stack,
                                 tmp)
end

# --------------------------------------------
# Pattern form matching.

"""
Try to match a pattern form node. Dispatch the matching to the specific pattern form
match function.
"""
function syntax_match_pattern_form(pattern_node::SyntaxPatternNode,
                                   src::JuliaSyntax.SyntaxNode,
                                   bindings::BindingSet;
                                   recovery_stack=[],
                                   tmp=false)::MatchResult
    args = (pattern_node, src, bindings)
    node_data = pattern_node.data
    # Dispatch on form type.
    isa(node_data, FailSyntaxData) && return syntax_match_fail(args...; recovery_stack, tmp)
    isa(node_data, VarSyntaxData)  && return syntax_match_var(args...; tmp)
    isa(node_data, OrSyntaxData)   && return syntax_match_or(args...; recovery_stack, tmp)
    isa(node_data, AndSyntaxData)  && return syntax_match_and(args...; tmp)
    isa(node_data, RepSyntaxData)  && return syntax_match_rep(args...)
    return MatchFail("unknown pattern form")
end

"""
Try to match a `fail` pattern form. If the fail condition is satisfied, return a
`MatchFail` with the fail message. Otherwise, return an empty `BindingSet`.
"""
function syntax_match_fail(fail_node::SyntaxPatternNode,
                           src::JuliaSyntax.SyntaxNode,
                           bindings::BindingSet;
                           recovery_stack=[],
                           tmp=false)::MatchResult
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
    return fail                                                                  ?
        recover(recovery_stack, _syntax_match; fail_ret=MatchFail(message), tmp) :
        bindings
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
    bindings::BindingSet = copy(bindings)
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
    if tmp
        bindings[pattern_var_name] =
            TemporaryBinding(pattern_var_name, src, match_result, 0)
    else
        bindings[pattern_var_name] = Binding(pattern_var_name, src, match_result, 0)
    end
    return bindings
end

"""
Try to match an `or` pattern form. Return the match for the first pattern alternative.
Fail if no alternative matches.
"""
function syntax_match_or(or_node::SyntaxPatternNode,
                         src::JuliaSyntax.SyntaxNode,
                         bindings=BindingSet;
                         recovery_stack=[],
                         tmp=false)::MatchResult
    failure = MatchFail("no matching alternative")
    for (i, p) in enumerate(children(or_node))
        match_result = _syntax_match(p, src, BindingSet(); recovery_stack, tmp)
        if isa(match_result, BindingSet)
            # If this is not the last branch, we might be able to recover from one of the
            # next branches if the encompassing pattern fails.
            if i < length(children(or_node))
                next_try_children = children(or_node)[i+1:end]
                next_try =
                    SyntaxPatternNode(or_node.parent, next_try_children, OrSyntaxData())
                [c.parent = next_try for c in next_try_children]
                push!(recovery_stack, (next_try, src, bindings))
            end
            return match_result
        end
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
                          bindings::BindingSet;
                          recovery_stack=[],
                          tmp=false)::MatchResult
    for p in children(and_node)
        match_result = _syntax_match(p, src, bindings; recovery_stack, tmp)
        isa(match_result, MatchFail) && return match_result
        bindings = match_result
    end
    return bindings
end

function syntax_match_rep(rep_node::SyntaxPatternNode,
                          src::JuliaSyntax.SyntaxNode,
                          bindings::BindingSet)::MatchResult
    kind(src) === K"toplevel" && return syntax_match_rep(rep_node, children(src), bindings)

    match_result = _syntax_match(rep_node.children[1], src, BindingSet(); tmp=true)
    isa(match_result, MatchFail) && return match_result

    bindings::BindingSet = deepcopy(bindings)
    for var in rep_vars(rep_node)
        var_name = var.name
        var_binding = match_result[var_name]
        # Add the bindings generated by matching the repetition inner node, but increase the
        # depth for the bound sources and sub-bindings.
        if haskey(bindings, var_name)
            # If the pattern variable is already bound, we need to add the new binding to
            # its binding list.
            b = bindings[var_name]
            b_src = b.src
            push!(b_src, var_binding.src)
            b_bindings = b.bindings
            push!(b_bindings, var_binding.bindings)
            bindings[var_name] =
                TemporaryBinding(var_name, b_src, b_bindings, var.ellipsis_depth)
        else
            bindings[var_name] = TemporaryBinding(var_name,
                                                  [var_binding.src],
                                                  [var_binding.bindings],
                                                  var.ellipsis_depth)
        end
    end
    return bindings
end
function syntax_match_rep(rep_node::SyntaxPatternNode,
                          srcs::Vector{JuliaSyntax.SyntaxNode},
                          bindings::BindingSet)::MatchResult
    bindings::BindingSet = deepcopy(bindings)
    if isempty(srcs)
        # If there are no source nodes, the repetition pattern variables bind to empty
        # vectors.
        for var in rep_vars(rep_node)
            var_name = var.name
            var_depth = var.ellipsis_depth
            if !haskey(bindings, var_name)
                bindings[var_name] =
                    TemporaryBinding(var_name,
                                     empty_vec(JuliaSyntax.SyntaxNode, var_depth),
                                     empty_vec(BindingSet, var_depth),
                                     var.ellipsis_depth)
            end
        end
        return bindings
    end
    # If there are source nodes, the match is a success only if the repetition node can
    # consume all the source nodes.
    partial_results = BindingSet[]
    while !isempty(srcs)
        partial_result, srcs =
            _partial_syntax_match(children(rep_node), srcs, BindingSet(); tmp=true)
        isa(partial_result, MatchFail) && return partial_result
        push!(partial_results, partial_result)
    end
    match_result = make_permanent(partial_results)
    return merge(bindings, match_result)
end

## Utils.

recover(recovery_stack::AbstractVector, from::Function; fail_ret, kwargs...) =
    isempty(recovery_stack) ?
    fail_ret                :
    from(pop!(recovery_stack)...; recovery_stack, kwargs...)

function compatible(ex1::JuliaSyntax.SyntaxNode, ex2::JuliaSyntax.SyntaxNode)
    head(ex1) == head(ex2) || return false
    ex1.data.val == ex2.data.val || return false
    xor(is_leaf(ex1), is_leaf(ex2)) && return false
    is_leaf(ex1) && return true
    return all(map(p -> p[1] == p[2], zip(children(ex1), children(ex2))))
end

(rest(v::Vector{JuliaSyntax.TreeNode{T}})::Vector{JuliaSyntax.TreeNode{T}}) where T =
    isempty(v) || length(v) == 1 ? [] : v[2:end]

remove_invalid_bindings(bs::BindingSet)::BindingSet =
    filter(p -> !isa(p.second, InvalidBinding), bs)

function make_permanent(bs::BindingSet)
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
"""
TODO: Explain better...

Should be called only when making the result of a repetition match permanent. Merges all the
binding sets corresponding to a pattern variable into a single binding, increasing ellipsis
depth. Does this for all repetition pattern variables.
"""
function make_permanent(bss::Vector{BindingSet})
    result = BindingSet()
    for bs in bss
        bs = make_permanent(bs)
        for (k, v) in bs
            if haskey(result, k)
                result_src = result[k].src
                result_bindings = result[k].bindings
                result_depth = result[k].ellipsis_depth
                result[k] = Binding(v.bname,
                                    push!(result_src, v.src),
                                    push!(result_bindings, v.bindings),
                                    result_depth)
            else
                result[k] = Binding(v.bname, [v.src], [v.bindings], v.ellipsis_depth + 1)
            end
        end
    end

    return result
end

empty_vec(type, depth::Int) = depth == 0      ?
    error("Can't create vector with 0 depth") :
    vec_type(type, depth)()
vec_type(type, depth::Int) = depth == 1 ? Vector{type} : Vector{vec_type(type, depth - 1)}
