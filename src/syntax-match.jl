# Match result
# ============

# TODO: Add location information.
"""
    MatchFail

The result of a pattern match failure. It contains a string explaining the reason for
failure.
"""
struct MatchFail
    message::String
end
MatchFail() = MatchFail("no match")

"""
    MatchResult

The result of a pattern match. It can either be a `MatchFail` with details of the failure
reason, or a `BindingSet` with all the pattern variables bound during the match. Alias for
`Union{MatchFail, BindingSet}`.
"""
const MatchResult = Union{MatchFail, BindingSet}

# Syntax matching
# ===============

"""
    syntax_match(pattern::Pattern, src::JuliaSyntax.SyntaxNode)::MatchResult
    syntax_match(syntax_class::SyntaxClass, src::JuliaSyntax.SyntaxNode)::MatchResult
    syntax_match(pattern_node::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)::MatchResult

Try to match a [`Pattern`](@ref), [`SyntaxClass`](@ref) or [`SyntaxPatternNode`](@ref) with
a source node exactly. Return a [`BindingSet`](@ref) with all the bound pattern variables
in case of a successful match. Otherwise, return a [`MatchFail`](@ref) with an informative
failure message explaining why the match failed.
"""
syntax_match(pattern::Pattern, src::JS.SyntaxNode)::MatchResult =
    syntax_match(pattern.src, src)
function syntax_match(syntax_class::SyntaxClass,
                      src::JS.SyntaxNode)::MatchResult
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
                      src::JS.SyntaxNode)::MatchResult
    match_result = _syntax_match(pattern_node, src)
    isa(match_result, MatchFail) && return match_result
    # Remove invalid bindings and permanentise temporary bindings (there can be one
    # temporary binding if the last matching pattern was a repetition).
    match_result = remove_invalid_bindings(match_result)
    return make_permanent(match_result)
end
"""
    syntax_match(pattern_nodes::Vector{SyntaxPatternNode},
                 srcs::Vector{JuliaSyntax.SyntaxNode})::MatchResult

Try to match a sequence of pattern nodes to a sequence of source nodes. Each pair of nodes
must match exactly. In case of `~rep` nodes, greedily "consume" all matching source nodes.
If the match fails, try to backtrack up to a matching state.
"""
function syntax_match(pattern_nodes::Vector{SyntaxPatternNode},
                      srcs::Vector{JS.SyntaxNode})::MatchResult
    match_result = _syntax_match(pattern_nodes, srcs)
    isa(match_result, MatchFail) && return match_result
    match_result = remove_invalid_bindings(match_result)
    return make_permanent(match_result)
end

"""
    _syntax_match(pattern::SyntaxPatternNode,
                  src::JuliaSyntax.SyntaxNode,
                  bindings::BindingSet=BindingSet();
                  recovery_stack=[],
                  recover=true,
                  tmp=false)::MatchResult

Returns a `BindingSet{AbstractBinding}` which may contain `InvalidBinding`s or
`TemporaryBinding`s. Keep track of all the possibly matching states in `recovery_stack`.
Gather the bindings along the way.

`tmp` marks repetitions. When `tmp` is `true`, all bound variables are stored as temporary.
New bound source nodes are pushed in the temporary bindings' source lists instead of being
compared for compatibility with previously bound nodes.

`recover` signals whether to recover from a match failure or not. It is always `true` except
inside `~and` branches. If an `~and` branch fails we don't want to pop the recovery stack
somewhere inside that branch, wherever the failure happened. That would cause a wrong state
recovery. Instead, we want to try another path for the branch _within_ the `~and`. We keep
the encompassing `~and` configuration, replace the current branch with another path and
try again.


# Examples
# ========

Allowing recoveries inside `~and` branches:

```
julia> pattern = @pattern ~and(
           ~or({a} + {b}, {b} + {a}),
           ~fail(b.value != 2, "not two")
       );

julia> syntax_match(pattern, parsestmt(SyntaxNode, "1 + 3"))
BindingSet with 2 entries:
  :a => Binding:
          Name: :a
          Bound source: 3 @ 1:5
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet with 0 entries
  :b => Binding:
          Name: :b
          Bound source: 1 @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet with 0 entries
```

Whats happens here is:

1.  Try the first `~and` branch. No bindings so far.
    > `~or({a} + {b}, {b} + {a})`
2.  Try the first `~or` alternative.
    > `{a} + {b}`
3.  Does it match `1 + 3`?
    > Yes.
4.  => Mark the state `(~or({b} + {a}), 1 + 3, BindingSet())` as a possible recovery state.
5.  => Return a success for the `~or`; bind `a` to `1` and `b` to `3`.

6.  Try the second `~and` branch with `a` and `b` bound.
    > `~fail(b.value != 2, "not two")`
7.  Does it match for `a` <-> `1` and `b` <-> `3`?
    > No.
8.  Do we have a state we can recover to?
    > Yes, `(~or({b} + {a}), 1 + 3, BindingSet())`.
9.  => Go to that state.

10. Try the first `~or` alternative.
    > `{b} + {a}`
11. Does it match `1 + 3`?
    > Yes.
12. It's the last possible `~or` branch, so we have no possible recovery states to mark.
13. => Return a success for the `~or`; bind `a` to `3` and `b` to `1`.

14. Done.


If we don't allow recoveries inside `~and` branches:

```
julia> syntax_match(pattern, parsestmt(SyntaxNode, "1 + 3"))
MatchFail("not two")
```

Steps 1-7 remain the same. Question 8 is never asked because we are not looking for recovery
states. We directly go back to the parent `~and`.
1-7. ...
8.  => Return a failure with the appropriate message ("not two").

9.  Back in the `~and`.
    Did the branch fail?
    > Yes.
10. Can we try another path for the same branch?
    > Yes, `(~or({b} + {a}), 1 + 3, BindingSet())`.
11. => Replace the current branch with this other path and try again.

12. Try the `~or` alternative.
    > `{b} + {a}`
13. Does it match `1 + 3`?
    > Yes.
14-15. Steps 12-13 from the above scenario.

16. Try the second `~and` branch.
    > `~fail(b.value != 2, "not two")`
17. Does it match for `a` <-> `3` and `b` <-> `1`?
    > No.
18. Do we have a state we can recover to?
    > No!
19. => Return a failure with the appropriate message ("not two").

20. Done.
"""
function _syntax_match(pattern::SyntaxPatternNode,
                       src::JS.SyntaxNode,
                       bindings::BindingSet=BindingSet();
                       recovery_stack=[],
                       recover=true,
                       greedy=true,
                       tmp=false)::MatchResult
    # Special syntax.
    if is_pattern_form(pattern)
        match_result = syntax_match_pattern_form(pattern,
                                                 src,
                                                 bindings;
                                                 recovery_stack,
                                                 recover,
                                                 greedy,
                                                 tmp)
        isa(match_result, MatchFail) &&
            # Try another path if possible.
            return recover!(recovery_stack,
                            _syntax_match,
                            recover;
                            fail_ret=match_result,
                            tmp)
        # If there is a match, return it.
        return match_result
    end
    # Regular syntax.
    if head(pattern) != head(src)           ||
        pattern.data.val != src.data.val    ||
        xor(is_leaf(pattern), is_leaf(src))
        return recover!(recovery_stack,
                        _syntax_match,
                        recover;
                        fail_ret=MatchFail(),
                        greedy,
                        tmp)
    end
    # Recurse on children if there are any.
    is_leaf(src) && return bindings
    return _syntax_match(children(pattern),
                         children(src),
                         bindings;
                         recovery_stack,
                         recover,
                         tmp)
end
function _syntax_match(pattern_nodes::Vector{SyntaxPatternNode},
                       srcs::Vector{JS.SyntaxNode},
                       bindings::BindingSet=BindingSet();
                       recovery_stack=[],
                       recover=true,
                       greedy=true,
                       tmp=false)::MatchResult
    match_result, srcs = partial_syntax_match(pattern_nodes,
                                              srcs,
                                              bindings;
                                              greedy,
                                              tmp)
    isa(match_result, MatchFail) && return recover!(recovery_stack,
                                                    _syntax_match,
                                                    recover;
                                                    fail_ret=match_result,
                                                    greedy,
                                                    tmp)
    isempty(srcs) || return recover!(recovery_stack,
                                     _syntax_match,
                                     recover;
                                     fail_ret=MatchFail(),
                                     tmp)
    return match_result
end

"""
    partial_syntax_match(pattern_nodes::Vector{SyntaxPatternNode},
                         srcs::Vector{JS.SyntaxNode},
                         bindings::BindingSet=BindingSet();
                         recovery_stack=[],
                         recover=true,
                         greedy=true,
                         tmp=false)::Tuple{MatchResult, Vector{JS.SyntaxNode}}

Try to match a sequence of pattern nodes with a sequence of source nodes exactly once. If
the sequences can't match, return a tuple containing [`MatchFail`](@ref) with the
appropriate message and the original source nodes. If there is a match, return the bindings
and the remaining unmatched source nodes.

In case of `~rep` nodes, greedily "consume" all matching source nodes. If the remaining
patterns can't match the remaining source nodes, backtrack to find a matching state.

The algorithm can be made non-greedy by setting `greedy` to `false`.

# Examples
# ========

```
julia> using JuliaSyntax: children

julia> srcs = [parsestmt(SyntaxNode, "a + 1"), parsestmt(SyntaxNode, "b + 1"), parsestmt(SyntaxNode, "c")];

julia> partial_result, srcs = partial_syntax_match(children(@pattern ({x} + 1)...), srcs)
(BindingSet(:x => Binding(:x, a @ 1:1, BindingSet())), SyntaxNode[(call-i b + 1), c])

julia> partial_result, srcs = partial_syntax_match(children(@pattern ({x} + 1)...), srcs)
(BindingSet(:x => Binding(:x, b @ 1:1, BindingSet())), SyntaxNode[c])

julia> partial_result, srcs = partial_syntax_match(children(@pattern ({x} + 1)...), srcs)
(MatchFail("no match"), SyntaxNode[c])

julia> srcs = [parsestmt(SyntaxNode, "a + 1"), parsestmt(SyntaxNode, "b + 1"), parsestmt(SyntaxNode, "c")];

julia> partial_result, srcs = partial_syntax_match(children(@pattern (({x} + 1)...)...), srcs)
(BindingSet(:x => Binding(:x, [a @ 1:1, b @ 1:1], BindingSet[BindingSet(), BindingSet()])), SyntaxNode[c])

julia> partial_result, srcs = partial_syntax_match(children(@pattern (({x} + 1)...)...), srcs)
(BindingSet(), SyntaxNode[c])
```
"""
function partial_syntax_match(pattern_nodes::Vector{SyntaxPatternNode},
                              srcs::Vector{JS.SyntaxNode},
                              bindings::BindingSet=BindingSet();
                              recovery_stack=[],
                              recover=true,
                              greedy=true,
                              tmp=false)::Tuple{MatchResult, Vector{JS.SyntaxNode}}
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
        isempty(pattern_nodes) && return (bindings, srcs)                          # 1.a,2.a
        # Try the first pattern. There can only be a match if it's a repetition.
        p = pattern_nodes[1]
        if is_rep(p)
            match_result = syntax_match_rep(p, srcs, bindings)
            # This could never fail, but it's good to be exhaustive.
            isa(match_result, MatchFail) && return (match_result, srcs)
            return partial_syntax_match(rest(pattern_nodes),                       # 1.a,
                                        srcs,                                      # 1.b,
                                        match_result;                              # 2.a,
                                        recovery_stack,                            # 2.b
                                        recover,
                                        greedy,
                                        tmp)
        end
        # The first pattern node is not a repetition, so there can be no match on this path.
        return recover!(recovery_stack,                      # `recovery_stack` empty => 1.b
                        partial_syntax_match,                # else                   => 2.b
                        recover;
                        fail_ret=(MatchFail(), srcs),
                        greedy,
                        tmp)
    end
    # If we're here, there still are unmatched source nodes.
    #
    # No pattern nodes left marks the end of the partial match.
    isempty(pattern_nodes) && return (bindings, srcs)
    # Here we know we have at least one pattern node and at least one source node.
    p = pattern_nodes[1]
    s = srcs[1]
    match_result = _syntax_match(p, s, bindings; recover, greedy, tmp)
    if !is_rep(p)
        # The first pattern node is not a repetition so it needs to match the first source
        # node exactly.
        if isa(match_result, MatchFail)
            # If the two don't match, we might be able to try a previous state. If there is
            # no other state to return to, the overall match fails.
            return recover!(recovery_stack,
                            partial_syntax_match,
                            recover;
                            fail_ret=(match_result, srcs),
                            greedy,
                            tmp)
        end
        # If the pattern and source nodes match, we can continue matching the remainders of
        # the node lists.
        return partial_syntax_match(rest(pattern_nodes),
                                    rest(srcs),
                                    match_result;
                                    recovery_stack,
                                    recover,
                                    greedy,
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
        return partial_syntax_match(rest(pattern_nodes),
                                    srcs,
                                    bindings;
                                    recovery_stack,
                                    recover,
                                    greedy,
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
    if greedy
        # Greedy, like me trying to do two months' worth of work in two weeks:
        #
        # In the recovery state, the repetition is finished and the repetition node did not
        # match the current source node.
        push!(recovery_stack, (rest(pattern_nodes), srcs, make_permanent(bindings)))
        # 2. Continue on the preferred path. This path is greedy, meaning that a repetition
        #    node consumes as many source nodes as possible.
        return partial_syntax_match(pattern_nodes,
                                    rest(srcs),
                                    match_result;
                                    recovery_stack,
                                    recover,
                                    greedy,
                                    tmp)
    else
        # Not greedy:
        #
        # The opposite of the greedy algorithm. The recovery state continues the current
        # repetition.
        push!(recovery_stack, (pattern_nodes, rest(srcs), match_result))
        # 2. Continue on the preferred path. This path is not greedy, meaning that a
        #    repetition stops after the first consume source node.
        return partial_syntax_match(rest(pattern_nodes),
                                    srcs,
                                    make_permanent(bindings);
                                    recovery_stack,
                                    recover,
                                    greedy,
                                    tmp)
    end
end

# Pattern form matching
# ---------------------

"""
    syntax_match_pattern_form(pattern_node::SyntaxPatternNode,
                              src::JS.SyntaxNode,
                              bindings::BindingSet;
                              recovery_stack=[],
                              recover=true,
                              tmp=false)::MatchResult

Try to match a pattern form node. Dispatch the matching to the specific pattern form
match function.
"""
function syntax_match_pattern_form(pattern_node::SyntaxPatternNode,
                                   src::JS.SyntaxNode,
                                   bindings::BindingSet;
                                   recovery_stack=[],
                                   recover=true,
                                   greedy=true,
                                   tmp=false)::MatchResult
    args = (pattern_node, src, bindings)
    kwargs = (recovery_stack=recovery_stack, recover=recover, tmp=tmp)
    node_data = pattern_node.data
    # Dispatch on form type.
    isa(node_data, FailSyntaxData) && return syntax_match_fail(args...)
    isa(node_data, VarSyntaxData)  && return syntax_match_var(args...; tmp)
    isa(node_data, OrSyntaxData)   && return syntax_match_or(args...; kwargs...)
    isa(node_data, AndSyntaxData)  && return syntax_match_and(args...; tmp)
    isa(node_data, RepSyntaxData)  && return syntax_match_rep(args...)
    return MatchFail("unknown pattern form")
end

"""
    syntax_match_var(var_node::SyntaxPatternNode,
                     src::JS.SyntaxNode,
                     bindings::BindingSet;
                     tmp=false)::MatchResult

Try to match a `~var` pattern form. If there's a match, bind the pattern variable to `src`
and add the binding to `bindings`.
"""
function syntax_match_var(var_node::SyntaxPatternNode,
                          src::JS.SyntaxNode,
                          bindings::BindingSet;
                          tmp=false)::MatchResult
    pattern_var_name = var_node.data.var_name
    syntax_class_name = var_node.data.syntax_class_name
    syntax_class =
        try
            SYNTAX_CLASS_REGISTRY[syntax_class_name]
        catch err
            if isa(err, KeyError)
                throw(SyntaxClassRegistryKeyError(syntax_class_name))
            else
                rethrow(err)
            end
        end
    # Try to match the pattern syntax class to the AST.
    match_result = syntax_match(syntax_class, src)
    isa(match_result, MatchFail) && return match_result
    # If there's a match and the pattern variable is not anonymous or if `tmp`
    # is `true`, bind the pattern variable and add it to the `BindingSet`.
    is_anonymous_pattern_variable(pattern_var_name) && !tmp &&
        return bindings
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
    if tmp
        bindings[pattern_var_name] =
            TemporaryBinding(pattern_var_name, src, match_result, 0)
    else
        bindings[pattern_var_name] = Binding(pattern_var_name, src, match_result, 0)
    end
    return bindings
end

"""
    syntax_match_fail(fail_node::SyntaxPatternNode,
                      src::JS.SyntaxNode,
                      bindings::BindingSet)::MatchResult

Try to match a `~fail` pattern form. If the fail condition is satisfied return a
[`MatchFail`](@ref) with an informative fail message. Otherwise, return `bindings`.
"""
function syntax_match_fail(fail_node::SyntaxPatternNode,
                           src::JS.SyntaxNode,
                           bindings::BindingSet)::MatchResult
    condition = get_fail_condition(fail_node)
    message = get_fail_message(fail_node)
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
    syntax_match_or(or_node::SyntaxPatternNode,
                    src::JS.SyntaxNode,
                    bindings=BindingSet;
                    recovery_stack=[],
                    recover=true,
                    tmp=false)::MatchResult

Try to match an `~or` pattern form. Return the bindings for the first successful matching
alternative. Return a [`MatchFail`](@ref) if no alternative matches.

If an alternative fails to match, try to backtrack up to a successful state. Store all
possible matching paths in `recovery_stack`.
"""
function syntax_match_or(or_node::SyntaxPatternNode,
                         src::JS.SyntaxNode,
                         bindings=BindingSet;
                         recovery_stack=[],
                         recover=true,
                         tmp=false)::MatchResult
    alt_bindings::BindingSet = deepcopy(bindings)
    failure = MatchFail("no matching alternative")
    for (i, p) in enumerate(children(or_node))
        match_result = _syntax_match(p, src, alt_bindings; recovery_stack, recover, tmp)
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
        # Reset the bindings for each alernative. The alternatives should not communicate
        # with each other.
        alt_bindings = deepcopy(bindings)
        # Track the failures.
        failure = match_result
    end
    # TODO: Return the most specific error.
    return failure
end

"""
    syntax_match_and(and_node::SyntaxPatternNode,
                     src::JS.SyntaxNode,
                     bindings::BindingSet;
                     tmp=false)

Try to match an `~and` pattern form. Return the bindings resulted from all branches, or
the `MatchFail` of the first failing branch.
"""
function syntax_match_and(and_node::SyntaxPatternNode,
                          src::JS.SyntaxNode,
                          bindings::BindingSet;
                          tmp=false)
    and_node = deepcopy(and_node)
    bindings::BindingSet = deepcopy(bindings)
    recovery_stack = []
    for (i, p) in enumerate(children(and_node))
        # Try to match the `~and` branch. Don't recover internally from failures, treat all
        # branch failures inside the `~and`.
        match_result = _syntax_match(p, src, bindings; recovery_stack, recover=false, tmp)
        if isa(match_result, MatchFail)
            # If we can, try another path until we get a match.
            while !isempty(recovery_stack) && isa(match_result, MatchFail)
                new_p, src, bindings = pop!(recovery_stack)
                and_node.children[i - 1] = new_p
                match_result = syntax_match_and(and_node, src, bindings; tmp)
            end
            # If we have no more paths to try and the match still failed return the failure.
            isa(match_result, MatchFail) && return match_result
        end
        # Here we know the match was a success so we can continue.
        bindings = match_result
    end
    return bindings
end

"""
    syntax_match_rep(rep_node::SyntaxPatternNode,
                     src::JS.SyntaxNode,
                     bindings::BindingSet)::MatchResult
    syntax_match_rep(rep_node::SyntaxPatternNode,
                     src::Vector{JS.SyntaxNode},
                     bindings::BindingSet)::MatchResult

Try to match a `~rep` pattern form (ellipsis). An ellipsis of depth 1 matches a sequence of
nodes. An ellipsis of depth 2 matches a sequence of sequences of nodes. Ellipses can have
any depth.

# Examples
# ========

```
julia> match_result = Argus.syntax_match_rep(SyntaxPatternNode(:( {x}... )),
                                             parsestmt(SyntaxNode, "[a, b, c]"),
                                             BindingSet())
BindingSet with 1 entry:
  :x => Argus.TemporaryBinding{Vector{SyntaxNode}, Vector{BindingSet}}:
          Name: :x
          Bound sources: [(vect a b c) @ 1:1]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet with 0 entries
            ]

julia> match_result = Argus.syntax_match_rep(SyntaxPatternNode(:( ({x}...)... )),
                                             parsestmt(SyntaxNode, "[a, b, c]"),
                                             BindingSet())
BindingSet with 1 entry:
  :x => Argus.TemporaryBinding{Vector{Vector{SyntaxNode}}, Vector{Vector{BindingSet}}}:
          Name: :x
          Bound sources: [[(vect a b c) @ 1:1]]
          Ellipsis depth: 2
          Sub-bindings:
            [
             [
              BindingSet with 0 entries
             ]
            ]

julia> match_result = Argus.syntax_match_rep(SyntaxPatternNode(:( {x}... )),
                                             parseall(SyntaxNode, \"""
                                                                  a
                                                                  b
                                                                  c
                                                                  \"""),
                                             BindingSet())
BindingSet with 1 entry:
  :x => Binding:
          Name: :x
          Bound sources: [a @ 1:1, b @ 2:1, c @ 3:1]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet with 0 entries,
             BindingSet with 0 entries,
             BindingSet with 0 entries
            ]
```
"""
function syntax_match_rep(rep_node::SyntaxPatternNode,
                          src::JS.SyntaxNode,
                          bindings::BindingSet)::MatchResult
    kind(src) === K"toplevel" && return syntax_match_rep(rep_node, children(src), bindings)

    match_result = _syntax_match(rep_node.children[1], src, BindingSet(); tmp=true)
    isa(match_result, MatchFail) && return match_result

    bindings::BindingSet = deepcopy(bindings)
    for var in get_rep_vars(rep_node)
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
                          srcs::Vector{JS.SyntaxNode},
                          bindings::BindingSet)::MatchResult
    bindings::BindingSet = deepcopy(bindings)
    if isempty(srcs)
        # If there are no source nodes, the repetition pattern variables bind to empty
        # vectors.
        for var in get_rep_vars(rep_node)
            var_name = var.name
            var_depth = var.ellipsis_depth
            if !haskey(bindings, var_name)
                bindings[var_name] =
                    TemporaryBinding(var_name,
                                     empty_vec(JS.SyntaxNode, var_depth),
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
            partial_syntax_match(children(rep_node), srcs, BindingSet(); tmp=true)
        isa(partial_result, MatchFail) && return partial_result
        push!(partial_results, partial_result)
    end
    match_result = make_permanent(partial_results)
    return merge(bindings, match_result)
end

# Utils
# -----

is_anonymous_pattern_variable(ex) = ex === :_
is_exported_pattern_variable(ex) = !startswith(string(ex), "_")

"""
    recover!(recovery_stack::AbstractVector,
             from::Function,
             recover::Bool;
             fail_ret,
             kwargs...)

Try to recover from a match failure by calling `from` with the last possible recovery state.
If there are no recovery states return `fail_ret`.
"""
recover!(recovery_stack::AbstractVector,
         from::Function,
         recover::Bool;
         fail_ret,
         kwargs...) =
    isempty(recovery_stack) || !recover ?
    fail_ret                            :
    from(pop!(recovery_stack)...; recovery_stack, kwargs...)

"""
    compatible(ex1::JS.SyntaxNode, ex2::JS.SyntaxNode)

Determine whether two `JuliaSyntax.SyntaxNode`s are compatible with each other. Compatible
nodes have the same head, value and number of children and all their children are `==` to
each other one by one.
"""
function compatible(ex1::JS.SyntaxNode, ex2::JS.SyntaxNode)
    head(ex1) == head(ex2) || return false
    ex1.data.val == ex2.data.val || return false
    xor(is_leaf(ex1), is_leaf(ex2)) && return false
    is_leaf(ex1) && return true
    return all(map(p -> p[1] == p[2], zip(children(ex1), children(ex2))))
end

"""
    rest(v::Vector{JuliaSyntax.TreeNode{T}})

Return the remaning vector if the first element is removed from `v`. If `v` is empty, return
an empty vector of the same type.
"""
(rest(v::Vector{JS.TreeNode{T}})::Vector{JS.TreeNode{T}}) where T =
    isempty(v) || length(v) == 1 ? [] : v[2:end]

"""
    remove_invalid_bindings(bs::BindingSet)

Filter out all elements of type [`InvalidBinding`](@ref) from a [`BindingSet`](@ref).
"""
remove_invalid_bindings(bs::BindingSet)::BindingSet =
    filter(p -> !isa(p.second, InvalidBinding), bs)

"""
    make_permanent(bs::BindingSet)

Turn all `TemporaryBinding`s into `Binding`s.

# Examples
# ========

```
julia> srcs = [parsestmt(SyntaxNode, "1 + a"), parsestmt(SyntaxNode, "1 + 2"), parsestmt(SyntaxNode, "c + 1")];

julia> partial_result, srcs = partial_syntax_match(children(@pattern begin (1 + {x})... end),
                                                   srcs;
                                                   tmp=true)
(BindingSet(:x => Argus.TemporaryBinding{SyntaxNode, BindingSet}(:x, a @ 1:5, BindingSet())), SyntaxNode[(call-i 1 + 2), (call-i c + 1)])

julia> Argus.make_permanent(partial_result)
BindingSet with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: a @ 1:5
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet with 0 entries
```
"""
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
    make_permanent(bss::Vector{BindingSet})

Merge all the binding sets corresponding to a temporarily bound pattern variable into a
single [`Binding`](@ref) and increase ellipsis depth accordingly.

Should only be called at the end of a repetition match.

# Examples
# ========

```
julia> pattern = @pattern begin
           ({f}({args}...) = {_})...
       end;

julia> src =
       \"""
       f() = begin 2 end
       g(x) = begin x + 1 end
       h(a; b::Int=2) = begin a - b end
       \""";

julia> partial_result1, srcs1 = partial_syntax_match(children(pattern), children(parseall(SyntaxNode, src)); tmp=true);

julia> partial_result1.bindings
Dict{Symbol, Argus.AbstractBinding} with 3 entries:
OrderedCollections.OrderedDict{Symbol, Argus.AbstractBinding} with 3 entries:
  :f    => TemporaryBinding{SyntaxNode, BindingSet}(:f, f @ 1:1, BindingSet())
  :args => TemporaryBinding{Vector{SyntaxNode}, Vector{BindingSet}}(:args, [], BindingSet[])
  :_    => TemporaryBinding{SyntaxNode, BindingSet}(:_, 2 @ 1:13, BindingSet())

julia> srcs1
2-element Vector{SyntaxNode}:
 (function-= (call g x) (block (call-i x + 1)))
 (function-= (call h a (parameters (= (::-i b Int) 2))) (block (call-i a - b)))

julia> partial_result2, srcs2 = partial_syntax_match(children(pattern), srcs1; tmp=true);

julia> partial_result2.bindings
OrderedCollections.OrderedDict{Symbol, Argus.AbstractBinding} with 3 entries:
  :f    => TemporaryBinding{SyntaxNode, BindingSet}(:f, g @ 2:1, BindingSet())
  :args => TemporaryBinding{Vector{SyntaxNode}, Vector{BindingSet}}(:args, [x @ 2:3], BindingSet[BindingSet()])
  :_    => TemporaryBinding{SyntaxNode, BindingSet}(:_, (call-i x + 1) @ 2:14, BindingSet())

julia> srcs2
1-element Vector{SyntaxNode}:
 (function-= (call h a (parameters (= (::-i b Int) 2))) (block (call-i a - b)))

julia> partial_result3, srcs3 = partial_syntax_match(children(pattern), srcs2; tmp=true);

julia> partial_result3.bindings
OrderedCollections.OrderedDict{Symbol, Argus.AbstractBinding} with 3 entries:
  :f    => TemporaryBinding{SyntaxNode, BindingSet}(:f, h @ 3:1, BindingSet())
  :args => TemporaryBinding{Vector{SyntaxNode}, Vector{BindingSet}}(:args, [a @ 3:3, (parameters (= (::-i b Int) 2)) @ 3:4], BindingSet[BindingSet(), BindingSet()])
  :_    => TemporaryBinding{SyntaxNode, BindingSet}(:_, (call-i a - b) @ 3:24, BindingSet())

julia> srcs3
SyntaxNode[]

julia> Argus.make_permanent(BindingSet[partial_result1, partial_result2, partial_result3])
BindingSet with 2 entries:
  :f => Binding:
          Name: :f
          Bound sources: [f @ 1:1, g @ 2:1, h @ 3:1]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet with 0 entries,
             BindingSet with 0 entries,
             BindingSet with 0 entries
            ]
  :args => Binding:
             Name: :args
             Bound sources: [[], [x @ 2:3], [a @ 3:3, (parameters (= (::-i b Int) 2)) @ 3:4]]
             Ellipsis depth: 2
             Sub-bindings:
               [
                [],
                [
                 BindingSet with 0 entries
                ],
                [
                 BindingSet with 0 entries,
                 BindingSet with 0 entries
                ]
               ]
```
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

"""
    empty_vec(type, depth::Int)

Create an empty vector of a given depth, eventually holding elements of `type`.

# Examples
# ========

```
julia> Argus.empty_vec(Int, 4)
Vector{Vector{Vector{Int64}}}[]

julia> Argus.empty_vec(SyntaxPatternNode, 1)
SyntaxPatternNode[]
```
"""
empty_vec(type, depth::Int) = depth == 0      ?
    error("Can't create vector with 0 depth") :
    vec_type(type, depth)()
vec_type(type, depth::Int) = depth == 1 ? Vector{type} : Vector{vec_type(type, depth - 1)}
