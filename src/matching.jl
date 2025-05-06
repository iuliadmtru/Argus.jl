# ------------------------------------------------------------------------------------------
# `SyntaxMatch` and `SyntaxMatches`.

"""
    SyntaxMatch

Type containing the matched source (sub-)AST and, if the pattern used for matching had
any special syntax, the bound placeholders.
"""
struct SyntaxMatch
    ast::JuliaSyntax.SyntaxNode
    placeholders::Union{Nothing, Vector{AbstractSyntaxPlaceholder}}
end
SyntaxMatch(src::JuliaSyntax.SyntaxNode) = SyntaxMatch(src, nothing)

# `JuliaSyntax` overwrites.

JuliaSyntax.source_location(m::SyntaxMatch) = source_location(m.ast)

# `Base` overwrites.

function Base.getproperty(m::SyntaxMatch, name::Symbol)
    name === :placeholders && return getfield(m, :placeholders)
    ast = getfield(m, :ast)
    name === :ast && return ast
    return getproperty(ast, name)
end

# --------------------------------------------

"""
    SyntaxMatches = Vector{SyntaxMatch}

Vector of `SyntaxMatch`es.
"""
const SyntaxMatches = Vector{SyntaxMatch}

# Display.

Base.show(io::IO, ::Type{SyntaxMatches}) = print(io, "SyntaxMatches")

# ------------------------------------------------------------------------------------------
# Rule and pattern matching.

"""
    rule_match!(rule::Rule, src::JuliaSyntax.SyntaxNode)::SyntaxMatches

Try to match the given rule's pattern with a source AST and all its children.
See `pattern_match!`.
"""
rule_match!(rule::Rule, src::JuliaSyntax.SyntaxNode)::SyntaxMatches =
    pattern_match!(rule.pattern, src)
"""
    rule_match!(rule::Rule, src_file::AbstractString)::SyntaxMatches

Try to match the given rule's pattern with a source file. See `pattern_match!`.
"""
rule_match!(rule::Rule, src_file::AbstractString)::SyntaxMatches =
    pattern_match!(rule.pattern, src_file)

"""
    pattern_match!(pattern::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)::SyntaxMatches

Try to match the given pattern with a source AST and all its children. When a match is
found bind the placeholders in the pattern, if any. Return an array of `SyntaxMatch`es.
"""
function pattern_match!(pat::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)::SyntaxMatches
    # Treat composite patterns specially.
    # TODO: Make these return a tuple
    #       `(matches_for_each_branch::Dict{SyntaxPatternNode, SyntaxMatches},
    #         overall_matches::SyntaxMatches)`
    #       for debugging purposes?
    is_or(pat) && return match_union!(children(pat), src)
    is_and(pat) && return match_intersect!(pat, src)
    # Match regular patterns.
    if pattern_compare!(pat, src)
        matches = SyntaxMatches([SyntaxMatch(src, placeholders(pat))])
        placeholders_unbind!(pat)
        return matches
    end
    if is_leaf(src)
        return SyntaxMatches()
    end
    # Search for matches within children.
    matches = SyntaxMatches()
    for c in children(src)
        append!(matches, pattern_match!(pat, c))
    end
    placeholders_unbind!(pat)

    return matches
end
"""
    pattern_match!(pattern::SyntaxPatternNode, src_file::AbstractString)::SyntaxMatches

Try to match the given pattern with a source file.
"""
function pattern_match!(pattern::SyntaxPatternNode, src_file::AbstractString)::SyntaxMatches
    src_txt = read(src_file, String)
    src = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src_txt; filename=src_file)

    return pattern_match!(pattern, src)
end

# Utils.

function match_union!(patterns::Vector{SyntaxPatternNode}, src::JuliaSyntax.SyntaxNode)
    matches = SyntaxMatches()
    for subpattern in patterns
        append!(matches, pattern_match!(subpattern, src))
        placeholders_unbind!(subpattern)
    end
    # Keep duplicate matches caused by branches overlapping. The information might be
    # useful for debugging purposes.
    # TODO: Return tuple with the matches for each branch and the overall matches?
    return matches
end

function match_intersect!(pattern::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)
    # Filter out conflicting branches.
    # TODO: Is this useful enough to justify the extra complexity?
    patterns = remove_conflicting(children(pattern))
    matches = SyntaxMatches()
    for subpattern in patterns
        subpattern_matches = pattern_match!(subpattern, src)
        placeholders_unbind!(subpattern)
        # Return if there is no match for one subpattern.
        isempty(subpattern_matches) && return subpattern_matches
        append!(matches, subpattern_matches)
    end
    # Filter out conflicting matches.
    # patterns_copy = copy.(patterns)
    # matches = remove_conflicting(matches, placeholders_unbind!.(patterns_copy))
    matches = remove_conflicting(matches, pattern)
    return matches
end

function remove_conflicting(patterns::Vector{SyntaxPatternNode})
    isempty(patterns) && return patterns
    len = length(patterns)
    len == 1 && return patterns
    # Mark all conflicting patterns.
    marked = falses(len)
    for (i, p) in enumerate(patterns)
        # Skip already marked patterns.
        marked[i] && continue
        for j in i+1:len
            marked[j] && continue
            if conflicts(p, patterns[j])
                marked[i] = true
                marked[j] = true
                @warn "Conflicting `and` branches:\n$p\n$(patterns[j])"
            end
        end
    end
    # Remove all marked patterns.
    res = SyntaxPatternNode[]
    for (i, p) in enumerate(patterns)
        marked[i] || push!(res, p)
    end

    return res
end
# TODO: Move this at `SyntaxPatternNode` construction.
function remove_conflicting(matches::SyntaxMatches, pattern::SyntaxPatternNode)
    isempty(matches) && return matches
    len = length(matches)
    len == 1 && return matches
    # Mark all conflicting matches.
    marked = falses(len)
    for (i, m) in enumerate(matches)
        # Check if the matched AST matches all patterns.
        for p in children(pattern)
            # Mark if the comparison is unsuccessful.
            if !pattern_compare!(p, m.ast)
                marked[i] = true
                break
            end
        end
        placeholders_unbind!(pattern)
    end
    # Remove all marked matches.
    res = SyntaxMatches()
    for (i, m) in enumerate(matches)
        marked[i] || push!(res, m)
    end

    return res
end

function conflicts(p1::SyntaxPatternNode, p2::SyntaxPatternNode)
    # If at least one of the patterns is a metavariable, there's no conflict.
    # The metavariable can match any expression.
    # TODO: Metavariable conditions.
    (is_placeholder(p1) || is_placeholder(p2)) && return false
    # No special syntax.
    head(p1) != head(p2) && return true
    p1.data.val != p2.data.val && return true
    xor(is_leaf(p1), is_leaf(p2)) && return true
    # Recurse on children if any.
    is_leaf(p1) && return false
    length(children(p1)) != length(children(p2)) && return true
    zipped_children = zip(children(p1), children(p2))
    return any(p -> conflicts(p[1], p[2]), zipped_children)
end

# --------------------------------------------
# Pattern comparison.

# TODO: Needs more work.
"""
    pattern_compare!(pattern::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)

Compare a given pattern to a source AST. If the pattern contains placeholders, fill them.
The comparison fails if the source doesn't fit the pattern or if the placeholders' bindings
don't agree with each other.

Return `true` if the pattern matches the source AST, `false` otherwise.
"""
function pattern_compare!(pattern::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)
    # The node is a placeholder that needs to be filled.
    # TODO: Take into account repetitions with ellipses.
    is_placeholder(pattern) && return placeholder_fill!(pattern.pattern_data, src)

    # The node itself is not a special node, but it has a successor
    # with some special syntax.
    if contains_placeholders(pattern)
        if head(pattern) != head(src)
            # A `[=]` node in the pattern can match a short form function definition.
            if kind(pattern) === K"=" && kind(src) === K"function"
                src_flags = JuliaSyntax.flags(src)
                src_flags === JuliaSyntax.SHORT_FORM_FUNCTION_FLAG || return false
            else
                return false
            end
        end
        if length(children(pattern)) == length(children(src))
            zipped_children = zip(children(pattern), children(src))
            return all(p -> pattern_compare!(p[1], p[2]), zipped_children)
        else
            # The rule might have ellipses.
            # TODO.
            return false
        end
    end

    # No special syntax.
    head(pattern) != head(src) && return false
    pattern.data.val != src.data.val && return false
    xor(is_leaf(pattern), is_leaf(src)) && return false
    # Recurse on children if there are any.
    is_leaf(src) && return true
    length(children(pattern)) != length(children(src)) && return false
    zipped_children = zip(children(pattern), children(src))
    return all(p -> pattern_compare!(p[1], p[2]), zipped_children)
end
