## -----------------------------------------------------------------------------------------
## `SyntaxMatch` and `SyntaxMatches`.

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

## `JuliaSyntax` overwrites.

JuliaSyntax.source_location(m::SyntaxMatch) = source_location(m.ast)

## `Base` overwrites.

function Base.getproperty(m::SyntaxMatch, name::Symbol)
    name === :placeholders && return getfield(m, :placeholders)
    ast = getfield(m, :ast)
    name === :ast && return ast
    return getproperty(ast, name)
end

## -------------------------------------------

"""
    SyntaxMatches = Vector{SyntaxMatch}

Vector of `SyntaxMatch`es.
"""
const SyntaxMatches = Vector{SyntaxMatch}

## Display.

Base.show(io::IO, ::Type{SyntaxMatches}) = print(io, "SyntaxMatches")

## -----------------------------------------------------------------------------------------
## Pattern matching.

"""
    pattern_match!(pattern::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)::SyntaxMatches

Try to match the given pattern with a source AST and all its children. When a match is
found bind the placeholders in the pattern, if any. Return an array of `SyntaxMatch`es.
"""
function pattern_match!(pat::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)::SyntaxMatches
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

## -------------------------------------------
## Pattern comparison.

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
    xor(!is_leaf(pattern), !is_leaf(src)) && return false
    # Recurse on children if there are any.
    is_leaf(src) && return true
    length(children(pattern)) != length(children(src)) && return false
    zipped_children = zip(children(pattern), children(src))
    # TODO: This doesn't seem finished.
    return all(p -> pattern_compare!(p[1], p[2]), zipped_children)
end
