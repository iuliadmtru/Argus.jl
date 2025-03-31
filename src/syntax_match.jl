"""
    SyntaxMatch

Type containing the matched source (sub-)AST and, if the template used for matching had
any special syntax, the bound placeholders.
"""
struct SyntaxMatch
    ast::JuliaSyntax.SyntaxNode
    placeholders::Union{Nothing, Vector{AbstractSyntaxPlaceholder}}
end
# SyntaxMatch(src::JuliaSyntax.SyntaxNode) = SyntaxMatch(src, nothing)

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
    SyntaxMatches <: AbstractVector{SyntaxMatch}

Vector of `SyntaxMatch`es.
"""
struct SyntaxMatches <: AbstractVector{SyntaxMatch}
    matches::AbstractVector{SyntaxMatch}
end
SyntaxMatches() = SyntaxMatches(SyntaxMatch[])

## `Base` overwrites.

Base.size(v::SyntaxMatches) = size(v.matches)
Base.getindex(v::SyntaxMatches, i::Int) = v.matches[i]
Base.getindex(v::SyntaxMatches, r::UnitRange) = SyntaxMatches(view(v.matches, r))
Base.setindex!(v::SyntaxMatches, el::SyntaxMatch, i::Int) =
    SyntaxMatches(setindex!(v.matches, el, i))
Base.push!(v::SyntaxMatches, el::SyntaxMatch) = SyntaxMatches(push!(v.matches, el))
Base.pushfirst!(v::SyntaxMatches, el::SyntaxMatch) = SyntaxMatches(pushfirst!(v.matches, el))
