## -----------------------------------------------------------------------------------------
## Syntax matching structs.

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
