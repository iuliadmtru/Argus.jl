"""
    AbstractBinding

Sypertype for [`Binding`](@ref).
"""
abstract type AbstractBinding end

# --------------------------------------------
# Binding set.

"""
Set of bindings. Implemented as a dict for ease of access.
"""
struct BindingSet{T <: AbstractBinding} <: AbstractDict{Symbol, T}
    bindings::Dict{Symbol, T}
end

BindingSet() = BindingSet(Dict{Symbol, Binding}())

# `Base` overwrites.

function Base.union!(bs1::BindingSet, bs2::BindingSet)
    # TODO.
    return bs1
end

# --------------------------------------------
# Binding.

"""
Binding of a pattern variable to a syntax tree. Pattern variables are created with the
`~var` pattern form.
"""
struct Binding <: AbstractBinding
    name::Symbol
    ast::JuliaSyntax.SyntaxNode
    bindings::BindingSet
end
