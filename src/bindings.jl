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
BindingSet(kvs...) = BindingSet(Dict{Symbol, Binding}(kvs...))

## Dict interface.

Base.isempty(bs::BindingSet) = isempty(bs.bindings)
Base.empty!(bs::BindingSet) = empty!(bs.bindings)
Base.length(bs::BindingSet) = length(bs.bindings)

Base.iterate(bs::BindingSet) = iterate(bs.bindings)
Base.iterate(bs::BindingSet, i::Int) = iterate(bs.bindings, i)
Base.setindex!(bs::BindingSet, v, k...) = setindex!(bs.bindings, v, k...)

Base.haskey(bs::BindingSet, k) = haskey(bs.bindings, k)
Base.get(bs::BindingSet, k, d) = get(bs.bindings, k, d)
Base.get(f::Union{Function, Type}, bs::BindingSet, k) = get(f, bs.bindings, k)
Base.get!(bs::BindingSet, k, d) = get!(bs.bindings, k, d)
Base.get!(f::Union{Function, Type}, bs::BindingSet, k) = get!(f, bs.bindings, k)
Base.getkey(bs::BindingSet, k, d) = getkey(bs.bindings, k, d)
Base.delete!(bs::BindingSet, k) = delete!(bs.bindings, k)
Base.pop!(bs::BindingSet, k) = pop!(bs.bindings, k)
Base.pop!(bs::BindingSet, k, d) = pop!(bs.bindings, k, d)
Base.keys(bs::BindingSet) = keys(bs.bindings)
Base.values(bs::BindingSet) = values(bs.bindings)
Base.pairs(bs::BindingSet) = pairs(bs.bindings)
Base.merge(bs::BindingSet, others::BindingSet...) =
    BindingSet(merge(bs.bindings, others...))
Base.mergewith(c, bs::BindingSet, others::BindingSet...) =
    BindingSet(mergewith(c, bs.bindings, others...))
Base.merge!(bs::BindingSet, others::BindingSet...) =
    BindingSet(merge!(bs.bindings, others...))
Base.mergewith!(c, bs::BindingSet, others::BindingSet...) =
    BindingSet(mergewith!(c, bs.bindings, others...))
Base.keytype(bs::BindingSet) = keytype(bs.bindings)
Base.valtype(bs::BindingSet) = valtype(bs.bindings)

# --------------------------------------------
# Binding.

"""
Binding of a pattern variable to a syntax tree. Pattern variables are created with the
`~var` pattern form.
"""
struct Binding <: AbstractBinding
    bname::Symbol
    ast::JuliaSyntax.SyntaxNode
    bindings::BindingSet
end

## `Base` overwrites.

# Allow accessing sub-bindings as fields.
function Base.getproperty(b::Binding, name::Symbol)
    name === :bname && return getfield(b, :bname)
    ast = getfield(b, :ast)
    name === :ast && return ast
    bindings = getfield(b, :bindings)
    name === :bindings && return bindings
    if name === :value
        JuliaSyntax.is_literal(ast) && return ast.val
        error("binding `$(b.bname)` has no field `value` because ",
              "the bound expression is not a literal")
    end
    if name === :name
        JuliaSyntax.is_identifier(ast) && return string(ast.val)
        error("binding `$(b.bname)` has no field `name` because ",
              "the bound expression is not an identifier")
    end
    return get(bindings, name) do
        sub_bindings = isempty(bindings) ?
            "none"                       :
            join(map(string, collect(keys(bindings))), ", ")
        error("binding `$(b.bname)` has no sub-binding `$name`; ",
              "available sub-bindings: $sub_bindings")
    end
end

## Display.

Base.show(io::IO, ::Type{BindingSet{Binding}}) = print(io, "BindingSet")
