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
BindingSet() = BindingSet(Dict{Symbol, AbstractBinding}())
BindingSet(kvs...) = BindingSet(Dict{Symbol, AbstractBinding}(kvs...))

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
# Errors.

struct BindingFieldError <: Exception
    binding::AbstractBinding
    field::Symbol
    available_fields::Vector{Symbol}
    reason::String
end

struct BindingSetKeyError <: Exception
    key
end

## Display.

function Base.showerror(io::IO, err::BindingFieldError)
    print(io, "BindingFieldError: ")
    println(io,
            "binding `", err.binding.bname, "` has no field `", err.field, "` ",
            "because ", err.reason, ".")
    println(io, "Available fields: ", join(map(s -> "`$s`", err.available_fields), ", "))
end

function Base.showerror(io::IO, err::BindingSetKeyError)
    print(io, "BindingSetKeyError: ")
    println(io, "binding ", err.key, " not found")
end

# --------------------------------------------
# Binding.

"""
Binding of a pattern variable to a syntax tree. Pattern variables are created with the
`~var` pattern form.
"""
struct Binding{S, B} <: AbstractBinding
    bname::Symbol
    src::S
    bindings::B
    ellipsis_depth::Int
end

"""
Internal binding type used for validating bindings corresponding to multiple appearances
of the same pattern variable.
"""
struct InvalidBinding <: AbstractBinding
    msg::String
end

"""
Internal binding type used for storing bindings for unfinished repetitions.
"""
struct TemporaryBinding{S, B} <: AbstractBinding
    bname::Symbol
    src::S
    bindings::B
    ellipsis_depth::Int
end
Binding(b::TemporaryBinding) = Binding(b.bname, b.src, b.bindings, b.ellipsis_depth)

## `Base` overwrites.

# Allow accessing sub-bindings as fields.
function Base.getproperty(b::Binding, name::Symbol)
    name === :bname && return getfield(b, :bname)
    src = getfield(b, :src)
    name === :src && return src
    bindings = getfield(b, :bindings)
    name === :bindings && return bindings
    name === :ellipsis_depth && return getfield(b, :ellipsis_depth)
    # Gather available fields to show in error messages.
    available_fields = [:bname, :src, :bindings]
    [push!(available_fields, subb) for subb in keys(bindings)]
    JuliaSyntax.is_identifier(src) && push!(available_fields, :name)
    JuliaSyntax.is_literal(src) && push!(available_fields, :value)
    # Check for node-specific field access.
    if name === :value
        JuliaSyntax.is_literal(src) && return src.val
        # Only literals have a `value` field.
        throw(BindingFieldError(b,
                                :value,
                                available_fields,
                                "the bound expression is not a literal"))
    end
    if name === :name
        JuliaSyntax.is_identifier(src) && return string(src.val)
        # Only identifiers have a `name` field.
        throw(BindingFieldError(b,
                                :name,
                                available_fields,
                                "the bound expression is not an identifier"))
    end
    return get(bindings, name) do
        throw(BindingFieldError(b,
                                name,
                                available_fields,
                                "`$name` is not a sub-binding of `$(b.bname)`"))
    end
end

## Display.

Base.show(io::IO, ::Type{BindingSet{Binding}}) = print(io, "BindingSet")
