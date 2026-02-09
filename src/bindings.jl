"""
    AbstractBinding

Sypertype for bindings.
"""
abstract type AbstractBinding end

# Binding set
# ===========

"""
    BindingSet{T <: AbstractBinding} <: AbstractDict{Symbol, T}

Set of bindings resulted from a successful syntax match. Contains only [`Binding`](@ref)
values if resulted from a successful and complete syntax match. May contain
[`TemporaryBinding`](@ref) and/or [`InvalidBinding`](@ref) if resulted from a successful
but incomplete syntax match.
"""
mutable struct BindingSet{T <: AbstractBinding} <: AbstractDict{Symbol, T}
    bindings::Dict{Symbol, T}  # TODO: Order by appearance in the source code?
    source_location::Tuple{Int64, Int64}
    file_name::String
end
BindingSet() = BindingSet(Dict{Symbol, AbstractBinding}(), (0, 0), "")
BindingSet(kvs...) = BindingSet(Dict{Symbol, AbstractBinding}(kvs...), (0, 0), "")

# Dict interface
# --------------

Base.isempty(bs::BindingSet) = isempty(bs.bindings)
Base.empty(bs::BindingSet) = BindingSet(empty(bs.bindings), bs.source_location, bs.file_name)
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

function Base.copy(bs::BindingSet)
    new_bs = empty(bs)
    for (k, v) in bs
        new_bs[k] = copy(v)
    end
    return new_bs
end

# Display
# -------

function Base.summary(io::IO, bs::BindingSet)
    show(io, typeof(bs))
    print(io, " @ $(_repr_location(bs)) with $(length(bs)) entries")
end

Base.show(io::IO, ::Type{BindingSet{AbstractBinding}}) = print(io, "BindingSet")
Base.show(io::IO, ::MIME"text/plain", bs::BindingSet) =
    _show_binding_set(io, bs, "")

function _show_binding_set(io::IO, bs, indent)
    if isa(bs, AbstractVector)
        print(io, indent * "[")
        if isempty(bs)
            print(io, "]")
        else
            for (i, el) in enumerate(bs)
                println(io)
                _show_binding_set(io, el, indent * " ")
                if i < length(bs)
                    print(io, ",")
                end
            end
            println(io)
            print(io, indent * "]")
        end
    else
        print(io, indent)
        summary(io, bs)
        if !isempty(bs)
            print(io, ":")
            for (k, v) in bs
                println(io)
                s = indent * "  $(repr(k)) => "
                print(io, s)
                b_indent = repeat(' ', length(s))
                _show_binding(io, v, b_indent)
            end
        end
    end
end

function _repr_location(bs::BindingSet)
    file_name = isempty(bs.file_name) ? "" : bs.file_name * ":"
    location = string(file_name, bs.source_location[1], ":", bs.source_location[2])

    return location
end

# Errors
# ======

"""
    BindingFieldError <: Exception

A fail condition tried to access an invalid field of a binding.

A binding field is valid if any one of the following is true:
  - It is one of the corresponding struct fields (e.g. `src` for a [`Binding`](@ref) or
    `msg` for an [`InvalidBinding`](@ref));
  - It is the name of one of the binding's sub-bindings;
  - The binding's `src` is an identifier and the field is `name`;
  - The binding's `src` is a literal and the field is `value`.
"""
struct BindingFieldError <: Exception
    binding::AbstractBinding
    field::Symbol
    available_fields::Vector{Symbol}
    internal_fields::Vector{Symbol}
    reason::String
end

"""
    BindingSetKeyError <: Exception

A fail condition tried to access a non-existent binding.
"""
struct BindingSetKeyError <: Exception
    key
end

# Display
# -------

function Base.showerror(io::IO, err::BindingFieldError)
    print(io, "BindingFieldError: ")
    println(io,
            "binding `", err.binding.bname, "` has no field `", err.field, "` ",
            "because ", err.reason, ".")
    available = isempty(err.available_fields) ?
        "none"                                :
        join(map(s -> "`$s`", err.available_fields), ", ")
    println(io, "Available fields: ", available)
    println(io)
    println(io,
            "The following fields are internal, avoid using them in patterns: ",
            join(map(s -> "`$s`", err.internal_fields), ", "))
end

function Base.showerror(io::IO, err::BindingSetKeyError)
    print(io, "BindingSetKeyError: ")
    println(io, "binding ", err.key, " not found")
end

# Bindings
# ========

"""
    Binding <: AbstractBinding

A bound pattern variable. Variables with ellipsis depth 0 (i.e. variables not contained
within any ellipsis nodes) bind to single nodes. Variables with ellipsis depth `n` bind to
a `Vector` of source nodes of depth `n`.

# Examples

```
julia> match_result = syntax_match((@pattern {x}), parsestmt(SyntaxNode, "[a, b, c]"))
BindingSet with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: (vect a b c) @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet with 0 entries

julia> match_result = syntax_match((@pattern [{x}...]), parsestmt(SyntaxNode, "[a, b, c]"))
BindingSet with 1 entry:
  :x => Binding:
          Name: :x
          Bound sources: [a @ 1:2, b @ 1:5, c @ 1:8]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet with 0 entries,
             BindingSet with 0 entries,
             BindingSet with 0 entries
            ]

julia> match_result = syntax_match((@pattern [({x}...)...]), parsestmt(SyntaxNode, "[a, b, c]"))
BindingSet with 1 entry:
  :x => Binding:
          Name: :x
          Bound sources: [[a @ 1:2], [b @ 1:5], [c @ 1:8]]
          Ellipsis depth: 2
          Sub-bindings:
            [
             [
              BindingSet with 0 entries
             ],
             [
              BindingSet with 0 entries
             ],
             [
              BindingSet with 0 entries
             ]
            ]
```
"""
struct Binding{S, B} <: AbstractBinding
    bname::Symbol
    src::S
    bindings::B
    ellipsis_depth::Int
end

"""
    InvalidBinding <: AbstractBinding

Internal binding type used for invalidating bindings. Multiple appearances of the same
pattern variable that are not consistent with each other are marked as invalid in order to
stop matching them further.

Compatibility is decided through the [`compatible`](@ref) predicate.

# Examples

```
julia> pattern = @pattern begin
           {x:::identifier} = 2
           {x:::identifier} = 3
       end;

julia> syntax_match(pattern, parseall(SyntaxNode,
                                      \"""
                                      a = 2
                                      b = 3
                                      \"""))
MatchFail("conflicting bindings for pattern variable x")
```

The `syntax_match` steps are:

1. Does `{x:::identifier} = 2` match `a = 2`?
   > Yes.
2. Do we have a binding for `x` already?
   > No.
3. => Bind `x` to `a`.

4. Does `{x:::identifier} = 3` match `b = 3`?
   > Yes.
5. Do we have a binding for `x` already?
   > Yes, `a`.
6. Is `a` compatible with `b` (same head, same children, same value)?
   > No.
7. => Replace the binding for `x` with an `InvalidBinding`. Attach the message
   "conflicting bindings for pattern variable x" to it.
8. => Return a `MatchFail` with the same message.
"""
struct InvalidBinding <: AbstractBinding
    msg::String
end

"""
    TemporaryBinding <: AbstractBinding

Internal binding type used for storing bindings for unfinished repetitions.

# Examples

```
julia> pattern = @pattern {_}...
Pattern:
[~rep]
  _:::expr                               :: ~var

julia> syntax_match(pattern, parseall(SyntaxNode, \"""
                                                  ex1
                                                  ex2
                                                  \"""))
BindingSet with 0 entries
```

The `syntax_match` steps are:

1. Does `{_:::expr}` match `ex1`?
   > Yes.
2. Do we have a binding for `_` already?
   > No.
3. => Bind `_` to `[ex1]` as a `TemporaryBinding`.

4. Does `{_:::expr}` match `ex2`?
   > Yes.
5. Do we have a binding for `_` already?
   > Yes, `[ex1]`.
6. => Add the new matching source node to `_`'s source list.

7. No more source nodes to match, time to finish. Remove all `InvalidBinding`s (none in
   this example) and transform all temporary bindings for non-anonymous pattern variables
   into regular `Binding`s.
8. Only anonymous pattern variables here.
   => Return an empty binding set.
"""
struct TemporaryBinding{S, B} <: AbstractBinding
    bname::Symbol
    src::S
    bindings::B
    ellipsis_depth::Int
end
Binding(b::TemporaryBinding{S, B}) where {S, B} =
    Binding{S, B}(b.bname, b.src, b.bindings, b.ellipsis_depth)

# Base overwrites
# ---------------

# Allow accessing sub-bindings as fields.
function Base.getproperty(b::AbstractBinding, name::Symbol)
    name === :bname && return getfield(b, :bname)
    src = getfield(b, :src)
    name === :src && return src
    bindings = getfield(b, :bindings)
    name === :bindings && return bindings
    name === :ellipsis_depth && return getfield(b, :ellipsis_depth)
    # Gather available fields to show in error messages.
    internal_fields = [:bname, :src, :bindings]
    available_fields = []
    [push!(available_fields, subb) for subb in keys(bindings)]
    JS.is_identifier(src) && push!(available_fields, :name)
    JS.is_literal(src) && push!(available_fields, :value)
    if kind(src) === K"macrocall"
        push!(available_fields, :name)
        push!(available_fields, :args)
    end
    # Check for node-specific field access.
    if kind(src) === K"macrocall"
        name === :name && return string(src.children[1].val)
        name === :args && return src.children[2:end]
    end
    if kind(src) === K"macro"
        name === :call && return src.children[1]
        name === :body && return src.children[2]
    end
    if name === :value
        JS.is_literal(src) && return src.val
        kind(src) === K"string" && return src.children[1].val
        # Only literals have a `value` field.
        throw(BindingFieldError(b,
                                :value,
                                available_fields,
                                internal_fields,
                                "the bound expression is not a literal"))
    end
    if name === :name
        JS.is_identifier(src) && return string(src.val)
        # Only identifiers and macro calls have a `name` field.
        throw(BindingFieldError(b,
                                :name,
                                available_fields,
                                internal_fields,
                                "the bound expression is not an identifier or a macro call"))
    end
    return get(bindings, name) do
        throw(BindingFieldError(b,
                                name,
                                available_fields,
                                internal_fields,
                                "`$name` is not a sub-binding of `$(b.bname)`"))
    end
end
Base.getproperty(b::InvalidBinding, name::Symbol) =
    name === :msg ? getfield(b, :msg) : getfield(b, name)

Base.copy(b::Binding) = Binding(b.bname, copy(b.src), copy(b.bindings), b.ellipsis_depth)
Base.copy(b::InvalidBinding) = InvalidBinding(b.msg)
Base.copy(b::TemporaryBinding) =
    TemporaryBinding(b.bname, copy(b.src), copy(b.bindings), b.ellipsis_depth)

# Display
# -------

Base.show(io::IO, ::MIME"text/plain", b::AbstractBinding) =
    _show_binding(io, b, "")
Base.show(io::IO, b::AbstractBinding) =
    print(io,
          typeof(b), "(",
          repr(b.bname), ", ",
          _src_with_location_str(b.src), ", ",
          repr(b.bindings),
          ")")
Base.show(io::IO, b::InvalidBinding) =
    print(io, "InvalidBinding(", b.msg, ")")
Base.show(io::IO, ::Type{Binding{S, B}}) where {S, B} = print(io, "Binding")

function _show_bindings(io::IO, bs, outer_indent)
    println(io)
    _show_binding_set(io, bs, outer_indent * "  ")
end

function _show_binding(io::IO, b::AbstractBinding, outer_indent)
    indent = outer_indent * "  "
    indent_size = length(indent)

    bound_src_label, bound_src = _repr_source_nodes(b.src)
    bound_src_str = indent * bound_src_label * bound_src

    println(io, typeof(b), ":")
    println(io, indent, "Name: ", repr(b.bname))
    println(io, indent, bound_src_label, bound_src)
    println(io, indent, "Ellipsis depth: ", b.ellipsis_depth)
    print(io, indent, "Sub-bindings: ")
    _show_bindings(io, b.bindings, indent)
end
function _show_binding(io::IO, b::InvalidBinding, outer_indent)
    println(io, typeof(b), ":")
    print(io, outer_indent * "  ", "Message: ", b.msg)
end

function _repr_source_nodes(src)
    label = isa(src, JS.SyntaxNode) ? "Bound source: " : "Bound sources: "
    return label, _src_with_location_str(src)
end

function _src_with_location_str(src)
    if isa(src, JS.SyntaxNode)
        (line, col) = JS.source_location(src)
        return string(src, " @ ", line, ":", col)
    end
    str = "["
    str *= join(_src_with_location_str.(src), ", ")
    str *= "]"
    return str
end
