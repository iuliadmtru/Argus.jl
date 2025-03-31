## Syntax data.

"""
    SyntaxTemplateData

Light wrapper around either a `JuliaSyntax.SyntaxData` or an `AbstractSyntaxPlaceholder`.
"""
struct SyntaxTemplateData{NodeData}
    data::NodeData
end

## `Base` overwrites.

function Base.getproperty(data::SyntaxTemplateData, name::Symbol)
    d = getfield(data, :data)
    name === :data && return d
    return getproperty(d, name)
end

## -----------------------------------------------------------------------------------------

## Syntax placeholders

"""
    AbstractSyntaxPlaceholder

Supertype for syntax placeholders.
"""
abstract type AbstractSyntaxPlaceholder end

"""
    placeholder_fill!(placeholder::AbstractSyntaxPlaceholder, ast::JuliaSyntax.SyntaxNode)

Fill the placeholder with the data contained within `ast`. Return `false`
if the placeholder had already been filled. Return `true` otherwise.
"""
function placeholder_fill!(p::AbstractSyntaxPlaceholder, ast::JuliaSyntax.SyntaxNode) end

"""
    placeholder_unbind!(p::AbstractSyntaxPlaceholder)

Remove binding from the given placeholder, if any. Return the placeholder.
"""
function placeholder_unbind!(p::AbstractSyntaxPlaceholder) end

# No placeholder for regular `SyntaxData`.
placeholder_unbind!(p::JuliaSyntax.SyntaxData) = p

## ------------------------------------------------------------------

"""
    Metavariable <: AbstractSyntaxPlaceholder

Syntax placeholder in the form of a metavariable that can be bound to an expression.
"""
mutable struct Metavariable <: AbstractSyntaxPlaceholder
    name::Symbol
    # Should this be bound to something else?
    binding::Union{Nothing, JuliaSyntax.SyntaxData}
end
Metavariable(name::Symbol) = Metavariable(name, nothing)

has_binding(m::Metavariable) = !isnothing(m.binding)
function set_binding!(m::Metavariable, b::JuliaSyntax.SyntaxData)
    !isnothing(m.binding) && return false
    m.binding = b
    return true
end
function remove_binding!(m::Metavariable)
    m.binding = nothing
    return m
end

## Wrappers for supertype compatibility.

placeholder_fill!(m::Metavariable, ast::JuliaSyntax.SyntaxNode) = set_binding!(m, ast.data)
placeholder_unbind!(m::Metavariable) = remove_binding!(m)

## `Base` overwrites.

Base.copy(m::Metavariable) = Metavariable(m.name, m.binding)

## -------------------------------------------

## Utils

"""
    _is_metavariable(node::JuliaSyntax.SyntaxNode)

Internal utility function for checking whether a syntax node corresponds to the
`Argus`-specific syntax for a `Metavariable`.
"""
function _is_metavariable(node::JuliaSyntax.SyntaxNode)
    return kind(node) == K"call" && node.children[1].data.val == :Metavariable
end
function _get_metavar_name(node::JuliaSyntax.SyntaxNode)
    !_is_metavariable(node) &&
        @error "Trying to get metavariable name from non-Metavariable node"
    return node.children[2].children[1].data.val
end

## -------------------------------------------

## Display.

# TODO: Rename "special syntax".
# TODO: Change `posstr` to something useful.
function _show_special_syntax(io::IO, m::Metavariable, indent)
    posstr = "$(lpad("-", 4)):$(rpad("-", 3))|"
    val = m.name  # Metavariable name.
    nodestr = "M\"$val\""
    treestr = string(indent, nodestr)
    binding = m.binding
    binding_val_str = isnothing(binding)       ? "nothing"           :
                      isa(binding.val, Symbol) ? string(binding.val) : repr(binding.val)
    treestr = string(rpad(treestr, 40), "| $binding_val_str")

    println(io, posstr, treestr)
end

# TODO: Rename "special syntax".
function _show_special_syntax_sexpr(io::IO, m::Metavariable)
    print(io, "M\"$(m.name)\"")
end

## ------------------------------------------------------------------

# TODO.
