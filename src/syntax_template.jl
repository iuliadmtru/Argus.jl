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
    m.binding = ast.data && return true
end
remove_binding!(m::Metavariable) = m.binding = nothing

"""
    placeholder_fill(placeholder, ast::JuliaSyntax.SyntaxNode)

Fill the placeholder with the data contained within `ast`. Return `false`
if the placeholder had already been filled. Return `true` otherwise.
"""
placeholder_fill!(m::Metavariable, ast::JuliaSyntax.SyntaxNode) = set_binding!(m, ast.data)

## `Base` overwrites.

Base.copy(m::Metavariable) = Metavariable(m.name, m.binding)

## -------------------------------------------

## Utils

function _is_metavariable(node::JuliaSyntax.SyntaxNode)
    return kind(node) == K"call" && node.children[1].data.val == :Metavariable
end
function _get_metavar_name(node::JuliaSyntax.SyntaxNode)
    !is_metavariable(node) && @error "Trying to get metavariable name from non-Metavariable node"
    return node.children[2].children[1].data.val
end

## -------------------------------------------

## Display.

# TODO: Change `posstr` to something useful.
function _show_special_syntax(io::IO, data::SyntaxTemplateData{Metavariable}, indent)
    posstr = "$(lpad("-", 4)):$(rpad("-", 3))|"
    val = data.name  # Metavariable name.
    nodestr = "M\"$val\""
    treestr = string(indent, nodestr)
    binding = data.binding
    binding_val_str = isnothing(binding)       ? "nothing"           :
                      isa(binding.val, Symbol) ? string(binding.val) : repr(binding.val)
    treestr = string(rpad(treestr, 40), "| $binding_val_str")

    println(io, posstr, treestr)
end

function _show_special_syntax_sexpr(io::IO, data::SyntaxTemplateData{Metavariable})
    print(io, "M\"$(data.name)\"")
end

## ------------------------------------------------------------------

# TODO.
