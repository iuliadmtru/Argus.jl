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
    binding::Union{Nothing, JuliaSyntax.SyntaxData}
end
Metavariable(name::Symbol) = Metavariable(name, nothing)

has_binding(m::Metavariable) = !isnothing(m.binding)
function set_binding!(m::Metavariable, b::JuliaSyntax.SyntaxData)
    if isnothing(m.binding)
        m.binding = b
        return true
    end
    # Check if the metavariable is bound to the same expression
    return _isequal(m.binding, b) ? true : false
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
Base.isequal(m1::Metavariable, m2::Metavariable) =
    isequal(m1.name, m2.name) && _isequal(m1.binding, m2.binding)

## -------------------------------------------
## Utils

"""
    _is_metavariable(node::JuliaSyntax.SyntaxNode)

Internal utility function for checking whether a syntax node corresponds to the
`Argus`-specific syntax for a `Metavariable`.
"""
_is_metavariable(node::JuliaSyntax.SyntaxNode) =
    kind(node) == K"call" && node.children[1].data.val == :Metavariable
function _get_metavar_name(node::JuliaSyntax.SyntaxNode)
    !_is_metavariable(node) &&
        @error "Trying to get metavariable name from non-Metavariable node"
    # TODO: Error handling for wrong syntax.
    return node.children[2].children[1].data.val
end

@enum SugaredMetavariableRet sugar no_sugar err
function _is_metavariable_sugared(node::JuliaSyntax.SyntaxNode)::SugaredMetavariableRet
    is_error_call = kind(node) == K"call" && kind(node.children[1]) == K"error"
    is_error_call || return no_sugar

    length(node.children) > 2 && return err
    error_node = node.children[1]
    (is_leaf(error_node) || length(error_node.children) > 1) && return err
    !is_leaf(error_node.children[1]) && return err
    error_node.children[1].val !== :% && return err
    !is_leaf(node.children[2]) && return err

    return sugar
end
function _get_metavar_name_sugared(node::JuliaSyntax.SyntaxNode)
    _is_metavariable_sugared(node) !== sugar &&
        @error "Trying to get metavariable name from non-Metavariable node"
    return node.children[2].data.val
end
function _desugar_metavariable(node::JuliaSyntax.SyntaxNode)
    name = string(_get_metavar_name_sugared(node))
    return JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "Metavariable(:$name)")
end

_isequal(b1::JuliaSyntax.SyntaxData, b2::JuliaSyntax.SyntaxData) =
    isequal(b1.raw, b2.raw) && isequal(b1.val, b2.val)
_isequal(::Nothing, ::Nothing) = true

## -------------------------------------------
## Display.

function Base.show(io::IO, m::Metavariable)
    str = "Metavariable($(m.name), "
    if isnothing(m.binding)
        str *= "nothing)"
    else
        b = m.binding
        b_name = isa(b.val, Symbol) ? string(b.val) : repr(b.val)
        str *= "$b_name@$(b.position))"
    end
    print(io, str)
end

# TODO: Rename "special syntax".
# TODO: Change `posstr` to something useful.
function _show_special_syntax(io::IO, m::Metavariable, indent)
    posstr = "$(lpad("-", 4)):$(rpad("-", 3))|"
    val = m.name  # Metavariable name.
    nodestr = "%$val"
    treestr = string(indent, nodestr)
    binding = m.binding
    binding_val_str = isnothing(binding)       ? "nothing"           :
                      isa(binding.val, Symbol) ? string(binding.val) : repr(binding.val)
    treestr = string(rpad(treestr, 40), "| $binding_val_str")

    println(io, posstr, treestr)
end

# TODO: Rename "special syntax".
function _show_special_syntax_sexpr(io::IO, m::Metavariable)
    print(io, "%$(m.name)")
end

## ------------------------------------------------------------------

# TODO.
