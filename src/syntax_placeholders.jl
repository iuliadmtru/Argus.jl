# ------------------------------------------------------------------------------------------
# Syntax placeholders.

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

# -------------------------------------------------------------------

"""
    Metavariable <: AbstractSyntaxPlaceholder

Syntax placeholder in the form of a metavariable that can be bound to an expression.
"""
mutable struct Metavariable <: AbstractSyntaxPlaceholder
    name::Symbol
    binding::Union{Nothing, JuliaSyntax.SyntaxData}
end
Metavariable(name::Symbol) = Metavariable(name, nothing)

macro m_str(name)
    Metavariable(name)
end

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
is_anonymous(m::Metavariable) = m.name === :_

# Wrappers for supertype compatibility.

placeholder_fill!(m::Metavariable, ast::JuliaSyntax.SyntaxNode) = set_binding!(m, ast.data)
placeholder_unbind!(m::Metavariable) = remove_binding!(m)

# --------------------------------------------
# `Base` overwrites.

Base.copy(m::Metavariable) = Metavariable(m.name, m.binding)
Base.isequal(m1::Metavariable, m2::Metavariable) =
    isequal(m1.name, m2.name) && _isequal(m1.binding, m2.binding)

# --------------------------------------------
# Utils.

_isequal(b1::JuliaSyntax.SyntaxData, b2::JuliaSyntax.SyntaxData) =
    isequal(b1.raw, b2.raw) && isequal(b1.val, b2.val)
_isequal(::Nothing, ::JuliaSyntax.SyntaxData) = false
_isequal(::JuliaSyntax.SyntaxData, ::Nothing) = false
_isequal(::Nothing, ::Nothing) = true

# --------------------------------------------
# Display.

function Base.show(io::IO, m::Metavariable)
    str = "Metavariable($(m.name), "
    if isnothing(m.binding)
        str *= "nothing)"
    else
        b = m.binding
        b_name = isnothing(b.val)   ? string(kind(b.raw)) :
                 isa(b.val, Symbol) ? string(b.val)       : repr(b.val)
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
    binding_val_str = isnothing(binding)       ? "nothing"                      :
                      isnothing(binding.val)   ? _green_node_sexpr(binding.raw) :
                      isa(binding.val, Symbol) ? string(binding.val)            :
                      repr(binding.val)
    treestr = string(rpad(treestr, 40), "| $binding_val_str")

    println(io, posstr, treestr)
end

# TODO: Rename "special syntax".
function _show_special_syntax_sexpr(io::IO, m::Metavariable)
    print(io, "%$(m.name)")
end

function _green_node_sexpr(node::JuliaSyntax.GreenNode)
    str = ""
    if is_leaf(node)
        if is_error(node)
            return string("(", untokenize(head(node)), ")")
        elseif is_trivia(node)
            return ""
        else
            return string(kind(node))
        end
    else
        str *= string("(", untokenize(head(node)))
        first = true
        for n in children(node)
            str *= ' '
            str *= _green_node_sexpr(n)
            first = false
        end
        str *= ')'
    end
end

# -------------------------------------------------------------------

# TODO.
