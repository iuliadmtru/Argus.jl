# TODO: Rename!!!
struct RuleSyntaxData{SpecialSyntax}
    # TODO: Keep only one field which can be either a `JuliaSyntax.SyntaxData`
    #       or some special syntax.
    syntax_data::Union{Nothing, JuliaSyntax.SyntaxData}
    special_syntax::SpecialSyntax
end

is_special_syntax(data::RuleSyntaxData) = !isnothing(data.special_syntax)
clean_up_special_syntax!(data::RuleSyntaxData{Nothing}) = nothing

function Base.getproperty(data::RuleSyntaxData, name::Symbol)
    name === :special_syntax && return getfield(data, :special_syntax)
    d = getfield(data, :syntax_data)
    name === :syntax_data && return d
    return getproperty(d, name)
end


#=
    Special pattern syntax
=#

mutable struct Metavariable
    name::Symbol
    # Should this be bound to something else? Only value and position
    # are necessary.
    binding::Union{Nothing, JuliaSyntax.SyntaxData}
end
Metavariable(name::Symbol) = Metavariable(name, nothing)

# TODO: `has_binding`

function set_binding!(m::Metavariable, b::JuliaSyntax.SyntaxData)
    # There is no binding yet.
    if isnothing(m.binding)
        m.binding = b
        return true
    end
    # The metavariable is already bound to the same value; do nothing.
    m.binding.val == b.val && return true
    # The metavariable is bound to a different value; no match.
    return false
end
remove_binding!(m::Metavariable) = m.binding = nothing
clean_up_special_syntax!(data::RuleSyntaxData{Metavariable}) = remove_binding!(data.special_syntax)

function is_metavariable(node::JuliaSyntax.SyntaxNode)
    return kind(node) == K"call" && node.children[1].data.val == :Metavariable
end

function get_metavar_name(node::JuliaSyntax.SyntaxNode)
    !is_metavariable(node) && @error "Trying to get metavariable name from non-Metavariable node"

    return node.children[2].children[1].data.val
end

Base.copy(m::Metavariable) = Metavariable(m.name, m.binding)

## Display.

# TODO: Change `posstr` to something useful.
function _show_special_syntax(io::IO, data::RuleSyntaxData{Metavariable}, indent)
    posstr = "$(lpad("-", 4)):$(rpad("-", 3))|"
    val = data.special_syntax.name  # Metavariable name.
    nodestr = "M\"$val\""
    treestr = string(indent, nodestr)
    binding = data.special_syntax.binding
    binding_val_str = isnothing(binding)       ? "nothing"           :
                      isa(binding.val, Symbol) ? string(binding.val) : repr(binding.val)
    treestr = string(rpad(treestr, 40), "| $binding_val_str")

    println(io, posstr, treestr)
end

function _show_special_syntax_sexpr(io::IO, data::RuleSyntaxData{Metavariable})
    print(io, "M\"$(data.special_syntax.name)\"")
end

