# TODO: Rename.
# abstract type SpecialSyntax end

# TODO: Rename!!!
struct RuleSyntaxData{SpecialSyntax}
    syntax_data::Union{Nothing, JuliaSyntax.SyntaxData}
    special_syntax::SpecialSyntax
end

is_special_syntax(data::RuleSyntaxData) = !isnothing(data.special_syntax)

function Base.getproperty(data::RuleSyntaxData, name::Symbol)
    name === :special_syntax && return getfield(data, :special_syntax)
    d = getfield(data, :syntax_data)
    name === :syntax_data && return d
    return getproperty(d, name)
end


#=
    Special pattern syntax
=#

struct Metavariable
    name::Symbol
    binding::Union{Nothing, JuliaSyntax.SyntaxData}
end
Metavariable(name::Symbol) = Metavariable(name, nothing)

function is_metavariable(node::JuliaSyntax.SyntaxNode)
    return kind(node) == K"call" && node.children[1].data.val == :Metavariable
end

function get_metavar_name(node::JuliaSyntax.SyntaxNode)
    !is_metavariable(node) && @error "Trying to get metavariable name from non-Metavariable node"

    return node.children[2].children[1].data.val
end

## Display.

# TODO: Improve this.
function _show_special_syntax(io::IO, data::RuleSyntaxData{Metavariable})
    posstr = "$(lpad("-", 4)):$(rpad("-", 3))|"
    val = data.special_syntax.name  # Metavariable name.
    nodestr = string(val)
    treestr = string(indent, nodestr)

    println(io, posstr, treestr)
end

