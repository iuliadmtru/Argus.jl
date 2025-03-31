module Argus

## -----------------------------------------------------------------------------------------

## Types

export SyntaxTemplateNode, SyntaxTemplateData
export AbstractSyntaxPlaceholder, Metavariable
export Pattern
export SyntaxMatch, SyntaxMatches

## Utils
export is_placeholder, placeholder, contains_placeholders, placeholders,
    placeholder_fill!, placeholder_unbind!,
    has_binding, set_binding!
export pattern_match!

## -------------------------------------------

using JuliaSyntax
using JuliaSyntax: haschildren, children, head, kind, source_location, untokenize, is_error

include("syntax_template.jl")

## -----------------------------------------------------------------------------------------

## Template AST interface

const SyntaxTemplateNode = JuliaSyntax.TreeNode{SyntaxTemplateData}
function SyntaxTemplateNode(node::JuliaSyntax.SyntaxNode)
    data = _is_metavariable(node)                                 ?
        SyntaxTemplateData(Metavariable(_get_metavar_name(node))) :
        SyntaxTemplateData(node.data)

    if !haschildren(node)
        return SyntaxTemplateNode(nothing, nothing, data)
    else
        cs = [SyntaxTemplateNode(c) for c in children(node)]
        templ_node = SyntaxTemplateNode(nothing, cs, data)
        [c.parent = templ_node for c in cs]

        return templ_node
    end
end

## `JuliaSyntax` overwrites.

# TODO: Add `head` for placeholders.
JuliaSyntax.head(node::SyntaxTemplateNode) =
    is_placeholder(node) ? nothing : head(node.data.raw)
JuliaSyntax.kind(node::SyntaxTemplateNode) = head(node).kind

JuliaSyntax.build_tree(::Type{SyntaxTemplateNode}, stream::JuliaSyntax.ParseStream; kws...) =
    SyntaxTemplateNode(JuliaSyntax.build_tree(SyntaxNode, stream; kws...))

## `Base` overwrites.

function Base.getproperty(node::SyntaxTemplateNode, name::Symbol)
    name === :parent && return getfield(node, :parent)
    name === :children && return getfield(node, :children)
    d = getfield(node, :data)
    # Don't like this; internals of `SyntaxTemplateData` are spilled.
    name === :data && return getfield(d, :data)
    return getproperty(d, name)
end

## -------------------------------------------

## Utils.

is_placeholder(node::SyntaxTemplateNode) = isa(node.data, AbstractSyntaxPlaceholder)

"""
    placeholder(node::SyntaxTemplateNode)

If the template node has placeholder data, return the placeholder. Else, return `nothing`.
"""
placeholder(node::SyntaxTemplateNode) = is_placeholder(node) ? node.data : nothing

"""
    contains_placeholders(node::SyntaxTemplateNode)

Return `true` if the node or its children contain one or more placeholders. Return `false`
otherwise.
"""
function contains_placeholders(node::SyntaxTemplateNode)
    is_placeholder(node) && return true
    # If it is not a special sytax node and it has no children then
    # it is a regular leaf.
    !haschildren(node) && return false
    # If any child has some special syntax then the node has special syntax.
    any(c -> contains_placeholders(c), children(node)) && return true
    # No child has special syntax.
    return false
end

"""
    placeholders(templ::SyntaxTemplateNode)

Return an array with copies of all the placeholders contained within the template.
"""
function placeholders(templ::SyntaxTemplateNode)
    ps = AbstractSyntaxPlaceholder[]
    if is_placeholder(templ)
        push!(ps, copy(templ.data))
    else
        for c in children(templ)
            append!(ps, placeholders(c))
        end
    end

    return ps
end

"""
    placeholders_unbind!(node::SyntaxTemplateNode)

Remove bindings from all placeholders within `node` and its children. Return the node.
"""
function placeholders_unbind!(node::SyntaxTemplateNode)
    placeholder_unbind!(node.data)
    for c in children(node)
        placeholders_unbind!(c)
    end

    return node
end

## -------------------------------------------

## Display.

# TODO: Improve these. For now, they are basically copy-pasted from `JuliaSyntax`.

function _show_syntax_template_node(io::IO, node::SyntaxTemplateNode, indent)
    if is_placeholder(node)
        _show_special_syntax(io, node.data, indent)
    else
        # TODO: Change `posstr` to something useful.
        posstr = "$(lpad("-", 4)):$(rpad("-", 3))|"
        val = node.val
        nodestr = haschildren(node) ? "[$(untokenize(head(node)))]" :
            isa(val, Symbol)        ? string(val)                   : repr(val)
        treestr = string(indent, nodestr)
        # No metadata.
        treestr = string(rpad(treestr, 40), "|")
        println(io, posstr, treestr)
        if haschildren(node)
            new_indent = indent * "  "
            for c in children(node)
                _show_syntax_template_node(io, c, new_indent)
            end
        end
    end
end

function _show_syntax_template_node_sexpr(io, node::SyntaxTemplateNode)
    if is_placeholder(node)
        _show_special_syntax_sexpr(io, node.data)
    else
        if !haschildren(node)
            if is_error(node)
                print(io, "(", untokenize(head(node)), ")")
            else
                val = node.val
                print(io, val isa Symbol ? string(val) : repr(val))
            end
        else
            print(io, "(", untokenize(head(node)))
            first = true
            for n in children(node)
                print(io, ' ')
                _show_syntax_template_node_sexpr(io, n)
                first = false
            end
            print(io, ')')
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::SyntaxTemplateNode)
    println(io, "line:col│ tree                                   │ metadata")
    _show_syntax_template_node(io, node, "")
end

function Base.show(io::IO, ::MIME"text/x.sexpression", node::SyntaxTemplateNode)
    _show_syntax_template_node_sexpr(io, node)
end

function Base.show(io::IO, node::SyntaxTemplateNode)
    _show_syntax_template_node_sexpr(io, node)
end

## -----------------------------------------------------------------------------------------

include("syntax_pattern.jl")
include("syntax_match.jl")
include("template_compare.jl")

## -----------------------------------------------------------------------------------------

end # Argus
