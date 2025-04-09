## -----------------------------------------------------------------------------------------
## Template AST interface.

## -------------------------------------------
## Template data.

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

## -------------------------------------------
## Template tree.

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
function SyntaxTemplateNode(template_src::AbstractString)
    node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, template_src)
    clean_node = kind(node) === K"toplevel" ? children(node)[1] : node

    return SyntaxTemplateNode(clean_node)
end

## `JuliaSyntax` overwrites.

# TODO: Add `head` for placeholders.
JuliaSyntax.head(node::SyntaxTemplateNode) =
    is_placeholder(node) ? nothing : head(node.data.raw)
JuliaSyntax.kind(node::SyntaxTemplateNode) = head(node).kind

JuliaSyntax.build_tree(::Type{SyntaxTemplateNode}, stream::JuliaSyntax.ParseStream; kws...) =
    SyntaxTemplateNode(JuliaSyntax.build_tree(SyntaxNode, stream; kws...))

function JuliaSyntax.source_location(node::SyntaxTemplateNode)
    if is_placeholder(node)
        # TODO: Treat other placeholders.
        # The node contains a `Metavariable`.
        if (has_binding(node.data))
            source_file = node.data.binding.source
            byte_idx = node.data.binding.position
            return JuliaSyntax.source_location(source_file, byte_idx)
        else
            return (0, 0)
        end
    end
    # The node contains regular `SyntaxData`.
    source_file = node.data.source
    byte_idx = node.data.position
    return JuliaSyntax.source_location(source_file, byte_idx)
end

## `Base` overwrites.

function Base.getproperty(node::SyntaxTemplateNode, name::Symbol)
    name === :parent && return getfield(node, :parent)
    name === :children && return getfield(node, :children)
    d = getfield(node, :data)
    # Don't like this; internals of `SyntaxTemplateData` are spilled.
    name === :data && return getfield(d, :data)
    return getproperty(d, name)
end

## -----------------------------------------------------------------------------------------
## Template matching.

"""
    template_match!(template::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)

Try to match the given template with a source AST and all its children. When a match is
found bind the placeholders in the template, if any. Return an array of `SyntaxMatch`es.
"""
function template_match!(tp::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)::SyntaxMatches
    if template_compare!(tp, src)
        return SyntaxMatches([SyntaxMatch(src, placeholders(tp))])
    end
    if !haschildren(src)
        return SyntaxMatches()
    end
    # Clean up bound placeholders.
    # TODO: Should this be done elsewhere?
    placeholders_unbind!(tp)
    # Search for matches within children.
    matches = SyntaxMatches()
    for c in children(src)
        append!(matches, template_match!(tp, c))
    end

    return matches
end

## -------------------------------------------
## Template comparison.

# TODO: Needs more work.
"""
    template_compare!(template::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)

Compare a given template to a source AST. If the template contains placeholders, fill them.
The comparison fails if the source doesn't fit the template or if the placeholders' bindings
don't agree with each other.

Return `true` if the template matches the source AST, `false` otherwise.
"""
function template_compare!(template::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)
    # The node is a placeholder that needs to be filled.
    # TODO: Take into account repetitions with ellipses.
    is_placeholder(template) && return placeholder_fill!(template.data, src)

    # The node itself is not a special node, but it has a successor
    # with some special syntax.
    if contains_placeholders(template)
        head(template) != head(src) && return false
        if length(children(template)) == length(children(src))
            zipped_children = zip(children(template), children(src))
            return all(p -> template_compare!(p[1], p[2]), zipped_children)
        else
            # The rule might have ellipses.
            # TODO.
            return false
        end
    end

    # No special syntax.
    head(template) != head(src) && return false
    template.data.val != src.data.val && return false
    xor(haschildren(template), haschildren(src)) && return false
    # Recurse on children if there are any.
    !haschildren(src) && return true
    length(children(template)) != length(children(src)) && return false
    zipped_children = zip(children(template), children(src))
    # TODO: This doesn't seem finished.
    return all(p -> template_compare!(p[1], p[2]), zipped_children)
end

## -----------------------------------------------------------------------------------------
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
    elseif haschildren(templ)
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
    isnothing(children(node)) && return node
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
