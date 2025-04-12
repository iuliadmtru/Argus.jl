## -----------------------------------------------------------------------------------------
## Pattern AST interface.

## -------------------------------------------
## Pattern data.

"""
    SyntaxPatternData

Light wrapper around either a `JuliaSyntax.SyntaxData` or an `AbstractSyntaxPlaceholder`.
"""
mutable struct SyntaxPatternData{NodeData}
    pattern_data::NodeData
end

## `Base` overwrites.

function Base.getproperty(data::SyntaxPatternData, name::Symbol)
    d = getfield(data, :pattern_data)
    name === :pattern_data && return d
    return getproperty(d, name)
end

## -------------------------------------------
## Syntax pattern tree.

const SyntaxPatternNode = JuliaSyntax.TreeNode{SyntaxPatternData}
function SyntaxPatternNode(node::JuliaSyntax.SyntaxNode)
    pattern_node = _SyntaxPatternNode(node)
    _unify_placeholders!(pattern_node)

    return pattern_node
end
function SyntaxPatternNode(pattern_src::AbstractString)
    node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, pattern_src; ignore_errors=true)
    clean_node = kind(node) === K"toplevel" ? children(node)[1] : node

    return SyntaxPatternNode(clean_node)
end

function _SyntaxPatternNode(node::JuliaSyntax.SyntaxNode)
    ret = _is_metavariable_sugared(node)
    # TODO: This is not always the correct error. This probably needs to pass through
    #       JuliaSyntax.
    ret == err && error("Invalid metavariable name")
    if ret == sugar
        node = _desugar_metavariable(node)
    end
    data = _is_metavariable(node)                                 ?
        SyntaxPatternData(Metavariable(_get_metavar_name(node))) :
        SyntaxPatternData(node.data)

    if is_leaf(node)
        return SyntaxPatternNode(nothing, nothing, data)
    else
        cs = [SyntaxPatternNode(c) for c in children(node)]
        templ_node = SyntaxPatternNode(nothing, cs, data)
        for c in cs
            c.parent = templ_node
            if _is_metavariable(c)
                if JuliaSyntax.flags(templ_node) === JuliaSyntax.SHORT_FORM_FUNCTION_FLAG
                    c.parent.data =
                        _update_data_head(data, JuliaSyntax.SyntaxHead(K"=", 0))
                end
            end
        end
    end

    return templ_node
end

## `JuliaSyntax` overwrites.

# TODO: Add `head` for placeholders.
JuliaSyntax.head(node::SyntaxPatternNode) =
    is_placeholder(node) ? nothing : head(node.data.raw)
JuliaSyntax.kind(node::SyntaxPatternNode) = head(node).kind

JuliaSyntax.build_tree(::Type{SyntaxPatternNode}, stream::JuliaSyntax.ParseStream; kws...) =
    SyntaxPatternNode(JuliaSyntax.build_tree(SyntaxNode, stream; kws...))

function JuliaSyntax.source_location(node::SyntaxPatternNode)
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

function Base.getproperty(node::SyntaxPatternNode, name::Symbol)
    name === :parent && return getfield(node, :parent)
    name === :children && return getfield(node, :children)
    d = getfield(node, :data)
    name === :data && return d
    return getproperty(d, name)
end

## -----------------------------------------------------------------------------------------
## Pattern matching.

"""
    pattern_match!(pattern::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)

Try to match the given pattern with a source AST and all its children. When a match is
found bind the placeholders in the pattern, if any. Return an array of `SyntaxMatch`es.
"""
function pattern_match!(tp::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)::SyntaxMatches
    if pattern_compare!(tp, src)
        matches = SyntaxMatches([SyntaxMatch(src, placeholders(tp))])
        placeholders_unbind!(tp)
        return matches
    end
    if is_leaf(src)
        return SyntaxMatches()
    end
    # Search for matches within children.
    matches = SyntaxMatches()
    for c in children(src)
        append!(matches, pattern_match!(tp, c))
    end
    placeholders_unbind!(tp)

    return matches
end

## -------------------------------------------
## Pattern comparison.

# TODO: Needs more work.
"""
    pattern_compare!(pattern::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)

Compare a given pattern to a source AST. If the pattern contains placeholders, fill them.
The comparison fails if the source doesn't fit the pattern or if the placeholders' bindings
don't agree with each other.

Return `true` if the pattern matches the source AST, `false` otherwise.
"""
function pattern_compare!(pattern::SyntaxPatternNode, src::JuliaSyntax.SyntaxNode)
    # The node is a placeholder that needs to be filled.
    # TODO: Take into account repetitions with ellipses.
    is_placeholder(pattern) && return placeholder_fill!(pattern.pattern_data, src)

    # The node itself is not a special node, but it has a successor
    # with some special syntax.
    if contains_placeholders(pattern)
        head(pattern) != head(src) && return false
        if length(children(pattern)) == length(children(src))
            zipped_children = zip(children(pattern), children(src))
            return all(p -> pattern_compare!(p[1], p[2]), zipped_children)
        else
            # The rule might have ellipses.
            # TODO.
            return false
        end
    end

    # No special syntax.
    head(pattern) != head(src) && return false
    pattern.data.val != src.data.val && return false
    xor(!is_leaf(pattern), !is_leaf(src)) && return false
    # Recurse on children if there are any.
    is_leaf(src) && return true
    length(children(pattern)) != length(children(src)) && return false
    zipped_children = zip(children(pattern), children(src))
    # TODO: This doesn't seem finished.
    return all(p -> pattern_compare!(p[1], p[2]), zipped_children)
end

## -----------------------------------------------------------------------------------------
## Utils.

function _update_data_head(
    old_data::SyntaxPatternData{JuliaSyntax.SyntaxData},
    new_head::JuliaSyntax.SyntaxHead
)
    old_raw = old_data.raw
    new_raw = JuliaSyntax.GreenNode(
        new_head,
        old_raw.span,
        old_raw.children
    )
    new_data = JuliaSyntax.SyntaxData(
        old_data.source,
        new_raw,
        old_data.position,
        old_data.val
    )

    return SyntaxPatternData(new_data)
end

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
_is_metavariable(node::SyntaxPatternNode) = isa(node.data.pattern_data, Metavariable)

is_placeholder(node::SyntaxPatternNode) = isa(node.pattern_data, AbstractSyntaxPlaceholder)

"""
    placeholder(node::SyntaxPatternNode)

If the pattern node has placeholder data, return the placeholder. Else, return `nothing`.
"""
placeholder(node::SyntaxPatternNode) = is_placeholder(node) ? node.pattern_data : nothing

"""
    contains_placeholders(node::SyntaxPatternNode)

Return `true` if the node or its children contain one or more placeholders. Return `false`
otherwise.
"""
function contains_placeholders(node::SyntaxPatternNode)
    is_placeholder(node) && return true
    # If it is not a special sytax node and it has no children then
    # it is a regular leaf.
    is_leaf(node) && return false
    # If any child has some special syntax then the node has special syntax.
    any(c -> contains_placeholders(c), children(node)) && return true
    # No child has special syntax.
    return false
end

"""
    placeholders(pattern::SyntaxPatternNode)

Return an array with all placeholders contained within the given pattern.
"""
function placeholders(templ::SyntaxPatternNode)
    # TODO: Does this make sense to be anything else than a `Metavariable` vector?
    ps = AbstractSyntaxPlaceholder[]
    _placeholders!(templ, ps)

    return ps
end
function _placeholders!(pat::SyntaxPatternNode, ps::Vector{AbstractSyntaxPlaceholder})
    if is_placeholder(pat) && isnothing(findfirst(p -> isequal(p, pat.pattern_data), ps))
        push!(ps, copy(pat.pattern_data))
    elseif !is_leaf(pat)
        for c in children(pat)
            _placeholders!(c, ps)
        end
    end
end

"""
    placeholders_unbind!(node::SyntaxPatternNode)

Remove bindings from all placeholders within `node` and its children. Return the node.
"""
function placeholders_unbind!(node::SyntaxPatternNode)
    placeholder_unbind!(node.pattern_data)
    is_leaf(node) && return node
    for c in children(node)
        placeholders_unbind!(c)
    end

    return node
end

function _unify_placeholders!(node::SyntaxPatternNode)
    metavars = Metavariable[]
    _unify_placeholders!(node, metavars)
end
function _unify_placeholders!(node::SyntaxPatternNode, metavars::Vector{Metavariable})
    if _is_metavariable(node)
        m_idx = findfirst(m -> isequal(m, node.pattern_data), metavars)
        if !isnothing(m_idx)
            # Replace the current metavariable with the existing one.
            node.pattern_data = metavars[m_idx]
        else
            push!(metavars, node.pattern_data)
        end
    elseif !is_leaf(node)
        for c in children(node)
            _unify_placeholders!(c, metavars)
        end
    end
end

## -------------------------------------------
## Display.

# TODO: Improve these. For now, they are basically copy-pasted from `JuliaSyntax`.

function _show_syntax_pattern_node(io::IO, node::SyntaxPatternNode, indent)
    if is_placeholder(node)
        _show_special_syntax(io, node.pattern_data, indent)
    else
        # TODO: Change `posstr` to something useful.
        posstr = "$(lpad("-", 4)):$(rpad("-", 3))|"
        val = node.val
        nodestr = !is_leaf(node) ? "[$(untokenize(head(node)))]" :
            isa(val, Symbol)     ? string(val)                   : repr(val)
        treestr = string(indent, nodestr)
        # No metadata.
        treestr = string(rpad(treestr, 40), "|")
        println(io, posstr, treestr)
        if !is_leaf(node)
            new_indent = indent * "  "
            for c in children(node)
                _show_syntax_pattern_node(io, c, new_indent)
            end
        end
    end
end

function _show_syntax_pattern_node_sexpr(io, node::SyntaxPatternNode)
    if is_placeholder(node)
        _show_special_syntax_sexpr(io, node.pattern_data)
    else
        if is_leaf(node)
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
                _show_syntax_pattern_node_sexpr(io, n)
                first = false
            end
            print(io, ')')
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::SyntaxPatternNode)
    println(io, "line:col│ tree                                   │ metadata")
    _show_syntax_pattern_node(io, node, "")
end
Base.show(io::IO, ::MIME"text/x.sexpression", node::SyntaxPatternNode) =
    _show_syntax_pattern_node_sexpr(io, node)
Base.show(io::IO, node::SyntaxPatternNode) = _show_syntax_pattern_node_sexpr(io, node)
Base.show(io::IO, ::Type{SyntaxPatternNode}) = print(io, "SyntaxPatternNode")
