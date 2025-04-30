# ------------------------------------------------------------------------------------------
# Pattern AST interface.

# --------------------------------------------
# Syntax pattern directive.

struct SyntaxPatternDirective
    directive::Symbol  # `:and` or `:or`
end

# Display.

function _show_directive(io::IO, d::SyntaxPatternDirective, indent)
    posstr = "$(lpad("-", 4)):$(rpad("-", 3))|"
    nodestr = string(indent, "[directive-", d.directive, "]")
    treestr = string(indent, nodestr)
    treestr = string(rpad(treestr, 40), "|")

    println(io, posstr, treestr)
end

# --------------------------------------------
# Syntax pattern data.

"""
    SyntaxPatternData

Light wrapper around a `JuliaSyntax.SyntaxData`, a `SyntaxPatternDirective` or an
`AbstractSyntaxPlaceholder`.
"""
mutable struct SyntaxPatternData{NodeData} <: JuliaSyntax.AbstractSyntaxData
    pattern_data::NodeData
end

# `JuliaSyntax` overwrites.

JuliaSyntax.head(d::SyntaxPatternData{JuliaSyntax.SyntaxData}) = head(d.pattern_data.raw)
JuliaSyntax.head(d::SyntaxPatternData{SyntaxPatternDirective}) = nothing
JuliaSyntax.head(d::SyntaxPatternData{Metavariable}) = nothing

# `Base` overwrites.

function Base.getproperty(data::SyntaxPatternData, name::Symbol)
    d = getfield(data, :pattern_data)
    name === :pattern_data && return d
    return getproperty(d, name)
end

function Base.isequal(d1::SyntaxPatternData, d2::SyntaxPatternData)
    if isa(d1.pattern_data, Metavariable)
        isa(d2.pattern_data, Metavariable) || return false
        # Both are metavariables.
        return isequal(d1.pattern_data, d2.pattern_data)
    elseif isa(d1.pattern_data, SyntaxPatternDirective)
        isa(d2.pattern_data, SyntaxPatternDirective) || return false
        # Both are pattern directives.
        return d1.directive === d2.directive
    elseif isa(d1.pattern_data, JuliaSyntax.SyntaxData)
        isa(d2.pattern_data, JuliaSyntax.SyntaxData) || return false
        # Both are syntax data.
        return _isequal(d1.pattern_data, d2.pattern_data)
    end
end
Base.isequal(::Nothing, ::SyntaxPatternData) = false
Base.isequal(::SyntaxPatternData, ::Nothing) = false

Base.copy(d::SyntaxPatternData) = SyntaxPatternData(copy(d.pattern_data))

# --------------------------------------------
# Syntax pattern tree.

const SyntaxPatternNode = JuliaSyntax.TreeNode{SyntaxPatternData}
function SyntaxPatternNode(node::JuliaSyntax.SyntaxNode)
    pattern_node = _SyntaxPatternNode(node)
    _unify_placeholders!(pattern_node)

    return pattern_node
end
function SyntaxPatternNode(pattern_src::AbstractString)
    node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, pattern_src; ignore_errors=true)
    clean_node =
        if kind(node) === K"toplevel"
            isempty(children(node)) && error("Can't create empty pattern")
            children(node)[1]
        else
            node
        end

    return SyntaxPatternNode(clean_node)
end
"""
    SyntaxPatternNode(ex::Expr)

Composite pattern constructor for allowing `and` and `or` patterns. The `and` and `or`
directives require at least one argument. Nested composite patterns are allowed.
"""
function SyntaxPatternNode(ex::Expr)
    num_patterns = length(ex.args) - 1
    num_patterns >= 1 || error("Composite pattern must have at least one sub-pattern")
    # If there's only one sub-pattern there's no need for special treatment.
    num_patterns == 1 && return SyntaxPatternNode(ex.args[2])
    # If there are at least two sub-patterns create a node with the pattern directive
    # as data and the subpatterns as children.
    directive = SyntaxPatternDirective(ex.args[1])
    subpatterns = [SyntaxPatternNode(p) for p in ex.args[2:end]]
    return SyntaxPatternNode(nothing, subpatterns, SyntaxPatternData(directive))
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
        pat_node = SyntaxPatternNode(nothing, cs, data)
        for c in cs
            c.parent = pat_node
            if _is_metavariable(c)
                if JuliaSyntax.flags(pat_node) === JuliaSyntax.SHORT_FORM_FUNCTION_FLAG
                    c.parent.data =
                        _update_data_head(data, JuliaSyntax.SyntaxHead(K"=", 0))
                end
            end
        end
    end

    return pat_node
end

# `JuliaSyntax` overwrites.

# TODO: Add `head` for placeholders.
JuliaSyntax.head(node::SyntaxPatternNode) = head(node.data)
JuliaSyntax.kind(node::SyntaxPatternNode) = head(node).kind

JuliaSyntax.build_tree(::Type{SyntaxPatternNode}, stream::JuliaSyntax.ParseStream; kws...) =
    SyntaxPatternNode(JuliaSyntax.build_tree(SyntaxNode, stream; kws...))

function JuliaSyntax.source_location(node::SyntaxPatternNode)
    if is_placeholder(node)
        # TODO: Treat other placeholders.
        # The node contains a `Metavariable`.
        if (has_binding(node.pattern_data))
            source_file = node.pattern_data.binding.source
            byte_idx = node.pattern_data.binding.position
            return JuliaSyntax.source_location(source_file, byte_idx)
        else
            # TODO: Return some position here?
            return (0, 0)
        end
    end
    # The node contains regular `SyntaxData`.
    source_file = node.pattern_data.source
    byte_idx = node.pattern_data.position
    return JuliaSyntax.source_location(source_file, byte_idx)
end

# `Base` overwrites.

function Base.getproperty(node::SyntaxPatternNode, name::Symbol)
    name === :parent && return getfield(node, :parent)
    name === :children && return getfield(node, :children)
    d = getfield(node, :data)
    name === :data && return d
    return getproperty(d, name)
end

function Base.isequal(p1::SyntaxPatternNode, p2::SyntaxPatternNode)
    isequal(p1.data, p2.data) || return false
    (is_leaf(p1) && !is_leaf(p2)) && return false
    is_leaf(p1) && return true  # Both are leaves.
    # Both have children.
    return all(p -> isequal(p[1], p[2]), zip(children(p1), children(p2)))
end

# ------------------------------------------------------------------------------------------
# Utils.

is_directive(node::SyntaxPatternNode) = isa(node.pattern_data, SyntaxPatternDirective)
is_or(node::SyntaxPatternNode) = is_directive(node) && node.directive === :or
is_and(node::SyntaxPatternNode) = is_directive(node) && node.directive === :and

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
    _is_metavariable(node) ||
        error("Trying to get metavariable name from non-Metavariable node")
    # TODO: Error handling for wrong syntax.
    return node.children[2].children[1].data.val
end
_is_metavariable(node::SyntaxPatternNode) = isa(node.data.pattern_data, Metavariable)

@enum SugaredMetavariableRet sugar no_sugar err
function _is_metavariable_sugared(node::JuliaSyntax.SyntaxNode)::SugaredMetavariableRet
    # TODO: Allow %f(x).
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
function placeholders(pattern::SyntaxPatternNode)
    # TODO: Does this make sense to be anything else than a `Metavariable` vector?
    ps = AbstractSyntaxPlaceholder[]
    _placeholders!(pattern, ps)

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

# --------------------------------------------
# Display.

# TODO: Improve these. For now, they are basically copy-pasted from `JuliaSyntax`.

function _show_syntax_pattern_node(io::IO, node::SyntaxPatternNode, indent)
    if is_placeholder(node)
        _show_special_syntax(io, node.pattern_data, indent)
    elseif is_directive(node)
        _show_directive(io, node.pattern_data, indent)
        for c in children(node)
            _show_syntax_pattern_node(io, c, indent * "  ")
        end
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
    elseif is_directive(node)
        print(io, "(directive-", node.pattern_data.directive)
        for c in children(node)
            print(io, ' ')
            _show_syntax_pattern_node_sexpr(io, c)
        end
        print(io, ')')
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
