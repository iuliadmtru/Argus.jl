# ------------------------------------------------------------------------------------------
# Syntax pattern tree interface.

# --------------------------------------------
# Special syntax data.

"""
    AbstractSpecialSyntaxData

Supertype for all special syntax data such as pattern forms.
"""
abstract type AbstractSpecialSyntaxData end

"""
    VarSyntaxData

Data for a `~var` pattern form holding an id name and a [`SyntaxClass`](@ref) name. The
latter is a name expected to be found in the syntax class registry (TODO: docs) when the
[`registry_check`](@ref) function is called. I.e. forward references are allowed as long as
the "promise" to define missing syntax classes is fulfilled when checking the registry for
consistency.
"""
struct VarSyntaxData <: AbstractSpecialSyntaxData
    id::Symbol
    syntax_class_name::Symbol
end

# TODO: Pattern forms registry? Or remove.
const PATTERN_FORMS = [:var]

## `JuliaSyntax` overwrites and utils.

"""
Register new syntax kinds for pattern forms.
"""
_register_kinds() = JuliaSyntax.register_kinds!(Argus, 3, ["~var"])
_register_kinds()

JuliaSyntax.head(data::VarSyntaxData) = JuliaSyntax.SyntaxHead(K"~var", 0)

## `Base` overwrites.

Base.getproperty(data::VarSyntaxData, name::Symbol) =
    name === :id                ? getfield(data, :id)                :
    name === :syntax_class_name ? getfield(data, :syntax_class_name) :
    nothing

# --------------------------------------------
# Syntax node.

"""
    SyntaxPatternNode

Internal type for pattern ASTs. It can hold either `JuliaSyntax.SyntaxData` or
[`AbstractSpecialSyntaxData`](@ref).
"""
const SyntaxPatternNode =
    JuliaSyntax.TreeNode{Union{JuliaSyntax.SyntaxData, AbstractSpecialSyntaxData}}

"""
    SyntaxPatternNode(node::JuliaSyntax.SyntaxNode)

Construct a `SyntaxPatternNode` from a `SyntaxNode` in two passes. First, desugar any
sugared special syntax nodes such as [pattern form]() nodes with `~` syntax. Then, traverse
the resulting syntax tree and create `SyntaxPatternNode`s with `SyntaxData` for regular
nodes and with `Data <: AbstractSpecialSyntaxData` for special syntax nodes.
"""
function SyntaxPatternNode(node::JuliaSyntax.SyntaxNode)
    is_pattern_form(node) && return SyntaxPatternNode_pattern_form(node)
    # Regular syntax node.
    is_leaf(node) && return SyntaxPatternNode(nothing, nothing, node.data)
    cs = [SyntaxPatternNode(c) for c in children(node)]
    pattern_node = SyntaxPatternNode(nothing, cs, node.data)
    [c.parent = pattern_node for c in cs]
    return pattern_node
end
"""
    SyntaxPatternNode(ex::Expr)

Construct a `SyntaxPatternNode` corresponding to `ex`.
"""
function SyntaxPatternNode(ex::Expr)
    node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, string(ex))
    node = kind(node) === K"toplevel" ? node.children[1] : node

    return SyntaxPatternNode(node)
end

"""
    SyntaxPatternNode_pattern_form(node::JuliaSyntax.SyntaxNode)

Construct a `SyntaxPatternNode` from a `SyntaxNode` corresponding to a pattern form node,
which has the following structure:

[call-pre]
  ~
  [call]
    <pattern_form_name>
    <pattern_form_arg>*
"""
function SyntaxPatternNode_pattern_form(node::JuliaSyntax.SyntaxNode)
    # Extract the name and arguments.
    pattern_form_name = node.children[2].children[1].val
    pattern_form_args = node.children[2].children[2:end]
    pattern_form_arg_names = [c.val for c in pattern_form_args]
    # Construct a node with the specific special data.
    cs = SyntaxPatternNode.(pattern_form_args)
    pattern_data =
        pattern_form_name === :var ? VarSyntaxData(pattern_form_arg_names...) : nothing
    pattern_node = SyntaxPatternNode(nothing,
                                     cs,
                                     pattern_data)
    [c.parent = pattern_node for c in cs]
    return pattern_node
end

## `JuliaSyntax` overwrites.

JuliaSyntax.head(node::SyntaxPatternNode) =
    is_special_syntax(node) ? head(node.data) : head(node.data.raw)
JuliaSyntax.kind(node::SyntaxPatternNode) = head(node).kind

## Utils.

# TODO: Remove this?
is_special_syntax(node::SyntaxPatternNode) = is_pattern_form(node)

is_pattern_form(node::SyntaxPatternNode) = isa(node.data, AbstractSpecialSyntaxData)
function is_pattern_form(node::JuliaSyntax.SyntaxNode)
    # General checks.
    is_leaf(node) && return false
    kind(node) !== K"call" && return false
    # Tilda syntax checks.
    tilda_node = node.children[1]
    kind(tilda_node) !== K"Identifier" && return false
    tilda_node.val !== :~ && return false
    # TODO: Change syntax in order to not have conflicts?
    if length(node.children) != 2 || kind(node.children[2]) !== K"call"
        # TODO: Extract warning somewhere.
        @warn "Pattern syntax contains `~` not followed by valid pattern form syntax"
        return false
    end
    pattern_form_name = node.children[2].children[1].val
    if isnothing(pattern_form_name)
        @warn "Pattern syntax contains `~` not followed by valid pattern form syntax"
        return false
    end
    # Pattern form checks.
    if !(pattern_form_name in PATTERN_FORMS)
        @warn "Pattern syntax contains `~` not followed by an existing pattern form name"
        return false
    end
    return true
end

## Display.

using JuliaSyntax: untokenize
function _show_syntax_node(io, node::SyntaxPatternNode, indent)
    nodestr = is_leaf(node) ? leaf_string(node) : "[$(untokenize(head(node)))]"
    treestr = string(indent, nodestr)
    if is_leaf(node)
        treestr = rpad(treestr, 40) * " :: " * string(kind(node))
    end
    println(io, treestr)
    if !is_leaf(node)
        new_indent = indent * "  "
        for n in children(node)
            _show_syntax_node(io, n, new_indent)
        end
    end
end

function _show_syntax_node_sexpr(io, node::SyntaxPatternNode, show_kind)
    if is_leaf(node)
        if is_error(node)
            print(io, "(", untokenize(head(node)), ")")
        else
            print(io, leaf_string(node))
            if show_kind
                print(io, "::", kind(node))
            end
        end
    else
        print(io, "(", untokenize(head(node)))
        first = true
        for n in children(node)
            print(io, ' ')
            _show_syntax_node_sexpr(io, n, show_kind)
            first = false
        end
        print(io, ')')
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::SyntaxPatternNode)
    println(io, "SyntaxPatternNode:")
    _show_syntax_node(io, node, "")
end
function Base.show(io::IO, ::MIME"text/x.sexpression", node::SyntaxPatternNode; show_kind=false)
    _show_syntax_node_sexpr(io, node, show_kind)
end
function Base.show(io::IO, node::SyntaxPatternNode)
    _show_syntax_node_sexpr(io, node, false)
end
Base.show(io::IO, ::Type{SyntaxPatternNode}) = print(io, "SyntaxPatternNode")
