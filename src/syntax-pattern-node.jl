# ------------------------------------------------------------------------------------------
# Syntax pattern tree interface.

# -------------------------------------------------------------------
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
[`syntax_class_registry_check`](@ref) function is called. I.e. forward references are
allowed as long as the "promise" to define missing syntax classes is fulfilled when checking
the registry for consistency.
"""
struct VarSyntaxData <: AbstractSpecialSyntaxData
    id::Symbol
    syntax_class_name::Symbol
end

struct OrSyntaxData <: AbstractSpecialSyntaxData end

# TODO: Pattern forms registry? Or remove.
const PATTERN_FORMS = [:var, :or]

## `JuliaSyntax` overwrites and utils.

"""
Register new syntax kinds for pattern forms.
"""
_register_kinds() = JuliaSyntax.register_kinds!(Argus, 3, ["~var", "~or"])
_register_kinds()

JuliaSyntax.head(data::VarSyntaxData) = JuliaSyntax.SyntaxHead(K"~var", 0)
JuliaSyntax.head(data::OrSyntaxData) = JuliaSyntax.SyntaxHead(K"~or", 0)

## `Base` overwrites.

Base.getproperty(data::VarSyntaxData, name::Symbol) =
    name === :id                ? getfield(data, :id)                :
    name === :syntax_class_name ? getfield(data, :syntax_class_name) :
    name === :val               ? nothing                            :
    getfield(data, name)

Base.getproperty(data::OrSyntaxData, name::Symbol) =
    name === :val ? nothing : getfield(data, name)

# -------------------------------------------------------------------
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
    pattern_data = node.data
    is_leaf(node) && return SyntaxPatternNode(nothing, nothing, pattern_data)
    cs = [SyntaxPatternNode(c) for c in children(node)]
    # Link the node with its children.
    pattern_node = SyntaxPatternNode(nothing, cs, pattern_data)
    for c in cs
        c.parent = pattern_node
        # If the node is an assignment to a pattern form don't interpret it as a short form
        # function call. Interpret it as a plain `K"="` node instead.
        if is_pattern_form(c) &&
            JuliaSyntax.flags(pattern_node) === JuliaSyntax.SHORT_FORM_FUNCTION_FLAG
            c.parent.data =
                _update_data_head(pattern_data, JuliaSyntax.SyntaxHead(K"=", 0))
        end
    end

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
    pattern_form_name = _pattern_form_name(node)
    pattern_form_args = _pattern_form_args(node)
    # Construct a node with the specific special data.
    cs = SyntaxPatternNode.(pattern_form_args)
    pattern_data =
        pattern_form_name === :var ? VarSyntaxData(_var_arg_names(pattern_form_args)...) :
        pattern_form_name === :or  ? OrSyntaxData()                                      :
        nothing
    # Link the node with its children.
    pattern_node = SyntaxPatternNode(nothing,
                                     cs,
                                     pattern_data)
    [c.parent = pattern_node for c in cs]

    return pattern_node
end

## -------------------------------------------
## `JuliaSyntax` overwrites.

JuliaSyntax.head(node::SyntaxPatternNode) =
    is_pattern_form(node) ? head(node.data) : head(node.data.raw)
JuliaSyntax.kind(node::SyntaxPatternNode) = head(node).kind

## -------------------------------------------
## Utils.

### AST checking and manipulation.

is_symbol_node(node::JuliaSyntax.SyntaxNode) =
    !is_leaf(node) &&
    kind(node) === K"quote" &&
    length(children(node)) == 1 &&
    is_identifier(children(node)[1])

is_pattern_form(node::SyntaxPatternNode) = isa(node.data, AbstractSpecialSyntaxData)
function is_pattern_form(node::JuliaSyntax.SyntaxNode)
    # General checks.
    is_leaf(node) && return false
    kind(node) !== K"call" && return false
    # Tilda syntax checks.
    tilda_node = node.children[1]
    kind(tilda_node) !== K"Identifier" && return false
    tilda_node.val !== :~ && return false
    # The node is a `~` call. It can only be a pattern form node if `~` is followed
    # directly by a pattern form name. Otherwise, `~` is used with its regular meaning.
    length(node.children) != 2 && return false
    kind(node.children[2]) !== K"call" && return false
    pattern_form_name = _pattern_form_name(node)
    isnothing(pattern_form_name) && return false
    return pattern_form_name in PATTERN_FORMS
end

function _update_data_head(old_data::JuliaSyntax.SyntaxData,
                           new_head::JuliaSyntax.SyntaxHead)
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

    return new_data
end

_strip_quote_node(node::JuliaSyntax.SyntaxNode) =
    kind(node) === K"quote" ? node.children[1] : node

### Pattern form utils.

_pattern_form_name(node::JuliaSyntax.SyntaxNode)::Symbol = node.children[2].children[1].val
function _pattern_form_args(node::JuliaSyntax.SyntaxNode)
    name = _pattern_form_name(node)
    args = node.children[2].children[2:end]
    name === :var && return args
    name === :or && return _strip_quote_node.(args)
    error("Trying to extract args for unimplemented pattern form $name")
end

function _var_arg_names(args::Vector{JuliaSyntax.SyntaxNode})
    arg_names = Symbol[]
    for c in args
        # TODO: Throw and catch error.
        cs = children(c)
        is_symbol_node(c) ||
            error("Invalid pattern form argument `$c` at $(source_location(c))\n",
                  "`~var` pattern form arguments should be `Symbol`s.")
        push!(arg_names, c.children[1].val)
    end

    return arg_names
end

## -------------------------------------------
## Display.

using JuliaSyntax: untokenize, leaf_string, is_error
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
