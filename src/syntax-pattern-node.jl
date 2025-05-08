# ------------------------------------------------------------------------------------------
# Syntax pattern tree interface.

# --------------------------------------------
# Special syntax data.

abstract type AbstractSpecialSyntaxData end

struct VarSyntaxData <: AbstractSpecialSyntaxData
    id::Symbol
    syntax_class_name::Symbol
end

# TODO: Pattern forms registry?
const PATTERN_FORMS = [:var]

# --------------------------------------------
# Syntax node.

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
    cs = [SyntaxPatternNode(c) for c in node.children]
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
    pattern_form_args = [c.val for c in node.children[2].children[2:end]]
    # Construct a node with the specific special data.
    pattern_form_name === :var &&
        return SyntaxPatternNode(nothing, nothing, VarSyntaxData(pattern_form_args...))
    # Something is fishy with the pattern.
    # TODO: Throw and catch?
    error("Invalid pattern form syntax at $(source_location(node))")
end


## Utils.

function is_pattern_form(node::JuliaSyntax.SyntaxNode)
    # General checks.
    is_leaf(node) && return false
    kind(node) !== K"call" && return false
    # Tilda syntax checks.
    tilda_node = node.children[1]
    kind(tilda_node) !== K"Identifier" && return false
    tilda_node.val !== :~ && return false
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

Base.show(io::IO, ::Type{SyntaxPatternNode}) = print(io, "SyntaxPatternNode")
