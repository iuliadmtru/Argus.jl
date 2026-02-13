# Hashed syntax nodes
# ===================

# HashSyntaxData
# --------------

"""
    HashSyntaxData <: JuliaSyntax.AbstractSyntaxData

The same as `JuliaSyntax.SyntaxData`, with an extra field for the node's hash.
"""
struct HashSyntaxData <: JS.AbstractSyntaxData
    source::JS.SourceFile
    raw::JS.GreenNode{JS.SyntaxHead}
    position::Int  # TODO: Update in newer JuliaSyntax versions.
    val::Any
    hash::UInt64
end
HashSyntaxData(data::JS.SyntaxData, h::UInt64) = HashSyntaxData(data.source,
                                                                data.raw,
                                                                data.position,
                                                                data.val,
                                                                h)
HashSyntaxData(data::HashSyntaxData, h::UInt64) = HashSyntaxData(data.source,
                                                                data.raw,
                                                                data.position,
                                                                data.val,
                                                                h)

# Base overwrites

Base.copy(data::HashSyntaxData) =
    HashSyntaxData(data.source, data.raw, data.position, data.val, data.hash)

Base.hash(data::HashSyntaxData) = data.hash

# HashSyntaxNode
# --------------

"""
    HashSyntaxNode <: JS.TreeNode{HashSyntaxData}

The same as `JuliaSyntax.SyntaxNode`, with [`HashSyntaxData`](@ref) instead of
`JuliaSyntax.SyntaxData`.
"""
const HashSyntaxNode = JS.TreeNode{HashSyntaxData}
function HashSyntaxNode(node::JS.SyntaxNode)
    data = node.data
    hash_data = HashSyntaxData(data.source, data.raw, data.position, data.val, hash(node))

    if is_leaf(node)
        return HashSyntaxNode(nothing, nothing, hash_data)
    else
        hash_children = HashSyntaxNode.(node.children)
        hash_node = HashSyntaxNode(nothing, hash_children, hash_data)
        for hash_c in hash_children
            hash_c.parent = hash_node
        end

        return hash_node
    end
end
function _HashSyntaxNode(parent::Union{Nothing, HashSyntaxNode},
                         children::Union{Nothing, Vector{HashSyntaxNode}},
                         data::HashSyntaxData)
    # Hash according to `Base.hash(node::TreeNode, h::UInt)`.
    # https://github.com/JuliaLang/JuliaSyntax.jl/blob/20fde7451edd6d0e37c5fb66e3a0a9f17c158a6c/src/porcelain/syntax_tree.jl#L21-L32
    h = data.hash
    if children === nothing
        return hash(nothing, h)
    else
        for child in children
            h = hash(child, h)
        end
    end
    new_data = HashSyntaxData(data.source, data.raw, data.position, data.val, h)

    return HashSyntaxNode(parent, children, new_data)
end

# Base overwrites

Base.hash(node::HashSyntaxNode) = node.data.hash

# JuliaSyntax overwrites.

function JS.SyntaxNode(hash_node::HashSyntaxNode)
    hash_data = hash_node.data
    data =
        JS.SyntaxData(hash_data.source, hash_data.raw, hash_data.position, hash_data.val)

    if is_leaf(hash_node)
        return JS.SyntaxNode(nothing, nothing, data)
    else
        cs = JS.SyntaxNode.(hash_node.children)
        node = JS.SyntaxNode(nothing, cs, data)
        for c in cs
            c.parent = node
        end

        return node
    end
end

# Necessary for `show`.
JS._expr_leaf_val(node::HashSyntaxNode) = node.val
