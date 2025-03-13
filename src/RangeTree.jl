using AbstractTrees

struct RangeTree <: AbstractNode{T}
    interval::UnitRange # Should keep `low` and `high` instead?
    children::AbstractVector{RangeTree}
    max::Int
    value::T
end
children(tree::RangeTree) = tree.children
