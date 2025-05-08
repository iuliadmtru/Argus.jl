module Argus

export SyntaxPatternNode, Pattern, SyntaxClass

using JuliaSyntax: JuliaSyntax, head, kind, @K_str,
    children, is_leaf, leaf_string, is_error, source_location

include("syntax-pattern-node.jl")
include("pattern.jl")
include("syntax-class.jl")

end  # Argus
