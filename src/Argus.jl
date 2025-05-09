module Argus

export SyntaxPatternNode, Pattern, SyntaxClass, @syntax_class

using JuliaSyntax: JuliaSyntax, head, kind, @K_str,
    children, is_leaf, is_identifier, source_location

include("syntax-pattern-node.jl")
include("pattern.jl")
include("syntax-class.jl")

function __init__()
    _register_kinds()
end

end  # Argus
