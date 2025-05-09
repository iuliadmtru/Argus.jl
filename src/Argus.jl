module Argus

export SyntaxPatternNode, Pattern, SyntaxClass, @syntax_class,
    Binding, BindingSet, MatchFail
export syntax_match

using JuliaSyntax: JuliaSyntax, head, kind, @K_str,
    children, is_leaf, is_identifier, source_location

include("syntax-pattern-node.jl")
include("pattern.jl")
include("syntax-class.jl")
include("bindings.jl")
include("syntax-match.jl")

function __init__()
    _register_kinds()
end

end  # Argus
