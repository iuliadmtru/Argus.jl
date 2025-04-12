using Test
using Argus
using JuliaSyntax
using JuliaSyntax: children, head, kind, source_location, is_leaf

include("syntax_placeholders.jl")
include("syntax_pattern_tree.jl")
include("rules.jl")
include("matching.jl")
