using Test
using Argus
using JuliaSyntax
using JuliaSyntax: source_location, kind, @K_str, is_leaf, ParseError, flags

include("bindings.jl")
include("pattern.jl")
include("syntax-class.jl")
include("rules.jl")
