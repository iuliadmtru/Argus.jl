module Argus

using JuliaSyntax: JuliaSyntax, head, kind, @K_str,
    children, is_leaf, is_identifier, source_location,
    @isexpr

# Part 1: Pattern matching mechanism.

export SyntaxPatternNode, Pattern, @pattern, SyntaxClass, @syntax_class
export register_syntax_class!, syntax_class_registry_check
export Binding, BindingSet, MatchFail
export syntax_match

include("syntax-pattern-node.jl")
include("pattern.jl")
include("syntax-class.jl")
include("bindings.jl")
include("syntax-match.jl")

# Part 2: Rule writing mechanism.

export Rule, @rule, RuleGroup, @define_rule_in_group, register_rule!
export RuleMatchResult, rule_match

include("rules.jl")

function __init__()
    _register_kinds()
    _register_syntax_classes()
end

end  # Argus
