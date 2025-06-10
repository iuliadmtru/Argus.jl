module Argus

import MacroTools

using JuliaSyntax: JuliaSyntax, head, kind, @K_str,
    children, is_leaf, is_identifier, source_location,
    @isexpr

const JS = JuliaSyntax


# Part 1: Syntax matching mechanism
# =================================

export SyntaxPatternNode, Pattern, @pattern, SyntaxClass, @syntax_class, SyntaxError
export SyntaxClassRegistry, @define_syntax_class, @define_syntax_class_in_registry,
    register_syntax_class!, SyntaxClassRegistryKeyError
export Binding, BindingSet, BindingFieldError, BindingSetKeyError, MatchFail, MatchError
export syntax_match

include("syntax-pattern-node.jl")
include("pattern.jl")
include("syntax-class.jl")
include("bindings.jl")
include("syntax-match.jl")

# Part 2: Rule writing mechanism
# ==============================

export Rule, @rule, RuleGroup, @define_rule_in_group, register_rule!
export RuleMatchResult, rule_match, RuleGroupMatchResult, rule_group_match

include("rules.jl")


function __init__()
    _register_kinds()
    _register_syntax_classes()
end

end  # Argus
