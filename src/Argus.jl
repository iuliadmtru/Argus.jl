module Argus

import MacroTools
using RuntimeGeneratedFunctions
RuntimeGeneratedFunctions.init(@__MODULE__)

using JuliaSyntax: JuliaSyntax, head, kind, @K_str,
    children, is_leaf, is_identifier, source_location,
    @isexpr

const JS = JuliaSyntax


# Part 1: Syntax matching mechanism
# =================================

export SyntaxPatternNode, Pattern, @pattern, Template, @template,
    SyntaxClass, @syntax_class, SyntaxError
export SyntaxClassRegistry, @define_syntax_class, @define_syntax_class_in_registry,
    register_syntax_class!, SyntaxClassRegistryKeyError
export Binding, BindingSet, BindingFieldError, BindingSetKeyError
export syntax_match, partial_syntax_match, syntax_match_all, is_successful,
    MatchFail, MatchError, MatchResults
export expand

include("syntax-pattern-node.jl")
include("pattern.jl")
include("template.jl")
include("syntax-class.jl")
include("bindings.jl")
include("syntax-match.jl")
include("expand.jl")

# Part 2: Rule writing mechanism
# ==============================

export Rule, @rule, RuleHook, @define_rule_hook, register_rule_hook!,
    RuleGroup, @define_rule_in_group, register_rule!
export rule_match, rules_match, rule_group_match, RuleMatchResult, RuleGroupMatchResult
export RuleDisabler, CommentDisabler

include("rules.jl")


function __init__()
    _register_kinds()
    _register_syntax_classes()
end

end  # Argus
