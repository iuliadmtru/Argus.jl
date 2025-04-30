module Argus

export SyntaxPatternNode, SyntaxPatternData
export AbstractSyntaxPlaceholder, Metavariable
export SyntaxMatch, SyntaxMatches
export Rule, RuleGroup

export @rule, @define_rule_in_group, define_rule_in_group

export register_rule!
export rule_match!, pattern_match!, pattern_compare!

using JuliaSyntax
using JuliaSyntax: is_leaf, children, head, kind, source_location
using JuliaSyntax: untokenize, is_error, is_trivia
using JuliaSyntax: @isexpr

using MacroTools: MacroTools, striplines

include("syntax_placeholders.jl")
include("syntax_pattern_tree.jl")
include("rules.jl")
include("matching.jl")

end  # Argus
