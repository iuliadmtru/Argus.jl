module Argus

## -----------------------------------------------------------------------------------------

## Types

export SyntaxPatternNode, SyntaxPatternData
export AbstractSyntaxPlaceholder, Metavariable
export SyntaxMatch, SyntaxMatches
export RuleGroup

export @rule, create_rule, @define_rule_in_group, define_rule_in_group

## Utils
export register_rule!
export pattern_match!, pattern_compare!

## -------------------------------------------

using JuliaSyntax
using JuliaSyntax: is_leaf, children, head, kind, source_location
using JuliaSyntax: untokenize, is_error, is_trivia
using JuliaSyntax: @isexpr

using MacroTools: MacroTools, striplines

## -------------------------------------------

include("syntax_placeholders.jl")
include("syntax_pattern_tree.jl")
include("rules.jl")
include("pattern_match.jl")
include("syntax_match.jl")

## -----------------------------------------------------------------------------------------

end  # Argus
