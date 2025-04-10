module Argus

## -----------------------------------------------------------------------------------------

## Types

export SyntaxTemplateNode, SyntaxTemplateData
export AbstractSyntaxPlaceholder, Metavariable
export SyntaxMatch, SyntaxMatches
export RuleGroup

## Utils
export @define_rule, @define_rule_in_group, define_rule, define_rule_in_group,
    active_rule_groups, activate_rule_group
export is_placeholder, placeholder, contains_placeholders, placeholders,
    placeholder_fill!, placeholder_unbind!, placeholders_unbind!,
    has_binding, set_binding!
export rule_match!
export template_compare!, template_match!

## -------------------------------------------

using JuliaSyntax
using JuliaSyntax: is_leaf, children, head, kind, source_location, untokenize, is_error
using JuliaSyntax: @isexpr

using MacroTools: MacroTools, striplines

## -------------------------------------------

include("syntax_placeholders.jl")
include("syntax_template_tree.jl")
include("rules.jl")
include("syntax_match.jl")

## -----------------------------------------------------------------------------------------

end  # Argus
