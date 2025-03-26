"""
    AbstractRule

Abstract supertype for all rules.
"""
abstract type AbstractRule end

"""
    Pattern <: AbstractRule

Rule that searches for code matching its expression.
"""
struct Pattern <: AbstractRule
    ast::RuleSyntaxNode
end
Pattern(text::String) = Pattern(JuliaSyntax.parsestmt(RuleSyntaxNode, text))

