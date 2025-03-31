"""
    AbstractPattern

Abstract supertype for all patterns.
"""
abstract type AbstractPattern end

"""
    Pattern <: AbstractPattern

Basic pattern that searches for code matching its expression.
"""
struct Pattern <: AbstractPattern
    ast::SyntaxTemplateNode
end
# TODO: Remove qualifier from `parsestmt`?
Pattern(text::String) = Pattern(JuliaSyntax.parsestmt(SyntaxTemplateNode, text))

