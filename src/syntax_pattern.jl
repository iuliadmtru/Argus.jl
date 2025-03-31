"""
    AbstractSyntaxPattern

Abstract supertype for all patterns.
"""
abstract type AbstractSyntaxPattern end

"""
    Pattern <: AbstractSyntaxPattern

Basic pattern that searches for code matching its expression.
"""
struct Pattern <: AbstractSyntaxPattern
    ast::SyntaxTemplateNode
end
# TODO: Remove qualifier from `parsestmt`?
Pattern(text::String) = Pattern(JuliaSyntax.parsestmt(SyntaxTemplateNode, text))
