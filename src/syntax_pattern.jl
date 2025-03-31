"""
    AbstractSyntaxPattern

Abstract supertype for all patterns.
"""
abstract type AbstractSyntaxPattern end

"""
    pattern_match!(pattern:AbstractSyntaxPattern, src::JuliaSyntax.SyntaxNode)::SyntaxMatches

Try to match the given pattern to the source AST `src`. Return all matches as a
`SyntaxMatches` array.
"""
function pattern_match!(p::AbstractSyntaxPattern, src::JuliaSyntax.SyntaxNode)::SyntaxMatches
end

"""
    pattern_match!(pattern::AbstractSyntaxPattern, src_file::AbstractString)::SyntaxMatches

Try to match the given pattern to the source code contained in `src_file`. Return all
matches as a `SyntaxMatches` array.
"""
function pattern_match!(p::AbstractSyntaxPattern, file::AbstractString)::SyntaxMatches end

## -------------------------------------------

"""
    Pattern <: AbstractSyntaxPattern

Basic pattern that searches for code matching its expression.
"""
struct Pattern <: AbstractSyntaxPattern
    template::SyntaxTemplateNode
end
# TODO: Remove qualifier from `parsestmt`?
Pattern(text::String) = Pattern(JuliaSyntax.parsestmt(SyntaxTemplateNode, text))

pattern_match!(pattern::Pattern, src::JuliaSyntax.SyntaxNode)::SyntaxMatches =
    template_match!(pattern.template, src)

function pattern_match!(pattern::Pattern, src_file::AbstractString)::SyntaxMatches
    src_txt = read(src_file, String)
    src = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src_txt; filename=src_file)

    return pattern_match!(pattern, src)
end
