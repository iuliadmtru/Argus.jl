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
    templ::SyntaxTemplateNode
end
# TODO: Remove qualifier from `parsestmt`?
Pattern(text::String) = Pattern(JuliaSyntax.parsestmt(SyntaxTemplateNode, text))

## -------------------------------------------

## Pattern matching.

"""
    pattern_match!(pattern::AbstractSyntaxPattern, src_file::AbstractString)::SyntaxMatches

Try to match the given pattern to the source code contained in `src_file`. Return all
matches as a `SyntaxMatches` array.
"""
function pattern_match!(pattern::Pattern, src_file::AbstractString)::SyntaxMatches
    src_txt = read(src_file, String)
    # Obtain template and source AST.
    templ = pattern.templ
    src = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src_txt; filename=src_file)
    # Obtain and return matches.
    return template_match!(templ, src)
end
