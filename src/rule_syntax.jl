#=
    Rules
=#

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

# struct PatternRegex <: AbstractRule
#     regex::Regex
# end
# PatternRegex(regex_str::String) = PatternRegex(Regex(regex_str))


#=
    Matches
=#

struct RuleMatch
    # text::String
    ast::JuliaSyntax.SyntaxNode
    source_location # Change to byte range? Keep both? Is this field necessary? A: It depends on what the `ast` above is.
end
RuleMatch(ast::SyntaxNode) = RuleMatch(ast, source_location(ast))

struct RuleMatches <: AbstractVector{RuleMatch}
    matches::AbstractVector{RuleMatch}
end
RuleMatches() = RuleMatches(RuleMatch[])

Base.size(v::RuleMatches) = size(v.matches)
Base.getindex(v::RuleMatches, i::Int) = v.matches[i]
Base.getindex(v::RuleMatches, r::UnitRange) = RuleMatches(view(v.matches, r))
Base.setindex!(v::RuleMatches, el::RuleMatch, i::Int) = RuleMatches(setindex!(v.matches, el, i))
Base.push!(v::RuleMatches, el::RuleMatch) = RuleMatches(push!(v.matches, el))
Base.pushfirst!(v::RuleMatches, el::RuleMatch) = RuleMatches(pushfirst!(v.matches, el))

