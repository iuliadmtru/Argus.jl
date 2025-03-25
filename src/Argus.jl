module Argus

export Pattern, PatternRegex
export RuleMatch, RuleMatches

using JuliaSyntax: TreeNode, SyntaxNode, AbstractSyntaxData, SyntaxData
using JuliaSyntax: parseatom, parsestmt, parseall
using JuliaSyntax: source_location

include("utils.jl")


#=
    Rule AST interface
=#

struct RuleSyntaxData <: AbstractSyntaxData
    raw::GreenNode{SyntaxData}
    val::Any
end

const RuleSyntaxNode = TreeNode{RuleSyntaxData}


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
function Pattern(text::String)
    # TODO
end

# struct PatternRegex <: AbstractRule
#     regex::Regex
# end
# PatternRegex(regex_str::String) = PatternRegex(Regex(regex_str))


#=
    Matches
=#

struct RuleMatch
    # text::String
    ast::SyntaxNode
    source_location # Change to byte range? Keep both?
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


#=
    Checks
=#

# TODO: Rename.
function check(rule::AbstractRule, filename::String)::RuleMatches
    # src = read(filename, String)

    # # Obtain ASTs.
    # rule_ast = rule.ast
    # source_ast = parseall(SyntaxNode, src; filename=filename)

    # # Compare ASTs.
    # matches = search_ast(rule_ast, source_ast)

    # return matches
end

# function check(rule::PatternRegex, filename::String)::RuleMatches
#     src = read(filename, String)

#     # Get regex matches.
#     regex_matches, regex_ranges = ([m.match for m in eachmatch(rule.regex, src)], [m.offset:(m.offset + length(m.match)) for m in eachmatch(rule.regex, src)])
#     println(regex_matches)
#     println(regex_ranges)

#     # Find nodes at the matches' ranges.

#     return RuleMatches()
# end

end # Argus
