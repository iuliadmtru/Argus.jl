module Argus

export Pattern, RuleMatch, RuleMatches

using JuliaSyntax: SyntaxNode
using JuliaSyntax: parseatom, parsestmt, parseall
using JuliaSyntax: source_location

include("utils.jl")

abstract type AbstractRule end

struct Pattern <: AbstractRule
    ast::SyntaxNode
end
Pattern(text::String; filename="") = Pattern(parseall(SyntaxNode, text; filename=filename).children[1])


struct RuleMatch
    # text::String
    ast::SyntaxNode
    source_location
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


# TODO: Rename.
function check(rule::AbstractRule, filename::String)::RuleMatches
    src = read(filename, String)

    # Obtain ASTs.
    rule_ast = rule.ast
    source_ast = parseall(SyntaxNode, src; filename=filename)

    # Compare ASTs.
    matches = search_ast(rule_ast, source_ast)
    # println(matches)

    return matches
end

end # Argus