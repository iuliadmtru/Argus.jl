module Argus

export RuleSyntaxData, Metavariable, RuleSyntaxNode
export Pattern
export RuleMatch, RuleMatches

using JuliaSyntax
# TODO: Reorder these, maybe remove some.
using JuliaSyntax: haschildren, children, is_trivia, head, kind,
                   source_location, untokenize, is_error

include("pattern_syntax.jl")


#=
    Rule AST interface
=#

const RuleSyntaxNode = JuliaSyntax.TreeNode{RuleSyntaxData}
function RuleSyntaxNode(node::JuliaSyntax.SyntaxNode)
    data = is_metavariable(node) ? RuleSyntaxData(nothing, Metavariable(get_metavar_name(node))) : RuleSyntaxData(node.data, nothing)

    if !haschildren(node)
        return RuleSyntaxNode(nothing, nothing, data)
    else
        children = [RuleSyntaxNode(c) for c in node.children]
        rs_node = RuleSyntaxNode(nothing, children, data)
        [child.parent = rs_node for child in children]

        return rs_node
    end
end

JuliaSyntax.head(node::RuleSyntaxNode) = is_special_syntax(node.data) ? nothing : head(node.data.syntax_data.raw)
JuliaSyntax.kind(node::RuleSyntaxNode) = head(node).kind

function JuliaSyntax.build_tree(::Type{RuleSyntaxNode}, stream::JuliaSyntax.ParseStream; kws...)
    return RuleSyntaxNode(JuliaSyntax.build_tree(SyntaxNode, stream; kws...))
end

function _show_rule_syntax_node(io::IO, node::RuleSyntaxNode, indent)
    if is_special_syntax(node.data)
        _show_special_syntax(io, node.data, indent)
    else
        # TODO: Change `posstr` to something useful.
        posstr = "$(lpad("-", 4)):$(rpad("-", 3))|"
        val = node.val
        nodestr = haschildren(node) ? "[$(untokenize(head(node)))]" :
            isa(val, Symbol)  ? string(val)                   : repr(val)
        treestr = string(indent, nodestr)
        println(io, posstr, treestr)
        if haschildren(node)
            new_indent = indent * "  "
            for c in children(node)
                _show_rule_syntax_node(io, c, new_indent)
            end
        end
    end
end

function _show_rule_syntax_node_sexpr(io, node::RuleSyntaxNode)
    if is_special_syntax(node.data)
        _show_special_syntax_sexpr(io, node.data)
    else
        if !haschildren(node)
            if is_error(node)
                print(io, "(", untokenize(head(node)), ")")
            else
                val = node.val
                print(io, val isa Symbol ? string(val) : repr(val))
            end
        else
            print(io, "(", untokenize(head(node)))
            first = true
            for n in children(node)
                print(io, ' ')
                _show_rule_syntax_node_sexpr(io, n)
                first = false
            end
            print(io, ')')
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::RuleSyntaxNode)
    println(io, "line:col│ tree")
    _show_rule_syntax_node(io, node, "")
end

function Base.show(io::IO, ::MIME"text/x.sexpression", node::RuleSyntaxNode)
    _show_rule_syntax_node_sexpr(io, node)
end

function Base.show(io::IO, node::RuleSyntaxNode)
    _show_rule_syntax_node_sexpr(io, node)
end


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


#=
    Checks
=#

include("utils.jl")

# TODO: Rename.
function check(rule::Pattern, filename::String)::RuleMatches
    src = read(filename, String)

    # Obtain ASTs.
    rule_ast = rule.ast
    source_ast = parseall(SyntaxNode, src; filename=filename)

    # Compare ASTs.
    matches = search_ast(rule_ast, source_ast)

    return matches
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
