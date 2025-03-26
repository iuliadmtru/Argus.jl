module Argus

export RuleSyntaxData, Metavariable, RuleSyntaxNode
export Pattern
export RuleMatch, RuleMatches

using JuliaSyntax
# TODO: Reorder these, maybe remove some.
using JuliaSyntax: haschildren, children, is_trivia, head, kind,
                   source_location, untokenize, is_error, is_valid_identifier

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

## `JuliaSyntax` overwrites.

JuliaSyntax.head(node::RuleSyntaxNode) = is_special_syntax(node.data) ? nothing : head(node.data.syntax_data.raw)
JuliaSyntax.kind(node::RuleSyntaxNode) = head(node).kind

function JuliaSyntax.build_tree(::Type{RuleSyntaxNode}, stream::JuliaSyntax.ParseStream; kws...)
    return RuleSyntaxNode(JuliaSyntax.build_tree(SyntaxNode, stream; kws...))
end

## Utils.

function has_special_syntax(node::RuleSyntaxNode)
    is_special_syntax(node.data) && return true
    # If it is not a special sytax node and it has no children then
    # it is a regular leaf
    !haschildren(node) && return false
    # If any child has some special syntax then the node has special syntax.
    any(c -> has_special_syntax(c), children(node)) && return true
    # No child has special syntax.
    return false
end

## Display.

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


include("rule_syntax.jl")
include("matching.jl")


end # Argus
