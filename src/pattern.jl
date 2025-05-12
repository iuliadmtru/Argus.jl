# TODO: Support for fail conditions.
struct Pattern
    ast::SyntaxPatternNode
    fail_conditions::Vector{Function}
end

# TODO: Remove.
function Pattern(ex)
    return Pattern(SyntaxPatternNode(ex), Function[])
end

macro pattern(ex)
    # TODO: Create a pattern using the default constructor. Build the arguments here.
    return :( Pattern($ex) )
end

# Display.

function _show_var_node(node::SyntaxPatternNode)
    id = _get_var_id(node)
    syntax_class_name = _get_var_syntax_class_name(node)

    return string(id, ":::", syntax_class_name)
end

function _show_pattern_syntax_node(io::IO, node::SyntaxPatternNode, indent)
    nodestr =
        is_leaf(node) ? leaf_string(node)    :
        is_var(node)  ? _show_var_node(node) :
        "[$(untokenize(head(node)))]"
    treestr = string(indent, nodestr)
    if is_leaf(node) || is_var(node)
        treestr = rpad(treestr, 40) * " :: " * string(kind(node))
    end
    println(io, treestr)
    if !is_leaf(node) && !is_var(node)
        new_indent = indent * "  "
        for n in children(node)
            _show_pattern_syntax_node(io, n, new_indent)
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", pattern::Pattern)
    println(io, "Pattern:")
    _show_pattern_syntax_node(io, pattern.ast, "")
end
function Base.show(io::IO, ::MIME"text/x.sexpression", pattern::Pattern; show_kind=false)
    _show_syntax_node_sexpr(io, pattern.ast, show_kind)
end
function Base.show(io::IO, pattern::Pattern)
    _show_syntax_node_sexpr(io, pattern.ast, false)
end
