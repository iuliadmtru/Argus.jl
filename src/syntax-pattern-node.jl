# ------------------------------------------------------------------------------------------
# Syntax pattern tree interface.

## What to add when creating a new pattern form:
##
## -------------------------------------------
## special-syntax-data.jl
##
## At/near syntax data structs:
## - struct <FormName>SyntaxData <: AbstractSpecialSyntaxData ... end
## - const PATTERN_FORMS = [..., :<form_name>]
##
## At/near `JuliaSyntax` overwrites for syntax data:
## - register_kinds() = JuliaSyntax.register_kinds!(Argus, 3, [..., "~<form_name>"])
## - JuliaSyntax.head(data::<FormName>SyntaxData) = JuliaSyntax.SyntaxHead(K"~<form_name>", 0)
## - Base.getproperty(data::<FormName>SyntaxData, name::Symbol) = ...
##    - Note: `data.val` should not error.
##
## -------------------------------------------
## syntax-pattern-node.jl (this file)
##
## At/near AST pass utils:
## - `is_<form_name>(node::SyntaxPatternNode) = isa(node.data, <FormName>SyntaxData)`
##
## In `parse_pattern_form`:
## - `... pattern_form_name === :<form_name>  ? <FormName>SyntaxData(...) : ...`
##
## In `_pattern_form_args` and `_pattern_form_arg_names`:
## - `name === :<form_name> && return ...
##
## -------------------------------------------
## syntax-match.jl
##
## In `syntax_match_pattern_form`:
## - `isa(pattern_node.data, <FormName>SyntaxData) && return syntax_match_<form_name>(...)`
##
## - `function syntax_match_<form_name>(...) ... end`

include("special-syntax-data.jl")

"""
    SyntaxPatternNode

Internal type for pattern ASTs. It can hold either `JuliaSyntax.SyntaxData` or
[`AbstractSpecialSyntaxData`](@ref).
"""
const SyntaxPatternNode =
    JuliaSyntax.TreeNode{Union{JuliaSyntax.SyntaxData, AbstractSpecialSyntaxData}}

"""
    SyntaxPatternNode(ex::Expr)

Construct a `SyntaxPatternNode` from an `Expr`.

Passes:
1. Desugar special syntax nodes: `::Expr -> ::JuliaSyntax.SyntaxNode`.
2. Parse pattern form nodes: `::JuliaSyntax.SyntaxNode -> ::SyntaxPatternNode`.
3. Add alternatives for ambiguous nodes (e.g. `=` could be either an assignment or a short
   form function definition): `::SyntaxPatternNode -> ::SyntaxPatternNode`.
"""
function SyntaxPatternNode(ex)
    syntax_node = desugar_expr(ex)
    syntax_pattern_node = parse_pattern_forms(syntax_node)
    return disambiguate!(syntax_pattern_node)
end

## Passes.

"""
    desugar_expr(ex)::JuliaSyntax.SyntaxNode

The first pass in the construction of a `SyntaxPatternNode`.
"""
function desugar_expr(ex)::JuliaSyntax.SyntaxNode
    desugared_ex = _desugar_expr(ex)
    return JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, string(desugared_ex))
end
function _desugar_expr(ex)
    is_invalid_sugared_var_form(ex) &&
        error("Invalid constraint syntax: $ex\n",
              "Pattern variable constraints should follow the syntax: ",
              "<pattern_variable>:::<syntax_class>")
    if is_sugared_var_form(ex)
        id = _get_sugared_var_id(ex)
        syntax_class_name = _get_sugared_var_syntax_class_name(ex)
        return :( ~var($(QuoteNode(id)), $(QuoteNode(syntax_class_name))) )
    end
    !isa(ex, Expr) && return ex
    return Expr(ex.head, _desugar_expr.(ex.args)...)
end

"""
    parse_pattern_forms(node::JuliaSyntax.SyntaxNode)::SyntaxPatternNode

The second pass in the construction of a `SyntaxPatternNode`.
"""
function parse_pattern_forms(node::JuliaSyntax.SyntaxNode)::SyntaxPatternNode
    is_pattern_form(node) && return _parse_pattern_form(node)
    # Regular syntax node.
    pattern_data = node.data
    is_leaf(node) && return SyntaxPatternNode(nothing, nothing, pattern_data)
    cs = [parse_pattern_forms(c) for c in children(node)]
    # Link the node with its children.
    pattern_node = SyntaxPatternNode(nothing, cs, pattern_data)
    [c.parent = pattern_node for c in cs]

    return pattern_node
end
"""
Construct a `SyntaxPatternNode` from a `SyntaxNode` corresponding to a pattern form node,
which has the following structure:

[call-pre]
  ~
  [call]
    <pattern_form_name>
    <pattern_form_arg>*
"""
function _parse_pattern_form(node::JuliaSyntax.SyntaxNode)
    # Extract the name and arguments.
    pattern_form_name = _pattern_form_name(node)
    pattern_form_arg_nodes = _pattern_form_arg_nodes(node)
    pattern_form_args = _pattern_form_args(node)
    # Construct a node with the specific special data.
    cs = parse_pattern_forms.(pattern_form_arg_nodes)
    pattern_data =
        pattern_form_name === :fail ? FailSyntaxData(pattern_form_args...) :
        pattern_form_name === :var  ? VarSyntaxData(pattern_form_args...)  :
        pattern_form_name === :or   ? OrSyntaxData()                       :
        pattern_form_name === :and  ? AndSyntaxData()                      :
        nothing
    # Link the node with its children.
    pattern_node = SyntaxPatternNode(nothing,
                                     cs,
                                     pattern_data)
    [c.parent = pattern_node for c in cs]

    return pattern_node
end

"""
    disambiguate!(node::SyntaxPatternNode)::SyntaxPatternNode

The third and final pass in the construction of a `SyntaxPatternNode`.
"""
function disambiguate!(node::SyntaxPatternNode)::SyntaxPatternNode
    if !is_ambiguous(node)
        # Recurse on children, if any.
        is_leaf(node) && return node
        [disambiguate!(c) for c in children(node)]
        return node
    end

    k = kind(node)
    if k === K"function"
        lhs = node.children[1]
        rhs = node.children[2]
        # If the lhs is constrained to `:identifier`, the node should be an assignment,
        # not a function definition.
        _get_var_syntax_class_name(lhs) === :identifier &&
            return funcall_to_assign(lhs, rhs, node)
        new_node_data = OrSyntaxData()
        # Use the original pattern form variable name.
        id = _get_var_id(lhs)
        # Create alternative for `id:::identifier`.
        id_syntax_pattern_node =
            parse_pattern_forms(JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode,
                                                      "~var(:$id, :identifier)"))
        assignment_data = update_data_head(node.data, JuliaSyntax.SyntaxHead(K"=", 0))
        assignment_alternative =
            SyntaxPatternNode(nothing, [id_syntax_pattern_node, rhs], assignment_data)
        # Create alternative for `id:::funcall`.
        funcall_syntax_pattern_node =
            parse_pattern_forms(JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode,
                                                      "~var(:$id, :funcall)"))
        funcall_alternative =
            SyntaxPatternNode(nothing, [funcall_syntax_pattern_node, rhs], node.data)
        # Link the alternatives to the new `~or` pattern form node.
        cs = [assignment_alternative, funcall_alternative]
        new_node = SyntaxPatternNode(node.parent,
                                    [assignment_alternative, funcall_alternative],
                                    new_node_data)
        assignment_alternative.parent = new_node
        funcall_alternative.parent = new_node
        # Return the `~or` node.
        return new_node
    end

    error("No disambiguation method for [$(JuliaSyntax.untokenize(head(node)))] node")
end

## -------------------------------------------
## `JuliaSyntax` overwrites.

JuliaSyntax.head(node::SyntaxPatternNode) =
    is_pattern_form(node) ? head(node.data) : head(node.data.raw)
JuliaSyntax.kind(node::SyntaxPatternNode) = head(node).kind

## -------------------------------------------
## Utils.

### AST utils.

is_symbol_node(node::JuliaSyntax.SyntaxNode) =
    !is_leaf(node) &&
    kind(node) === K"quote" &&
    length(children(node)) == 1 &&
    is_identifier(children(node)[1])

#### Pass 1 (`Expr` desugaring).

is_pattern_variable(ex) = isa(ex, Symbol) && startswith(string(ex), "_")
is_sugared_var_form(ex) =
    is_pattern_variable(ex)         ||
    @isexpr(ex, :(::), 2)           &&
    is_pattern_variable(ex.args[1]) &&
    isa(ex.args[2], QuoteNode)      &&
    isa(ex.args[2].value, Symbol)
function is_invalid_sugared_var_form(ex)
    is_triple_colon =
        @isexpr(ex, :(::), 2)           &&
        isa(ex.args[2], QuoteNode)      &&
        isa(ex.args[2].value, Symbol)
    is_invalid_triple_colon =
        is_triple_colon                 &&
        !is_pattern_variable(ex.args[1])
    is_double_colon =
        @isexpr(ex, :(::), 2)           &&
        is_pattern_variable(ex.args[1]) &&
        isa(ex.args[2], Symbol)
    is_colon =
        @isexpr(ex, :call, 3)           &&
        ex.args[1] === :(:)             &&
        is_pattern_variable(ex.args[2]) &&
        isa(ex.args[3], Symbol)

    # TODO: Special error handling for invalid triple colon.
    return is_invalid_triple_colon || is_double_colon || is_colon
end

_get_sugared_var_id(ex) = isa(ex, Expr) ? ex.args[1] : ex
_get_sugared_var_syntax_class_name(ex) = isa(ex, Expr) ? ex.args[2].value : :expr

#### Pass 2 (pattern form parsing).

is_pattern_form(node::SyntaxPatternNode) = isa(node.data, AbstractSpecialSyntaxData)
function is_pattern_form(node::JuliaSyntax.SyntaxNode)
    # General checks.
    is_leaf(node) && return false
    kind(node) !== K"call" && return false
    # Tilda syntax checks.
    tilda_node = node.children[1]
    kind(tilda_node) !== K"Identifier" && return false
    tilda_node.val !== :~ && return false
    # The node is a `~` call. It can only be a pattern form node if `~` is followed
    # directly by a pattern form name. Otherwise, `~` is used with its regular meaning.
    length(node.children) != 2 && return false
    kind(node.children[2]) !== K"call" && return false
    pattern_form_name = _pattern_form_name(node)
    isnothing(pattern_form_name) && return false
    return pattern_form_name in PATTERN_FORMS
end

_pattern_form_name(node::JuliaSyntax.SyntaxNode)::Symbol = node.children[2].children[1].val
function _pattern_form_arg_nodes(node::JuliaSyntax.SyntaxNode)
    name = _pattern_form_name(node)
    args = node.children[2].children[2:end]
    name === :fail && return [_strip_quote_node(args[1]), _strip_string_node(args[2])]
    name === :var && return args
    name === :or && return _strip_quote_node.(args)
    name === :and && return _strip_quote_node.(args)
    error("Trying to extract argument nodes for unimplemented pattern form ~$name")
end
function _pattern_form_args(node::JuliaSyntax.SyntaxNode)
    name = _pattern_form_name(node)
    arg_nodes = node.children[2].children[2:end]
    name === :fail && return [JuliaSyntax.to_expr(arg_nodes[1]),
                              _strip_string_node(arg_nodes[2]).data.val]
    name === :var && return _var_arg_names(arg_nodes)
    name === :or && return []
    name === :and && return []
    error("Trying to extract arguments for unimplemented pattern form ~$name")
end

_strip_quote_node(node::JuliaSyntax.SyntaxNode) =
    kind(node) === K"quote" ? node.children[1] : node
_strip_string_node(node::JuliaSyntax.SyntaxNode) =
    kind(node) === K"string" ? node.children[1] : node

#### Pass 3 (disambiguation).

function update_data_head(old_data::JuliaSyntax.SyntaxData,
                          new_head::JuliaSyntax.SyntaxHead)
    old_raw = old_data.raw
    new_raw = JuliaSyntax.GreenNode(
        new_head,
        old_raw.span,
        old_raw.children
    )
    new_data = JuliaSyntax.SyntaxData(
        old_data.source,
        new_raw,
        old_data.position,
        old_data.val
    )

    return new_data
end

function is_ambiguous(node::SyntaxPatternNode)
    if kind(node) === K"function"
        JuliaSyntax.flags(node) === JuliaSyntax.SHORT_FORM_FUNCTION_FLAG || return false
        lhs = node.children[1]
        is_var(lhs) || return false
        # An `=` node with unconstrained lhs could either be an assignment or a short form
        # function definition.
        # An `=` node constrained to `:identifier` should be interpreted as an assignment,
        # not a function definition
        syntax_class_name = _get_var_syntax_class_name(lhs)
        return syntax_class_name === :expr || syntax_class_name === :identifier
    end
    # Add other ambiguous cases here.

    return false
end

function funcall_to_assign(lhs::SyntaxPatternNode,
                           rhs::SyntaxPatternNode,
                           old_node::SyntaxPatternNode)
    old_data = old_node.data
    new_node_data = update_data_head(old_data, JuliaSyntax.SyntaxHead(K"=", 0))

    return SyntaxPatternNode(old_node.parent, [lhs, rhs], new_node_data)
end

### Pattern form utils.

#### Predicates.

is_fail(node::SyntaxPatternNode) = isa(node.data, FailSyntaxData)
is_var(node::SyntaxPatternNode) = isa(node.data, VarSyntaxData)
is_or(node::SyntaxPatternNode) = isa(node.data, OrSyntaxData)
is_and(node::SyntaxPatternNode) = isa(node.data, AndSyntaxData)

#### Getters.

function _get_fail_condition(node::SyntaxPatternNode)::Union{Nothing, Function}
    isa(node.data, FailSyntaxData) || return nothing
    return node.data.condition
end
function _get_fail_message(node::SyntaxPatternNode)::Union{Nothing, String}
    isa(node.data, FailSyntaxData) || return nothing
    return node.data.message
end

function _get_var_id(node::SyntaxPatternNode)::Union{Nothing, Symbol}
    isa(node.data, VarSyntaxData) || return nothing
    return node.data.id
end
function _get_var_syntax_class_name(node::SyntaxPatternNode)::Union{Nothing, Symbol}
    isa(node.data, VarSyntaxData) || return nothing
    return node.data.syntax_class_name
end
function _var_arg_names(args::Vector{JuliaSyntax.SyntaxNode})
    arg_names = Symbol[]
    for c in args
        # TODO: Throw and catch error.
        cs = children(c)
        is_symbol_node(c) ||
            error("Invalid pattern form argument `$c` at $(source_location(c))\n",
                  "`~var` pattern form arguments should be `Symbol`s.")
        push!(arg_names, c.children[1].val)
    end

    return arg_names
end

## -------------------------------------------
## Display.

using JuliaSyntax: untokenize, leaf_string, is_error
function _show_syntax_node(io, node::SyntaxPatternNode, indent)
    nodestr = is_leaf(node) ? leaf_string(node) : "[$(untokenize(head(node)))]"
    treestr = string(indent, nodestr)
    if is_leaf(node)
        treestr = rpad(treestr, 40) * " :: " * string(kind(node))
    end
    println(io, treestr)
    if !is_leaf(node)
        new_indent = indent * "  "
        for n in children(node)
            _show_syntax_node(io, n, new_indent)
        end
    end
end

function _show_syntax_node_sexpr(io, node::SyntaxPatternNode, show_kind)
    if is_leaf(node)
        if is_error(node)
            print(io, "(", untokenize(head(node)), ")")
        else
            print(io, leaf_string(node))
            if show_kind
                print(io, "::", kind(node))
            end
        end
    else
        print(io, "(", untokenize(head(node)))
        first = true
        for n in children(node)
            print(io, ' ')
            _show_syntax_node_sexpr(io, n, show_kind)
            first = false
        end
        print(io, ')')
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::SyntaxPatternNode)
    println(io, "SyntaxPatternNode:")
    _show_syntax_node(io, node, "")
end
function Base.show(io::IO, ::MIME"text/x.sexpression", node::SyntaxPatternNode; show_kind=false)
    _show_syntax_node_sexpr(io, node, show_kind)
end
function Base.show(io::IO, node::SyntaxPatternNode)
    _show_syntax_node_sexpr(io, node, false)
end
Base.show(io::IO, ::Type{SyntaxPatternNode}) = print(io, "SyntaxPatternNode")
