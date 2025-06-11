# Syntax pattern tree interface
# =============================

## What to add when creating a new pattern form:
##
##
## special-syntax-data.jl
## ----------------------
##
## At/near syntax data structs:
## - struct <FormName>SyntaxData <: AbstractSpecialSyntaxData ... end
## - const PATTERN_FORMS = [..., :<form_name>]
##
## At/near JuliaSyntax overwrites for syntax data:
## - register_kinds() = JS.register_kinds!(Argus, 3, [..., "~<form_name>"])
## - JS.head(data::<FormName>SyntaxData) = JS.SyntaxHead(K"~<form_name>", 0)
## - Base.getproperty(data::<FormName>SyntaxData, name::Symbol) = ...
##    - Note: `data.val` should not error.
##
##
## syntax-pattern-node.jl (this file)
## ----------------------------------
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
##
## syntax-match.jl
## ---------------
##
## In `syntax_match_pattern_form`:
## - `isa(pattern_node.data, <FormName>SyntaxData) && return syntax_match_<form_name>(...)`
##
## - `function syntax_match_<form_name>(...) ... end`

include("special-syntax-data.jl")

"""
    SyntaxPatternNode

Internal type for pattern ASTs. It can hold either `JuliaSyntax.SyntaxData` or
[`AbstractPatternFormSyntaxData`](@ref).
"""
const SyntaxPatternNode = JS.TreeNode{Union{JS.SyntaxData, AbstractPatternFormSyntaxData}}

"""
    SyntaxPatternNode(ex)

Construct a `SyntaxPatternNode` from an `Expr`.

Passes:
1.   Desugar expressions with special syntax: `::Expr -> ::JuliaSyntax.SyntaxNode`.
2.   Parse pattern form nodes: `::JuliaSyntax.SyntaxNode -> ::SyntaxPatternNode`.
3.0. If the pattern contains multiple expressions, wrap them in a `[toplevel]` node.
3.1. Add alternatives for ambiguous nodes (e.g. `=` could be either an assignment or a short
     form function definition) and fix misparsed nodes (e.g. `_x:::identifier = 2` is parsed
     as a short form function definition): `::SyntaxPatternNode -> ::SyntaxPatternNode`
"""
function SyntaxPatternNode(ex)
    syntax_node = desugar_expr(ex)
    syntax_pattern_node = parse_pattern_forms(syntax_node)
    # Parse multiple pattern expressions as toplevel expressions. If the pattern only
    # contains one pattern expression, leave it as it is.
    syntax_pattern_node = parse_multiple_exprs_as_toplevel(syntax_pattern_node)
    # TODO: Check that all repetitions contain pattern variables that don't appear anywhere
    #       else in the pattern.
    return fix_misparsed!(syntax_pattern_node)
end

# Passes
# ------

"""
    desugar_expr(ex)::JuliaSyntax.SyntaxNode

The first pass in the construction of a `SyntaxPatternNode`.

Desugar all special syntax expressions and transform the resulting `Expr` into a
`JuliaSyntax.SyntaxNode`.

There are some special regular Julia syntax cases such as `function f end` and
`function (f) end`:

```
julia> Meta.parse("function f end")
:(function f end)

julia> Meta.parse("function (f) end")
:(function (f,)
      #= none:1 =#
      #= none:1 =#
  end)
```

The user should be able to write patterns to match both these cases. For the first one, it
makes sense for `function _f end` to work. For the second one, `function (_f) end` should
work. What about `function (_f:::expr) end`? Since `function _f end` is much more widely
used than `function (_f) end`, it makes more sense to parse `function (_f:::expr) end` the
same as `function _f end`. Therefore, the `:tuple` head is eliminated in this case.

# Examples
# ========

```
julia> Argus.desugar_expr(:( {x} ))
SyntaxNode:
[call-pre]
  ~                                      :: Identifier
  [call]
    var                                  :: Identifier
    [quote-:]
      x                                  :: Identifier
    [quote-:]
      expr                               :: Identifier


julia> Argus.desugar_expr(:( {_}... ))
SyntaxNode:
[call-pre]
  ~                                      :: Identifier
  [call]
    rep                                  :: Identifier
    [call-pre]
      ~                                  :: Identifier
      [call]
        var                              :: Identifier
        [quote-:]
          _                              :: Identifier
        [quote-:]
          expr                           :: Identifier


julia> Argus.desugar_expr(:( function ({f:::expr}) end ))
SyntaxNode:
[function]
  [call]
    ~                                    :: Identifier
    [call]
      var                                :: Identifier
      [quote-:]
        f                                :: Identifier
      [quote-:]
        expr                             :: Identifier
  [block]


julia> Argus.desugar_expr(:( function ({f}) end ))
SyntaxNode:
[function]
  [tuple-p-,]
    [call-pre]
      ~                                  :: Identifier
      [call]
        var                              :: Identifier
        [quote-:]
          f                              :: Identifier
        [quote-:]
          expr                           :: Identifier
  [block]
```
"""
function desugar_expr(ex)::JS.SyntaxNode
    desugared_ex = _desugar_expr(ex)
    # TODO: Remove this horrible hack!
    #       I did this because I don't understand expression interpolation well enough.
    #       Example problem:
    #
    #       ```
    #       julia> assign_ex = :( b = e );
    #
    #       julia> call_ex = :( f(a, $assign_ex) )
    #       :(f(a, $(Expr(:(=), :b, :e))))
    #
    #       julia> call_ex.args
    #       3-element Vector{Any}:
    #        :f
    #        :a
    #        :(b = e)
    #
    #       julia> JuliaSyntax.parsestmt(SyntaxNode, string(call_ex))
    #       SyntaxNode:
    #       [call]
    #         f                                      :: Identifier
    #         a                                      :: Identifier
    #         [$]
    #           [call]
    #             Expr                               :: Identifier
    #             [quote-:]
    #               =                                :: =
    #             [quote-:]
    #               b                                :: Identifier
    #             [quote-:]
    #               e                                :: Identifier
    #       ```
    #
    #       This is not a very useful example but it shows the problem -- extra nodes!
    #       This could surely be avoided by finding a nicer way to go from `Expr` to
    #       `SyntaxNode`...
    desugared_ex_str = string(desugared_ex)
    regex = r"\$\((?<ex>Expr(?<parens>\(([^()]|(?&parens))*\)))\)"
    for m in eachmatch(regex, desugared_ex_str)
        desugared_ex_str =
            replace(desugared_ex_str, regex => string(eval(Meta.parse(m[:ex]))); count=1)
    end
    return JS.parsestmt(JS.SyntaxNode, desugared_ex_str)
end
function _desugar_expr(ex; inside_fail=false)
    if is_sugared_rep(ex)
        rep_arg = _desugar_expr(_get_sugared_rep_arg(ex); inside_fail)
        return :( ~rep($rep_arg) )
    elseif is_sugared_var(ex) && !inside_fail
        # `<pattern_var>:::<syntax_class>` -> `~var(<pattern_var>, <syntax_class>)`
        #
        # `<pattern_var>`                  -> `~var(<pattern_var>, :expr)`
        #                                  -> Remains the same inside `~fail` conditions.
        id = get_sugared_var_name(ex)
        syntax_class_name = get_sugared_var_syntax_class_name(ex)
        return :( ~var($(QuoteNode(id)), $(QuoteNode(syntax_class_name))) )
    elseif @isexpr(ex, :function) && Meta.isexpr(ex.args[1], :tuple, 1)
        # Remove the extra `:tuple` node in the function signature for expressions like
        # `function (_f:::funcall) _body end`, but not for `function (_f) _body end`.
        fun_ex = ex.args[1].args[1]
        if !is_pattern_variable(fun_ex) && (is_sugared_var(fun_ex) || is_var(fun_ex))
            args = [fun_ex, ex.args[2:end]...]
            return Expr(ex.head, _desugar_expr.(args; inside_fail)...)
        end
    elseif is_fail(ex)
        inside_fail = true
    end
    !isa(ex, Expr) && return ex  # Literals remain the same.
    # Recurse on children.
    return Expr(ex.head, _desugar_expr.(ex.args; inside_fail)...)
end

"""
    parse_pattern_forms(node::JuliaSyntax.SyntaxNode)::SyntaxPatternNode

The second pass in the construction of a `SyntaxPatternNode`.

Parse pattern form `SyntaxNode`s into `SyntaxPatternNode`s with the corresponding pattern
form syntax data. Parse regular `SyntaxNode`s into `SyntaxPatternNode`s with regular
`SyntaxData`.

# Examples
# ========

```
julia> using JuliaSyntax: parsestmt, SyntaxNode

julia> Argus.parse_pattern_forms(parsestmt(SyntaxNode, "~var(:x, :stx_cls)"))
SyntaxPatternNode:
[~var]
  [quote-:]
    x                                    :: Identifier
  [quote-:]
    stx_cls                              :: Identifier

julia> Argus.parse_pattern_forms(parsestmt(SyntaxNode, "~and(~var(:x, :stx_cls), ~fail(x.value != 2, \"not two\"))"))
SyntaxPatternNode:
[~and]
  [~var]
    [quote-:]
      x                                  :: Identifier
    [quote-:]
      stx_cls                            :: Identifier
  [~fail]
    [call-i]
      [.]
        x                                :: Identifier
        value                            :: Identifier
      !=                                 :: Identifier
      2                                  :: Integer
    "not two"                            :: String
```
"""
function parse_pattern_forms(node::JS.SyntaxNode)::SyntaxPatternNode
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
    _parse_pattern_form(node::JuliaSyntax.SyntaxNode)

Construct a `SyntaxPatternNode` from a `SyntaxNode` corresponding to a pattern form node,
which has the following structure:

[call-pre]
  ~
  [call]
    <pattern_form_name>
    <pattern_form_arg>*
"""
function _parse_pattern_form(node::JS.SyntaxNode)::SyntaxPatternNode
    # Extract the name and arguments.
    pattern_form_name = get_pattern_form_name(node)
    pattern_form_arg_nodes = _get_pattern_form_arg_nodes(node)
    pattern_form_args = _get_pattern_form_args(node)
    # Construct a node with the specific special data.
    pattern_data =
        pattern_form_name === :fail ? FailSyntaxData(pattern_form_args...) :
        pattern_form_name === :var  ? VarSyntaxData(pattern_form_args...)  :
        pattern_form_name === :or   ? OrSyntaxData()                       :
        pattern_form_name === :and  ? AndSyntaxData()                      :
        pattern_form_name === :rep  ? RepSyntaxData(pattern_form_args...)  :
        nothing
    # Link the node with its children.
    cs = parse_pattern_forms.(pattern_form_arg_nodes)
    pattern_node = SyntaxPatternNode(nothing,
                                     cs,
                                     pattern_data)
    [c.parent = pattern_node for c in cs]

    return pattern_node
end

"""
    parse_multiple_exprs_as_toplevel(node::SyntaxPatternNode)::SyntaxPatternNode

The first pass for processing a `SyntaxPatternNode` and second-to-last pass in the
`SyntaxPatternNode` construction.

Wrap multiple pattern expressions in a `[toplevel]` node. A block with multiple pattern
expressions is written as a vector of expressions which starts with the symbol
`:pattern_toplevel`. This is how the [`@pattern`](@ref) macro processes its input
expression.

# Examples
# ========

```
julia> Argus.parse_multiple_exprs_as_toplevel(SyntaxPatternNode(:(
           [
               :pattern_toplevel,
               :( {x:::identifier} = {val1} ),
               :( {_}... ),
               :( {x:::identifier} = {val2} )
           ]
       )))
SyntaxPatternNode:
[toplevel]
  [=]
    [~var]
      [quote-:]
        x                                :: Identifier
      [quote-:]
        identifier                       :: Identifier
    [~var]
      [quote-:]
        val1                             :: Identifier
      [quote-:]
        expr                             :: Identifier
  [~rep]
    [~var]
      [quote-:]
        _                                :: Identifier
      [quote-:]
        expr                             :: Identifier
  [=]
    [~var]
      [quote-:]
        x                                :: Identifier
      [quote-:]
        identifier                       :: Identifier
    [~var]
      [quote-:]
        val2                             :: Identifier
      [quote-:]
        expr                             :: Identifier
```
"""
function parse_multiple_exprs_as_toplevel(node::SyntaxPatternNode)::SyntaxPatternNode
    if kind(node) === K"~and" && kind(children(node)[1]) === K"vect"
        # The expression has the following form:
        #
        # :(
        #   ~and(
        #        [
        #         :pattern_toplevel,
        #         <pattern_expr>+
        #        ],
        #        (~fail(<cond>, <msg>))+
        #       )
        #  )
        vec_expr = children(node)[1]
        new_children = [
            parse_multiple_exprs_as_toplevel(vec_expr),
            children(node)[2:end]...
        ]
        return SyntaxPatternNode(nothing, new_children, node.data)
    elseif kind(node) === K"vect"            &&
        kind(children(node)[1]) === K"quote" &&
        children(children(node)[1])[1].data.val === :pattern_toplevel
        # The expression has the following form:
        #
        # :(
        #   [
        #    :pattern_toplevel,
        #    <pattern_expr>+
        #   ]
        #  )
        toplevel_data = update_data_head(node.data, JS.SyntaxHead(K"toplevel", 0))
        new_children = [children(c)[1] for c in children(node)[2:end]]
        return SyntaxPatternNode(nothing, new_children, toplevel_data)
    end
    return node
end

"""
    fix_misparsed!(node::SyntaxPatternNode)::SyntaxPatternNode

The final pass in the construction of a `SyntaxPatternNode`.

Re-parse ambigous nodes or nodes that are misinterpreted in the transformation from
`SyntaxNode`s to `SyntaxPatternNode`s.

Ambiguous cases:
  - `=`: Pass 2 parses assignments to pattern variables as short form function definitions.
         This happens because `:( <pattern_var> = <ex> )` is desugared into a `~var` call.
         However, `<pattern_var>` could either be a short form function definition or a
         variable assignment. This pass replaces ambigous `=` nodes with `~or` pattern forms
         containing both alternatives.

Misparsed cases:
  - `=`: Assignments to pattern variables constrained with `:::identifier` are parsed as
         short form function definitions, but they should be parsed as assignments. This
         pass removes the short form function definition flag from the assignment head node.

# Examples
# ========

```
julia> ambiguous = Argus.parse_pattern_forms(Argus.desugar_expr(:( {x} = 2 )))
SyntaxPatternNode:
[function-=]
  [~var]
    [quote-:]
      x                                  :: Identifier
    [quote-:]
      expr                               :: Identifier
  2                                      :: Integer

julia> Argus.fix_misparsed!(ambiguous)
SyntaxPatternNode:
[~or]
  [=]
    [~var]
      [quote-:]
        x                                :: Identifier
      [quote-:]
        identifier                       :: Identifier
    2                                    :: Integer
  [function-=]
    [~var]
      [quote-:]
        x                                :: Identifier
      [quote-:]
        funcall                          :: Identifier
    2                                    :: Integer

julia> misparsed = Argus.parse_pattern_forms(Argus.desugar_expr(:( {x:::identifier} = 2 )))
SyntaxPatternNode:
[function-=]
  [~var]
    [quote-:]
      x                                  :: Identifier
    [quote-:]
      identifier                         :: Identifier
  2                                      :: Integer

julia> Argus.fix_misparsed!(misparsed)
SyntaxPatternNode:
[=]
  [~var]
    [quote-:]
      x                                  :: Identifier
    [quote-:]
      identifier                         :: Identifier
  2                                      :: Integer
```
"""
function fix_misparsed!(node::SyntaxPatternNode)::SyntaxPatternNode
    if !is_misparsed(node)
        # Recurse on children, if any.
        is_leaf(node) && return node
        new_children = [fix_misparsed!(c) for c in children(node)]
        return SyntaxPatternNode(node.parent, new_children, node.data)
    end

    k = kind(node)
    if k === K"function"
        lhs = fix_misparsed!(node.children[1])
        rhs = fix_misparsed!(node.children[2])
        # If the lhs is constrained to `:identifier` the node should be an assignment,
        # not a function definition.
        get_var_syntax_class_name(lhs) === :identifier &&
            return fundef_to_assign(lhs, rhs, node)
        new_node_data = OrSyntaxData()
        # Use the original pattern form variable name.
        id = get_var_name(lhs)
        # Create alternative for `id:::identifier`.
        id_syntax_pattern_node =
            parse_pattern_forms(JS.parsestmt(JS.SyntaxNode,
                                                      "~var(:$id, :identifier)"))
        assignment_data = update_data_head(node.data, JS.SyntaxHead(K"=", 0))
        assignment_alternative =
            SyntaxPatternNode(nothing, [id_syntax_pattern_node, rhs], assignment_data)
        # Create alternative for `id:::funcall`.
        funcall_syntax_pattern_node =
            parse_pattern_forms(JS.parsestmt(JS.SyntaxNode,
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

    error("no misparse fix for [$(JS.untokenize(head(node)))] node")
end

# JuliaSyntax overwrites
# ----------------------

JS.head(node::SyntaxPatternNode) =
    is_pattern_form(node) ? head(node.data) : head(node.data.raw)
JS.kind(node::SyntaxPatternNode) = head(node).kind

# Utils
# -----

## AST utils

is_symbol_node(node::JS.SyntaxNode) =
    !is_leaf(node)              &&
    kind(node) === K"quote"     &&
    length(children(node)) == 1 &&
    is_identifier(children(node)[1])

### Pass 1 (`Expr` desugaring)

is_pattern_variable(ex) = @isexpr(ex, :braces, 1) && isa(ex.args[1], Symbol)

is_pattern_form(ex) =
    # A pattern form expression has the following structure:
    #   `~<form_name>(<args>*)`
    @isexpr(ex, :call, 2)          &&
    ex.args[1] === :~              &&
    Meta.isexpr(ex.args[2], :call) &&
    get_pattern_form_name(ex) in PATTERN_FORMS

is_var(ex)  = is_pattern_form(ex) && ex.args[2].args[1] === :var
is_fail(ex) = is_pattern_form(ex) && ex.args[2].args[1] === :fail
is_or(ex)   = is_pattern_form(ex) && ex.args[2].args[1] === :or
is_and(ex)  = is_pattern_form(ex) && ex.args[2].args[1] === :and
is_rep(ex)  = is_pattern_form(ex) && ex.args[2].args[1] === :rep

is_sugared_var(ex) =
    is_pattern_variable(ex)               ||
    @isexpr(ex, :braces, 1)               &&
    Meta.isexpr(ex.args[1], :(::), 2)     &&
    isa(ex.args[1].args[2], QuoteNode)    &&
    isa(ex.args[1].args[2].value, Symbol) &&
    isa(ex.args[1].args[1], Symbol)
is_sugared_rep(ex) = @isexpr(ex, :..., 1)

get_pattern_variable_name(ex) = ex.args[1]

get_pattern_form_name(ex) = ex.args[2].args[1]

function get_var_name(ex::Expr)
    var_name_node = ex.args[2].args[2]
    return isa(var_name_node, Symbol) ? var_name_node : var_name_node.value
end
get_sugared_var_name(ex) =  is_pattern_variable(ex) ?
    get_pattern_variable_name(ex)                   :
    ex.args[1].args[1]
get_sugared_var_syntax_class_name(ex) =
    is_pattern_variable(ex) ? :expr : ex.args[1].args[2].value

_get_sugared_rep_arg(ex) = ex.args[1]

### Pass 2 (pattern form parsing)

is_pattern_form(node::SyntaxPatternNode) = isa(node.data, AbstractPatternFormSyntaxData)
function is_pattern_form(node::JS.SyntaxNode)
    # General checks.
    is_leaf(node)                                   && return false
    kind(node) !== K"call"                          && return false
    # Tilda syntax checks.
    tilda_node = node.children[1]
    kind(tilda_node) !== K"Identifier"              && return false
    tilda_node.val !== :~                           && return false
    # The node is a `~` call. It can only be a pattern form node if `~` is followed
    # directly by a pattern form name. Otherwise, `~` is used with its regular meaning.
    length(node.children) != 2                      && return false
    kind(node.children[2]) !== K"call"              && return false
    pattern_form_name = get_pattern_form_name(node)
    isnothing(pattern_form_name)                    && return false
    return pattern_form_name in PATTERN_FORMS
end

get_pattern_form_name(node::JS.SyntaxNode)::Symbol = node.children[2].children[1].val
function _get_pattern_form_arg_nodes(node::JS.SyntaxNode)
    name = get_pattern_form_name(node)
    args = node.children[2].children[2:end]
    name === :var  && return args
    name === :fail && return [_strip_quote_node(args[1]), _strip_string_node(args[2])]
    name === :or   && return _strip_quote_node.(args)
    name === :and  && return _strip_quote_node.(args)
    name === :rep  && return args
    error("Trying to extract argument nodes for unimplemented pattern form ~$name.")
end
function _get_pattern_form_args(node::JS.SyntaxNode)
    name = get_pattern_form_name(node)
    arg_nodes = node.children[2].children[2:end]
    name === :var  && return _get_var_arg_names(arg_nodes)
    name === :fail && return [JS.to_expr(arg_nodes[1]),
                              _strip_string_node(arg_nodes[2]).data.val]
    name === :or   && return []
    name === :and  && return []
    name === :rep  && return arg_nodes
    error("Trying to extract arguments for unimplemented pattern form ~$name.")
end

_strip_quote_node(node::JS.SyntaxNode) =
    kind(node) === K"quote" ? node.children[1] : node
_strip_string_node(node::JS.SyntaxNode) =
    kind(node) === K"string" ? node.children[1] : node

### Pass 3 (misparse fix)

function is_misparsed(node::SyntaxPatternNode)
    if kind(node) === K"function"
        lhs = node.children[1]
        is_var(lhs) || return false
        syntax_class_name = get_var_syntax_class_name(lhs)
        # An `=` node constrained to `:identifier` should be interpreted as an assignment,
        # not a function definition.
        syntax_class_name === :identifier && return true
        # An `=` node constrained to `:funcall` should indeed be interpreted as a function
        # definition.
        syntax_class_name === :funcall && return false
        # Any other constraint on the lhs causes makes the `=` node ambiguous.
        return true
    end
    # Add other ambiguous cases here.

    return false
end

is_short_form_function_def(node) =
    JS.flags(node) === JS.SHORT_FORM_FUNCTION_FLAG

"""
    fundef_to_assign(lhs::SyntaxPatternNode,
                     rhs::SyntaxPatternNode,
                     old_node::SyntaxPatternNode)

Create a new `SyntaxPatternNode` using `old_node`s parent, `lhs` and `rhs`. Use the
`old_node`'s data but replace the head with an `=` head.
"""
function fundef_to_assign(lhs::SyntaxPatternNode,
                          rhs::SyntaxPatternNode,
                          old_node::SyntaxPatternNode)
    old_data = old_node.data
    new_node_data = update_data_head(old_data, JS.SyntaxHead(K"=", 0))

    return SyntaxPatternNode(old_node.parent, [lhs, rhs], new_node_data)
end

"""
    update_data_head(old_data::JuliaSyntax.SyntaxData, new_head::JuliaSyntax.SyntaxHead)

Create a new `SyntaxData` using the `old_data`'s green node with the head replaced by
`new_head`.

This operation is not entirely correct because changing the head should most likely change
the green node as well (e.g. span). However, the green node is not really relevant so this
does not impact match correctness. Ideally, patterns would have location information, which
would make the green nodes relevant.
"""
function update_data_head(old_data::JS.SyntaxData, new_head::JS.SyntaxHead)
    old_raw = old_data.raw
    new_raw = JS.GreenNode(
        new_head,
        old_raw.span,
        old_raw.children
    )

    return JS.SyntaxData(
        old_data.source,
        new_raw,
        old_data.position,
        old_data.val
    )
end

## Pattern form utils

### Predicates

is_var(node::JS.SyntaxNode) =
    is_pattern_form(node) && get_pattern_form_name(node) === :var
is_rep(node::JS.SyntaxNode) =
    is_pattern_form(node) && get_pattern_form_name(node) === :rep

is_var(node::SyntaxPatternNode) = isa(node.data, VarSyntaxData)
is_fail(node::SyntaxPatternNode) = isa(node.data, FailSyntaxData)
is_or(node::SyntaxPatternNode) = isa(node.data, OrSyntaxData)
is_and(node::SyntaxPatternNode) = isa(node.data, AndSyntaxData)
is_rep(node::SyntaxPatternNode) = isa(node.data, RepSyntaxData)

### Getters

get_var_name(node::SyntaxPatternNode) = is_var(node) ? node.data.var_name : nothing
get_var_name(node::JS.SyntaxNode) =
    is_var(node) ? node.children[2].children[2].children[1].val : nothing
get_var_syntax_class_name(node::SyntaxPatternNode) =
    is_var(node) ? node.data.syntax_class_name : nothing
function _get_var_arg_names(args::Vector{JS.SyntaxNode})
    arg_names = Symbol[]
    for c in args
        cs = children(c)
        is_symbol_node(c) ||
            throw(SyntaxError("""
                              invalid pattern form argument `$c`
                              `~var` pattern form arguments should be `Symbol`s."""))
        push!(arg_names, c.children[1].val)
    end

    return arg_names
end

get_fail_condition(node::SyntaxPatternNode) = is_fail(node) ? node.data.condition : nothing
get_fail_message(node::SyntaxPatternNode) = is_fail(node) ? node.data.message : nothing

_get_rep_arg(node::SyntaxPatternNode) = is_rep(node) ? node.children[1] : nothing
get_rep_vars(node::SyntaxPatternNode) = is_rep(node) ? node.data.rep_vars : nothing

# Display
# -------

using JuliaSyntax: untokenize, leaf_string, is_error
function _show_syntax_node(io, node::SyntaxPatternNode, indent)
    nodestr = is_leaf(node) ? leaf_string(node) : "[$(untokenize(head(node)))]"
    treestr = string(indent, nodestr)
    if is_leaf(node)
        treestr = rpad(treestr, 40) * " :: " * string(kind(node))
    end
    print(io, treestr)
    if !is_leaf(node)
        new_indent = indent * "  "
        for n in children(node)
            println(io)
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
Base.show(io::IO, ::MIME"text/x.sexpression", node::SyntaxPatternNode; show_kind=false) =
    _show_syntax_node_sexpr(io, node, show_kind)
Base.show(io::IO, node::SyntaxPatternNode) = _show_syntax_node_sexpr(io, node, false)
Base.show(io::IO, ::Type{SyntaxPatternNode}) = print(io, "SyntaxPatternNode")
