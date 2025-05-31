struct Pattern
    ast::SyntaxPatternNode
    fail_conditions::Vector{Function}
end

macro pattern(expr)
    pattern_macro_err =
        """
        Invalid `@pattern` syntax:
        ```
        $(Meta.quot(expr))
        ```

        Patterns should be created in one of the following ways:

        `@pattern <expr>`

        or

        ```
        @pattern begin
            <expr>
            (@fail <condition> <message>)*
        end
        ```
        """

    expr = MacroTools.striplines(expr)
    @isexpr(expr, :quote, 1) && Meta.isexpr(expr.args[1], :block) &&
        error(pattern_macro_err)
    # Here, `expr` is one of the following:
    #   - `<atom>`                   -- e.g. `2`
    #   - `:($(QuoteNode(<atom>)))`  -- e.g. `:( _x )`
    #   - `quote <expr>* end`        -- syntax for patterns with fail conditions
    #   - `<expr>`                   -- other expressions such as
    #                                   `~var(<var_name>, <syntax_class_name>)`

    pattern_expr =
        @isexpr(expr, :quote) || @isexpr(expr, :block) ? expr.args[1] :
        isa(expr, QuoteNode)                           ? expr.value   :
        expr
    # `pattern_expr` is one of the following:
    #   - <atom>  -- from either of the first two `expr` possibilities
    #   - <expr>  -- the first `<expr>` from the third `expr` possibility or
    #                the last `expr` possibility.
    fail_conditions = Function[]
    if @isexpr(expr, :block)
        length(expr.args) == 1 && is_fail_macro(expr.args[1]) &&
            error("Invalid `@pattern` syntax.\n",
                  "The first expression cannot be a fail condition.")
        if length(expr.args) > 1
            # The pattern has fail conditions. The user pattern syntax should be:
            #   ```
            #   @pattern begin
            #       <pattern_expr>
            #       @fail <condition> <msg>
            #       (@fail <condition> <msg>)*
            #   end
            #   ```

            # Turn `@fail` macros into `~fail` expressions.
            fail_exprs = Expr[]
            for fail_macro in expr.args[2:end]
                is_fail_macro(fail_macro) || error(pattern_macro_err)
                condition_expr = fail_macro.args[end-1]
                message = fail_macro.args[end]
                push!(fail_exprs, :( ~fail($condition_expr, $message) ))
                # Add the fail condition functions to the pattern.
                #
                # TODO: This is really not necessary.
                push!(fail_conditions, fail_condition(condition_expr))
            end
            # Create the pattern as an `~and` between the pattern expression and the
            # fail conditions.
            pattern_expr = :( ~and($pattern_expr, $(fail_exprs...)) )
        end
    end
    pattern_node = SyntaxPatternNode(pattern_expr)
    return :( Pattern($pattern_node, $fail_conditions) )
end

# Utils.

# TODO: Why do macrocalls have 1 extra child?
is_fail_macro(ex) = @isexpr(ex, :macrocall, 4) && ex.args[1] === Symbol("@fail")

function cannot_eval_to_Pattern(ex)
    isa(ex, Symbol) && return false
    @isexpr(ex, :macrocall, 3) && ex.args[1] === Symbol("@pattern") && return false
    @isexpr(ex, :call, 3) && ex.args[1] === :Pattern && return false
    return true
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
