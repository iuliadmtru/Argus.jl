struct Pattern
    ast::SyntaxPatternNode
end

"""
    @pattern(expr)

Create a `Pattern` from the given expression.

# Examples

julia> @pattern a + b  # Simple expression with no pattern variables.
Pattern:
[call-i]
  a                                      :: Identifier
  +                                      :: Identifier
  b                                      :: Identifier


julia> assign_to_x = @pattern begin
           _x:::assign                                   # Pattern variable matching an assignment.
           @fail _x.__lhs.name == "x" "assignment to x"  # The matching fails if the rhs variable's name is "x".
       end
Pattern:
[~and]
  _x:::assign                            :: ~var
  [~fail]
    [call-i]
      [.]
        [.]
          _x                             :: Identifier
          __lhs                          :: Identifier
        name                             :: Identifier
      ==                                 :: Identifier
      [string]
        "x"                              :: String
    "assignment to x"                    :: String


julia> syntax_match(assign_to_x, JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "x = 2"))
MatchFail("assignment to x")

julia> pattern = @pattern begin
           _a:::identifier = _
           _a:::identifier = _  # Two consecutive assignments to the same variable.
       end
Pattern:
[toplevel]
  [=]
    _a:::identifier                      :: ~var
    _:::expr                             :: ~var
  [=]
    _a:::identifier                      :: ~var
    _:::expr                             :: ~var


julia> syntax_match(pattern, parseall(SyntaxNode,
                                      """
                                      x = 2
                                      x = 3
                                      """))

BindingSet with 1 entry:
  :_a => Binding(:_a, x, BindingSet(:__id=>Binding(:__id, x, BindingSet())))
"""
macro pattern(expr)
    # Error messages.
    err_msg_general =
        """
        invalid `@pattern` syntax
        Patterns should be created in one of the following ways:
         -- `@pattern <expr>`
         -- ```
        |   @pattern begin
        |       <expr>+
        |       (@fail <condition> <message>)*
        |   end
         -- ```"""
    err_msg_should_not_be_fail =
        """
        invalid `@pattern` syntax
        The first expression cannot be a fail condition."""
    err_msg_should_be_fail =
        """
        invalid `@pattern` syntax
        Pattern expressions and fail conditions cannot be intercalated."""

    @isexpr(expr, :quote) &&
        throw(SyntaxError(err_msg_general, __source__.file, __source__.line))
    # Here, `expr` is one of the following:
    #   - `<atom>`                   -- e.g. `2`
    #   - `:($(QuoteNode(<atom>)))`  -- e.g. `:( _x )`
    #   - `quote <expr>* end`        -- syntax for patterns with fail conditions
    #   - `<expr>`                   -- other expressions such as
    #                                   `~var(<var_name>, <syntax_class_name>)`
    pattern_expr =  # The first pattern expression.
        @isexpr(expr, :block) ? expr.args[2] :  # Skip `LineNumberNode`.
        expr
    # `pattern_expr` is one of the following:
    #   - <atom>  -- from either of the first two `expr` possibilities
    #   - <expr>  -- the first `<expr>` from the third `expr` possibility or
    #                the last `expr` possibility.
    if @isexpr(expr, :block)
        first_expr_line_number = expr.args[1]
        length(expr.args) == 2 && is_fail_macro(expr.args[2]) &&
            throw(SyntaxError(err_msg_should_not_be_fail,
                              first_expr_line_number.file,
                              first_expr_line_number.line))
        # The pattern has at least one pattern expression.
        #
        #   ```
        #   @pattern begin
        #       <pattern_expr>
        #       <pattern_expr>*
        #       (@fail <condition> <msg>)*
        #   end
        #   ```
        idx = 4  # Index of expressions inside the macro body, skipping `LineNumberNode`s.
        toplevel_children = [pattern_expr]
        while idx <= length(expr.args) && !is_fail_macro(expr.args[idx])
            # The pattern has more than one pattern expression.
            #
            #   ```
            #   @pattern begin
            #       <pattern_expr>
            #       <pattern_expr>
            #       <pattern_expr>*
            #       (@fail <condition> <msg>)*
            #   end
            #   ```
            #
            # Pattern expressions should be wrapped in a `toplevel` node.
            push!(toplevel_children, expr.args[idx])
            idx += 2
        end
        if length(toplevel_children) > 1
            pattern_expr = :( [$(toplevel_children...)] )
        end
        # All expressions from `idx` onwards, if any, should be fail conditions.
        if idx <= length(expr.args)
            # Turn `@fail` macros into `~fail` expressions.
            # Skip `LineNumberNode`s when iterating but include them in the error message.
            fail_exprs = Expr[]
            for (line_number_idx, fail_macro) in zip(Iterators.countfrom(idx - 1, 2),
                                                     expr.args[idx:2:end])
                # Pattern expressions and fail conditions cannot be intercalated.
                fail_macro_line_number = expr.args[line_number_idx]
                is_fail_macro(fail_macro) ||
                    throw(SyntaxError(err_msg_should_be_fail,
                                      fail_macro_line_number.file,
                                      fail_macro_line_number.line))
                condition_expr = fail_macro.args[3]
                message = fail_macro.args[4]
                push!(fail_exprs, :( ~fail($condition_expr, $message) ))
            end
            # Create the pattern as an `~and` between the pattern expression and the
            # fail conditions.
            pattern_expr = :( ~and($pattern_expr, $(fail_exprs...)) )
        end
    end
    pattern_node = SyntaxPatternNode(pattern_expr)
    return :( Pattern($pattern_node) )
end

# `JuliaSyntax` overwrites.

JuliaSyntax.head(p::Pattern) = head(p.ast)
JuliaSyntax.children(p::Pattern) = children(p.ast)

# Utils.

is_fail_macro(ex) = @isexpr(ex, :macrocall, 4) && ex.args[1] === Symbol("@fail")

function cannot_eval_to_Pattern(ex)
    isa(ex, Symbol) && return false
    @isexpr(ex, :macrocall, 3) && ex.args[1] === Symbol("@pattern") && return false
    @isexpr(ex, :call) && ex.args[1] === :Pattern && return false
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
