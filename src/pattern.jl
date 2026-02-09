# Pattern
# =======

"""
    Pattern

Syntax pattern used for matching syntax.
"""
struct Pattern
    src::SyntaxPatternNode
end

"""
    @pattern(expr)

Create a [`Pattern`](@ref) from the given expression. Special syntax can be escaped by
wrapping it in an `@esc` macro. Fail conditions are defined with the `@fail` macro.

`@esc` usage:
  - `@esc(ex)`       : Escape everything inside `ex`.
  - `@esc(ex, depth)`: Escape `ex` only up to `depth`.

`@fail` usage:
  - `@fail(condition, msg)`: Fail with the message `msg` if `condition` is satisfied.

# Examples

```
julia> @pattern a + b  # Simple expression with no pattern variables.
Pattern:
[call-i]
  a                                      :: Identifier
  +                                      :: Identifier
  b                                      :: Identifier

julia> assign_to_x = @pattern begin
           {x:::assign}                               # Pattern variable matching an assignment.
           @fail x.lhs.name == "x" "assignment to x"  # The matching fails if the rhs variable's name is "x".
       end
Pattern:
[~and]
  x:::assign                             :: ~var
  [~fail]
    [call-i]
      [.]
        [.]
          x                              :: Identifier
          lhs                            :: Identifier
        name                             :: Identifier
      ==                                 :: Identifier
      [string]
        "x"                              :: String
    "assignment to x"                    :: String

julia> syntax_match(assign_to_x, JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "x = 2"))
MatchFail("assignment to x")

julia> pattern = @pattern begin
           const {a:::identifier} = {_}
           {_}...
           const {a:::identifier} = {_}  # Reassignment of `const`.
       end
Pattern:
[toplevel]
  [const]
    [=]
      a:::identifier                     :: ~var
      _:::expr                           :: ~var
  [~rep]
    _:::expr                             :: ~var
  [const]
    [=]
      a:::identifier                     :: ~var
      _:::expr                           :: ~var

julia> syntax_match(pattern, parseall(SyntaxNode,
                                      \"""
                                      const x = 2
                                      other_ex
                                      const x = 3
                                      \"""))
BindingSet with 1 entry:
  :a => Binding:
          Name: :a
          Bound source: x @ 3:7
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet with 1 entry:
              :_id => Binding:
                        Name: :_id
                        Bound source: x @ 3:7
                        Ellipsis depth: 0
                        Sub-bindings:
                          BindingSet with 0 entries

julia> pattern = @pattern @esc(x...)
Pattern:
[...]
  x                                      :: Identifier

julia> syntax_match(pattern, parsestmt(SyntaxNode, "x..."))
BindingSet with 0 entries

julia> pattern = @pattern @esc({x}..., 1)
Pattern:
[...]
  x:::expr                               :: ~var

julia> syntax_match(pattern, parsestmt(SyntaxNode, "x..."))
BindingSet with 1 entry:
  :x => Binding:
          Name: :x
          Bound source: x @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet with 0 entries
```

Note: `@fail` and `@esc` macros only exist inside `@pattern` bodies.

```
julia> @fail :false ""
ERROR: LoadError: UndefVarError: `@fail` not defined in `Main`
Suggestion: check for spelling errors or missing imports.
in expression starting at ...

julia> @esc
ERROR: LoadError: UndefVarError: `@esc` not defined in `Main`
Suggestion: check for spelling errors or missing imports.
Hint: a global variable of this name also exists in MacroTools.
in expression starting at ...
```
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
        Pattern expressions and fail conditions cannot be interspersed."""

    @isexpr(expr, :quote) &&
        throw(SyntaxError(err_msg_general, __source__.file, __source__.line))
    # Here, `expr` is one of the following:
    #   - `<atom>`                   -- e.g. `2`
    #   - `:($(QuoteNode(<atom>)))`  -- e.g. `:( _x )`
    #   - `quote <expr>* end`        -- syntax for patterns with multiple expressions
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
            # `Meta.quot` each toplevel expressions to avoid accidental misparse.
            # For example, this pattern:
            #
            # ```
            # @pattern begin
            #     const _:::identifier = _...
            #     _...
            # end
            # ```
            #
            # would, without quoting, be parsed as the same as:
            #
            # `@pattern const _:::identifier = (_..., _...)`
            pattern_expr = :( [:pattern_toplevel, $(Meta.quot.(toplevel_children)...)] )
        end
        # All expressions from `idx` onwards, if any, should be fail conditions.
        if idx <= length(expr.args)
            # Turn `@fail` macros into `~fail` expressions.
            # Skip `LineNumberNode`s when iterating but include them in the error message.
            fail_exprs = Expr[]
            for (line_number_idx, fail_macro) in zip(Iterators.countfrom(idx - 1, 2),
                                                     @views expr.args[idx:2:end])
                fail_macro_line_number = expr.args[line_number_idx]
                # Pattern expressions and fail conditions cannot be interspersed.
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
            if !isempty(fail_exprs)
                pattern_expr = :( ~and($(Meta.quot(pattern_expr)), $(fail_exprs...)) )
            end
        end
    end
    pattern_node = SyntaxPatternNode(pattern_expr)
    return :( Pattern($pattern_node) )
end

# JuliaSyntax overwrites
# ----------------------

JuliaSyntax.head(p::Pattern) = head(p.src)
JuliaSyntax.children(p::Pattern) = children(p.src)

# Utils
# -----

is_fail_macro(ex) = @isexpr(ex, :macrocall, 4) && ex.args[1] === Symbol("@fail")
is_template_macro(ex) = @isexpr(ex, :macrocall, 3) && ex.args[1] === Symbol("@template")

"""
    cannot_eval_to_Pattern(ex)

Return `true` only if the given expression can evaluate to a [`Pattern`](@ref).

An expression can evaluate to a `Pattern` if it is a `@pattern` call, a `Pattern` call or
a variable name.
"""
function cannot_eval_to_Pattern(ex)
    isa(ex, Symbol) && return false
    @isexpr(ex, :macrocall, 3) && ex.args[1] === Symbol("@pattern") && return false
    @isexpr(ex, :call) && ex.args[1] === :Pattern && return false
    return true
end

is_toplevel(p::Pattern) = is_toplevel(p.src)

# Display
# -------

function _repr_var_node(node::SyntaxPatternNode)
    id = get_var_name(node)
    syntax_class_name = get_var_syntax_class_name(node)

    return string(id, ":::", syntax_class_name)
end

function _show_pattern_syntax_node(io::IO, node::SyntaxPatternNode, indent)
    nodestr =
        is_leaf(node) ? leaf_string(node)    :
        is_var(node)  ? _repr_var_node(node) :
        "[$(untokenize(head(node)))]"
    treestr = string(indent, nodestr)
    if is_leaf(node) || is_var(node)
        treestr = rpad(treestr, 40) * " :: " * string(kind(node))
    end
    print(io, treestr)
    if !is_leaf(node) && !is_var(node)
        new_indent = indent * "  "
        for n in children(node)
            println(io)
            _show_pattern_syntax_node(io, n, new_indent)
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", pattern::Pattern)
    println(io, "Pattern:")
    _show_pattern_syntax_node(io, pattern.src, "")
end
Base.show(io::IO, ::MIME"text/x.sexpression", pattern::Pattern) =
    _show_syntax_pattern_node_sexpr(io, pattern.src, false)
Base.show(io::IO, pattern::Pattern) =
    show(io, "text/x.sexpression", pattern)
