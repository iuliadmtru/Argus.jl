# TODO: Let syntax classes take arguments.
"""
    SyntaxClass

Syntax classes provide the basis for a syntax matching mechanism. A `SyntaxClass` object
specifies a syntactic "shape" and provides a description for that shape. For example,
a syntax class corresponding to a binary function call could be defined as such:
```
binary_funcall = @syntax_class "binary function call" begin
    _(_, _)
end
```
"""
struct SyntaxClass
    description::String
    pattern_alternatives::Vector{Pattern}
end

macro syntax_class(description, body)
    # Error messages.
    err_msg_general =
        """
        invalid `@syntax_class` syntax
        The `@syntax_class` body should be defined using a `begin ... end` block."""
    err_msg_body =
        """
        invalid `@syntax_class` syntax
        All expressions in a `@syntax_class` body should be `Pattern`s."""

    @isexpr(body, :block) ||
        throw(SyntaxError(err_msg_general, __source__.file, __source__.line))
    pattern_exprs = body.args
    for (line_number_idx, expr) in zip(Iterators.countfrom(1, 2), pattern_exprs[2:2:end])
        expr_line_number = pattern_exprs[line_number_idx]
        # Each expression in a syntax class should evaluate to a `Pattern`:
        #   - `@pattern ...`
        #   - `Pattern(...)`
        #   - `<variable>(::Pattern)`
        cannot_eval_to_Pattern(expr) &&
            throw(SyntaxError(err_msg_body,
                                   expr_line_number.file,
                                   expr_line_number.line))
    end
    # Skip the `LineNumberNode`.
    pattern_exprs = pattern_exprs[2:2:end]

    return :( SyntaxClass($description, [$(esc.(pattern_exprs)...)]) )
end

"""
Global registry containing all registered syntax classes. When registering a syntax class,
it is not necessary for it to be defined. The registry can be checked for consistency
before matching using [`syntax_class_registry_check`](@ref).
"""
SYNTAX_CLASS_REGISTRY = Dict{Symbol, Union{Nothing, SyntaxClass}}()

# TODO: Allow registering to specific registry.
function register_syntax_class!(name::Symbol, syntax_class::SyntaxClass)
    # TODO: Interactive overwrite?
    SYNTAX_CLASS_REGISTRY[name] = syntax_class
end

# ------------------------------------------------------------------------------------------
# Pre-registered syntax classes.

function _register_syntax_classes()
    # `expr`: match any expression.
    register_syntax_class!(:expr,
                           @syntax_class "expr" begin
                               @pattern ~fail(:false, "")
                           end)

    # `identifier`: match an identifier.
    register_syntax_class!(:identifier,
                           @syntax_class "identifier" begin
                               @pattern begin
                                   __id
                                   @fail begin
                                       using JuliaSyntax: is_identifier
                                       !is_identifier(__id.src)
                                   end "not an identifier"
                               end
                           end)

    # `assign`: match an assignment.
    register_syntax_class!(:assign,
                           @syntax_class "assignment" begin
                               @pattern __lhs:::identifier = __rhs:::expr
                           end)

    # TODO: Change to general function call after adding repetitions.
    # `funcall`: match a function call.
    register_syntax_class!(:funcall,
                           @syntax_class "function call" begin
                               @pattern (__id:::identifier)()
                           end)
end
