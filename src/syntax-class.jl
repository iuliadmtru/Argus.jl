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

const SyntaxClassRegistry = Dict{Symbol, Union{Nothing, SyntaxClass}}

"""
Global registry containing all registered syntax classes. When registering a syntax class,
it is not necessary for it to be defined. The registry can be checked for consistency
before matching using [`syntax_class_registry_check`](@ref).
"""
SYNTAX_CLASS_REGISTRY = SyntaxClassRegistry()

"""
Register a syntax class in the default syntax class registry.
"""
register_syntax_class!(name::Symbol, syntax_class::SyntaxClass) =
    register_syntax_class!(SYNTAX_CLASS_REGISTRY, name, syntax_class)

"""
Register a syntax class in a given syntax class registry.
"""
function register_syntax_class!(registry::SyntaxClassRegistry,
                                name::Symbol,
                                syntax_class::SyntaxClass)
    registry[name] = syntax_class
end

"""
Define a syntax class and register it in the default registry.
"""
macro define_syntax_class(name, description, syntax_class_expr)
    syntax_class = :( @syntax_class($(esc(description)), $syntax_class_expr) )
    return :( register_syntax_class!($(esc(name)), $syntax_class) )
end

# TODO: This needs expr support. Maybe `:::registry.stx_cls_name`?
"""
Define a syntax class and register it in the given registry.
"""
macro define_syntax_class_in_registry(registry, name, description, syntax_class_expr)
    syntax_class = :( @syntax_class($(esc(description)), $syntax_class_expr) )
    return :( register_syntax_class!($(esc(registry)), $(esc(name)), $syntax_class) )
end

## Display.

function Base.show(io::IO, ::MIME"text/plain", sc::SyntaxClass)
    println(io, "SyntaxClass: ", sc.description)
    for (i, p) in enumerate(sc.pattern_alternatives)
        println(io, "  Pattern alternative #$i:")
        _show_pattern_syntax_node(io, p.src, "    ")
        if i != length(sc.pattern_alternatives)
            println(io)
        end
    end
end
Base.show(io::IO, ::Type{SyntaxClassRegistry}) = print(io, "SyntaxClassRegistry")

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

    # `literal`: match a literal.
    register_syntax_class!(:literal,
                           @syntax_class "literal" begin
                               @pattern begin
                                   __lit
                                   @fail begin
                                       using JuliaSyntax: is_literal
                                       !is_literal(__lit.src)
                                   end "not a literal"
                               end
                           end)

    # `assign`: match an assignment.
    register_syntax_class!(:assign,
                           @syntax_class "assignment" begin
                               @pattern __lhs:::identifier = __rhs:::expr
                           end)

    # `funcall`: match a function call.
    register_syntax_class!(:funcall,
                           @syntax_class "function call" begin
                               @pattern (__id:::identifier)(__args...)
                           end)
end
