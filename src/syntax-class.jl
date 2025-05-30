# TODO: Let syntax classes take arguments.
"""
    SyntaxClass

Syntax classes provide the basis for a syntax matching mechanism. A `SyntaxClass` object
specifies a syntactic "shape" and provides a description for that shape. For example,
a syntax class corresponding to a binary function call could be defined as such:
```
binary_funcall = @syntax_class "binary function call" quote
    _(_, _)
end
```
"""
struct SyntaxClass
    description::String
    pattern_alternatives::Vector{Pattern}
end

macro syntax_class(description, body)
    @isexpr(body, :block) ||
        error("Invalid syntax class syntax.\n",
              "The syntax class body should be defined using a `begin ... end` block")
    pattern_exprs = MacroTools.striplines(body).args
    for expr in pattern_exprs
        # Each expression in a syntax class should evaluate to a `Pattern`:
        #   - `@pattern ...`
        #   - `Pattern(...)`
        #   - `<variable>(::Pattern)`
        #
        # TODO: Expression and position information.
        cannot_eval_to_Pattern(expr) &&
            error("Invalid syntax class syntax.\n",
                  "All expressions in a syntax class should be `Pattern`s.")
    end

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
                               @pattern :( ~fail(:false, "") )
                           end)

    # `identifier`: match an identifier.
    register_syntax_class!(:identifier,
                           @syntax_class "identifier" begin
                               @pattern begin
                                   __id
                                   @fail begin
                                       using JuliaSyntax: is_identifier
                                       !is_identifier(__id.ast)
                                   end "not an identifier"
                               end
                           end)

    # `assign`: match an assignment.
    register_syntax_class!(:assign,
                           @syntax_class "assignment" begin
                               @pattern :( __lhs:::identifier = __rhs:::expr )
                           end)

    # TODO: Change to general function call after adding repetitions.
    # `funcall`: match a function call.
    register_syntax_class!(:funcall,
                           @syntax_class "function call" begin
                               @pattern :( (__id:::identifier)() )
                           end)
end
