using MacroTools: striplines

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

# TODO: Support for fail conditions.
macro syntax_class(description, patterns)
    args = striplines(patterns).args
    # TODO: Document examples.
    alternatives =
        if length(args) == 1 && Meta.isexpr(args[1], :block)
            if length(args[1].args) == 1 && Meta.isexpr(args[1].args[1], :tuple)
                Pattern.(args[1].args[1].args)
            else
                Pattern.(args[1].args)
            end
        else
            Pattern.(args)
        end

    return :( SyntaxClass($description, $alternatives) )
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
                           @syntax_class "expr" quote
                               ~fail(:false, "")
                           end)

    # `identifier`: match an identifier.
    register_syntax_class!(:identifier,
                           @syntax_class "identifier" quote
                               ~and(__id,
                                    ~fail(begin
                                              using JuliaSyntax: is_identifier
                                              !is_identifier(__id.ast)
                                          end,
                                          "not an identifier"))
                           end)

    # `assign`: match an assignment.
    register_syntax_class!(:assign,
                           @syntax_class "assignment" quote
                               __lhs:::identifier = __rhs:::expr
                           end)

    # TODO: Change to general function call after adding repetitions.
    # `funcall`: match a function call.
    register_syntax_class!(:funcall,
                           @syntax_class "function call" quote
                               (__id:::identifier)()
                           end)
end
