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
    alternatives =
        (length(args) == 1 && Meta.isexpr(args[1], :block)) ?
        Pattern.(args[1].args)                              :
        Pattern.(args)
    return :( SyntaxClass($description, $alternatives) )
end

"""
Global registry containing all registered syntax classes. When registering a syntax class,
it is not necessary for it to be defined. The registry can be checked for consistency
before matching using [`syntax_class_registry_check`](@ref).
"""
SYNTAX_CLASS_REGISTRY = Dict{Symbol, Union{Nothing, SyntaxClass}}()

function register_syntax_class!(name::Symbol)
    # TODO: Interactive overwrite?
    SYNTAX_CLASS_REGISTRY[name] = nothing
end
function register_syntax_class!(name::Symbol, syntax_class::SyntaxClass)
    # TODO: Interactive overwrite?
    SYNTAX_CLASS_REGISTRY[name] = syntax_class
end

function syntax_class_registry_check()
    for (syntax_class_name, syntax_class) in SYNTAX_CLASS_REGISTRY
        isnothing(syntax_class) &&
            error("Syntax class $syntax_class_name is referenced but undefined")
        # TODO: Check for syntax classes that reference undefined syntax classes.
    end
end

# ------------------------------------------------------------------------------------------
# Pre-registered syntax classes.

function _register_syntax_classes()
    # `expr`: match any expression.
    register_syntax_class!(:expr, @syntax_class "expr" quote
                               ~fail(:false, "")
                           end)

    # `identifier`: match an identifier.
    register_syntax_class!(:identifier, @syntax_class "identifier" quote
                               ~and(__id,
                                    ~fail(begin
                                              using JuliaSyntax: is_identifier
                                              !is_identifier(__id.ast)
                                          end,
                                          "not an identifier"))
                           end)

    # TODO: Change to general function call after adding repetitions.
    # `funcall`: match a function call.
    register_syntax_class!(:funcall, @syntax_class "function call" quote
                               (__id:::identifier)()
                           end)

    # `assign`: match an assignment.
    register_syntax_class!(:assign, @syntax_class "assignment" quote
                               __lhs:::identifier = __rhs:::expr
                           end)
end
