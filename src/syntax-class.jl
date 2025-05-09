using MacroTools: striplines

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
