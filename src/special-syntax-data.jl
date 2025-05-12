"""
    AbstractSpecialSyntaxData

Supertype for all special syntax data such as pattern forms.
"""
abstract type AbstractSpecialSyntaxData end

function get_pattern_vars(ex::Expr)::Vector{Symbol}
    pattern_vars = Symbol[]
    isempty(ex.args) && return Symbol[]
    for arg in ex.args
        append!(pattern_vars, get_pattern_vars(arg))
    end
    return pattern_vars
end
function get_pattern_vars(ex::QuoteNode)::Vector{Symbol}
    name_symbol = ex.value
    isa(name_symbol, Symbol) || return Symbol[]
    Meta.isidentifier(name_symbol) || return Symbol[]
    name_str = string(name_symbol)
    startswith(name_str, "_") || return Symbol[]
    return [name_symbol]
end
function get_pattern_vars(s::Symbol)::Vector{Symbol}
    Meta.isidentifier(s) || return Symbol[]
    name_str = string(s)
    startswith(name_str, "_") || return Symbol[]
    return [s]
end
get_pattern_vars(::T) where T = Symbol[]

"""
    FailSyntaxData

Data for `~fail` pattern form containing a fail condition and a message. The fail condition
is a function that, given a binding context (`::BindingSet`), creates an evaluation context
containing where the pattern variables found in the fail condition are defined as their
corresponding bindings. When the function is called, the fail condition is evaluated in this
evaluation context.

If the fail condition contains pattern variables that are not present in the provided binding
context, the function will throw an error.
"""
struct FailSyntaxData <: AbstractSpecialSyntaxData
    condition::Function
    message::String

    FailSyntaxData(cond::Function, msg::String) = new(cond, msg)
    function FailSyntaxData(condition::Expr, msg::String)
        pattern_variables = get_pattern_vars(condition)
        cond_fun = function (binding_context)
            # TODO: Leave the binding context as is.
            # TODO: Import and use `BindingSet`.

            # Create a smaller binding context containg only the bindings from `condition`.
            condition_binding_context = Dict{Symbol, Any}()
            for pattern_var_name in pattern_variables
                condition_binding_context[pattern_var_name] =
                    try
                        binding_context[pattern_var_name]
                    catch e
                        if isa(e, KeyError)
                            # TODO: Throw specific error type.
                            error("Binding context does not contain a binding for ",
                                  "$pattern_var_name.")
                        else
                            rethrow(e)
                        end
                    end
            end
            # Create an evaluation context with the condition binding context.
            ConditionContext = Module()
            for (var_name, binding) in condition_binding_context
                Core.eval(ConditionContext, :($var_name = $binding))
            end
            # Evaluate the condition within the evaluation context.
            #
            # TODO: Should `eval` exceptions be caught here?
            result = Core.eval(ConditionContext, condition)
            isa(result, Bool) ||
                error("Fail condition evaluated to non-Boolean value: ",
                      "$result (::$(typeof(result)))")
            return result
        end

        return new(cond_fun, msg)
    end
end

"""
    VarSyntaxData

Data for a `~var` pattern form holding an id name and a [`SyntaxClass`](@ref) name. The
latter is a name expected to be found in the syntax class registry (TODO: docs) when the
[`syntax_class_registry_check`](@ref) function is called. I.e. forward references are
allowed as long as the "promise" to define missing syntax classes is fulfilled when checking
the registry for consistency.
"""
struct VarSyntaxData <: AbstractSpecialSyntaxData
    id::Symbol
    syntax_class_name::Symbol

    function VarSyntaxData(id::Symbol, syntax_class_name::Symbol)
        is_pattern_variable(id) ||
            error("Invalid pattern variable name $id.\n",
                  "Pattern variable names should start with _.")
        return new(id, syntax_class_name)
    end
end

"""
    OrSyntaxData

Data for `~or` pattern form.
"""
struct OrSyntaxData <: AbstractSpecialSyntaxData end

"""
    AndSyntaxData

Data for `~and` pattern form.
"""
struct AndSyntaxData <: AbstractSpecialSyntaxData end

# TODO: Pattern forms registry? Or remove.
const PATTERN_FORMS = [:fail, :var, :or, :and]

## `JuliaSyntax` overwrites and utils.

"""
Register new syntax kinds for pattern forms.
"""
_register_kinds() = JuliaSyntax.register_kinds!(Argus, 3, ["~fail", "~var", "~or", "~and"])
_register_kinds()

JuliaSyntax.head(data::FailSyntaxData) = JuliaSyntax.SyntaxHead(K"~fail", 0)
JuliaSyntax.head(data::VarSyntaxData) = JuliaSyntax.SyntaxHead(K"~var", 0)
JuliaSyntax.head(data::OrSyntaxData) = JuliaSyntax.SyntaxHead(K"~or", 0)
JuliaSyntax.head(data::AndSyntaxData) = JuliaSyntax.SyntaxHead(K"~and", 0)

## `Base` overwrites.

Base.getproperty(data::FailSyntaxData, name::Symbol) =
    name === :message ? getfield(data, :message) :
    name === :val     ? nothing                  :
    getfield(data, name)

Base.getproperty(data::VarSyntaxData, name::Symbol) =
    name === :id                ? getfield(data, :id)                :
    name === :syntax_class_name ? getfield(data, :syntax_class_name) :
    name === :val               ? nothing                            :
    getfield(data, name)

Base.getproperty(data::OrSyntaxData, name::Symbol) =
    name === :val ? nothing : getfield(data, name)

Base.getproperty(data::AndSyntaxData, name::Symbol) =
    name === :val ? nothing : getfield(data, name)
