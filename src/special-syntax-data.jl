# --------------------------------------------
# Errors.

struct SyntaxError <: Exception
    msg::String
    file::Union{Nothing, Symbol, String}
    line::Union{Nothing, Int}
end
SyntaxError(msg::String) = SyntaxError(msg, nothing, nothing)

struct MatchError <: Exception
    eval_result
end

## Display.

function Base.showerror(io::IO, err::SyntaxError)
    print(io, "SyntaxError: ")
    println(io, err.msg)
    isnothing(err.file) && return
    println(io, "@ $(err.file):$(err.line)")
end

function Base.showerror(io::IO, err::MatchError)
    print(io, "MatchError: ")
    println(io,
            "Fail condition evaluated to ",
            typeof(err.eval_result),
            " instead of Bool (`",
            err.eval_result,
            "`)")
end

# --------------------------------------------
# Syntax data.

"""
    AbstractSpecialSyntaxData

Supertype for all special syntax data such as pattern forms.
"""
abstract type AbstractSpecialSyntaxData end

function get_pattern_vars(ex::Expr)::Vector{Symbol}
    isempty(ex.args) && return Symbol[]

    pattern_vars = Symbol[]
    if ex.head === :.
        append!(pattern_vars, get_pattern_vars(ex.args[1]))
    else
        for arg in ex.args
            append!(pattern_vars, get_pattern_vars(arg))
        end
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
    FailSyntaxData(cond, msg::String) = new(fail_condition(cond), msg)
end

"""
    VarSyntaxData

Data for a `~var` pattern form holding an id name and a [`SyntaxClass`](@ref) name. The
latter is a name expected to be found in the syntax class registry.
"""
struct VarSyntaxData <: AbstractSpecialSyntaxData
    id::Symbol
    syntax_class_name::Symbol

    function VarSyntaxData(id::Symbol, syntax_class_name::Symbol)
        is_pattern_variable(id) ||
            throw(SyntaxError("""
                              invalid pattern variable name $id
                              Pattern variable names should start with _."""))
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

"""
    RepSyntaxData

Data for repetition nodes.
"""
struct RepSyntaxData <: AbstractSpecialSyntaxData end

# TODO: Pattern forms registry? Or remove.
const PATTERN_FORMS = [:fail, :var, :or, :and, :rep]

## `JuliaSyntax` overwrites and utils.

"""
Register new syntax kinds for pattern forms.
"""
_register_kinds() = JuliaSyntax.register_kinds!(Argus,
                                                3,
                                                [
                                                    "~fail",
                                                    "~var",
                                                    "~or",
                                                    "~and",
                                                    "~rep",
                                                ])
_register_kinds()

JuliaSyntax.head(data::FailSyntaxData) = JuliaSyntax.SyntaxHead(K"~fail", 0)
JuliaSyntax.head(data::VarSyntaxData) = JuliaSyntax.SyntaxHead(K"~var", 0)
JuliaSyntax.head(data::OrSyntaxData) = JuliaSyntax.SyntaxHead(K"~or", 0)
JuliaSyntax.head(data::AndSyntaxData) = JuliaSyntax.SyntaxHead(K"~and", 0)
JuliaSyntax.head(data::RepSyntaxData) = JuliaSyntax.SyntaxHead(K"~rep", 0)

## `Base` overwrites.

Base.getproperty(data::RepSyntaxData, name::Symbol) =
    name === :val ? nothing : getfield(data, name)

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

## Utils.

function fail_condition(condition)
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
                catch err
                    if isa(err, KeyError)
                        throw(BindingSetKeyError(pattern_var_name))
                    else
                        rethrow(err)
                    end
                end
        end
        # Create an evaluation context with the condition binding context.
        ConditionContext = Module()
        for (var_name, binding) in condition_binding_context
            Core.eval(ConditionContext, :($var_name = $binding))
        end
        # Evaluate the condition within the evaluation context.
        result = Core.eval(ConditionContext, condition)
        isa(result, Bool) ||
            throw(MatchError(result))
        return result
    end

    return cond_fun
end
