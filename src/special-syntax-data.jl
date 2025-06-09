# Errors
# ======

"""
    SyntaxError <:  Exception

A syntax object could not be constructed due to invalid syntax.
"""
struct SyntaxError <: Exception
    msg::String
    file::Union{Nothing, Symbol, String}
    line::Union{Nothing, Int}
end
SyntaxError(msg::String) = SyntaxError(msg, nothing, nothing)

"""
    MatchError <: Exception

A pattern's fail condition was evaluated to a non-Boolean value.
"""
struct MatchError <: Exception
    eval_result
end

# Display
# -------

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

# Syntax data
# ===========

"""
    AbstractPatternFormSyntaxData

Supertype for all pattern form syntax data types.
"""
abstract type AbstractPatternFormSyntaxData end

"""
    VarSyntaxData <: AbstractPatternFormSyntaxData

Data for a `~var` pattern form containing an id name and a [`SyntaxClass`](@ref) name. The
latter is a name expected to be found in the [`SYNTAX_CLASS_REGISTRY`](@ref).
"""
struct VarSyntaxData <: AbstractPatternFormSyntaxData
    var_name::Symbol
    syntax_class_name::Symbol

    function VarSyntaxData(var_name::Symbol, syntax_class_name::Symbol)
        is_pattern_variable(var_name) ||
            throw(SyntaxError("""
                              invalid pattern variable name $var_name
                              Pattern variable names should start with _."""))
        return new(var_name, syntax_class_name)
    end
end

"""
    FailSyntaxData <: AbstractPatternFormSyntaxData

Data for a `~fail` pattern form containing a fail condition and a message. The fail
condition is a function that, given a binding context (`::BindingSet`), creates an
evaluation context where the pattern variables found in the fail condition are defined as
their corresponding bindings. When the function is called, the fail condition is evaluated
in this evaluation context.

Pattern match time exceptions:
  - [`MatchError`](@ref)
  - [`BindingSetKeyError`](@ref)
  - Any exception caused by the evaluation of the fail condition

Exceptions caught and returned as a [`MatchFail`] message:
  - [`BindingFieldError`](@ref)
"""
struct FailSyntaxData <: AbstractPatternFormSyntaxData
    condition::Function
    message::String

    FailSyntaxData(cond::Function, msg::String) = new(cond, msg)
    FailSyntaxData(cond, msg::String) = new(fail_condition(cond), msg)
end

"""
    OrSyntaxData <: AbstractPatternFormSyntaxData

Data for an `~or` pattern form.
"""
struct OrSyntaxData <: AbstractPatternFormSyntaxData end

"""
    AndSyntaxData <: AbstractPatternFormSyntaxData

Data for an `~and` pattern form.
"""
struct AndSyntaxData <: AbstractPatternFormSyntaxData end

"""
    RepVar

Pattern variable that appears in a `~rep` node, at any ellipsis depth.
"""
struct RepVar
    name::Symbol
    ellipsis_depth::Int
end

"""
    RepSyntaxData <: AbstractPatternFormSyntaxData

Data for a `~rep` pattern form. Contains a list of repetition variables contained within
the `~rep` node at any ellipsis depth. Pattern variables that appear inside a `~rep` node
cannot appear outside of it.
"""
struct RepSyntaxData <: AbstractPatternFormSyntaxData
    rep_vars::Vector{RepVar}
end
function RepSyntaxData(node::JS.SyntaxNode)
    rep_vars = [RepVar(p[1], p[2] + 1) for p in get_pattern_vars_with_depth(node)]
    return RepSyntaxData(rep_vars)
end

const PATTERN_FORMS = [:var, :fail, :or, :and, :rep]

# JuliaSyntax overwrites and utils
# --------------------------------

"""
    _register_kinds()

Register new syntax kinds for pattern forms. Called when initialising `Argus`.
"""
_register_kinds() = JS.register_kinds!(Argus,
                                       3,  # TODO: Should this be chosen in some other way?
                                       [
                                           "~var",
                                           "~fail",
                                           "~or",
                                           "~and",
                                           "~rep",
                                       ])
_register_kinds()

JS.head(data::VarSyntaxData)  = JS.SyntaxHead(K"~var",  0)
JS.head(data::FailSyntaxData) = JS.SyntaxHead(K"~fail", 0)
JS.head(data::OrSyntaxData)   = JS.SyntaxHead(K"~or",   0)
JS.head(data::AndSyntaxData)  = JS.SyntaxHead(K"~and",  0)
JS.head(data::RepSyntaxData)  = JS.SyntaxHead(K"~rep",  0)

# Base overwrites
# ---------------

# These are necessary because accessing the `val` field of a `SyntaxData` should not throw
# an error.

Base.getproperty(data::VarSyntaxData, name::Symbol) =
    name === :id                ? getfield(data, :id)                :
    name === :syntax_class_name ? getfield(data, :syntax_class_name) :
    name === :val               ? nothing                            :
    getfield(data, name)

Base.getproperty(data::FailSyntaxData, name::Symbol) =
    name === :message ? getfield(data, :message) :
    name === :val     ? nothing                  :
    getfield(data, name)

Base.getproperty(data::OrSyntaxData, name::Symbol) =
    name === :val ? nothing : getfield(data, name)

Base.getproperty(data::AndSyntaxData, name::Symbol) =
    name === :val ? nothing : getfield(data, name)

Base.getproperty(data::RepSyntaxData, name::Symbol) =
    name === :rep_vars ? getfield(data, :rep_vars) :
    name === :val      ? nothing                   :
    getfield(data, name)

# Utils
# -----

"""
    fail_condition(condition)

Parse and evaluate a fail condition `Expr` as a `Function`. The resulting function takes a
`BindingSet` as argument and returns a `Bool` (`true` if the pattern match should fail,
`false` otherwise).

Pattern match time exceptions:
  - [`MatchError`](@ref)
  - [`BindingSetKeyError`](@ref)
  - Any exception caused by the evaluation of the fail condition

Exceptions caught and returned as a [`MatchFail`] message:
  - [`BindingFieldError`](@ref)
"""
function fail_condition(condition)
    pattern_variables = get_pattern_vars(condition)
    cond_fun = function (binding_context)
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
        Core.eval(ConditionContext, :(using Argus))
        result = Core.eval(ConditionContext, condition)
        isa(result, Bool) ||
            throw(MatchError(result))
        return result
    end

    return cond_fun
end

"""
    get_pattern_vars(ex)::Vector{Symbol}

Return all the pattern variables that appear in `ex`. Used for injecting the pattern
variables in the evaluation context of the pattern's fail condition when parsing a fail
condition `Expr` as a `Function` (pattern parse time).
"""
function get_pattern_vars(ex::Expr)::Vector{Symbol}
    isempty(ex.args) && return Symbol[]

    pattern_vars = Symbol[]
    if @isexpr(ex, :call) && (ex.args[1] === :syntax_match || ex.args[1] === :Pattern) ||
        @isexpr(ex, :macrocall) && ex.args[1] === Symbol("@pattern")
        # Argus functions have their own, separate pattern variables.
        return pattern_vars
    elseif ex.head === :.
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
    name_str in ["__module__", "__source__", "__file__", "__context__"] && return Symbol[]
    return [s]
end
(get_pattern_vars(::T)::Vector{Symbol}) where T = Symbol[]

"""
    get_pattern_vars_with_depth(node::JS.SyntaxNode)

Get all the pattern variables in the given source node, from all ellipsis depths. Return
and array of tuples containing the pattern variable names and their ellipsis depths.
"""
function get_pattern_vars_with_depth(node::JS.SyntaxNode)
    is_leaf(node) && return []
    is_var(node) && return [(get_var_name(node), 0)]
    vs = []
    for c in children(node)
        append!(vs, get_pattern_vars_with_depth(c))
    end
    return is_rep(node) ? map(p -> (p[1], p[2] + 1), vs) : vs
end
