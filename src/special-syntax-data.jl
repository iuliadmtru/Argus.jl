# Errors
# ======

"""
    SyntaxError <:  Exception

A syntax object could not be constructed due to invalid syntax.
"""
struct SyntaxError <: Exception
    msg::String
    file::Union{Nothing, Symbol, String}
    line::Union{Nothing, Int64}
    column::Union{Nothing, Int64}
end
SyntaxError(msg::String) = SyntaxError(msg, nothing, nothing, nothing)
SyntaxError(msg::String, file::Union{Symbol, String}, line::Int64) =
    SyntaxError(msg, file, line, nothing)

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
    file_name = isnothing(err.file) || isa(err.file, String) && isempty(err.file) ?
        "<no file>" :
        err.file
    print(io, "@ $(file_name):$(err.line)")
    isnothing(err.column) ? println(io) : println(io, ":$(err.column)")
end

function Base.showerror(io::IO, err::MatchError)
    print(io, "MatchError: ")
    println(io,
            "Condition evaluated to ",
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

# Active patterns
# ---------------

"""
    VarSyntaxData <: AbstractPatternFormSyntaxData

Data for a `~var` pattern form containing a variable name and a [`SyntaxClass`](@ref) name.
The latter is a name expected to be found in the [`SYNTAX_CLASS_REGISTRY`](@ref).
"""
struct VarSyntaxData <: AbstractPatternFormSyntaxData
    var_name::Symbol
    syntax_class_name::Symbol
end

"""
    FailSyntaxData <: AbstractPatternFormSyntaxData

Data for a `~fail` pattern form containing a fail condition and a message. The fail
condition is a function that, given a binding context (`::BindingSet`), creates an
evaluation context where the pattern variables found in the fail condition are defined as
their corresponding bindings. When the function is called, the fail condition is evaluated
in this evaluation context. The match behaviour is opposite to that of
[`WhenSyntaxData`](@ref).

Pattern match time exceptions:
  - [`MatchError`](@ref)
  - [`BindingSetKeyError`](@ref)
  - Any exception caused by the evaluation of the fail condition

Exceptions caught and returned as a [`MatchFail`] message:
  - [`BindingFieldError`](@ref)

See also: [`WhenSyntaxData`](@ref)
"""
struct FailSyntaxData <: AbstractPatternFormSyntaxData
    condition::Function
    message::String

    FailSyntaxData(cond::Function, msg::String) = new(cond, msg)
    FailSyntaxData(pattern_vars, cond, msg::String) =
        new(create_function(cond, pattern_vars), msg)
end

"""
    WhenSyntaxData <: AbstractPatternFormSyntaxData

Data for a `~when` pattern form containing a condition and a message. The condition is a
function that, given a binding context (`::BindingSet`), creates an evaluation context
where the pattern variables found in the condition are defined as their corresponding
bindings. When the function is called, the condition is evaluated in this evaluation
context. The match behaviour is opposite to that of [`FailSyntaxData`](@ref).

Pattern match time exceptions:
  - [`MatchError`](@ref)
  - [`BindingSetKeyError`](@ref)
  - Any exception caused by the evaluation of the condition

Exceptions caught and returned as a [`MatchFail`] message:
  - [`BindingFieldError`](@ref)

See also: [`FailSyntaxData`](@ref)
"""
struct WhenSyntaxData <: AbstractPatternFormSyntaxData
    condition::Function

    WhenSyntaxData(cond::Function) = new(cond)
    WhenSyntaxData(pattern_vars, cond) = new(create_function(cond, pattern_vars))
end

"""
"""
struct ExecuteSyntaxData <: AbstractPatternFormSyntaxData
    code::Function

    ExecuteSyntaxData(code::Function) = new(code)
    ExecuteSyntaxData(pattern_vars, code) = new(create_function(code, pattern_vars))
end

# Passive patterns
# ----------------

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
    ellipsis_depth::UInt8
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

"""
    NotSyntaxData <: AbstractPatternFormSyntaxData

Data for a `~not` pattern form. Pattern variables that appear inside the `~not` node don't
get bound outside of it.
"""
struct NotSyntaxData <: AbstractPatternFormSyntaxData end

"""
    InsideSyntaxData <: AbstractPatternFormSyntaxData

Data for an `~inside` pattern form.
"""
struct InsideSyntaxData <: AbstractPatternFormSyntaxData end

"""
    ContainsSyntaxData <: AbstractPatternFormSyntaxData

Data for an `~contains` pattern form.
"""
struct ContainsSyntaxData <: AbstractPatternFormSyntaxData end

const PATTERN_FORMS = [
    :var,
    :fail,
    :when,
    :execute,
    :or,
    :and,
    :rep,
    :not,
    :inside,
    :contains,
]

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
                                           "~when",
                                           "~execute",
                                           "~or",
                                           "~and",
                                           "~rep",
                                           "~not",
                                           "~inside",
                                           "~contains",
                                       ])
_register_kinds()

JS.head(::VarSyntaxData)      = JS.SyntaxHead(K"~var",      0)
JS.head(::FailSyntaxData)     = JS.SyntaxHead(K"~fail",     0)
JS.head(::WhenSyntaxData)     = JS.SyntaxHead(K"~when",     0)
JS.head(::ExecuteSyntaxData)  = JS.SyntaxHead(K"~execute",  0)
JS.head(::OrSyntaxData)       = JS.SyntaxHead(K"~or",       0)
JS.head(::AndSyntaxData)      = JS.SyntaxHead(K"~and",      0)
JS.head(::RepSyntaxData)      = JS.SyntaxHead(K"~rep",      0)
JS.head(::NotSyntaxData)      = JS.SyntaxHead(K"~not",      0)
JS.head(::InsideSyntaxData)   = JS.SyntaxHead(K"~inside",   0)
JS.head(::ContainsSyntaxData) = JS.SyntaxHead(K"~contains", 0)

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
    name === :condition ? getfield(data, :condition) :
    name === :message   ? getfield(data, :message)   :
    name === :val       ? nothing                    :
    getfield(data, name)

Base.getproperty(data::WhenSyntaxData, name::Symbol) =
    name === :condition ? getfield(data, :condition) :
    name === :val       ? nothing                    :
    getfield(data, name)

Base.getproperty(data::ExecuteSyntaxData, name::Symbol) =
    name === :code ? getfield(data, :code) :
    name === :val  ? nothing               :
    getfield(data, name)

Base.getproperty(data::OrSyntaxData, name::Symbol) =
    name === :val ? nothing : getfield(data, name)

Base.getproperty(data::AndSyntaxData, name::Symbol) =
    name === :val ? nothing : getfield(data, name)

Base.getproperty(data::RepSyntaxData, name::Symbol) =
    name === :rep_vars ? getfield(data, :rep_vars) :
    name === :val      ? nothing                   :
    getfield(data, name)

Base.getproperty(data::NotSyntaxData, name::Symbol) =
    name === :val ? nothing : getfield(data, name)

Base.getproperty(data::InsideSyntaxData, name::Symbol) =
    name === :val ? nothing : getfield(data, name)

Base.getproperty(data::ContainsSyntaxData, name::Symbol) =
    name === :val ? nothing : getfield(data, name)

Base.copy(data::VarSyntaxData) = VarSyntaxData(data.var_name, data.syntax_class_name)
Base.copy(data::FailSyntaxData) = FailSyntaxData(data.condition, data.message)
Base.copy(data::WhenSyntaxData) = WhenSyntaxData(data.condition)
Base.copy(data::ExecuteSyntaxData) = ExecuteSyntaxData(data.code)
Base.copy(::OrSyntaxData) = OrSyntaxData()
Base.copy(::AndSyntaxData) = AndSyntaxData()
Base.copy(data::RepSyntaxData) = RepSyntaxData(data.rep_vars)
Base.copy(::NotSyntaxData) = NotSyntaxData()
Base.copy(::InsideSyntaxData) = InsideSyntaxData()
Base.copy(::ContainsSyntaxData) = ContainsSyntaxData()

# Utils
# -----

"""
    create_function(condition)

Parse and evaluate code in a pattern as a `Function`. The resulting function takes a
`BindingSet` as argument. `JuliaSyntax` is imported in the evaluation context.

Pattern match time exceptions:
  - [`BindingSetKeyError`](@ref)
  - [`MatchError`](@ref) – in the case of `~when` and `~fail`, when the function call does
                           not evaluate to `Bool`
  - Any exception caused by the evaluation of the condition

Exceptions caught and returned as a [`MatchFail`] message:
  - [`BindingFieldError`](@ref)
"""
function create_function(condition, pattern_vars)
    # Create the `let` bindings that will be bound in the returned function.
    bindings_sym = gensym()
    let_bindings = [:($v = $bindings_sym[$(QuoteNode(v))]) for v in pattern_vars]

    return @RuntimeGeneratedFunction(:(($bindings_sym) -> try
                                           let $(let_bindings...)
                                               $condition
                                           end
                                       catch err
                                           if isa(err, KeyError)
                                               throw(BindingSetKeyError(err.key))
                                           elseif isa(err, UndefVarError)
                                               throw(BindingSetKeyError(err.var))
                                           else
                                               rethrow(err)
                                           end
                                       end))
end

"""
    get_pattern_vars_with_depth(node::JS.SyntaxNode)

Get all the pattern variables in the given source node, from all ellipsis depths. Return
and array of tuples containing the pattern variable names and their ellipsis depths.
"""
function get_pattern_vars_with_depth(node::JS.SyntaxNode)
    is_leaf(node) && return Tuple{Symbol, UInt8}[]
    is_var(node) && return [(get_var_name(node), UInt8(0))]
    vs = Tuple{Symbol, UInt8}[]
    for c in children(node)
        append!(vs, get_pattern_vars_with_depth(c))
    end
    return is_rep(node) ? map(p -> (p[1], p[2] + 1), vs) : vs
end
