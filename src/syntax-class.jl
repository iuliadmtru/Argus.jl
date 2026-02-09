# TODO: Let syntax classes take arguments.
"""
    SyntaxClass

Syntax classes provide the basis for a syntax matching mechanism. A syntax class specifies
a syntactic "shape" and provides a description for that shape.

Syntax class bodies can contain one or more patterns. If there are multiple patterns, the
search for a syntax match stops at the first matching pattern. Pattern variables can be
constrained by syntax classes using the syntax `<pattern_var_name>:::<syntax_class_name>`.
Unconstrained pattern variables are constrained by default to `:::expr`.

# Examples

```
julia> binary_funcall = @syntax_class "binary function call" begin
           @pattern {_}({_}, {_})
       end
SyntaxClass: binary function call
  Pattern alternative #1:
    [call]
      _:::expr                           :: ~var
      _:::expr                           :: ~var
      _:::expr                           :: ~var

julia> fundef = @syntax_class "function definition" begin
           @pattern {f:::funcall} = {_}...
           @pattern function ({f:::funcall}) {_}... end
       end
SyntaxClass: function definition
  Pattern alternative #1:
    [function-=]
      f:::funcall                        :: ~var
      [~rep]
        _:::expr                         :: ~var
  Pattern alternative #2:
    [function]
      f:::funcall                        :: ~var
      [block]
        [~rep]
          _:::expr                       :: ~var
```
"""
struct SyntaxClass
    description::String
    pattern_alternatives::Vector{Pattern}
end

"""
    @syntax_class(description, body)

Create a [`SyntaxClass`](@ref) from a description string and an expression body. The body
should be a `begin ... end` block containing expressions that evaluate to `Pattern`s.

# Examples

```
julia> bool_literal = @syntax_class "`true` or `false`" begin
           @pattern true
           @pattern false
       end
SyntaxClass: `true` or `false`
  Pattern alternative #1:
    true                                 :: Bool
  Pattern alternative #2:
    false                                :: Bool

julia> syntax_match(bool_literal, parsestmt(SyntaxNode, "true"))
BindingSet with 0 entries

julia> syntax_match(bool_literal, parsestmt(SyntaxNode, "a"))
MatchFail("no match")

julia> bool_literal2 = @syntax_class "`true` or `false`" begin
           @pattern begin
               {b:::literal}
               @fail b.value != true && b.value != false "not `true` nor `false`"
           end
       end
SyntaxClass: `true` or `false`
  Pattern alternative #1:
    [~and]
      b:::literal                        :: ~var
      [~fail]
        [&&]
          [call-i]
            [.]
              b                          :: Identifier
              value                      :: Identifier
            !=                           :: Identifier
            true                         :: Bool
          [call-i]
            [.]
              b                          :: Identifier
              value                      :: Identifier
            !=                           :: Identifier
            false                        :: Bool
        "not `true` nor `false`"         :: String

julia> syntax_match(bool_literal2, parsestmt(SyntaxNode, "true"))
BindingSet with 1 entry:
  :b => Binding:
          Name: :b
          Bound source: true @ 1:1
          Ellipsis depth: 0
          Sub-bindings:
            BindingSet with 1 entry:
              :_lit => Binding:
                         Name: :_lit
                         Bound source: true @ 1:1
                         Ellipsis depth: 0
                         Sub-bindings:
                           BindingSet with 0 entries

julia> syntax_match(bool_literal2, parsestmt(SyntaxNode, "a"))
MatchFail("not a literal")
```

Note: A pattern can be written in multiple ways. Some ways can be easier to write, others
      can provide more detailed failure messages.
"""
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
    for (line_number_idx, expr) in zip(Iterators.countfrom(1, 2),
                                       @views pattern_exprs[2:2:end])
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
    SyntaxClassRegistry

Registry for storing syntax classes. Alias for `Dict{Symbol, Union{Nothing, SyntaxClass}}`.
"""
const SyntaxClassRegistry = Dict{Symbol, Union{Nothing, SyntaxClass}}

"""
    SYNTAX_CLASS_REGISTRY

Global registry containing all registered syntax classes.
"""
SYNTAX_CLASS_REGISTRY = SyntaxClassRegistry()

"""
    register_syntax_class!([registry::SyntaxClassRegistry,]
                           name::Symbol,
                           syntax_class::SyntaxClass)

Register a syntax class in a syntax class registry. If no registry is specified, use the
default ([`SYNTAX_CLASS_REGISTRY`](@ref)).
"""
register_syntax_class!(name::Symbol, syntax_class::SyntaxClass) =
    register_syntax_class!(SYNTAX_CLASS_REGISTRY, name, syntax_class)
function register_syntax_class!(registry::SyntaxClassRegistry,
                                name::Symbol,
                                syntax_class::SyntaxClass)
    registry[name] = syntax_class
end

"""
    @define_syntax_class([registry,] name, description, syntax_class_expr)

Define a syntax class and register it in the given registry.
"""
macro define_syntax_class(name, description, syntax_class_expr)
    syntax_class = :( @syntax_class($(esc(description)), $syntax_class_expr) )
    return :( register_syntax_class!($(esc(name)), $syntax_class) )
end
# TODO: This needs parse support. Maybe `:::registry.stx_cls_name`?
macro define_syntax_class_in_registry(registry, name, description, syntax_class_expr)
    syntax_class = :( @syntax_class($(esc(description)), $syntax_class_expr) )
    return :( register_syntax_class!($(esc(registry)), $(esc(name)), $syntax_class) )
end

# Display
# -------

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

# Errors
# ======

"""
    SyntaxClassRegistryKeyError <: Exception

A `~var` pattern form tried to access an unregistered syntax class.
"""
struct SyntaxClassRegistryKeyError <: Exception
    syntax_class_name::Symbol
end

function Base.showerror(io::IO, err::SyntaxClassRegistryKeyError)
    print(io, "SyntaxClassRegistryKeyError: ")
    println(io, "unregistered syntax class ", repr(err.syntax_class_name))
end

# Pre-registered syntax classes
# =============================

function _register_syntax_classes()
    # `expr`: match any expression.
    @define_syntax_class :expr "expr" begin
        @pattern ~fail(false, "")
    end

    # `identifier`: match an identifier.
    @define_syntax_class :identifier "identifier" begin
        @pattern begin
            {_id}
            @fail begin
                using JuliaSyntax: is_identifier
                !is_identifier(_id.src)
            end ""
        end
    end

    # `literal`: match a literal.
    @define_syntax_class :literal "literal" begin
        @pattern begin
            {_lit}
            @fail begin
                using JuliaSyntax: is_literal
                !is_literal(_lit.src)
            end ""
        end
    end

    # `bool_literal`: match `true` or `false`.
    @define_syntax_class :bool_literal "`Bool` literal" begin
        @pattern begin
            {_b:::literal}
            @fail _b.value != true && _b.value != false ""
        end
    end

    # `vec`: match a vector.
    @define_syntax_class :vec "vector" begin
        @pattern [{_}...]
    end

    # `assign`: match an assignment.
    @define_syntax_class :assign "assignment" begin
        @pattern {lhs:::identifier} = {rhs:::expr}
    end

    # `funcall`: match a function call.
    @define_syntax_class :funcall "function call" begin
        @pattern ({fun_name})({args}...)
    end

    # `fundef`: match a function definition.
    @define_syntax_class :fundef "function definition" begin
        @pattern {call:::funcall} = {body}
        @pattern function ({call:::funcall}) {body}... end
    end

    # `macrocall`: match a macro call.
    @define_syntax_class :macrocall "macro call" begin
        @pattern begin
            {_mcall}
            @fail begin
                using JuliaSyntax: kind, Kind
                kind(_mcall.src) !== Kind("macrocall")
            end ""
        end
    end

    # `macrodef`: match a macro definition.
    @define_syntax_class :macrodef "macro definition" begin
        @pattern begin
            {_mdef}
            @fail begin
                using JuliaSyntax: kind, Kind
                kind(_mdef.src) !== Kind("macro")
            end ""
        end
    end

    # `dotcall`: match a dotted function call.
    @define_syntax_class :dotcall "dot call" begin
        @pattern ({fun_name}).({args}...)
    end
end
