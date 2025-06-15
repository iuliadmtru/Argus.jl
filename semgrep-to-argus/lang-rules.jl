using Argus

lang_rules = RuleGroup("lang")

@define_rule_in_group lang_rules "chained-const-assignment" begin
    description = """
    Do not chain assignments with const. The right hand side is not constant here.
    """

    pattern = @pattern begin
        const {_:::identifier} = {_:::identifier} = {_}
    end
end

@define_rule_in_group lang_rules "compare-nothing" begin
    description = """
    Comparisons of `nothing` should be made with === or !== or with isnothing().
    """

    pattern = @pattern begin
        ~or(
            nothing == {_},
            {_} == nothing,
            nothing != {_},
            {_} != nothing
        )
    end
end


@define_rule_in_group lang_rules "useless-equals" begin
    description = """
    Comparing the same object in the RHS and LHS is pointless.
    """

    pattern = @pattern begin
        ~or(
            {x} ==  {x},
            {x} !=  {x},
            {x} === {x},
            {x} !== {x}
        )
    end
end

# TODO: Constant propagation.
@define_rule_in_group lang_rules "open-tmp-path" begin
    description = """
    Do not open file in /tmp directly. Use `mktemp` instead.
    """

    pattern = @pattern begin
        open({x}, {_}...)
        @fail match(r"^/tmp/.*", x.value) === nothing "path not /tmp/.*"
    end
end

@define_rule_in_group lang_rules "rand-bool" begin
    description = """
    To get a random Boolean, use `rand(Bool)`.
    """

    pattern = @pattern begin
        {randf}() < 0.5
        @fail match(r"^(Base.)?rand$", randf.name) === nothing "not `rand` call"
    end
end

# Syntax classes useful for the `useless-booleans` rule.
@define_syntax_class :chain_with_lit "logical chain with explicit literal" begin
    @pattern begin
        ~or(
            {b} && {_}...,
            {b} || {_}...,
            {_}... && {b},
            {_}... || {b}
        )
        @fail typeof(b.value) != Bool "not `Bool`"
    end
end
@define_syntax_class :lit_or_chain "literal or logical chain with explicit literal" begin
    @pattern ~or({b:::literal}, {c:::chain_with_lit})
end

# TODO: Needs more work in order to catch something like `x || y && true`.
@define_rule_in_group lang_rules "useless-booleans" begin
    description = """
    Boolean literals in conditions are unnecessary.
    """

    pattern = @pattern begin
        ~or(
            if {if_cond:::lit_or_chain}
                {_}...
            end,
            if {if_cond:::lit_or_chain}
                {_}...
            else
                {_}...
            end,
            if {if_cond:::lit_or_chain}
                {_}...
            elseif {_}...
                {_}...
            end,
            if {if_cond:::lit_or_chain}
                {_}...
            elseif {_}...
                {_}...
            else
                {_}...
            end,
            if {_}...
                {_}...
            elseif {if_cond:::lit_or_chain}
                {_}...
            end,
            if {_}...
                {_}...
            elseif {if_cond:::lit_or_chain}
                {_}...
            else
                {_}...
            end,
            while {while_cond:::chain_with_lit}
                {_}...
            end
        )
    end
end

# This doesn't work because the `Expr` representation is different than the
# `SyntaxNode` one.
# @define_rule_in_group lang_rules "overload-not-equals" begin
#     description = """
#     `!=` should not be overloaded since it is defined as a constant function.
#     Overload `==` instead.
#     """

#     pattern = @pattern begin
#         ~or(
#             (!=({_}, {_}) = {_}),
#             function !=({_}, {_})
#                 {_}...
#             end
#         )
#     end
# end
