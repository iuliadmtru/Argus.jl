using Argus

lang_rules = RuleGroup("lang")

@define_rule_in_group lang_rules "chained-const-assignment" begin
    description = """
    Do not chain assignments with const. The right hand side is not constant here.
    """

    pattern = @pattern begin
        const _:::identifier = _:::identifier = _
    end
end

@define_rule_in_group lang_rules "compare-nothing" begin
    description = """
    Comparisons of `nothing` should be made with === or !== or with isnothing().
    """

    pattern = @pattern begin
        ~or(
            nothing == _,
            _ == nothing,
            nothing != _,
            _ != nothing
        )
    end
end


@define_rule_in_group lang_rules "useless-equals" begin
    description = """
    Comparing the same object in the RHS and LHS is pointless.
    """

    pattern = @pattern begin
        ~or(
            _x == _x,
            _x != _x,
            _x === _x,
            _x !== _x
        )
    end
end
