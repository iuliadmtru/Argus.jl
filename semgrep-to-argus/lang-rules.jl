using Argus

lang_rules = RuleGroup("lang")

@define_rule_in_group lang_rules "chained-const-assignment" quote
    description = """
    Do not chain assignments with const. The right hand side is not constant here.
    """

    pattern = :(
        const _:::identifier = _:::identifier = _
    )
end

@define_rule_in_group lang_rules "compare-nothing" quote
    description = """
    Comparisons of `nothing` should be made with === or !== or with isnothing().
    """

    pattern = :(
        ~or(
            nothing == _,
            _ == nothing,
            nothing != _,
            _ != nothing
        )
    )
end


@define_rule_in_group lang_rules "useless-equals" quote
    description = """
    Comparing the same object in the RHS and LHS is pointless.
    """

    pattern = :(
        ~or(
            _x == _x,
            _x != _x,
            _x === _x,
            _x !== _x
        )
    )
end
