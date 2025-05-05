using Argus

lang_rules = RuleGroup("lang")

@define_rule_in_group lang_rules "chained-const-assignment" begin
    description = """
    Do not chain assignments with const. The right hand side is not constant here.
    """

    pattern = :(
    const m"x" = m"y" = m"_"
    )
end

@define_rule_in_group lang_rules "useless-equals" begin
    description = """
    Comparing the same object in the RHS and LHS is pointless.
    """

    # TODO: Add alternative for `===`.
    pattern = :(
    m"x" == m"x"
    )
end
