using Argus

lang_group = RuleGroup("lang")

@define_rule_in_group lang_group "chained-const-assignment" begin
    description = """
    Do not chain assignments with const. The right hand side is not constant here.
    """

    template = """
    const %x = %y = %_
    """
end
