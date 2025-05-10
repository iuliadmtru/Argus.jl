struct Rule
    name::String
    description::String
    pattern::Pattern
end

Rule(name::String, description::String, pattern::Expr) =
    Rule(name, description, Pattern(pattern))

macro rule(name, ex)
    # Remove line number nodes.
    rule = MacroTools.striplines(ex)
    # Check the rule syntax.
    @isexpr(rule, :block) || error("Invalid @rule syntax: $rule")
    length(rule.args) == 2 ||
        error("Invalid @rule syntax: $rule\n",
              "Expected 2 arguments, got $(length(rule.args))")
    # Get the first argument and see whether it's a description or a pattern argument.
    arg1 = rule.args[1]
    @isexpr(arg1, :(=), 2) || error("Invalid @rule argument syntax: $arg1")
    arg1_type = arg1.args[1]
    if arg1_type === :description
        description = arg1.args[2]
        isa(description, String) ||
            error("Invalid description type $(typeof(description)): $description")
        # Get the second argument, which should be the pattern.
        pattern_expr = rule.args[2]
        @isexpr(pattern_expr, :(=), 2) ||
            error("Invalid @rule argument syntax: $pattern_expr")
        pattern_expr.args[1] === :pattern ||
            error("Invalid @rule argument name: $(pattern_expr.args[1])")
        pattern = pattern_expr.args[2]
        # Don't include the wrapping quote in the pattern.
        if isa(pattern, Expr) && pattern.head === :quote
            pattern = pattern.args[1]
        end
    elseif arg1_type === :pattern
        pattern = pattern_expr.args[2]
        # Don't include the wrapping quote in the pattern.
        if isa(pattern, Expr) && pattern.head === :quote
            pattern = pattern.args[1]
        end
        # Get the second argument, which should be the description.
    else
        error("Invalid @rule argument: $arg1_type")
    end

    return :( Rule($name, $description, $pattern) )
end
