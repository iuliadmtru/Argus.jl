using MacroTools: MacroTools, striplines

struct Rule
    name::String
    description::String
    pattern::Pattern
end

Rule(name::String, description::String, pattern) =
    Rule(name, description, Pattern(pattern))

macro rule(name, ex)
    # Remove line number nodes.
    rule = MacroTools.striplines(ex)
    # Check the rule syntax.
    @isexpr(rule, :quote, 1) || error("Invalid rule syntax:\n$rule")
    rule = rule.args[1]
    length(rule.args) == 2 ||
        error("Invalid rule syntax: $rule\n",
              "Expected 2 arguments, got $(length(rule.args))")
    # Get the first argument and see whether it's a description or a pattern argument.
    arg1 = rule.args[1]
    @isexpr(arg1, :(=), 2) || error("Invalid rule argument syntax:\n$arg1")
    arg1_name = arg1.args[1]
    if arg1_name === :description
        description = arg1.args[2]
        isa(description, String) ||
            error("Invalid description type in $description\n",
                  "Expected String, got $(typeof(description))")
        # Get the second argument, which should be the pattern.
        pattern_node = rule.args[2]
        @isexpr(pattern_node, :(=), 2) ||
            error("Invalid rule argument syntax:\n$pattern_node")
        pattern_node.args[1] === :description &&
            error("Duplicate rule argument: description")
        pattern_node.args[1] === :pattern ||
            error("Invalid rule argument: $(pattern_node.args[1])")
        pattern = pattern_node.args[2]
    elseif arg1_name === :pattern
        pattern = arg1.args[2]
        # Get the second argument, which should be the description.
        description_node = rule.args[2]
        @isexpr(description_node, :(=), 2) ||
            error("Invalid rule argument syntax:\n$description_node")
        description_node.args[1] === :pattern &&
            error("Duplicate rule argument: pattern")
        description_node.args[1] === :description ||
            error("Invalid rule argument: $(description_node.args[1])")
        description = description_node.args[2]
        isa(description, String) ||
            error("Invalid description type in $description\n",
                  "Expected String, got $(typeof(description))")
    else
        error("Invalid rule argument: $arg1_name")
    end

    return :( Rule($name, $description, $pattern) )
end

struct RuleMatchResult
    failures::Vector{MatchFail}
    matches::Vector{BindingSet}
end
RuleMatchResult() = RuleMatchResult([], [])

Base.push!(rule_match_result::RuleMatchResult, res) =
    isa(res, MatchFail)                    ?
    push!(rule_match_result.failures, res) :
    push!(rule_match_result.matches, res)

function Base.append!(res1::RuleMatchResult, res2::RuleMatchResult)
    append!(res1.failures, res2.failures)
    append!(res1.matches, res2.matches)
end

"""
    rule_match(rule::Rule, src::Juliasyntax.SyntaxNode; only_matches=true)

Match a rule against an AST. Return the set of all matches. If `only_matches` is set to
`false` return failures as well.
"""
function rule_match(rule::Rule, src::JuliaSyntax.SyntaxNode; only_matches=true)
    rule_result = RuleMatchResult()
    match_result = syntax_match(rule.pattern, src)
    push!(rule_result, match_result)
    # Recurse on children, if any. Collect all match results.
    is_leaf(src) && return only_matches ? rule_result.matches : rule_result
    for c in children(src)
        rule_result_child = rule_match(rule, c; only_matches=false)
        append!(rule_result, rule_result_child)
    end
    return only_matches ? rule_result.matches : rule_result
end
"""
    rule_match(rule::Rule, src_file::String; only_matches=true)

Match a rule against a source file.
"""
function rule_match(rule::Rule, src_file::String; only_matches=true)
    src_txt = read(src_file, String)
    src = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src_txt; filename=src_file)

    return rule_match(rule, src; only_matches)
end
