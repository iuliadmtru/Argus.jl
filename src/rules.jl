using MacroTools: MacroTools, striplines

# ------------------------------------------------------------------------------------------
# Rules.

struct Rule
    name::String
    description::String
    pattern::Pattern
end

Rule(name::String, description::String, pattern) =
    Rule(name, description, Pattern(pattern))

macro rule(name, ex)
    # Remove line number nodes and unescape.
    rule = MacroTools.striplines(ex)
    rule = Meta.unescape(rule)
    # Check the rule syntax.
    @isexpr(rule, :quote, 1) ||
        error("Invalid rule syntax:\n```\n$rule\n```\n",
              "Rules should be defined inside `quote ... end`.")
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

# Display.

function Base.show(io::IO, rule::Rule)
    println(io, rule.name, ": ", rstrip(rule.description))
    show(io, MIME("text/plain"), rule.pattern)
end

# ------------------------------------------------------------------------------------------
# Rule groups.

const DEFAULT_RULE_GROUP_NAME = "default"

struct RuleGroup <: AbstractDict{String, Rule}
    name::String
    rules::Dict{String, Rule}

    RuleGroup() = new(DEFAULT_RULE_GROUP_NAME, Dict{String, Rule}())
    RuleGroup(name::String) = new(name, Dict{String, Rule}())
    RuleGroup(kvs) = new(DEFAULT_RULE_GROUP_NAME, Dict{String, Rule}(kvs))
    RuleGroup(name::String, kvs) = new(name, Dict{String, Rule}(kvs))
end

# Dict interface.

Base.isempty(rg::RuleGroup) = isempty(rg.rules)
Base.empty!(rg::RuleGroup) = empty!(rg.rules)
Base.length(rg::RuleGroup) = length(rg.rules)

Base.iterate(rg::RuleGroup) = iterate(rg.rules)
Base.iterate(rg::RuleGroup, i::Int) = iterate(rg.rules, i)
Base.setindex!(rg::RuleGroup, v, k...) = setindex!(rg.rules, v, k...)

Base.haskey(rg::RuleGroup, k) = haskey(rg.rules, k)
Base.get(rg::RuleGroup, k, d) = get(rg.rules, k, d)
Base.get(f::Union{Function, Type}, rg::RuleGroup, k) = get(f, rg.rules, k)
Base.get!(rg::RuleGroup, k, d) = get!(rg.rules, k, d)
Base.get!(f::Union{Function, Type}, rg::RuleGroup, k) = get!(f, rg.rules, k)
Base.getkey(rg::RuleGroup, k, d) = getkey(rg.rules, k, d)
Base.delete!(rg::RuleGroup, k) = delete!(rg.rules, k)
Base.pop!(rg::RuleGroup, k) = pop!(rg.rules, k)
Base.pop!(rg::RuleGroup, k, d) = pop!(rg.rules, k, d)
Base.keys(rg::RuleGroup) = keys(rg.rules)
Base.values(rg::RuleGroup) = values(rg.rules)
Base.pairs(rg::RuleGroup) = pairs(rg.rules)
Base.merge(rg::RuleGroup, others::RuleGroup...) =
    RuleGroup(merge(rg.rules, others...))
Base.mergewith(c, rg::RuleGroup, others::RuleGroup...) =
    RuleGroup(mergewith(c, rg.rules, others...))
Base.merge!(rg::RuleGroup, others::RuleGroup...) =
    RuleGroup(merge!(rg.rules, others...))
Base.mergewith!(c, rg::RuleGroup, others::RuleGroup...) =
    RuleGroup(mergewith!(c, rg.rules, others...))
Base.keytype(rg::RuleGroup) = keytype(rg.rules)
Base.valtype(rg::RuleGroup) = valtype(rg.rules)

# Display.

function Base.summary(io::IO, rg::RuleGroup)
    Base.showarg(io, rg, true)
    n = length(rg)
    print(io, "(\"", rg.name, "\") with ", n, (n==1 ? " entry" : " entries"))
end

Base.show(io::IO, rg::RuleGroup) =
    isempty(rg)                               ?
    print(io, "RuleGroup(\"", rg.name, "\")") :
    invoke(show, Tuple{IOBuffer, AbstractDict}, io, rg)

# --------------------------------------------
# Rule definition in groups.

function define_rule_in_group(group::RuleGroup, rule_name::String, rule::Rule)
    # rule = @rule(rule_name, rule_expr)
    register_rule!(group, rule)

    return rule
end

# # TODO: Make internal.
# function define_rule_in_group(group::RuleGroup, rule_name::String, rule_str::String)
#     rule = Meta.parse(rule_str)
#     return define_rule_in_group(group, rule_name, rule)
# end

# # TODO: Find a way to make this nicer...
# handle_define_rule_in_group(group, rule_name, rule_str) =
#     esc( :($define_rule_in_group($group, $rule_name, $rule_str)) )

macro define_rule_in_group(group, rule_name, rule_expr)
    # rule_str = string(rule)
    # handle_define_rule_in_group(group, rule_name, rule_str)
    rule = :( @rule($(esc(rule_name)), $(esc(rule_expr))) )
    return :( define_rule_in_group($(esc(group)), $(esc(rule_name)), $rule) )
end

# --------------------------------------------
# Utils.

function register_rule!(group::RuleGroup, rule::Rule)
    # TODO: Add interactive "overwrite?".
    group[rule.name] = rule
end

# ------------------------------------------------------------------------------------------
# Rule matching.

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
