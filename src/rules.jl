## -----------------------------------------------------------------------------------------
## Rule groups.

struct RuleGroup <: AbstractDict{String, SyntaxTemplateNode}
    name::String
    rules::Dict{String, SyntaxTemplateNode}

    RuleGroup() = new(DEFAULT_RULE_GROUP_NAME, Dict{String, SyntaxTemplateNode}())
    RuleGroup(name::String) = new(name, Dict{String, SyntaxTemplateNode}())
    RuleGroup(kvs) = new(DEFAULT_RULE_GROUP_NAME, Dict{String, SyntaxTemplateNode}(kvs))
    RuleGroup(name::String, kvs) = new(name, Dict{String, SyntaxTemplateNode}(kvs))
end

ACTIVE_RULE_GROUPS = RuleGroup[]

Base.in(item::RuleGroup, ACTIVE_RULE_GROUPS) =
    !isnothing(findfirst(g -> g.name == item.name, ACTIVE_RULE_GROUPS))

const DEFAULT_RULE_GROUP_NAME = "default"
DEFAULT_RULE_GROUP = RuleGroup()

## Dict interface.

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
    RuleGroup(merge(rg.rules, others...))
Base.mergewith!(c, rg::RuleGroup, others::RuleGroup...) =
    RuleGroup(mergewith(c, rg.rules, others...))
Base.keytype(rg::RuleGroup) = keytype(rg.rules)
Base.valtype(rg::RuleGroup) = valtype(rg.rules)

## Utils.

function activate_group(group::RuleGroup)
    if group in ACTIVE_RULE_GROUPS
        @info "Rule group $(group.name) already active"
    else
        push!(ACTIVE_RULE_GROUPS, group)
        @info "Activated rule group $(group.name)"
    end

    return nothing
end

## Display.

function Base.summary(io::IO, rg::RuleGroup)
    Base.showarg(io, rg, true)
    n = length(rg)
    print(io, " ", rg.name, " with ", n, (n==1 ? " entry" : " entries"))
end

## -----------------------------------------------------------------------------------------
## Rule definition.

handle_define_rule(rule_name, rule) =
    handle_define_rule(rule_name, DEFAULT_RULE_GROUP, rule)

function handle_define_rule(rule_name, group, rule)
    # TODO: Include position in error messages.
    # TODO: Generally improve error messages.
    isa(rule_name, String) ||
        error("Invalid rule name type $(typeof(rule_name)) for $name. Expected String")
    isa(group, String) || isa(group, RuleGroup) ||
        error("Invalid group name type $(typeof(group)) for $group")
    @isexpr(rule, :block) || error("Unrecognized @define_rule syntax: $rule")
    # Get description.
    description_node = rule.args[1]
    @isexpr(description_node, :(=), 2) ||
        error("Unrecognized description syntax: $description_node")
    description = description_node.args[2]
    isa(description, String) ||
        error("Invalid description type $(typeof(description)) for $description")
    # Get template.
    template_node = rule.args[2]
    @isexpr(template_node, :(=), 2) ||
        error("Unrecognized template syntax: $template_node")
    template = template_node.args[2]
    # @isexpr(template, :quote) || @isexpr(template, :where) || isa(template, QuoteNode) ||
    isa(template, String) || error("Unrecognized template pattern syntax: \"$template\"")
    # Upload to rule group.
    template_node = SyntaxTemplateNode(template)
    register!(template_node, rule_name, group)

    return template_node
end

macro define_rule(rule_name, rule)
    handle_define_rule(rule_name, MacroTools.striplines(rule))
end

macro define_rule(rule_name, group, rule)
    handle_define_rule(rule_name, group, MacroTools.striplines(rule))
end

## Utils.

function register!(rule::SyntaxTemplateNode, rule_name::String, group::RuleGroup)
    # TODO: Add interactive "overwrite?".
    group[rule_name] = rule
end
function register!(rule::SyntaxTemplateNode, rule_name::String, group_name::String)
    # TODO: Add interactive "overwrite?".
    group[rule_name] = rule
end

## -----------------------------------------------------------------------------------------
## Rule matching.

"""
    rule_match!(rule::AbstractSyntaxPattern, src::JuliaSyntax.SyntaxNode)::SyntaxMatches

Try to match a rule to source code. Return all matches as a `SyntaxMatches` array.
"""
function rule_match!(rule, src) end
# TODO: Keep this?
rule_match!(rule::SyntaxTemplateNode, src::JuliaSyntax.SyntaxNode)::SyntaxMatches =
    template_match!(rule, src)
# TODO: Move this functionality to `template_match!` instead?
function rule_match!(rule::SyntaxTemplateNode, src_file::AbstractString)::SyntaxMatches
    src_txt = read(src_file, String)
    src = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src_txt; filename=src_file)

    return rule_match!(rule, src)
end
function rule_match!(rule_path::AbstractString, src_file::AbstractString)::SyntaxMatches
    # Get the correct path to the rule.
    dir_name, file_name = splitdir(rule_path)
    registry_path = isempty(dir_name) ? DEFAULT_RULES_REGISTRY : dir_name
    ispath(registry_path) || error("Unexistent rule registry path $registry_path")
    full_rule_path = joinpath(registry_path, file_name)
    ispath(full_rule_path) || error("Unexistent rule path $full_rule_path")
    # Read the rule.
    rule = deserialize(full_rule_path)

    return rule_match!(rule, src_file)
end
function rule_match!(rule_path::AbstractString, src::JuliaSyntax.SyntaxNode)::SyntaxMatches
    # Get the correct path to the rule.
    dir_name, file_name = splitdir(rule_path)
    registry_path = isempty(dir_name) ? DEFAULT_RULES_REGISTRY : dir_name
    ispath(registry_path) || error("Unexistent rule registry path $registry_path")
    full_rule_path = joinpath(registry_path, file_name)
    ispath(full_rule_path) || error("Unexistent rule path $full_rule_path")
    # Read the rule.
    rule = deserialize(full_rule_path)

    return rule_match!(rule, src)
end
