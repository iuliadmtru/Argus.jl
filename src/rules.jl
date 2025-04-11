## -----------------------------------------------------------------------------------------
## Rule definition.

function create_rule(rule_name::String, rule::Expr)
    rule = MacroTools.striplines(rule)
    # TODO: Include position in error messages.
    # TODO: Generally improve error messages.
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

    return SyntaxTemplateNode(template)
end

macro rule(rule_name, rule)
    create_rule(rule_name, rule)
end

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

# ACTIVE_RULE_GROUPS = RuleGroup[]

# Base.in(item::RuleGroup, ACTIVE_RULE_GROUPS) =
#    !isnothing(findfirst(g -> g.name == item.name, ACTIVE_RULE_GROUPS))

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

## Display.

function Base.summary(io::IO, rg::RuleGroup)
    Base.showarg(io, rg, true)
    n = length(rg)
    print(io, " \"", rg.name, "\" with ", n, (n==1 ? " entry" : " entries"))
end

Base.show(io::IO, rg::RuleGroup) =
    isempty(rg)                                   ?
    print(io, "RuleGroup(\"", rg.name, "\", {})") :
    invoke(show, AbstractDict, io, rg)

## -------------------------------------------
## Utils.
# active_rule_groups() = ACTIVE_RULE_GROUPS

# function activate_rule_group(group::RuleGroup)
#     if group in active_rule_groups()
#         @info "Rule group $(group.name) already active"
#     else
#         push!(ACTIVE_RULE_GROUPS, group)
#         @info "Activated rule group $(group.name)"
#     end

#     return nothing
# end

function register_rule!(group::RuleGroup, rule::SyntaxTemplateNode, rule_name::String)
    # TODO: Add interactive "overwrite?".
    group[rule_name] = rule
end
# function register_rule!(rule::SyntaxTemplateNode, rule_name::String, group_name::String)
#     # TODO: Add interactive "overwrite?".
#     group_idx = findfirst(g -> g.name == group_name, ACTIVE_RULE_GROUPS)
#     isnothing(group_idx) &&
#         error("Could not register rule $rule_name. No active group $group_name")
#     group = active_rule_groups()[group_idx]
#     register_rule!(rule, rule_name, group)
# end

## -------------------------------------------
## Rule definition in groups.

function define_rule_in_group(group::RuleGroup, rule_name::String, rule::Expr)
    rule_node = create_rule(rule_name, rule)
    register_rule!(group, rule_node, rule_name)

    return rule_node
end

# TODO: Make internal.
function define_rule_in_group(group::RuleGroup, rule_name::String, rule_str::String)
    rule = Meta.parse(rule_str)
    return define_rule_in_group(group, rule_name, rule)
end

# TODO: Find a way to make this nicer...
handle_define_rule_in_group(group, rule_name, rule_str) =
    esc( :($define_rule_in_group($group, $rule_name, $rule_str)) )

macro define_rule_in_group(group, rule_name, rule)
    rule_str = string(rule)
    handle_define_rule_in_group(group, rule_name, rule_str)
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
