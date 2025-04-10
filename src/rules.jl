## -----------------------------------------------------------------------------------------
## Rules.

const DEFAULT_RULES_REGISTRY = "./rules-registry"

function handle_define_rule(name, rule)
    # TODO: Include position in error messages.
    # TODO: Generally improve error messages.
    isa(name, String) ||
        error("Invalid rule name type $(typeof(name)) for $name. Expected String")
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

function handle_define_rule(rule_name, rule_group, rule)
    # TODO: Include position in error messages.
    # TODO: Generally improve error messages.
    isa(name, String) ||
        error("Invalid rule name type $(typeof(name)) for $name. Expected String")
    isa(registry, String) ||
        error("Invalid registry type $(typeof(registry)) for $registry. Expected String")
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
    # Upload to rule registry.
    template_node = SyntaxTemplateNode(template)
    register(rule, name, registry)

    return template_node
end

macro define_rule(name, rule)
    handle_define_rule(name, MacroTools.striplines(rule))
end

macro define_rule(name, registry, rule)
    handle_define_rule(name, registry, MacroTools.striplines(rule))
end

## Utils.

# TODO: Should this receive `rule_name` and `registry_path` instead?
function register(rule::Expr, rule_name::String, registry_path::String)
    println(string(rule))
    # TODO: Error handling.
    ispath(registry_path) || mkpath(registry_path)
    rule_path = joinpath(registry_path, rule_name)
    touch(rule_path)  # Create rule file if non-existent.
    # Store rule.
    serialize(rule_path, rule)
    
    @info "Rule stored at $(abspath(rule_path))."
end

function _reassemble_rule(rule::Expr, rule_name::String)
    rule_str = "@define_rule \"$rule_name\""
end

## -----------------------------------------------------------------------------------------
## Rule groups.

struct RuleGroup <: AbstractDict{String, SyntaxTemplateNode}
    rules::Dict{String, SyntaxTemplateNode}
    RuleGroup() = new(Dict{String, SyntaxTemplateNode}())
    RuleGroup(kvs) = new(Dict{String, SyntaxTemplateNode}(kvs))
end

## Dict interface.

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
    RuleGroup(merge(rg.rules, others.rules...))
Base.mergewith(c, rg::RuleGroup, others::RuleGroup...) =
    RuleGroup(mergewith(c, rg.rules, others.rules...))
Base.merge!(rg::RuleGroup, others::RuleGroup...) =
    RuleGroup(merge(rg.rules, others.rules...))
Base.mergewith!(c, rg::RuleGroup, others::RuleGroup...) =
    RuleGroup(mergewith(c, rg.rules, others.rules...))
Base.keytype(rg::RuleGroup) = keytype(rg.rules)
Base.valtype(rg::RuleGroup) = valtype(rg.rules)

## Overwrites necessary for `show`.

Base.iterate(rg::RuleGroup) = iterate(rg.rules)
Base.iterate(rg::RuleGroup, i::Int) = iterate(rg.rules, i)
Base.length(rg::RuleGroup) = length(rg.rules)

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
