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
    @isexpr(template, :quote) || @isexpr(template, :where) || isa(template, QuoteNode) ||
        error("Unrecognized template pattern syntax: \"$template\"")
    # Upload to rule registry.
    rule = SyntaxTemplateNode(template)
    register(rule, name)

    return rule
end

macro define_rule(name, rule)
    handle_define_rule(name, MacroTools.striplines(rule))
end

## Utils.

# TODO: Should this receive `rule_name` and `registry_path` instead?
function register(rule::SyntaxTemplateNode, name::String)
    dir_name, file_name = splitdir(name)
    registry_path = isempty(dir_name) ? DEFAULT_RULES_REGISTRY : dir_name
    # TODO: Error handling.
    ispath(registry_path) || mkpath(registry_path)
    rule_path = joinpath(registry_path, file_name)
    touch(rule_path)  # Create rule file if non-existent.
    # Store rule.
    serialize(rule_path, rule)
    @info "Rule stored at $(abspath(rule_path))."
end
