# TODO: Add Rule struct with name, description and pattern.

# ------------------------------------------------------------------------------------------
# Rule definition.

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
    # Get pattern.
    pattern_node = rule.args[2]
    @isexpr(pattern_node, :(=), 2) ||
        error("Unrecognized pattern syntax: $pattern_node")
    pattern = pattern_node.args[2]
    # @isexpr(pattern, :quote) || @isexpr(pattern, :where) || isa(pattern, QuoteNode) ||
    isa(pattern, String) || error("Unrecognized pattern pattern syntax: \"$pattern\"")

    return SyntaxPatternNode(pattern)
end

macro rule(rule_name, rule)
    create_rule(rule_name, rule)
end

# ------------------------------------------------------------------------------------------
# Rule groups.

const DEFAULT_RULE_GROUP_NAME = "default"

struct RuleGroup <: AbstractDict{String, SyntaxPatternNode}
    name::String
    rules::Dict{String, SyntaxPatternNode}

    RuleGroup() = new(DEFAULT_RULE_GROUP_NAME, Dict{String, SyntaxPatternNode}())
    RuleGroup(name::String) = new(name, Dict{String, SyntaxPatternNode}())
    RuleGroup(kvs) = new(DEFAULT_RULE_GROUP_NAME, Dict{String, SyntaxPatternNode}(kvs))
    RuleGroup(name::String, kvs) = new(name, Dict{String, SyntaxPatternNode}(kvs))
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

## Display.

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

# --------------------------------------------
# Utils.

function register_rule!(group::RuleGroup, rule::SyntaxPatternNode, rule_name::String)
    # TODO: Add interactive "overwrite?".
    group[rule_name] = rule
end
