# ------------------------------------------------------------------------------------------
# Rules.

struct Rule
    name::String
    description::String
    pattern::Pattern
end

macro rule(name, ex)
    err_msg_general =
        """
        invalid `@rule` syntax
        The `@rule` body should be defined using a `begin ... end` block.
        """
    err_msg_invalid_arg_syntax =
        """
        invalid `@rule` argument syntax
        """
    # Check the rule syntax.
    @isexpr(ex, :block) ||
        throw(SyntaxError(err_msg_general, __source__.file, __source__.line))
    length(ex.args) == 4 ||
        throw(SyntaxError("""
                          invalid `@rule` syntax
                          Expected 2 arguments, got $(length(ex.args)/2).""",
                          __source__.file,
                          __source__.line))
    # Get the first argument, which should be the description.
    line_number_arg1 = ex.args[1]
    arg1 = MacroTools.striplines(ex.args[2])
    @isexpr(arg1, :(=), 2) ||
        throw(SyntaxError(err_msg_invalid_arg_syntax,
                          line_number_arg1.file,
                          line_number_arg1.line))
    arg1_name = arg1.args[1]
    arg1_name === :description ||
        throw(SyntaxError("""
                          invalid rule argument name: $arg1_name
                          The first argument of `@rule` should be `description`.""",
                          line_number_arg1.file,
                          line_number_arg1.line))
    description = arg1.args[2]
    # Get the second argument, which should be the pattern.
    line_number_arg2 = ex.args[3]
    arg2 = ex.args[4]
    @isexpr(arg2, :(=), 2) ||
        throw(SyntaxError(err_msg_invalid_arg_syntax,
                          line_number_arg2.file,
                          line_number_arg2.line))
    arg2_name = arg2.args[1]
    arg2_name === :pattern ||
        throw(SyntaxError("""
                          invalid rule argument name: $arg2_name
                          The second argument of `@rule` should be `pattern`.""",
                          line_number_arg2.file,
                          line_number_arg2.line))
    pattern_expr = arg2.args[2]

    return :( Rule($name, $description, $(esc(pattern_expr))) )
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

define_rule_in_group(group::RuleGroup, rule_name::String, rule::Rule) =
    register_rule!(group, rule)

macro define_rule_in_group(group, rule_name, rule_expr)
    rule = :( @rule($(esc(rule_name)), $(rule_expr)) )
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
    matches::Vector{BindingSet}
    failures::Vector{MatchFail}
end
RuleMatchResult() = RuleMatchResult([], [])

# TODO: Special treatment for repetitions. See TODO in `test/rules.jl`.
"""
    rule_match(rule::Rule, src::Juliasyntax.SyntaxNode; only_matches=true)

Match a rule against an AST. Return the set of all matches. If `only_matches` is set to
`false` return failures as well.
"""
function rule_match(rule::Rule, src::JuliaSyntax.SyntaxNode; only_matches=true)
    rule_result = RuleMatchResult()
    # If the pattern has multiple pattern expressions, they must be matched against all
    # the source "children windows". Otherwise, the pattern must match the source.
    windows = toplevel_wrapped_windows(src, toplevel_exprs_count(rule.pattern))
    for window in windows
        pattern_match_result = syntax_match(rule.pattern, window)
        if isa(pattern_match_result, MatchFail)
            only_matches || push!(rule_result.failures, pattern_match_result)
        else
            push!(rule_result.matches, pattern_match_result)
        end
    end
    # Recurse on children, if any.
    is_leaf(src) && return rule_result
    for c in children(src)
        rule_result_child = rule_match(rule, c; only_matches)
        append!(rule_result.failures, rule_result_child.failures)
        append!(rule_result.matches, rule_result_child.matches)
    end
    return rule_result
end
"""
    rule_match(rule::Rule, filename::String; only_matches=true)

Match a rule against a source file.
"""
function rule_match(rule::Rule, filename::String; only_matches=true)
    src_txt = read(filename, String)
    src = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src_txt; filename=filename)

    return rule_match(rule, src; only_matches)
end

# Utils.

function toplevel_wrapped_windows(src::JuliaSyntax.SyntaxNode, window_size::Int)
    window_size == 0 && return [src]  # The pattern does not have multiple pattern
                                      # expressions. Match the source node as-is.
    # Window sizes less than `window_size` will result in `MatchFail()`s.
    windows = JuliaSyntax.SyntaxNode[]
    is_leaf(src) && return windows
    # Get the window ranges.
    nodes = children(src)
    window_ranges = (:).(1:length(nodes)-(window_size-1), window_size:length(nodes))
    windows = JuliaSyntax.SyntaxNode[]
    # Wrap each window in a `[toplevel]` node.
    for wr in window_ranges
        window_nodes = nodes[wr]
        dummy_toplevel_data =
            update_data_head(window_nodes[1].data, JuliaSyntax.SyntaxHead(K"toplevel", 0))
        push!(windows, JuliaSyntax.SyntaxNode(nothing, window_nodes, dummy_toplevel_data))
    end
    # Return the array of `[toplevel]` nodes with windows as children.
    return windows
end
