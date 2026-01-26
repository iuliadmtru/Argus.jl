# Rules
# =====

"""
    Rule

Rule for syntax matching. Consists of a name, description, pattern and, optionally, a
refactoring template.
"""
struct Rule
    name::String
    description::String
    pattern::Pattern
    template::Union{Nothing, Template}
end
Rule(name::String, description::String, pattern::Pattern) =
    Rule(name, description, pattern, nothing)

"""
    @rule(name, ex)

Create a [`Rule`](@ref) from a name string and a rule body expression.

# Examples

```
julia> consecutive_assign = @rule "consecutive-assign" begin
           description = "Detected reassigned `const` variable."

           pattern = @pattern begin
               const {x:::identifier} = {_}...
               {_}...
               const {x:::identifier} = {_}...
           end
       end
consecutive-assign:
Detected reassigned `const` variable.

Pattern:
[toplevel]
  [const]
    [=]
      x:::identifier                     :: ~var
      [~rep]
        _:::expr                         :: ~var
  [~rep]
    _:::expr                             :: ~var
  [const]
    [=]
      x:::identifier                     :: ~var
      [~rep]
        _:::expr                         :: ~var

julia> rule_match(consecutive_assign, parseall(SyntaxNode, \"""
                                                           const a = 2
                                                           some_expr
                                                           const a = 4
                                                           \"""))
RuleMatchResult with 1 matches and 0 failures:
Matches:
  BindingSet(:x => Binding(:x, a @ 3:7, BindingSet(:_id => Binding(:_id, a @ 3:7, BindingSet()))))

julia> rule_match(consecutive_assign, parseall(SyntaxNode, \"""
                                                           const a = 2
                                                           some_expr
                                                           const a = 4
                                                           \"""); only_matches=false)
RuleMatchResult with 1 matches and 12 failures:
Matches:
  BindingSet(:x => Binding(:x, a @ 3:7, BindingSet(:_id => Binding(:_id, a @ 3:7, BindingSet()))))
Failures:
  MatchFail("no match")
  MatchFail("no match")
  MatchFail("no match")
  .
  .
  .
  MatchFail("no match")
```
"""
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
    length(ex.args) == 4 || length(ex.args) == 6 ||
        throw(SyntaxError("""
                          invalid `@rule` syntax
                          Expected 2 or 3 arguments, got $(length(ex.args)/2).""",
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
    # If there is one, get the third argument, which should be the template.
    template_expr = nothing
    if length(ex.args) == 6
        line_number_arg3 = ex.args[5]
        arg3 = ex.args[6]
        @isexpr(arg3, :(=), 2) ||
            throw(SyntaxError(err_msg_invalid_arg_syntax,
                              line_number_arg3.file,
                              line_number_arg3.line))
        arg3_name = arg3.args[1]
        arg3_name === :template ||
            throw(SyntaxError("""
                              invalid rule argument name: $arg3_name
                              The third argument of `@rule` should be `template`.""",
                              line_number_arg3.file,
                              line_number_arg3.line))
        template_expr = arg3.args[2]
    end

    return :( Rule($name, $description, $(esc(pattern_expr)), $(esc(template_expr))) )
end

# Display
# -------

function Base.show(io::IO, rule::Rule)
    name = isempty(rule.name) ? "<no name>" : rule.name
    description = isempty(rule.description) ? "<no description>" : rule.description
    println(io, name, ":\n", rstrip(description))
    println(io)
    show(io, MIME("text/plain"), rule.pattern)
end

# Rule groups
# ===========

const DEFAULT_RULE_GROUP_NAME = "default"

"""
    RuleGroup <: AbstractDict{String, Rule}

Group of rules with associated names.
"""
struct RuleGroup <: AbstractDict{String, Rule}
    name::String
    rules::Dict{String, Rule}

    RuleGroup() = new(DEFAULT_RULE_GROUP_NAME, Dict{String, Rule}())
    RuleGroup(name::String) = new(name, Dict{String, Rule}())
    RuleGroup(kvs) = new(DEFAULT_RULE_GROUP_NAME, Dict{String, Rule}(kvs))
    RuleGroup(name::String, kvs) = new(name, Dict{String, Rule}(kvs))
end

# Dict interface

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

# Display

function Base.summary(io::IO, rg::RuleGroup)
    Base.showarg(io, rg, true)
    n = length(rg)
    print(io, "(\"", rg.name, "\") with ", n, (n == 1 ? " entry" : " entries"))
end

Base.show(io::IO, rg::RuleGroup) =
    isempty(rg)                               ?
    print(io, "RuleGroup(\"", rg.name, "\")") :
    invoke(show, Tuple{IOBuffer, AbstractDict}, io, rg)

# Rule definition in groups
# -------------------------

"""
    @define_rule_in_group(group, rule_name, rule_expr)

Create a [`Rule`](@ref) from a name string and an expession body and register it in a given
group.

# Examples

```
lang_rules = RuleGroup("lang")

@define_rule_in_group lang_rules "chained-const-assignment" begin
    description = \"""
    Do not chain assignments with const. The right hand side is not constant here.
    \"""

    pattern = @pattern begin
        const {_:::identifier} = {_:::identifier} = {_}
    end
end
```
"""
macro define_rule_in_group(group, rule_name, rule_expr)
    rule = :( @rule($(esc(rule_name)), $rule_expr) )
    return :( register_rule!($(esc(group)), $rule) )
end

"""
    register_rule!(group::RuleGroup, rule::Rule)

Register a rule in a group. Used by [`@define_rule_in_group`](@ref).
"""
function register_rule!(group::RuleGroup, rule::Rule)
    group[rule.name] = rule
end

# Errors
# ======

struct RuleMatchError <: Exception
    msg::String
    rule_name::String
    file::Union{Nothing, String}
    source_location::Tuple{Int64, Int64}
end

# Display

function Base.showerror(io::IO, err::RuleMatchError)
    print(io, "RuleMatchError: ")
    println(io, err.msg)
    isnothing(err.file) ||
        print(io, "@ $(err.file):")
    println(io, "$(err.source_location[1]):$(err.source_location[2])")
end

# Rule matching
# =============

"""
    RuleMatchResult

The result of a rule match. Contains the list of all successful matches with their
associated code refactoring, if any, and the list of all non-trivial match failure messages
if matching with `only_matches=false`.
"""
struct RuleMatchResult
    matches::Vector{Tuple{BindingSet, Union{Nothing, JS.SyntaxNode}}}
    failures::Vector{MatchFail}
end
RuleMatchResult() = RuleMatchResult([], [])

"""
    rule_match(rule::Rule, src::Juliasyntax.SyntaxNode; greedy=true, only_matches=true)
    rule_match(rule::Rule, filename::AbstractString; greedy=true, only_matches=true)

Match a rule against a source code. Return the set of all matches with their associated
refactored code, if applicable. If `only_matches` is `false` return failures as well.
The rule pattern is matched against all children nodes in the source node, up to the leafs.
Matching is greedy by default.

See [`syntax_match_all`](@ref).
"""
function rule_match(rule::Rule, src::JS.SyntaxNode; greedy=true, only_matches=true)
    match_results = syntax_match_all(rule.pattern, src; greedy, only_matches)
    binding_sets = match_results.matches
    matches_with_refactorings = isnothing(rule.template) ?
        [(bs, nothing) for bs in binding_sets] :
        [(bs, expand(rule.template, bs)) for bs in binding_sets]
    return RuleMatchResult(matches_with_refactorings, match_results.failures)
end
function rule_match(rule::Rule, src::AbstractString; greedy=true, only_matches=true)
    if isfile(src)
        src_txt = read(src, String)
        src_node = JS.parseall(JS.SyntaxNode, src_txt; filename=src)
        return rule_match(rule, src_node; greedy, only_matches)
    end
    if isdir(src)
        files = source_files(src)
        match_results = RuleMatchResult()
        for f in files
            match_result = rule_match(rule, f; greedy, only_matches)
            append!(match_results, match_result)
        end
        return match_results
    end
    error("not a file or directory: $src")
end

"""
    RuleGroupMatchResult

The result of a rule group match. Alias for `Dict{String, RuleMatchResults}`.
"""
const RuleGroupMatchResult = Dict{String, RuleMatchResult}

"""
    rule_group_match(group::RuleGroup,
                     src::JuliaSyntax.SyntaxNode;
                     greedy=true,
                     only_matches=true)
    rule_group_match(group::RuleGroup,
                     src::AbstractString;
                     greedy=true,
                     only_matches=true)

Match all the rules in a given group against a source node. Return a
[`RuleGroupMatchResult`](@ref). Matching is greedy by default.
"""
function rule_group_match(group::RuleGroup,
                          src::JS.SyntaxNode;
                          greedy=true,
                          only_matches=true)
    match_result = RuleGroupMatchResult()
    for (name, rule) in group
        match_result[name] = rule_match(rule, src; greedy, only_matches)
    end

    return match_result
end
function rule_group_match(group::RuleGroup,
                          src::AbstractString;
                          greedy=true,
                          only_matches=true)
    if isfile(src)
        src_txt = read(src, String)
        src_node = JS.parseall(JS.SyntaxNode, src_txt; filename=src)
        return rule_group_match(group, src_node; greedy, only_matches)
    end
    if isdir(src)
        files = source_files(src)
        match_results = RuleGroupMatchResult()
        for f in files
            match_result = rule_group_match(group, f; greedy, only_matches)
            for (k, v) in match_result
                haskey(match_results, k)         ?
                    append!(match_results[k], v) :
                    match_results[k] = v
            end
        end
        return match_results
    end
    error("not a file or directory: $src")
end

# Utils

# TODO: More efficient way?
source_files(dir::AbstractString) = read(`find $dir -name '*.jl'`, String) |> split

# Base overwrites

function Base.append!(res1::RuleMatchResult, res2::RuleMatchResult)
    append!(res1.matches, res2.matches)
    append!(res1.failures, res2.failures)

    return res1
end

# Display

Base.summary(io::IO, res::RuleMatchResult) =
    print(io,
          "RuleMatchResult with $(length(res.matches)) matches ",
          "and $(length(res.failures)) failures")

function Base.show(io::IO, ::MIME"text/plain", res::RuleMatchResult)
    summary(io, res)
    matches = res.matches
    fails = res.failures
    isempty(matches) && isempty(fails) && return nothing
    print(io, ":")
    if !isempty(matches)
        println(io)
        print(io, "Matches:")
        for m in matches
            # Source location.
            println(io)
            print(io, "  @ $(_repr_location(m[1]))")
            # Match.
            println(io)
            print(io, "  ")
            show(io, m[1])
            # Substitute.
            if !isnothing(m[2])
                println(io)
                print(io, "  ")
                show(io, m[2])
            end
            println(io)
        end
    end
    if !isempty(fails)
        shorten = length(fails) > 10
        println(io)
        print(io, "Failures:")
        short_fails = fails[1:end-1]
        if shorten
            short_fails = fails[1:3]
        end
        for f in short_fails
            println(io)
            print(io, "  ")
            show(io, f)
        end
        # Show the last one.
        println(io)
        if shorten
            println(io, "  .\n  .\n  .")
        end
        print(io, "  ")
        show(io, fails[end])
    end
end

Base.show(io::IO, ::Type{RuleGroupMatchResult}) = "RuleGroupMatchResult"
