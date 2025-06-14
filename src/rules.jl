# Rules
# =====

"""
    Rule

Rule for syntax matching. Consists of a name, descrciption and pattern.
"""
struct Rule
    name::String
    description::String
    pattern::Pattern
end

"""
    @rule(name, ex)

Create a [`Rule`](@ref) from a name string and a rule body expression.

# Examples
# ========

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
# ========

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

# Rule matching
# =============

"""
    RuleMatchResult

The result of a rule match. It contains a vector of `BindingSet`s and a vector of
`MatchFail`s. Usually, only the binding sets are relevant when inspecting the result of a
rule match.
"""
struct RuleMatchResult
    matches::Vector{BindingSet}
    failures::Vector{MatchFail}
end
RuleMatchResult() = RuleMatchResult([], [])

"""
    RuleGroupMatchResult

The result of a rule group match. Alias for `Dict{String, RuleMatchResult}`.
"""
const RuleGroupMatchResult = Dict{String, RuleMatchResult}

# TODO: Optimisations!
"""
    rule_match(rule::Rule, src::Juliasyntax.SyntaxNode; greedy=true, only_matches=true)
    rule_match(rule::Rule, filename::String; greedy=true, only_matches=true)

Match a rule against a source code. Return the set of all matches. If `only_matches` is
`false` return failures as well. The rule pattern is matched against all children nodes in
the source node, up to the leafs. Matching is greedy by default.
"""
function rule_match(rule::Rule, src::JS.SyntaxNode; greedy=true, only_matches=true)
    rule_result = RuleMatchResult()
    pattern_node = rule.pattern.src
    if is_toplevel(rule.pattern) && (kind(src) == K"block" || kind(src) == K"toplevel")
        # Both the pattern and the source are series of expressions. Match the pattern's
        # expressions sequence with all the sub-sequences in the source.
        if has_reps(pattern_node) || length(children(pattern_node)) != length(children(src))
            srcs = children(src)
            while !isempty(srcs)
                recovery_stack = []
                partial_result, _ = partial_syntax_match(children(rule.pattern),
                                                         srcs;
                                                         recovery_stack,
                                                         greedy)
                # If there are recovery paths, try them all and store the results.
                while !isempty(recovery_stack)
                    partial_recovered_result, _ = recover!(recovery_stack,
                                                           partial_syntax_match,
                                                           true;
                                                           fail_ret=MatchFail(),
                                                           greedy)
                    push_match_result!(rule_result, partial_recovered_result; only_matches)
                end
                push_match_result!(rule_result, partial_result; only_matches)
                srcs = rest(srcs)
            end
            # Recurse on source children, if any.
            is_leaf(src) && return rule_result
            for c in children(src)
                rule_result_child = rule_match(rule, c; greedy, only_matches)
                append!(rule_result.failures, rule_result_child.failures)
                append!(rule_result.matches, rule_result_child.matches)
            end
            return rule_result
        else
            # The pattern doesn't have repetitions and it has the same number of children as
            # the source. Gather all the matching possibilities of their children and
            # combine the result.
            return match_and_combine_children(rule_result,
                                              rule,
                                              pattern_node,
                                              src,
                                              greedy,
                                              only_matches)
        end
    else
        # The pattern and the source are simple expressions. Exhaust all the possibly
        # matching paths.
        recovery_stack = []
        if _is_leaf(pattern_node)
            # If the pattern node has no children, we can try to match it with the source
            # node without recursing on the source node's children.
            match_result = _syntax_match(rule.pattern.src, src; recovery_stack, greedy)
            push_match_result!(rule_result, match_result; only_matches)
            while !isempty(recovery_stack)
                match_result = recover!(recovery_stack,
                                        _syntax_match,
                                        true;
                                        fail_ret=MatchFail(),
                                        greedy)
                push_match_result!(rule_result, match_result; only_matches)
            end
            return rule_result
        elseif !has_reps(pattern_node)
            # If the pattern doesn't have ellipsis nodes among its children, we need to get
            # all the possibilities for each child and combine the resulting binding sets
            # into a cartesian product.
            !compatible(pattern_node, src; recurse=false) &&
                # If the pattern node and source node are not compatible there can be no
                # match at this depth. Continue with the source node's children.
                return match_and_recover!(rule_result,
                                          rule,
                                          src,
                                          greedy,
                                          only_matches;
                                          recurse=true)
            # If they are compatible, gather all the results from matching their children.
            return match_and_combine_children(rule_result,
                                              rule,
                                              pattern_node,
                                              src,
                                              greedy,
                                              only_matches)
        else
            # If the pattern is not a leaf and it has repetitions, try to match it and
            # continue with the source's children.
            return match_and_recover!(rule_result,
                                      rule,
                                      src,
                                      greedy,
                                      only_matches;
                                      recurse=true)
        end
    end
end
function rule_match(rule::Rule, filename::String; greedy=true, only_matches=true)
    src_txt = read(filename, String)
    src = JS.parseall(JS.SyntaxNode, src_txt; filename=filename)

    return rule_match(rule, src; greedy, only_matches)
end

"""
    rule_group_match(group::RuleGroup,
                     src::JuliaSyntax.SyntaxNode;
                     greedy=true,
                     only_matches=true)
    rule_group_match(group::RuleGroup,
                     filename::String;
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
                          filename::String;
                          greedy=true,
                          only_matches=true)
    src_txt = read(filename, String)
    src = JS.parseall(JS.SyntaxNode, src_txt; filename=filename)

    return rule_group_match(group, src; greedy, only_matches)
end

# Utils

function push_match_result!(rule_result::RuleMatchResult,
                           match_result::MatchResult;
                           only_matches=true)
    if isa(match_result, MatchFail)
        only_matches || push!(rule_result.failures, match_result)
    else
        push!(rule_result.matches, make_permanent(match_result))
    end
end

function has_reps(pattern::SyntaxPatternNode)
    for p in children(pattern)
        is_rep(p) && return true
    end
    return false
end

"""
    resolve_conflicts_and_merge(matches::Vector{<:Tuple{Vararg{BindingSet}}})

Called after combining all the `BindingSet` possibilities of a pattern's children.

Try to merge all the children `BindingSet` pairs into singles `BindingSet`s. Return a vector
of `BindingSet`s. Don't include `BindingSet`s that have conflicting `Binding`s.

# Examples
# ========

For the following pattern-source pair:

```
julia> pattern_node = SyntaxPatternNode(:(
           function {f}()
               {_1}...
               {ex}
               {_2}...
           end
       ));

julia> src = parsestmt(SyntaxNode, \"""
                                   function f()
                                       ex1
                                       ex2
                                       ex3
                                   end
                                   \""");
```

the pattern's children have the following matching possibilities:

  - (call f) can match with the bindings:
    - BindingSet(
        :f => f
      )
  - (block (rep (_:::expr)) (ex:::expr) (rep (_:::expr))) can match:
    - BindingSet(
        :_1 => [ex1, ex2]
        :_2 => []
      )
    - BindingSet(
        :_1 => [ex1]
        :_2 => [ex2]
      )
    - BindingSet(
        :_1 => []
        :_2 => [ex1, ex2]
      )

The overall matching possibilities of the pattern are:

  - BindingSet(
      :f => f
      :_1 => [ex1, ex2]
      :_2 => []
    )
  - BindingSet(
      :f => f
      :_1 => [ex1]
      :_2 => [ex2]
    )
  - BindingSet(
      :f => f
      :_1 => []
      :_2 => [ex1, ex2]
    )

None of the binding sets has conflicting pattern variables so this is the final result.
"""
function resolve_conflicts_and_merge(matches::Vector{<:Tuple{Vararg{BindingSet}}})
    final_matches = BindingSet[]
    for bs_tuple in matches
        final_bs = BindingSet()
        conflicting = false
        for bs in bs_tuple
            conflicting && break
            for (b_name, b) in bs
                if haskey(final_bs, b_name)
                    b = final_bs[b_name]
                    if compatible(b.src, b.src)
                        final_bs[b_name] = b
                        continue
                    else
                        conflicting = true
                        break
                    end
                else
                    final_bs[b_name] = b
                end
            end
        end
        if !conflicting
            push!(final_matches, final_bs)
        end
    end
    return final_matches
end

function match_and_recover!(rule_result::RuleMatchResult,
                            rule::Rule,
                            src::JS.SyntaxNode,
                            greedy,
                            only_matches;
                            recurse)
    recovery_stack = []
    match_result =
        _syntax_match(rule.pattern.src, src; recovery_stack, greedy)
    push_match_result!(rule_result, match_result; only_matches)
    while !isempty(recovery_stack)
        match_result = recover!(recovery_stack,
                                _syntax_match,
                                true;
                                fail_ret=MatchFail(),
                                greedy)
        push_match_result!(rule_result, match_result; only_matches)
    end
    if recurse
        is_leaf(src) && return rule_result
        for c in children(src)
            rule_result_child = rule_match(rule, c; greedy, only_matches)
            append!(rule_result.failures, rule_result_child.failures)
            append!(rule_result.matches, rule_result_child.matches)
        end
    end
    return rule_result
end

"""
    match_and_combine_children(rule_result::RuleMatchResult,
                               rule::Rule,
                               pattern_node::SyntaxPatternNode,
                               src::JS.SyntaxNode,
                               greedy,
                               only_matches)

Match all the children of `pattern_node` against the children of `src`, keeping track of all
matching possibilities. Combine the resulting matches into a cartesian product. For each
resulting `BindingSet` pair, merge the `BindingSet`s into one (see
[`resolve_conflicts_and_merge`](@ref)). Return all resulting `BindingSet`s.

`pattern_node` and `src` must have the same number of children.

# Examples
# ========

```
julia> rule = Rule("", "", Pattern(SyntaxPatternNode(:( dummy ))));

julia> rule_result = RuleMatchResult();

julia> pattern_node = SyntaxPatternNode(:(
           function f()
               {_}...
               {ex}
               {_}...
           end
       ));

julia> src = parsestmt(SyntaxNode, \"""
                                   function f()
                                       ex1
                                       ex2
                                       ex3
                                   end
                                   \""");

julia> Argus.match_and_combine_children(rule_result, rule, pattern_node, src, true, true)
RuleMatchResult with 3 matches and 0 failures:
Matches:
  BindingSet(:ex => Binding(:ex, ex3 @ 4:5, BindingSet()))
  BindingSet(:ex => Binding(:ex, ex2 @ 3:5, BindingSet()))
  BindingSet(:ex => Binding(:ex, ex1 @ 2:5, BindingSet()))
```
"""
function match_and_combine_children(rule_result::RuleMatchResult,
                                    rule::Rule,
                                    pattern_node::SyntaxPatternNode,
                                    src::JS.SyntaxNode,
                                    greedy,
                                    only_matches)
    zipped_children = zip(children(pattern_node), children(src))
    children_matches = []
    for (pattern_c, pattern_s) in zipped_children
        rule_c = Rule("", "", Pattern(pattern_c))
        matches_c = rule_match(rule_c, pattern_s; greedy, only_matches).matches
        isempty(matches_c) &&
            # If a pattern's child node and the corresponding source child node
            # don't match there can be no match at this depth. Continue with the
            # source node's children.
            return match_and_recover!(rule_result,
                                      rule,
                                      src,
                                      greedy,
                                      only_matches;
                                      recurse=true)
        push!(children_matches, matches_c)
    end
    # Combine all the children match possibilities (cartesian product).
    combined_children_matches =
        vcat(collect(Iterators.product(children_matches...))...)
    # Remove all the matches that have conflicting bindings and add the remaining
    # matches to the final result.
    append!(rule_result.matches, resolve_conflicts_and_merge(combined_children_matches))
    return rule_result
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
            println(io)
            print(io, "  ")
            show(io, m)
        end
    end
    if !isempty(fails)
        println(io)
        print(io, "Failures:")
        short_fails = fails[1:end-1]
        if length(fails) > 10
            short_fails = fails[1:3]
        end
        for f in short_fails
            println(io)
            print(io, "  ")
            show(io, f)
        end
        # Show the last one.
        println(io)
        println(io, "  .\n  .\n  .")
        print(io, "  ")
        show(io, fails[end])
    end
end
Base.show(io::IO, ::Type{RuleGroupMatchResult}) = "RuleGroupMatchResult"
