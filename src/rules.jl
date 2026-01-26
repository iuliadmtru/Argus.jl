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

!!! note
    Matching a rule with a `SyntaxNode` may return different results than matching against
    a file. The reason is that `Expr` and `SyntaxNode` representations differ in some
    respects. These differences may be addressed when parsing a file. Therefore, in case
    of disagreeing results, prefer to trust file-parsing method.

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
        src_expr = JS.parseall(Expr, src_txt)
        src_node = _expr_to_syntax_node(src_expr; file_name=src, rule_name=rule.name)

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

function _expr_to_syntax_node(ex::Expr; file_name::AbstractString="", rule_name::String="")
    ex_str = string(JS.remove_linenums!(ex))
    # # Remove `$` and `Expr` calls.
    # regex = r"\$\((?<ex>Expr(?<parens>\(([^()]|(?&parens))*\)))\)"
    # for m in eachmatch(regex, ex_str; overlap=true)
    #     ex_str =
    #         replace(ex_str, regex => m[:ex])
    # end
    src_node = JS.parseall(JS.SyntaxNode, ex_str; filename=file_name)
    # Remove `$` and `Expr` calls and the first argument of `Expr` (`:toplevel`).
    # TODO: Error handling.
    src_node.children = src_node.children[1].children[1].children[3:end]
    # Remove `quote` calls.
    src_node.children =
        map(c -> c = kind(c) === K"quote" ? c.children[1] : c, src_node.children)

    # Obtain `SyntaxData` by parsing the original code as `SyntaxNode`.
    src_syntax_node =
        JS.parseall(JS.SyntaxNode, read(file_name, String); filename=file_name)
    # Fix potential disagreements between `Expr` and `SyntaxNode` parsing.
    _fix_disagreements!(src_node, src_syntax_node; rule_name=rule_name)
    # Replace the `Expr`-parsed `SyntaxNode`'s data with the `SyntaxNode`-parsed data.
    src_node.data = _update_data_without_head!(src_node, src_syntax_node; rule_name=rule_name)

    return src_node
end

function _update_data_without_head!(node_dst::JS.SyntaxNode,
                                    node_src::JS.SyntaxNode;
                                    rule_name::String="")
    new_raw_children = JS.GreenNode{JS.SyntaxHead}[]
    # TODO: More accurate error handling.
    if isnothing(node_dst.children)
        isnothing(node_src.children) ||
            throw(RuleMatchError("`Expr`-parsed `SyntaxNode` is different than `SyntaxNode`-parsed",
                                 rule_name,
                                 JS.filename(node_src),
                                 JS.source_location(node_src)))
    else
        # @info "here" node_dst node_src
        length(children(node_dst)) == length(children(node_src)) ||
            throw(RuleMatchError("`Expr`-parsed `SyntaxNode` is different than `SyntaxNode`-parsed",
                                 rule_name,
                                 JS.filename(node_src),
                                 JS.source_location(node_src)))
        for (c_dst, c_src) in zip(children(node_dst), children(node_src))
            c_dst.data = _update_data_without_head!(c_dst, c_src)
            push!(new_raw_children, c_dst.data.raw)
        end
    end
    if isempty(new_raw_children)
        new_raw_children = nothing
    end
    new_raw = JS.GreenNode{JS.SyntaxHead}(
        node_dst.data.raw.head,
        node_src.data.raw.span,
        new_raw_children
    )
    new_data = JS.SyntaxData(
        node_src.data.source,
        new_raw,
        node_src.data.position,
        node_src.data.val
    )

    node_dst.data = new_data

    return new_data
end

function _fix_disagreements!(node_expr::JS.SyntaxNode,
                             node_syntax_node::JS.SyntaxNode;
                             rule_name::String="")
    error_msg = """
        `Expr`-parsed `SyntaxNode` is different than `SyntaxNode`-parsed.

        `Expr`-parsed:
        $node_expr

        `SyntaxNode`-parsed:
        $node_syntax_node
        """

    kind_syntax_node = kind(node_syntax_node)
    kind_expr = kind(node_expr)
    # Disagreements between `Expr` and `SyntaxNode`.
    #
    # Docs.
    # Replace `Expr`-parsed docs node with `SyntaxNode`-parsed one.
    if kind_syntax_node === K"doc" && kind_expr !== K"doc"
        node_expr.children = [node_syntax_node.children[1], node_expr.children[3]]
        node_expr.data = update_data_head(node_expr.data, head(node_syntax_node))
    end
    # Short form function definition or anonymous function.
    # Remove the extra block added in the `Expr`-parsed node.
    if is_short_function_def_without_block(node_syntax_node) ||
        is_anon_function_without_block(node_syntax_node) &&
        is_anon_function_with_block(node_expr)
        kind_expr === kind_syntax_node ||
            throw(RuleMatchError(error_msg,
                                 rule_name,
                                 JS.filename(node_syntax_node),
                                 JS.source_location(node_syntax_node)))
        if JS.has_flags(node_syntax_node, JS.SHORT_FORM_FUNCTION_FLAG)
            JS.has_flags(node_expr, JS.SHORT_FORM_FUNCTION_FLAG) ||
                throw(RuleMatchError(error_msg,
                                     rule_name,
                                     JS.filename(node_syntax_node),
                                     JS.source_location(node_syntax_node)))
        end
        node_expr.children[2] = node_expr.children[2].children[1]
    end
    # Infix.
    # Swap `SyntaxNode`-parsed children in case of infix/non-infix parse.
    #
    # Explanation: `Expr`-parsing changes infixable notation to infix, regardless
    # of the original source code. `SyntaxNode`-parsing preserved source code order.
    # `Expr`:       :( in(a, b) ) -> :( a in b )
    # `SyntaxNode`:   "in(a, b)"  -> [call]
    #                                  in   :: Identifier
    #                                  a    :: Identifier
    #                                  b    :: Identifier
    #
    # HOWEVER!
    # `Expr`:        :( a + (b...) ) -> :( +(a, b...) )
    if xor([JS.has_flags(f, JS.INFIX_FLAG) for f in
                map(n -> n.data.raw.head.flags, [node_expr, node_syntax_node])]...)
        children_syntax_node = children(node_syntax_node)
        # Swap the first two children.
        tmp = children_syntax_node[1]
        children_syntax_node[1] = children_syntax_node[2]
        children_syntax_node[2] = tmp
    end
    # Multi-line string.
    # Keep information on line break.
    if kind_syntax_node === K"string" && (kind_expr in JS.KSet"string String")
        node_expr.children = node_syntax_node.children
    end
    # Ternary operator.
    # Replace `if` with `?`.
    if kind_syntax_node === K"?" && kind_expr !== K"?"
          kind_expr === K"if" && length(children(node_expr)) == 3 ||
            throw(RuleMatchError(error_msg,
                                 rule_name,
                                 JS.filename(node_syntax_node),
                                 JS.source_location(node_syntax_node)))
        node_expr.data = update_data_head(node_expr.data, head(node_syntax_node))
        # Remove `block`s.
        node_expr.children[2] = node_expr.children[2].children[1]
        node_expr.children[3] = node_expr.children[3].children[1]
    end
    # `braces` for type parameters.
    # Add the `braces` node.
    if kind_syntax_node === K"braces" && kind_expr !== K"braces"
        new_node_expr = JS.SyntaxNode(node_expr.parent, [node_expr], node_syntax_node.data)
        node_expr = _replace_node!(node_expr, new_node_expr)
    end
    # `$(Expr(:head, :body))` wraps.
    # Check if `head` is correct. Replace the node's head and body.
    #
    # Example: `:( :($x) )`
    #
    # SyntaxNode:
    # [toplevel]
    #   [$]
    #     [call]
    #       Expr             :: Identifier
    #       [quote-:]
    #         quote          :: Identifier     --> The head is `quote` -- it is correct.
    #       [quote-:]
    #         [$]                              --> The body starts here. Recursive call:
    #           [call]
    #             Expr       :: Identifier
    #             [quote-:]
    #               $        :: Identifier         --> The head is `$` -- it is correct.
    #             [quote-:]
    #               pid      :: Identifier         --> The body starts here.
    #
    # Result:
    # SyntaxNode:
    # [toplevel]
    #   [quote-:]
    #     [$]
    #       pid               :: Identifier
    if is_dollar_expr_call(node_expr)
        # Check the head.
        node_expr_head = node_expr.children[1].children[2].children[1].data.val
        kind_from_head = JS.Kind(string(node_expr_head))
        kind_syntax_node == kind_from_head ||
            throw(RuleMatchError(error_msg,
                                 rule_name,
                                 JS.filename(node_syntax_node),
                                 JS.source_location(node_syntax_node)))
        # Replace the head.
        node_expr.data = update_data_head(node_expr.data, head(node_syntax_node))
        # Replace the body.
        node_expr.children[1] =
            _replace_node!(node_expr.children[1],
                           node_expr.children[1].children[3].children[1])
        # Recurse (there might be other `$(Expr(...))` calls left).
        _fix_disagreements!(node_expr.children[1], node_syntax_node.children[1])
    end
    # `var"..."`.
    # Replace the `Symbol` call in the `Expr`-parsed node with the `var` node in the
    # `SyntaxNode`-parsed node.
    if is_var_symbol(node_expr)
        node_expr = _replace_node!(node_expr, node_syntax_node)
    end
    # `juxtapose`.
    # Replace the `juxtapose` node with `call-i`.
    #
    # I don't know why this happens:
    # :( const a = 2 * b ) -> :( const a = 2b )
    if kind_expr === K"juxtapose" && kind_syntax_node === K"call"
        node_expr.data = update_data_head(node_expr.data, head(node_syntax_node))
        node_expr.children = [node_syntax_node.children[1], node_expr.children...]
    end

    if isnothing(children(node_expr))
        isnothing(children(node_syntax_node)) ||
            throw(RuleMatchError(error_msg,
                                 rule_name,
                                 JS.filename(node_syntax_node),
                                 JS.source_location(node_syntax_node)))
        return
    end
    length(children(node_expr)) == length(children(node_syntax_node)) ||
        throw(RuleMatchError(error_msg,
                             rule_name,
                             JS.filename(node_syntax_node),
                             JS.source_location(node_syntax_node)))
   # Recurse.
    for (c_expr, c_syntax_node) in zip(children(node_expr), children(node_syntax_node))
        _fix_disagreements!(c_expr, c_syntax_node)
    end
end

function _replace_node!(old::JS.SyntaxNode, new::JS.SyntaxNode)
    idx = findfirst(c -> c == old, children(old.parent))
    old.parent.children[idx] = new
    return new
end

is_short_function_def_without_block(node::JS.SyntaxNode) =
    kind(node) === K"function" &&
    JS.has_flags(node, JS.SHORT_FORM_FUNCTION_FLAG) &&
    kind(node.children[2]) !== K"block"
is_anon_function_with_block(node::JS.SyntaxNode) =
    kind(node) === K"->" &&
    kind(node.children[2]) === K"block"
is_anon_function_without_block(node::JS.SyntaxNode) =
    kind(node) === K"->" &&
    kind(node.children[2]) !== K"block"
# SyntaxNode:
# [$]
#   [call]
#     Expr             :: Identifier
#     [quote-:]
#       ...            :: Identifier
#     [quote-:]
#       ...
is_dollar_expr_call(node::JS.SyntaxNode) =
    kind(node) === K"$" &&
    length(children(node)) == 1 &&
    kind(children(node)[1]) === K"call" &&
    length(children(children(node)[1])) == 3 &&
    node.children[1].children[1].data.val == :Expr
is_var_symbol(node::JS.SyntaxNode) =
    kind(node) === K"call" &&
    node.children[1].data.val === :Symbol &&
    kind(node.children[2]) === K"string"

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
