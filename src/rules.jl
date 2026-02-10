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
function rule_match(rule::Rule,
                    src::JS.SyntaxNode,
                    bindings::BindingSet=BindingSet();
                    greedy=true,
                    only_matches=true)
    match_results = syntax_match_all(rule.pattern, src, bindings; greedy, only_matches)
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
        src_node = _normalise!(src_node)

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
        src_node = _normalise!(src_node)
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

"""
    _normalise!(node::JS.SyntaxNode)

Normalise a `SyntaxNode`-parsed tree to an `Expr`-parsed equivalent.

# Examples

```
julia> short_form_fundef = parsestmt(SyntaxNode, "f(x) = 2")
SyntaxNode:
[function-=]
  [call]
    f                                    :: Identifier
    x                                    :: Identifier
  2                                      :: Integer


julia> Argus._normalise!(short_form_fundef)
SyntaxNode:
[function-=]
  [call]
    f                                    :: Identifier
    x                                    :: Identifier
  [block]
    2                                    :: Integer

julia> not_infix_op = parsestmt(SyntaxNode, "in(x, y)")
SyntaxNode:
[call]
  in                                     :: Identifier
  x                                      :: Identifier
  y                                      :: Identifier


julia> Argus._normalise!(not_infix_op)
SyntaxNode:
[call-i]
  x                                      :: Identifier
  in                                     :: Identifier
  y                                      :: Identifier
```
"""
function _normalise!(node::JS.SyntaxNode)
    k = kind(node)
    if k == K"var"
        new_node = node.children[1]
        remove_flag!(new_node, JS.RAW_STRING_FLAG)
        new_node.data =
            update_position_and_span(new_node.data, node.data.position, node.data.raw.span)
        node = _replace_node!(node, new_node)
    elseif k == K"?"
        node.data = update_data_head(node.data, JS.SyntaxHead(K"if", 0))
        # Wrap the first branch in a `block` node.
        new_branch1_node = _wrap_node(node.children[2], "begin end")
        node.children[2].parent = new_branch1_node
        new_branch1_node.children = [node.children[2]]
        node.children[2] = new_branch1_node
        # Wrap the second branch in a `block` node.
        new_branch2_node = _wrap_node(node.children[3], "begin end")
        node.children[3].parent = new_branch2_node
        new_branch2_node.children = [node.children[3]]
        node.children[3] = new_branch2_node
    elseif k == K"macrocall"
        if length(children(node)) >= 2
            if kind(node.children[1]) == K"CmdMacroName"
                node.children[1].data =
                    update_data_head(node.children[1].data, JS.SyntaxHead(K"MacroName", 0))
                node.children[2].data =
                    update_data_head(node.children[2].data, JS.SyntaxHead(K"string", 0))
                node.children[2].children[1].data =
                    update_data_head(node.children[2].children[1].data,
                                     JS.SyntaxHead(K"String", 0))
                # Check for flags.
                if length(node.children) == 3
                    string_node = _wrap_node(node.children[3], "\"\"")
                    node.children[3].parent = string_node
                    string_node.children = [node.children[3]]
                    node.children[3] = string_node
                end
            end
            # Check for `parameters` children.
            node = reorder_parameters_macrocall!(node)
            # TODO: `do` lambda errors.
        end
    elseif k == K"doc"
        new_node = _wrap_node(node, "Core.@doc")
        pushfirst!(node.children, new_node.children[1])
        new_node.children = node.children
        node = _replace_node!(node, new_node)
    elseif (k == K"dotcall" || k == K"call") && !isempty(children(node))
        if k == K"call"
            if is_operator(node.children[1]) && !JS.has_flags(node, JS.INFIX_FLAG)
                if length(node.children) == 2
                    add_flag!(node, JS.PREFIX_OP_FLAG)
                else
                    # +(x, y)
                    #
                    # Through `Expr`:
                    # [call-i]
                    #   x                                      :: Identifier
                    #   +                                      :: Identifier
                    #   y                                      :: Identifier
                    #
                    # Through `SyntaxNode`:
                    # [call]
                    #   +                                      :: Identifier
                    #   x                                      :: Identifier
                    #   y                                      :: Identifier
                    add_flag!(node, JS.INFIX_FLAG)
                    # Swap children.
                    node.children[1], node.children[2] = node.children[2], node.children[1]
                end
            elseif length(children(node)) > 2 &&
                is_operator(node.children[2]) &&
                JS.has_flags(node, JS.INFIX_FLAG) &&
                kind(node.children[3]) == K"..."
                # x + (y...)
                #
                # Through `Expr`:
                # [call]
                #   +                                      :: Identifier
                #   x                                      :: Identifier
                #   [...]
                #     y                                    :: Identifier
                #
                # Through `SyntaxNode`:
                # [call-i]
                #   x                                      :: Identifier
                #   +                                      :: Identifier
                #   [...]
                #     y                                    :: Identifier
                remove_flag!(node, JS.INFIX_FLAG)
                # Swap children.
                node.children[1], node.children[2] = node.children[2], node.children[1]
            end
        end
        if k == K"dotcall" && JS.has_flags(node, JS.PREFIX_OP_FLAG)
            # E.g. `.+(x)`:
            #
            # Through `Expr`:
            # [call]
            #   [.]
            #     +                                    :: Identifier
            #   x                                      :: Identifier
            #
            # Through `SyntaxNode`:
            # [dotcall-pre]
            #   +                                      :: Identifier
            #   x                                      :: Identifier
            node.data = update_data_head(node.data, JS.SyntaxHead(K"call", 0))
            new_op_node = _wrap_node(node.children[1], "begin end")
            new_op_node.data = update_data_head(new_op_node.data, JS.SyntaxHead(K".", 0))
            node.children[1].parent = new_op_node
            new_op_node.children = [node.children[1]]
            node.children[1] = new_op_node
        end
        JS.has_flags(node, JS.TRAILING_COMMA_FLAG) &&
            remove_flag!(node, JS.TRAILING_COMMA_FLAG)
        # Check for `parameters` children.
        node = reorder_parameters!(node)
    elseif k == K"tuple" && !isempty(children(node))
        num_children = length(children(node))
        if num_children == 1
            # x, = 1
            #
            # Through `Expr`:
            # [=]
            #   [tuple-p-,]
            #     x                                    :: Identifier
            #   1                                      :: Integer
            #
            # Through `SyntaxNode`:
            # [=]
            #   [tuple]
            #     x                                    :: Identifier
            #   1                                      :: Integer
            !JS.has_flags(node, JS.PARENS_FLAG) &&
                add_flag!(node, JS.PARENS_FLAG)
            !JS.has_flags(node, JS.TRAILING_COMMA_FLAG) &&
                add_flag!(node, JS.TRAILING_COMMA_FLAG)
        elseif num_children > 1 && !JS.has_flags(node, JS.PARENS_FLAG)
            # x,y = 1,2
            #
            # Through `Expr`:
            # [=]
            #   [tuple-p]
            #     x                                    :: Identifier
            #     y                                    :: Identifier
            #   [tuple-p]
            #     1                                    :: Integer
            #     2                                    :: Integer
            #
            # Through `SyntaxNode`:
            # [=]
            #   [tuple]
            #     x                                    :: Identifier
            #     y                                    :: Identifier
            #   [tuple]
            #     1                                    :: Integer
            #     2                                    :: Integer
            add_flag!(node, JS.PARENS_FLAG)
        end
        node = reorder_parameters!(node)
    elseif k in JS.KSet"ref curly"
        node = reorder_parameters!(node)
    elseif k == K"vect"
        JS.has_flags(node, JS.TRAILING_COMMA_FLAG) &&
            remove_flag!(node, JS.TRAILING_COMMA_FLAG)
        node = reorder_parameters_vect!(node)
    elseif k == K"braces"
        if length(children(node)) == 1
            if kind(node.parent) != K"macrocall"  # TODO: Is this correct?
                node.children[1].data = update_position_and_span(node.children[1].data,
                                                                 node.data.position,
                                                                 node.data.raw.span)
                node = _replace_node!(node, node.children[1])
            end
        else
            JS.has_flags(node, JS.TRAILING_COMMA_FLAG) &&
                remove_flag!(node, JS.TRAILING_COMMA_FLAG)
            node = reorder_parameters_vect!(node)
        end
    elseif k == K"->"
        args = node.children[1]
        args_num = length(children(args))
        if kind(args) == K"tuple" && JS.has_flags(args, JS.PARENS_FLAG)
            if args_num == 1 &&
                !JS.has_flags(args, JS.TRAILING_COMMA_FLAG) &&
                kind(args.children[1]) != K"parameters"
                # (x) -> y
                #
                # Through `Expr`:
                # [->]
                #   [tuple]
                #     x                                    :: Identifier
                #   [block]
                #     y                                    :: Identifier
                #
                # Through `SyntaxNode`:
                # [->]
                #   [tuple-p]
                #     x                                    :: Identifier
                #   [block]
                #     y                                    :: Identifier
                remove_flag!(node.children[1], JS.PARENS_FLAG)
            elseif args_num == 2 && kind(args.children[2]) == K"parameters"
                # (x; y=1) -> z
                #
                # Through `Expr`:
                # [->]
                #   [tuple]
                #     [block]
                #       x                                  :: Identifier
                #       [=]
                #         y                                :: Identifier
                #         1                                :: Integer
                #   [block]
                #     z                                    :: Identifier
                #
                # Through `SyntaxNode`:
                # [->]
                #   [tuple-p]
                #     x                                    :: Identifier
                #     [parameters]
                #       [=]
                #         y                                :: Identifier
                #         1                                :: Integer
                #   z                                      :: Identifier
                remove_flag!(node.children[1], JS.PARENS_FLAG)
                # Add `block` node and remove `parameters` node.
                block_args = _wrap_node(node.children[1].children[1], "begin end")
                node.children[1].children[1].parent = block_args
                node.children[1].children[2].children[1].parent = block_args
                block_args.children = [node.children[1].children[1],
                                       node.children[1].children[2].children[1]]
                node.children[1].children = [block_args]
            end
            node.children[1] = reorder_parameters!(node.children[1])
        end
        if kind(node.children[2]) != K"block"
            new_body = _wrap_node(node.children[2], "begin end")
            node.children[2].parent = new_body
            new_body.children = [node.children[2]]
            node.children[2] = new_body
        end
        if kind(args) == K"where"
            # (x where T) -> y
            #
            # Through `Expr`:
            # [where]
            #   x                                      :: Identifier
            #   [->]
            #     [tuple]
            #       T                                  :: Identifier
            #     [block]
            #       y                                  :: Identifier
            #
            # Through `SyntaxNode`:
            # [->]
            #   [where]
            #     x                                    :: Identifier
            #     T                                    :: Identifier
            #   y                                      :: Identifier
            idx = !isnothing(node.parent) ?
                findfirst(c -> c == node, children(node.parent)) :
                nothing
            # Create the `tuple` node (the args of `->`).
            lambda_args = _wrap_node(node.children[1].children[2], "()")
            remove_flag!(lambda_args, JS.PARENS_FLAG)
            lambda_args.parent = node
            lambda_args.children = [node.children[1].children[2]]
            # The second child of `->` is already a `block` node.
            #
            # Make the `where` node the principal node.
            node.children[1].parent = node.parent
            node.children[1].children = [node.children[1].children[1], node]
            node.parent = node.children[1]
            node.children = [lambda_args, node.children[2]]
            node = node.parent
            # Replace the node in the parent's children list.
            if !isnothing(idx)
                node.parent.children[idx] = node
            end
        end
        # Don't recurse on children.
        return node
    elseif k == K"function"
        args = node.children[1]
        if JS.has_flags(node, JS.SHORT_FORM_FUNCTION_FLAG) &&
            kind(node.children[2]) != K"block"
            # Wrap body in `block` node.
            body_node = node.children[2]
            new_body = _wrap_node(body_node, "begin end")
            body_node.parent = new_body
            new_body.children = [body_node]
            node.children[2] = new_body
        elseif kind(args) == K"tuple"
            num_args = length(children(node.children[1]))
            if num_args > 1 && JS.has_flags(node.children[1], JS.TRAILING_COMMA_FLAG)
                remove_flag!(node.children[1], JS.TRAILING_COMMA_FLAG)
            end
        elseif kind(args) == K"call" && JS.has_flags(args, JS.INFIX_FLAG)
            # Swap args children.
            node.children[1].children[1], node.children[1].children[2] =
                node.children[1].children[2], node.children[1].children[1]
            remove_flag!(node.children[1], JS.INFIX_FLAG)
            # Don't recurse on args.
            return _normalise!(node.children[2])
        end
    # elseif k == K"quote"  # TODO: a.:b
    elseif k in JS.KSet"global local" && length(children(node)) == 1
        k_child = kind(node.children[1])
        if k_child == K"const"
            # Swap `local`/`global` head with `const`.
            flags_node = JS.flags(node)
            flags_child = JS.flags(node.children[1])
            node.children[1].data =
                update_data_head(node.children[1].data, JS.SyntaxHead(k, flags_node))
            node.data = update_data_head(node.data, JS.SyntaxHead(K"const", flags_child))
        elseif k_child == K"tuple"
            # Remove the `tuple` node.
            node.children = node.children[1].children
        end
    elseif k == K"juxtapose" && !all(c -> JS.is_number(c) || JS.is_identifier(c),
                                     children(node))
        # Change head to `call-i`.
        node.data = update_data_head(node.data, JS.SyntaxHead(K"call", JS.INFIX_FLAG))
        # Add `*` node.
        star_node = JS.parsestmt(JS.SyntaxNode, "*")
        star_node.parent = node
        insert!(node.children, 2, star_node)
        # TODO: juxtapose: :+'y' ???
    elseif k in JS.KSet"string cmdstring" && JS.has_flags(node, JS.TRIPLE_STRING_FLAG)
        # """
        # a
        # $x
        # b
        # c
        # """
        #
        # Through `Expr`:
        # SyntaxNode:
        #  [string]
        #    "a\n"                                  :: String
        #    x                                      :: Identifier
        #    "\nb\nc"                               :: String
        #
        # Through `SyntaxNode`:
        # SyntaxNode:
        #  [string-s]
        #    "a\n"                                  :: String
        #    x                                      :: Identifier
        #    "\n"                                   :: String
        #    "b\n"                                  :: String
        #    "c"                                    :: String
        remove_flag!(node, JS.TRIPLE_STRING_FLAG)
        if length(children(node)) > 1
            new_children = [node.children[1]]
            for c in @views(children(node)[2:end])
                if kind(c) == K"String"
                    last_c = new_children[end]
                    if kind(last_c) == K"String"
                        new_val = last_c.data.val * c.data.val
                        new_children[end].data = update_data_val(last_c.data, new_val)
                    else
                        push!(new_children, c)
                    end
                else
                    push!(new_children, c)
                end
            end
            node.children = new_children
        end
    elseif k == K"MacroName" && node.data.val == Symbol("@.")
        node.data = update_data_val(node.data, Symbol("@__dot__"))
    end

    # Recurse on children.
    is_leaf(node) && return node
    [_normalise!(c) for c in children(node)]
    return node
end

is_operator(node::JS.SyntaxNode) =
    haskey(JS._kind_str_to_int, string(node.data.val)) &&
    JS.is_operator(JS.Kind(string(node.data.val)))

function remove_flag!(node::JS.SyntaxNode, flag::JS.RawFlags)
    new_flags = xor(JS.flags(node), flag)
    node.data = update_data_head(node.data, JS.SyntaxHead(kind(node), new_flags))
end
function add_flag!(node::JS.SyntaxNode, flag::JS.RawFlags)
    new_flags = JS.flags(node) | flag
    node.data = update_data_head(node.data, JS.SyntaxHead(kind(node), new_flags))
end
replace_flags!(node::JS.SyntaxNode, new_flags::JS.RawFlags) =
    node.data = update_data_head(node.data, JS.SyntaxHead(kind(node), new_flags))

function update_position_and_span(old_data::JS.SyntaxData, new_pos, new_span)
    old_raw = old_data.raw
    new_raw = JS.GreenNode(
        old_raw.head,
        new_span,
        old_raw.children
    )
    new_data = JS.SyntaxData(
        old_data.source,
        new_raw,
        new_pos,
        old_data.val
    )

    return new_data
end

function _wrap_node(node::JS.SyntaxNode, wrap_str::String)
    # Parse wrapper node.
    new_node = JS.parsestmt(JS.SyntaxNode, wrap_str)
    # Link parent.
    new_node.parent = node.parent
    # Update data.
    old_raw = node.data.raw
    new_raw = JS.GreenNode(
        new_node.raw.head,
        0,
        old_raw.children  # TODO: I don't think this is right.
    )
    new_data = JS.SyntaxData(
        node.data.source,
        new_raw,
        node.data.position,
        new_node.val
    )
    new_node.data = new_data

    return new_node
end

function reorder_parameters!(node::JS.SyntaxNode)
    params_idx = findfirst(c -> kind(c) == K"parameters", children(node))
    if !isnothing(params_idx)
        new_children = node.children[1:(params_idx - 1)]
        push!(new_children, squash_parameters!(@views node.children[params_idx:end]))
        node.children = new_children
    end

    return node
end
function reorder_parameters_macrocall!(node::JS.SyntaxNode)
    params_idx = findfirst(c -> kind(c) == K"parameters", children(node))
    if !isnothing(params_idx)
        params_num = length(children(node)) - params_idx + 1
        new_children =
            [node.children[1], squash_parameters!(@views node.children[params_idx:end])]
        for idx in 2:(1 + params_num)
            push!(new_children, node.children[idx])
        end
        node.children = new_children
    end

    return node
end
function reorder_parameters_vect!(node::JS.SyntaxNode)
    params_idx = findfirst(c -> kind(c) == K"parameters", children(node))
    if !isnothing(params_idx)
        new_children =
            [squash_parameters!(@views node.children[params_idx:end])]
        for idx in 1:(params_idx - 1)
            push!(new_children, node.children[idx])
        end
        node.children = new_children
    end

    return node
end

function squash_parameters!(nodes::T) where T <: AbstractVector{JS.SyntaxNode}
    isempty(nodes) && error("Invalid input?")
    length(nodes) == 1 && return nodes[1]
    # Squash the `parameters` nodes into one node.
    node = nodes[1]
    new_children = node.children
    map!(n -> n.parent = node, nodes[2:end])
    pushfirst!(new_children, squash_parameters!(@views nodes[2:end]))
    node.children = new_children

    return node
end

is_short_function_definition(node::JS.SyntaxNode) =
    kind(node) === K"function" &&
    JS.has_flags(node, JS.SHORT_FORM_FUNCTION_FLAG)
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
