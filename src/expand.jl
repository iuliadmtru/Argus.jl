# Template expansion
# ==================

"""
    expand(template::Template, bindings::BindingSet)

Expand a template using a given set of bindings.

# Examples
# ========

```
julia> p = @pattern ([{x}, {y}...])...;

julia> bs = syntax_match(p, parseall(SyntaxNode, \"""
                                                 [1, 2, 3]
                                                 [a, b]
                                                 \"""))
BindingSet with 2 entries:
  :y => Binding:
          Name: :y
          Bound sources: [[2 @ 1:5, 3 @ 1:8], [b @ 2:5]]
          Ellipsis depth: 2
          Sub-bindings:
            [
             [
              BindingSet with 0 entries,
              BindingSet with 0 entries
             ],
             [
              BindingSet with 0 entries
             ]
            ]
  :x => Binding:
          Name: :x
          Bound sources: [1 @ 1:2, a @ 2:2]
          Ellipsis depth: 1
          Sub-bindings:
            [
             BindingSet with 0 entries,
             BindingSet with 0 entries
            ]

julia> t = @template ([{y}..., {x}])...
SyntaxPatternNode:
[~rep]
  [vect]
    [~rep]
      [~var]
        [quote-:]
          y                              :: Identifier
        [quote-:]
          expr                           :: Identifier
    [~var]
      [quote-:]
        x                                :: Identifier
      [quote-:]
        expr                             :: Identifier

julia> expand(t, bs)
SyntaxNode:
[toplevel]
  [vect]
    2                                    :: Integer
    3                                    :: Integer
    1                                    :: Integer
  [vect]
    b                                    :: Identifier
    a                                    :: Identifier
```
"""
function expand(template::Template, bindings::BindingSet)::JS.SyntaxNode
    if is_rep(template)
        cs = expand_rep(template, bindings)
        # TODO: Better way to do this!!
        rep_node_str = join([string(Expr(c)) for c in cs], "\n")
        return JS.parseall(JS.SyntaxNode, rep_node_str)
    end
    return expand_rep(template, bindings)[1]  # TODO: Check length is 1.
end

"""
    expand_rep(template::Template,
               bindings::BindingSet,
               discarded_vars::Vector{Symbol}=[])::Vector{JS.SyntaxNode}

Expand a `~rep` template into the corresponding vector of `SyntaxNode`s.
"""
function expand_rep(template::Template,
                    bindings::BindingSet,
                    discarded_vars::Vector{Symbol}=Symbol[])::Vector{JS.SyntaxNode}
    # If the template is a leaf, return it as a vector.
    is_leaf(template) && return [JS.SyntaxNode(nothing, nothing, template.data)]
    # If the template is a `~var`, return the corresponding bound source node.
    if is_var(template)
        vname = template.var_name
        b = try
            bindings[vname]
        catch e
            # TODO: `ExpansionError`?
            if isa(e, BindingSetKeyError) && vname in discarded_vars
                error("""
                      template variable $vname has inconsistent ellipsis depth
                      Template variables should have the same depth as the corresponding pattern variables.
                      """)
            else
                rethrow(e)
            end
        end
        src = b.src
        isa(src, JS.SyntaxNode) ||
            error("""
                  template variable $vname has inconsistent ellipsis depth
                  Template variables should have the same depth as the corresponding pattern variables.
                  """)
        return [src]
    end
    # If the template is a `~rep`, expand it using a reduced binding set, effectively
    # decreasing the ellipsis depth.
    if is_rep(template)
        # Bindings with bound sources of type `SyntaxNode` correspond to the "previous"
        # depth and should be discarded.
        bindings, new_discarded_vars = remove_simple_bindings(bindings)
        # Only keep bindings of variables referenced in the `~rep` node.
        rep_var_names = [v.name for v in template.rep_vars]
        rep_bindings::BindingSet = filter(b -> b.first in rep_var_names, bindings)
        # The number of repetitions is the number of bound sources.
        reps_no = number_of_bound_sources(rep_bindings)
        # Repeat the expansion.
        reps_no == 0 && return []
        rep_node = template.children[1]
        return mapfoldl(idx -> expand_rep(rep_node,
                                          pick_from_binding_set(rep_bindings, idx),
                                          vcat(discarded_vars, new_discarded_vars)),
                        append!,
                        1:reps_no)
    end
    # The template is a regular node (inside a `~rep`).
    expanded = JS.SyntaxNode(nothing, nothing, template.data)
    cs = reduce(vcat, [expand_rep(c, bindings, discarded_vars) for c in children(template)])
    [c.parent = expanded for c in cs]
    expanded.children = cs
    return [expanded]
end

# Utils

"""
    remove_simple_bindings(bs::BindingSet)::Tuple{BindingSet, Vector{Symbol}}

Remove all bindings bound to `SyntaxNode`s from a [`BindingSet`](@ref). Keep track of the
names of the discarded binding names.
"""
function remove_simple_bindings(bs::BindingSet)::Tuple{BindingSet, Vector{Symbol}}
    new_bs = BindingSet()
    discarded_vars = []
    for (bname, b) in bs
        if isa(b.src, JS.SyntaxNode)
            push!(discarded_vars, bname)
        else
            new_bs[bname] = b
        end
    end

    return (new_bs, discarded_vars)
end

"""
    number_of_bound_sources(bs::BindingSet)

Get the number of sources bound at the first depth in a [`BindingSet`](@ref). The
binding set should contain only bindings bound to vectors of `SyntaxNode`s.
"""
function number_of_bound_sources(bs::BindingSet)
    isempty(bs) && return 0
    first_binding = first(bs)
    size = length(first_binding.second.src)
    # Since `bs` contains only repetition variables, all their bound sources should have
    # the same length.
    for (bname, b) in bs
        length(b.src) == size ||
            error("""
                  the lists of bound sources of repetition variables in a `~rep` should have the same length
                  Bindings $(first_binding.first) and $bname have different source sizes.
                  """)
    end

    return size
end

"""
    pick_from_binding_set(bs::BindingSet, idx::Int)

Pick only the `idx`th bound source from each binding in `bs`.
"""
function pick_from_binding_set(bs::BindingSet, idx::Int)
    new_bs = BindingSet()
    for (bname, b) in bs
        new_bs[bname] = Binding(bname,
                                b.src[idx],
                                b.bindings[idx],
                                b.ellipsis_depth - 1)
    end

    return new_bs
end
