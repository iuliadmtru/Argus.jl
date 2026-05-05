# Rules

Now we're ready to write some rules!

A _rule_ is useful for matching all apparitions of a pattern in a
source.

```julia
julia> assignments = @rule "assignments" begin
           description = "Assignment here."
           pattern = @pattern {a:::assign}
       end
assignments:
Assignment here.

Pattern:
a:::assign                               :: ~var

Template:
<no template>

Hooks:
<no hooks>

julia> src = """
       x = 2
       y = x + 1
       if y == x
           z = true
       end
       f() = 4
       """;

julia> match_result = rule_match(assignments, parseall(SyntaxNode, src))
RuleMatchResult with 3 matches and 0 failures:
Matches:
  @ 1:1
  BindingSet(:a => Binding(:a, (= x 2) @ 1:1, BindingSet(:rhs => Binding(:rhs, 2 @ 1:5, BindingSet()), :lhs => Binding(:lhs, x @ 1:1, BindingSet()))))

  @ 2:1
  BindingSet(:a => Binding(:a, (= y (call-i x + 1)) @ 2:1, BindingSet(:rhs => Binding(:rhs, (call-i x + 1) @ 2:5, BindingSet()), :lhs => Binding(:lhs, y @ 2:1, BindingSet()))))

  @ 4:5
  BindingSet(:a => Binding(:a, (= z true) @ 4:5, BindingSet(:rhs => Binding(:rhs, true @ 4:9, BindingSet()), :lhs => Binding(:lhs, z @ 4:5, BindingSet()))))
```

The `assignments` rule above matches all the assignments in the
source. The `assign` built-in syntax class uses the pattern variables
`lhs` and `rhs`, which appear as sub-bindings for the rule pattern
variable `a`. We can access sub-bindings of a bound pattern variable
inside the pattern. Let's say we want to only match assignments with
literals on the rhs:

```julia
julia> lit_assignments = @rule "assignments" begin
           description = "Assignment here."
           pattern = @pattern begin
               {a:::assign}
               @fail [:a] begin
                   !JuliaSyntax.is_literal(a.rhs.src)
               end "expected literal rhs"
           end
       end;

julia> match_result = rule_match(lit_assignments, parseall(SyntaxNode, src))
RuleMatchResult with 2 matches and 0 failures:
Matches:
  @ 1:1
  BindingSet(:a => Binding(:a, (= x 2) @ 1:1, BindingSet(:rhs => Binding(:rhs, 2 @ 1:5, BindingSet()), :lhs => Binding(:lhs, x @ 1:1, BindingSet()))))

  @ 4:5
  BindingSet(:a => Binding(:a, (= z true) @ 4:5, BindingSet(:rhs => Binding(:rhs, true @ 4:9, BindingSet()), :lhs => Binding(:lhs, z @ 4:5, BindingSet()))))
```

`rule_match` can keep track of all non-trivial failed matches as well
by setting the `only_matches` keyword argument to `false`. This can be
useful for debugging a rule.

```julia
julia> rule_match(lit_assignments, parseall(SyntaxNode, src); only_matches=false).failures
21-element Vector{MatchFail}:
 MatchFail: expected assignment @ :1:1
 MatchFail: expected assignment @ :1:1
 MatchFail: expected assignment @ :1:5
 MatchFail: expected literal rhs @ :2:1
 MatchFail: expected assignment @ :2:1
 MatchFail: expected assignment @ :2:5
 MatchFail: expected assignment @ :2:5
 MatchFail: expected assignment @ :2:7
 MatchFail: expected assignment @ :2:9
 MatchFail: expected assignment @ :3:1
 MatchFail: expected assignment @ :3:3
 MatchFail: expected assignment @ :3:4
 MatchFail: expected assignment @ :3:6
 MatchFail: expected assignment @ :3:9
 MatchFail: expected assignment @ :3:10
 MatchFail: expected assignment @ :4:5
 MatchFail: expected assignment @ :4:9
 MatchFail: expected assignment @ :6:1
 MatchFail: expected assignment @ :6:1
 MatchFail: expected assignment @ :6:1
 MatchFail: expected assignment @ :6:7
```

The result of a rule match consists of two vectors: one with all
non-trivial failures and one with all matches and their associated
template expansions, if applicable. The above rules don't have
templates, so they have `nothing` as template expansions. A rule with
a template is created using the `template` argument in the `@rule`
macro.

Say we want to replace all apparitions of `rand() < 0.5` with the call
`rand(Bool)`.

```julia
julia> rand_bool = @rule "rand-bool" begin
           description = """
           To get a random Boolean, use `rand(Bool)`.
           """

           pattern = @pattern begin
               {randf}() < 0.5
               @when [:randf] match(r"^(Base.)?rand$", randf.name) !== nothing
           end

           template = @template rand(Bool)
       end;
```

We could match this rule against a file:

```julia
julia> f = tempname();

julia> src = """
           # Match.
           rand() < 0.5

           function some_rand_function(x)
               # Match.
               if rand() < 0.5
                   println("Random")
               end
           end

           # Match.
           if some_flag && rand() < 0.5 || other_flag
               println("Random")
           end
           """;

julia> write(f, src);

julia> rule_match(rand_bool, f)
RuleMatchResult with 3 matches and 0 failures:
Matches:
  @ /var/folders/4p/xtm72jnx4654xybjwm1mpd0h0000gn/T/jl_OBsPMZSfGH:2:1
  BindingSet(:randf => Binding(:randf, rand @ 2:1, BindingSet()))
  (call rand Bool)

  @ /var/folders/4p/xtm72jnx4654xybjwm1mpd0h0000gn/T/jl_OBsPMZSfGH:6:7
  BindingSet(:randf => Binding(:randf, rand @ 6:8, BindingSet()))
  (call rand Bool)

  @ /var/folders/4p/xtm72jnx4654xybjwm1mpd0h0000gn/T/jl_OBsPMZSfGH:12:16
  BindingSet(:randf => Binding(:randf, rand @ 12:17, BindingSet()))
  (call rand Bool)
```

Rules can also be bulk-matched using `rules_match`:

```julia
julia> compare_nothing = @rule "compare-nothing" begin
           description = """
           Comparisons of `nothing` should be made with === or !== or with isnothing().
           """

           pattern = @pattern begin
               ~or(
                   nothing == {_},
                   {_} == nothing,
                   nothing != {_},
                   {_} != nothing
               )
           end
       end;

julia> useless_equals = @rule "useless-equals" begin
           description = """
           Comparing the same object in the RHS and LHS is pointless.
           """

           pattern = @pattern begin
               ~or(
                   {x} ==  {x},
                   {x} !=  {x},
                   {x} === {x},
                   {x} !== {x}
               )
           end
       end;

julia> rules_match([compare_nothing, useless_equals], parsestmt(SyntaxNode, "nothing == nothing"))
RuleGroupMatchResult with 2 entries:
  "useless-equals"  => RuleMatchResult(Tuple{BindingSet, Union{Nothing, SyntaxNode}}[(BindingSet(:x=>Binding(:x, nothing @ 1:12, BindingSet())), nothing)], MatchFail[])
  "compare-nothing" => RuleMatchResult(Tuple{BindingSet, Union{Nothing, SyntaxNode}}[(BindingSet(), nothing), (BindingSet(), nothing)], MatchFail[])
```

Sometimes it is useful to group rules by category. We can define a
rule group and store rules inside it:

```julia
julia> style_rules = RuleGroup("style")
RuleGroup("style")

julia> @define_rule_in_group style_rules "useless-equals" begin
           description = """
           Comparing an object with itself always returns `true`.
           """

           pattern = @pattern begin
               ~or(
                   {x} ==  {x},
                   {x} !=  {x},
                   {x} === {x},
                   {x} !== {x}
               )
           end
       end;

julia> @define_rule_in_group style_rules "lowercase-const" begin
           description = """
           Prefer writing `const` variables in all-uppercase.
           """

           pattern = @pattern begin
               const {x:::identifier} = {_}
               @when [:x] any(islowercase, x.name)
           end
       end;
```

To check if the rules are correct, let's try them on a source file:

```julia
julia> f = tempname();

julia> write(f, """
       a == a
       const low = 2
       const OK = true
       """);

julia> rule_group_match(style_rules, f; only_matches=false)
RuleGroupMatchResult with 2 entries:
  "useless-equals"  => RuleMatchResult(Tuple{BindingSet, Union{Nothing, SyntaxNode}}[(BindingSet(:x=>Binding(:x, a @ 1:6, BindingSet())), nothing)], MatchFail[])
  "lowercase-const" => RuleMatchResult(Tuple{BindingSet, Union{Nothing, SyntaxNode}}[(BindingSet(:x=>Binding(:x, low @ 2:7, BindingSet())), nothing)], MatchFail[])
```

## Disabling Rules

Linters provide a set of built-in linting rules. For a given code
base, not all of them may be useful in every case. That is why linters
generally also provide a mechanism for disabling rules. Argus'
consists of a user-defined `RuleDisabler`.

```julia
help?> RuleDisabler
search: RuleDisabler CommentDisabler

  RuleDisabler <: Function

  Supertype for all rule disablers.

  RuleDisablers other than CommentDisablers must define the following methods:

  """
      disabler(src::JuliaSyntax.SyntaxNode)::Bool

  Disable all rules for the given three.
  """
  disabler(src::JuliaSyntax.SyntaxNode)


  """
      disabler(rule::Rule, src::JuliaSyntax.SyntaxNode)::Bool

  Disable the given rule for the given three.
  """
  disabler(rule::Rule, src::JuliaSyntax.SyntaxNode)
```

Argus defines `CommentDisabler` as an abstract subtype of
`RuleDisabler`, and `DefaultDisabler` as the concrete type for the
built-in disabler.

```julia
help?> Argus.default_disabler
  │ Warning
  │
  │  The following bindings may be internal; they may change or be removed in future versions:
  │
  │    •  Argus.default_disabler

  default_disabler([rule::Rule,] line::AbstractString)

  The default rule disabler. Allows disabling rules in source code via comments of the form # lint-disable[: [<rule-name>, ]+]?. The rules are disabled for the annotated node.

  Examples:
  ≡≡≡≡≡≡≡≡≡

  julia> src = """
         f(x) = x

         # lint-disable
         f(x, y)

         # lint-disable: disabled_rule
         function g(x)
             f(x + 1)
         end

         # lint-disable: another_rule
         function g(x)
             f(x + 1)
         end
         """;

  julia> rule = @rule "disabled_rule" begin
             description = ""
             pattern = @pattern f({_}...)
         end;

  julia> rule_match(rule, parseall(SyntaxNode, src))
  RuleMatchResult with 2 matches and 0 failures:
  Matches:
    @ :1:1
    BindingSet()

    @ :13:5
    BindingSet()
```

The default disabler works on entire AST nodes rather than on lines of
code. A disabling annotation disables the specified rules (or all
rules if no rule name is given) for the AST node that follows it. This
is the behaviour of all `CommentDisabler`s.

## Rule Hooks

> [!WARNING]
> Rule hooks will most likely change both behaviour and structure in
> the future.

It may be useful to define pre- and post-match hooks for certain
rules. For example, it might be necessary for some rules to only run
in certain directories or not to run in some files. For these cases,
it is possible to define `RuleHook`s:

```julia
julia> @define_rule_hook :only_in_dirs begin
    args = @pattern [{dirs}...]

    pre_check = @check [:dirs] begin
        dir_names = map(s -> s.children[1].val, dirs.src)
        if !any(contains.(current_file(), dir_names))
            skip_match()
        end
    end

    post_check = nothing
end;

julia> is_nothing = @rule "isnothing" begin
           description = "Don't use `isnothing` in performance-critical code."

           pattern = @pattern isnothing({x})

           template = @template {x} === nothing

           hooks = Dict(
               :only_in_dirs => ["performance/", "critical/"]
           )
       end
isnothing:
Don't use `isnothing` in performance-critical code.

Pattern:
[call]
  isnothing                              :: Identifier
  x:::expr                               :: ~var

Template:
SyntaxPatternNode:
[call-i]
  [~var]
    [quote-:]
      x                                  :: Identifier
    [quote-:]
      expr                               :: Identifier
  ===                                    :: Identifier
  nothing                                :: Identifier

Hooks:
  :only_in_dirs => ["performance/", "critical/"]


julia> rule_match(is_nothing, parsestmt(SyntaxNode, "isnothing(x)"))
RuleMatchResult with 0 matches and 0 failures

julia> rule_match(is_nothing, parsestmt(SyntaxNode, "isnothing(x)"; filename="performance/f.jl"))
RuleMatchResult with 1 matches and 0 failures:
Matches:
  @ performance/f.jl:1:1
  BindingSet(:x => Binding(:x, x @ 1:11, BindingSet()))
  (call-i x === nothing)
```
