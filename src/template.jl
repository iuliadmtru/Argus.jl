# Template
# ========

"""
    Template

Syntax template, to be filled using a [`BindingSet`](@ref) obtained after pattern matching.
Alias for `SyntaxPatternNode`.
"""
const Template = SyntaxPatternNode

"""
    @template(expr)

Create a [`Template`](@ref) from the given expression.

# Examples

```
julia> t = @template {x} + 1
SyntaxPatternNode:
[call-i]
  [~var]
    [quote-:]
      x                                  :: Identifier
    [quote-:]
      expr                               :: Identifier
  +                                      :: Identifier
  1                                      :: Integer

julia> t = @template {f}({args}...)
SyntaxPatternNode:
[call]
  [~var]
    [quote-:]
      f                                  :: Identifier
    [quote-:]
      expr                               :: Identifier
  [~rep]
    [~var]
      [quote-:]
        args                             :: Identifier
      [quote-:]
        expr                             :: Identifier
```
"""
macro template(expr)
    # Error messages.
    err_msg_general =
        """
        invalid `@template` syntax
        Templates should be created in one of the following ways:
         -- `@template <expr>`
         -- ```
        |   @template begin
        |       <expr>+
        |   end
         -- ```"""

    @isexpr(expr, :quote) &&
        throw(SyntaxError(err_msg_general, __source__.file, __source__.line))

    t = Template(expr)
    return :( $t )
end
