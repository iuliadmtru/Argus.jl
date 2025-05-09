using MacroTools: striplines

"""
    SyntaxClass

Syntax classes provide the basis for a syntax matching mechanism. A `SyntaxClass` object
specifies a syntactic "shape" and provides a description for that shape. For example,
a syntax class corresponding to a binary function call could be defined as such:
```
binary_funcall = @syntax_class "binary function call" quote
    ~var(_(_, _))
end
```
"""
struct SyntaxClass
    description::String
    alternatives::Vector{Pattern}
end

# TODO: Support for fail conditions.
macro syntax_class(description, patterns)
    args = striplines(patterns).args
    alternatives =
        (length(args) == 1 && Meta.isexpr(args[1], :block)) ?
        Pattern.(args[1].args)                              :
        Pattern.(args)
    return :( SyntaxClass($description, $alternatives) )
end
