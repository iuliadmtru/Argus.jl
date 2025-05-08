struct SyntaxClass
    description::String
    alternatives::Vector{Pattern}
end

macro syntax_class(description, pattern)
    return :( SyntaxClass($description, $pattern) )
end
