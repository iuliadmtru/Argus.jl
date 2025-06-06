# This doesn't work because the `Expr` representation is different than the
# `SyntaxNode` one.

# Match.
!=(x::Float32, y::Float32) = true

# Match.
function !=(x::Float32, y::Float32)  
    return true
end

# No match.
const != = !(==)
