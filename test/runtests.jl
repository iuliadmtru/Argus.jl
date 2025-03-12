using Test
using Argus

@testset "Pattern" begin
    src = """
function f(a, b)
    y = a + b
    return y
end
"""
end
