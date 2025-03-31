@testset "SyntaxMatch" begin
    src = "f(x)"
    syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, src)
    m = SyntaxMatch(syntax_node)
    @test kind(m.ast) === K"call"
    @test source_location(m) == (1, 1)
end

@testset "SyntaxMatches" begin
    @test isempty(SyntaxMatches())
    syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "x")
    m = SyntaxMatch(syntax_node)
    ms = SyntaxMatches([m])
    @test length(ms) == 1
    @test kind(ms[1].ast) === K"Identifier"
    push!(ms, m)
    @test length(ms) == 2
end
