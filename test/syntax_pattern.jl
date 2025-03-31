@testset "AbstractSyntaxPattern" begin

    @testset "Pattern" begin
        src = """
        function f(a, b)
            y = a + b
            return y
        end
        """
        pattern = Pattern(src)
        @test !contains_placeholders(pattern.template)
        syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, src)
        matches = pattern_match!(pattern, syntax_node)
        @test length(matches) == 1
        m = matches[1]
        @test head(m.ast) == head(syntax_node)
    end

end
