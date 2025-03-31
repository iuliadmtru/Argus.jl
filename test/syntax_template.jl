@testset "SyntaxTemplateData" begin
    src = "x"
    syntax_data = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, src).data
    metavar = Metavariable(:X)
    @test SyntaxTemplateData(syntax_data).val === :x
    @test SyntaxTemplateData(metavar).name === :X
end

@testset "Placeholders" begin

    @testset "Metavariable" begin
        metavar = Metavariable(:X)
        @test !has_binding(metavar)

        syntax_data = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "2").data
        set_binding!(metavar, syntax_data)
        @test has_binding(metavar)
        @test metavar.binding.val === 2

        placeholder_unbind!(metavar)
        @test !has_binding(metavar)
        syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "x")
        @test placeholder_fill!(metavar, syntax_node)
        @test has_binding(metavar)
        @test metavar.binding.val === :x
    end

end
