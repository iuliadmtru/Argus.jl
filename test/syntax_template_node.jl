using JuliaSyntax: children

@testset "SyntaxTemplateNode" begin

    @testset "Without placeholders" begin
        template_str = """
        begin
            a = "a"
            b = 1
            f(x) = x + 1
            println(a, f(b))
        end
        """
        syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, template_str)
        template = SyntaxTemplateNode(syntax_node)
        @test template.data == syntax_node.data
        @test length(children(template)) == length(children(syntax_node))
        @test isnothing(template.parent)
        @test head(template) == head(syntax_node)
        b = children(children(syntax_node)[2])[1]
        @test children(children(template)[2])[1].data.val === :b
        @test isempty(placeholders(template))
    end

    @testset "With placeholders" begin
        template_str = "f(Metavariable(:X)) = Metavariable(:X) + 1"
        syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, template_str)
        template = SyntaxTemplateNode(syntax_node)
        @test contains_placeholders(template)
        @test kind(template) === K"="

        metavar1 = children(children(template)[1])[2]
        @test is_placeholder(metavar1)
        m1 = placeholder(metavar1)
        @test !isnothing(m1)
        @test m1.name === :X
        @test isnothing(m1.binding)

        metavar2 = children(children(template)[2])[1]
        @test m1.name == placeholder(metavar2).name
    end

end
