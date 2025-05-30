@testset "Syntax classes" begin
    fundef = @syntax_class "fundef" quote
        @pattern quote
            function (_f:::funcall)
                _body
            end
        end
    end
    @test length(fundef.pattern_alternatives) == 1
    pattern = fundef.pattern_alternatives[1]
    @test kind(pattern.ast) === K"function"
    @test !is_leaf(pattern.ast)
    @test length(pattern.ast.children) == 2
    @test kind(pattern.ast.children[1]) === K"~var"
    @test kind(pattern.ast.children[2]) === K"block"
end
