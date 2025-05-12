@testset "Syntax classes" begin
    assign2 = @syntax_class "assignment2" quote
        __lhs:::identifier = __rhs:::expr
    end
    register_syntax_class!(:assign2)
    err = "Syntax class assign2 is referenced but undefined"
    @test_throws err syntax_class_registry_check()
    register_syntax_class!(:assign2, assign2)
    @test_nowarn syntax_class_registry_check()

    fundef = @syntax_class "fundef" quote
        function (_f:::funcall)
            _body
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
