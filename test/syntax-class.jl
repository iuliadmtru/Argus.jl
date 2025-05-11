@testset "Syntax classes" begin
    assign2 = @syntax_class "assignment2" quote
        __lhs:::identifier = __rhs:::expr
    end
    register_syntax_class!(:assign2)
    err = "Syntax class assign2 is referenced but undefined"
    @test_throws err syntax_class_registry_check()
    register_syntax_class!(:assign2, assign2)
    @test_nowarn syntax_class_registry_check()
end
