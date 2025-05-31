@testset "Syntax classes" begin
    fundef = @syntax_class "fundef" begin
        @pattern begin
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

    let
        fundef = @syntax_class "function definition" begin
            @pattern _f:::funcall = _
            @pattern function (_g:::funcall) _ end
        end
        match_first = syntax_match(fundef, parsestmt(SyntaxNode, "f() = begin 2 end"))
        @test isa(match_first, BindingSet)
        @test collect(keys(match_first)) == [:_f]
        match_second = syntax_match(fundef, parsestmt(SyntaxNode, "function f() 2 end"))
        @test isa(match_second, BindingSet)
        @test collect(keys(match_second)) == [:_g]
    end

    # Invalid syntax.
    @test_nowarn @macroexpand @syntax_class "abc" begin
        @pattern 2
        Pattern(SyntaxPatternNode(2), [])
    end
    @test_throws "body should be defined using a `begin" @macroexpand(
        @syntax_class "" @pattern 2
    )
    @test_throws "should be `Pattern`" @macroexpand @syntax_class "" begin
        2
    end
end
