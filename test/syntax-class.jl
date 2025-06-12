@testset "Syntax classes" begin

    @testset "Built in syntax classes" begin
        let
            vec = Argus.SYNTAX_CLASS_REGISTRY[:vec]
            match = syntax_match(vec, parsestmt(SyntaxNode, "[1, [2]]"))
            @test isa(match, BindingSet)
            @test isempty(match)
            no_match = syntax_match(vec, parsestmt(SyntaxNode, "a"))
            @test no_match == MatchFail("not a vector")
        end
        let
            fundef = Argus.SYNTAX_CLASS_REGISTRY[:fundef]

            match_first = syntax_match(fundef, parsestmt(SyntaxNode, "f(x) = 2"))
            @test length(match_first) == 2
            @test match_first[:call].fun_name.name == "f"
            @test length(match_first[:call].args.bindings) == 1
            @test match_first[:call].args.src[1].val == :x
            @test match_first[:body].value == 2

            match_second = syntax_match(fundef, parsestmt(SyntaxNode, """
                                                                  function f()
                                                                      ex1
                                                                      ex2
                                                                  end
                                                                  """))
            @test length(match_second) == 2
            @test match_second[:call].fun_name.name == "f"
            @test isempty(match_second[:call].args.bindings)
            @test length(match_second[:body].src) == 2
            @test map(s -> source_location(s), match_second[:body].src) == [(2, 5), (3, 5)]

            no_match = syntax_match(fundef, parsestmt(SyntaxNode, "x"))
            @test no_match == MatchFail("not a function definition")
        end
    end

    @testset "General" begin
        let
            unregistered = @pattern {x:::y}
            err_msg = "SyntaxClassRegistryKeyError: unregistered syntax class :y"
            @test_throws err_msg syntax_match(unregistered, parsestmt(SyntaxNode, "dummy"))
        end
        let
            fundef = @syntax_class "fundef" begin
                @pattern begin
                    function ({f:::funcall})
                        {body}
                    end
                end
            end
            @test length(fundef.pattern_alternatives) == 1
            pattern = fundef.pattern_alternatives[1]
            @test kind(pattern.src) === K"function"
            @test !is_leaf(pattern.src)
            @test length(pattern.src.children) == 2
            @test kind(pattern.src.children[1]) === K"~var"
            @test kind(pattern.src.children[2]) === K"block"
        end
        let
            fundef = @syntax_class "fundef" begin
                @pattern {f:::funcall} = {_}
                @pattern function ({g:::funcall}) {_} end
            end
            match_first = syntax_match(fundef, parsestmt(SyntaxNode, "f(x) = begin 2 end"))
            @test isa(match_first, BindingSet)
            @test collect(keys(match_first)) == [:f]
            f_args = match_first[:f].args
            @test length(f_args.src) == 1
            match_second = syntax_match(fundef, parsestmt(SyntaxNode, "function f() 2 end"))
            @test isa(match_second, BindingSet)
            @test collect(keys(match_second)) == [:g]
        end
    end

    # Invalid syntax.
    @test_nowarn @macroexpand @syntax_class "abc" begin
        @pattern 2
        Pattern(SyntaxPatternNode(2))
    end
    @test_throws "body should be defined using a `begin" @macroexpand(
        @syntax_class "" @pattern 2
    )
    @test_throws "should be `Pattern`" @macroexpand @syntax_class "" begin
        2
    end
end
