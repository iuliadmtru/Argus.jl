@testset "Syntax classes" begin

    @testset "Built in syntax classes" begin
        let
            vec = Argus.SYNTAX_CLASS_REGISTRY[:vec]
            match = syntax_match(vec, parsestmt(SyntaxNode, "[1, [2]]"))
            @test isa(match, BindingSet)
            @test isempty(match)
            no_match = syntax_match(vec, parsestmt(SyntaxNode, "a"))
            @test no_match == MatchFail("expected vector")
        end
        let
            funcall = Argus.SYNTAX_CLASS_REGISTRY[:funcall]

            match_simple = syntax_match(funcall, parsestmt(SyntaxNode, "f(x, y)"))
            @test length(match_simple) == 2
            @test match_simple[:fun_name].name == "f"
            @test length(match_simple[:args].bindings) == 2
            @test [s.val for s in match_simple[:args].src] == [:x, :y]

            match_qualified = syntax_match(funcall, parsestmt(SyntaxNode, "M.f()"))
            @test length(match_qualified) == 2
            @test kind(match_qualified[:fun_name].src) == K"."
            @test isempty(match_qualified[:args].bindings)

            match_anonymous = syntax_match(funcall, parsestmt(SyntaxNode, "()()"))
            @test length(match_anonymous) == 2
            @test kind(match_anonymous[:fun_name].src) == K"tuple"

            no_match = syntax_match(funcall, parsestmt(SyntaxNode, "x"))
            @test no_match == MatchFail("expected function call")
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
            @test no_match == MatchFail("expected function definition")
        end
        let
            macrocall = Argus.SYNTAX_CLASS_REGISTRY[:macrocall]

            match1 = syntax_match(macrocall, parsestmt(SyntaxNode, "@m()"))
            @test isa(match1, BindingSet)
            @test match1[:_mcall].name == "@m"
            @test isempty(match1[:_mcall].args)

            match2 = syntax_match(macrocall, parsestmt(SyntaxNode, "@m 2 3"))
            @test isa(match2, BindingSet)
            @test match2[:_mcall].name == "@m"
            @test length(match2[:_mcall].args) == 2

            match3 = syntax_match(macrocall, parsestmt(SyntaxNode, "r\"abc\""))
            @test isa(match3, BindingSet)
            @test match3[:_mcall].name == "@r_str"
            @test length(match3[:_mcall].args) == 1
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
