@testset "Rules" begin
    p = @pattern begin
        const _a:::identifier = _b:::identifier = _
    end
    chained_const_assignment = @rule "chained-const-assignment" begin
        description = """
        Do not chain assignments with const. The right hand side is not constant here.
        """

        pattern = p
    end

    # Match.
    let
        src = "f(a, b) = const a = b = 1 + 2"
        match_result = rule_match(chained_const_assignment, parsestmt(SyntaxNode, src))
        @test isempty(match_result.failures)
        @test length(match_result.matches) == 1
        @test length(match_result.matches[1]) == 2
    end

    # No match.
    let
        src = "const a = b"
        match_result =
            rule_match(chained_const_assignment, parsestmt(SyntaxNode, src); only_matches=false)
        @test length(match_result.failures) == 4
        @test isempty(match_result.matches)
    end

    # Invalid syntax.
    @test_throws ArgusSyntaxError @macroexpand @rule "" quote
        description = ""
        pattern = p
    end
    @test_throws "Expected 2 arguments, got 3" @macroexpand @rule "" begin
        description = ""
        pattern = p
        arg3 = "bla"
    end
    @test_throws "Invalid `@rule` argument syntax" @macroexpand @rule "" begin
        description => ""
        pattern = p
    end
    @test_throws "Invalid `@rule` argument syntax" @macroexpand @rule "" begin
        description = ""
        pattern(p)
    end
    @test_throws "should be `description`" @macroexpand @rule "" begin
        other = ""
        pattern = p
    end
    @test_throws "should be `pattern`" @macroexpand @rule "" begin
        description = ""
        other = p
    end
end

@testset "Rule groups" begin
    dir = "../semgrep-to-argus"
    include(joinpath(dir, "lang-rules.jl"))

    # Chained `const` assignment.
    let
        chained_const_assignment = lang_rules["chained-const-assignment"]
        test_file = joinpath(dir, "chained-const-assignment.jl")
        match_result = rule_match(chained_const_assignment, test_file)
        @test length(match_result.matches) == 4
    end

    # Compare `nothing`.
    let
        compare_nothing = lang_rules["compare-nothing"]
        test_file = joinpath(dir, "compare-nothing.jl")
        match_result = rule_match(compare_nothing, test_file)
        @test length(match_result.matches) == 6
    end

    # Useless `equals`.
    let
        useless_equals = lang_rules["useless-equals"]
        test_file = joinpath(dir, "useless-equals.jl")
        match_result = rule_match(useless_equals, test_file)
        @test length(match_result.matches) == 4
    end
end
