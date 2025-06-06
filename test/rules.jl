@testset "Rules" begin

    @testset "Invalid syntax" begin
        # Invalid syntax.
        @test_throws SyntaxError @macroexpand @rule "" quote
            description = ""
            pattern = p
        end
        @test_throws "Expected 2 arguments, got 3" @macroexpand @rule "" begin
            description = ""
            pattern = p
            arg3 = "bla"
        end
        @test_throws "invalid `@rule` argument syntax" @macroexpand @rule "" begin
            description => ""
            pattern = p
        end
        @test_throws "invalid `@rule` argument syntax" @macroexpand @rule "" begin
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

    @testset "Rule matching" begin
        let
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
        end
        let
            rule = @rule "" begin
                description = ""
                pattern = @pattern begin
                    _x = 2
                    _y()
                end
            end
            let
                src = """
                x = 2
                function f(a)
                    a = 2
                    f()
                    g()
                end
                for el in vec
                    if c
                        el = 2
                        g(y) = 2
                        h()
                    end
                end
                """
                match_result = rule_match(rule, parseall(SyntaxNode, src))
                @test length(match_result.matches) == 2
                first_match = match_result.matches[1]
                @test first_match[:_x].name == "a"
                @test first_match[:_y].name == "f"
                second_match = match_result.matches[2]
                @test kind(second_match[:_x].src) === K"call"
                @test second_match[:_y].name == "h"
            end
        end
        let
            rule = @rule "" begin
                description = ""
                pattern = @pattern begin
                    _x:::identifier = _
                    _...
                    _x = _
                end
            end
            src = """
            a = :bla
            while false
                blu()
            end
            a = :bli
            b = :ble
            a = :blo
            """
            match_result = rule_match(rule, parseall(SyntaxNode, src))
            @test length(match_result.matches) == 3
        end
    end

end

function test_rule_in_group(rule_name, rule_group, test_dir, expected_matches)
    rule = rule_group[rule_name]
    test_file = joinpath(test_dir, rule_name * ".jl")
    match_result = rule_match(rule, test_file)
    @test length(match_result.matches) == expected_matches
end

@testset "Rule groups" begin
    dir = "../semgrep-to-argus"
    include(joinpath(dir, "lang-rules.jl"))
    test_rule_in_group("chained-const-assignment", lang_rules, dir, 4)
    test_rule_in_group("compare-nothing", lang_rules, dir, 6)
    test_rule_in_group("useless-equals", lang_rules, dir, 4)
    test_rule_in_group("useless-booleans", lang_rules, dir, 7)
    test_rule_in_group("open-tmp-path", lang_rules, dir, 3)
    test_rule_in_group("rand-bool", lang_rules, dir, 3)
end
