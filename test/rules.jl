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
                const {a:::identifier} = {b:::identifier} = {_}
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
                @test length(match_result.failures) == 0
                @test isempty(match_result.matches)
            end
        end
        let
            rule = @rule "failures test" begin
                description = ""
                pattern = @pattern {_:::identifier}
            end
            match_result = rule_match(rule, parsestmt(SyntaxNode, "a = 2"); only_matches=false)
            @test length(match_result.matches) == 1
            @test length(match_result.failures) == 2
            @test match_result.failures[1] == match_result.failures[2] ==
                MatchFail("expected identifier")
        end
        let
            rule = @rule "" begin
                description = ""
                pattern = @pattern begin
                    {x} = 2
                    {y}()
                end
            end
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
            @test first_match[:x].name == "a"
            @test first_match[:y].name == "f"
            second_match = match_result.matches[2]
            @test kind(second_match[:x].src) === K"call"
            @test second_match[:y].name == "h"
        end
        let
            rule = @rule "" begin
                description = ""
                pattern = @pattern begin
                    {x:::identifier} = {_}
                    {_}...
                    {x} = {_}
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
        let
            rule = @rule "test non-greedy" begin
                description = ""
                pattern = @pattern [1, {one1}..., 1, {one2}...]
            end
            src = "[1, 1, 1, 1]"
            match_result = rule_match(rule, parsestmt(SyntaxNode, src); greedy=false)
            @test length(match_result.matches) == 3
            one1 = map(m -> m[:one1], match_result.matches)
            @test length(one1[1].src) == 0
            @test length(one1[2].src) == 1
            @test length(one1[3].src) == 2
            one2 = map(m -> m[:one2], match_result.matches)
            @test length(one2[3].src) == 0
            @test length(one2[2].src) == 1
            @test length(one2[1].src) == 2
        end
        let
            rule = @rule "test no ellipses children" begin
                description = ""
                pattern = @pattern begin
                    f()
                    begin
                        {a1}...
                        a
                        {a2}...
                    end
                end
            end
            src = """
            f()
            begin
                a
                a
                a
            end
            """;
            @test length(rule_match(rule, parseall(SyntaxNode, src)).matches) == 3
        end
        let
            rule = @rule "test no ellipses children 2" begin
                description = ""
                pattern = @pattern begin
                    function {f:::identifier}()
                        {a1}...
                        a
                        {a2}...
                    end
                    @fail begin
                        println("Function name: ", f.name)
                        false
                    end ""
                end
            end
            src = """
                  function f()
                      a
                      a
                      a
                  end
                  """;
            original_stdout = stdout
            (read_pipe, write_pipe) = redirect_stdout()
            match_result = rule_match(rule, parseall(SyntaxNode, src))
            redirect_stdout(original_stdout)
            close(write_pipe)
            @test length(match_result.matches) == 3
            @test readline(read_pipe) == "Function name: f"
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
    test_rule_in_group("useless-booleans", lang_rules, dir, 6)
    test_rule_in_group("open-tmp-path", lang_rules, dir, 3)
    test_rule_in_group("rand-bool", lang_rules, dir, 3)
    test_rule_in_group("invalid-module-name", lang_rules, dir, 1)

    rand_bool_rule_path = joinpath(dir, "rand-bool.jl")
    rule_group_match_result = rule_group_match(lang_rules, rand_bool_rule_path)
    rand_bool_rule_matches = rule_group_match_result["rand-bool"].matches
    @test length(rand_bool_rule_matches) == 3
    for (rule_name, result) in filter(p -> p.first != "rand-bool", rule_group_match_result)
        @test length(result.matches) == 0
    end

    # Refactoring.
    for m in rand_bool_rule_matches
        @test isa(m, PatternSubstitute)
        @test m.substitute == SyntaxPatternNode(:( rand(Bool) ))
    end
end
