@testset "Rules" begin
    chained_const_assignment = @rule "chained-const-assignment" quote
        description = """
        Do not chain assignments with const. The right hand side is not constant here.
        """

        pattern = :(
            const _a:::identifier = _b:::identifier = _
        )
    end
    ## Match.
    src = "f(a, b) = const a = b = 1 + 2"
    match_result = rule_match(chained_const_assignment, parsestmt(SyntaxNode, src))
    @test isa(match_result, Vector{BindingSet})
    @test length(match_result) == 1
    @test length(match_result[1]) == 2
    ## No match.
    src = "const a = b"
    match_result =
        rule_match(chained_const_assignment, parsestmt(SyntaxNode, src); only_matches=false)
    @test isa(match_result, RuleMatchResult)
    @test length(match_result.failures) == 4
    @test isempty(match_result.matches)

    # Rule groups.
    dir = "../semgrep-to-argus"
    include(joinpath(dir, "lang-rules.jl"))
    ## Chained `const` assignment.
    chained_const_assignment = lang_rules["chained-const-assignment"]
    test_file = joinpath(dir, "chained-const-assignment.jl")
    match_result = rule_match(chained_const_assignment, test_file)
    @test length(match_result) == 4
    ## Compare `nothing`.
    compare_nothing = lang_rules["compare-nothing"]
    test_file = joinpath(dir, "compare-nothing.jl")
    match_result = rule_match(compare_nothing, test_file)
    @test length(match_result) == 6
    ## Useless `equals`.
    useless_equals = lang_rules["useless-equals"]
    test_file = joinpath(dir, "useless-equals.jl")
    match_result = rule_match(useless_equals, test_file)
    @test length(match_result) == 4
end
