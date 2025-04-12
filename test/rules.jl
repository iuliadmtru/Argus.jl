using Base: DEFAULT_READ_BUFFER_SZ
import Argus: DEFAULT_RULE_GROUP_NAME

@testset "Rules" begin
    rule_create_func = create_rule("test-rule", :(
        begin
            description = "Test rule"
            pattern = "(%f)(x) = 2"
        end
    ))
    rule_macro = @rule "test-rule" begin
        description = "Test rule"
        pattern = """
        (%f)(x) = 2
        """
    end
    @test isequal(rule_create_func, rule_macro)
    # TODO: More tests.
end

@testset "Rule groups" begin
    # Constructors.
    rg1 = RuleGroup()
    rg2 = RuleGroup(DEFAULT_RULE_GROUP_NAME)
    @test rg1.name == rg2.name
    @test isempty(rg1) && isempty(rg2)
    test_rule_x = SyntaxPatternNode("x")
    rg3 = RuleGroup("test-rule1" => test_rule_x)
    @test rg2.name == rg3.name
    @test !isempty(rg3)
    @test length(rg3) == 1
    rg4 = RuleGroup("default", "test-rule2" => test_rule_x)
    @test rg4.name == rg1.name
    @test !isempty(rg4)
    @test length(rg4) == 1
    @test isequal(rg3["test-rule1"], rg4.rules["test-rule2"])
    # Dict functionality.
    # TODO: More tests?
    @test haskey(rg3, "test-rule1")
    @test !haskey(rg1, "test-rule1")
    @test !haskey(rg4, "test-rule1")
    @test haskey(rg4, "test-rule2")
    test_rule_y = SyntaxPatternNode("y")
    @test isequal(test_rule_x, get(rg3, "test-rule1", test_rule_y))
    @test isequal(test_rule_y, get(rg3, "test-rule2", test_rule_y))
    @test length(rg3) == 1
    @test isequal(test_rule_y, get!(rg4, "test-rule1", test_rule_y))
    @test length(rg4) == 2
    @test length(merge(rg3, rg4)) == 2
    @test !isequal(get(rg3, "test-rule1", test_rule_x), get(rg4, "test-rule1", test_rule_y))
    @test length(merge!(rg3, rg4)) == 2
    @test isequal(get(rg3, "test-rule1", test_rule_x), get(rg4, "test-rule1", test_rule_x))
    delete!(rg3, "test-rule1")
    @test length(rg3) == 1
    # Rule registering.
    register_rule!(rg1, SyntaxPatternNode("x = test"), "rule-test")
    @test !isempty(rg1)
    @test haskey(rg1, "rule-test")
    @test isequal(rg1["rule-test"], SyntaxPatternNode("x = test"))
    @test length(rg1) == 1
    @test isempty(rg2)
    # Rule definition in groups.
    rule_func = define_rule_in_group(rg1, "test-rule", :(
        begin
            description = "Test"
            pattern = "test"
        end
    ))
    @test haskey(rg1, "test-rule")
    rule_macro = @define_rule_in_group rg2 "test-rule" begin
        description = "Test"
        pattern = "test"
    end
    @test !isempty(rg2)
    @test length(rg2) == 1
    @test haskey(rg2, "test-rule")
    @test isequal(rg2["test-rule"], rule_macro)
end
