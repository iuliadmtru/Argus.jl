import Argus: placeholders, contains_placeholders, placeholders_unbind!, has_binding

@testset "SyntaxMatch" begin
    syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "f(x)")
    m = SyntaxMatch(syntax_node)
    @test kind(m.ast) === K"call"
    @test source_location(m) == (1, 1)
    @test isnothing(m.placeholders)
    m = SyntaxMatch(syntax_node, [Metavariable(:x, syntax_node.children[2].data)])
    @test kind(m.ast) === K"call"
    @test source_location(m) == (1, 1)
    @test !isnothing(m.placeholders)
    @test length(m.placeholders) == 1
    @test m.placeholders[1].name === :x
    @test m.placeholders[1].binding.val === :x
end

@testset "SyntaxMatches" begin
    @test isempty(SyntaxMatches())
    syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "x")
    m = SyntaxMatch(syntax_node)
    ms = SyntaxMatches([m])
    @test length(ms) == 1
    @test kind(ms[1].ast) === K"Identifier"
    push!(ms, m)
    @test length(ms) == 2
end

@testset "Pattern comparison" begin

    @testset "Without placeholders" begin
        src = """
        a = 1
        b = 2
        a + b
        f(x, y) = x + y
        """
        syntax_node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src)
        pattern = SyntaxPatternNode("a + b")
        @test !contains_placeholders(pattern)
        @test !pattern_compare!(pattern, syntax_node)
        @test !pattern_compare!(pattern, syntax_node.children[1])
        @test pattern_compare!(pattern, syntax_node.children[3])
        @test !pattern_compare!(pattern, syntax_node.children[4].children[2])
    end

    @testset "With placeholders" begin
        src = """
        a = 1
        b = 2
        a + b
        f(x, y) = x + y
        """
        syntax_node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src)
        pattern = SyntaxPatternNode("%A + %B")
        @test contains_placeholders(pattern)
        @test !pattern_compare!(pattern, syntax_node)
        @test !pattern_compare!(pattern, syntax_node.children[1])
        ## First match.
        @test pattern_compare!(pattern, syntax_node.children[3])
        metavar_a = pattern.children[1]
        @test metavar_a.binding.val === :a
        @test source_location(metavar_a) == (3, 1)
        metavar_b = pattern.children[3]
        @test metavar_b.binding.val === :b
        @test source_location(metavar_b) == (3, 5)
        placeholders_unbind!(pattern)
        ## Second match.
        @test pattern_compare!(pattern, syntax_node.children[4].children[2])
        metavar_x = pattern.children[1]
        @test metavar_x.binding.val === :x
        @test source_location(metavar_x) == (4, 11)
        metavar_y = pattern.children[3]
        @test metavar_y.binding.val === :y
        @test source_location(metavar_y) == (4, 15)
        # Expression binding.
        pattern = SyntaxPatternNode("%x = %y")
        @test !pattern_compare!(pattern, syntax_node)
        ## First match.
        @test pattern_compare!(pattern, syntax_node.children[1])
        metavar_x = pattern.children[1]
        @test metavar_x.binding.val === :a
        @test source_location(metavar_x) == (1, 1)
        metavar_y = pattern.children[2]
        @test metavar_y.binding.val === 1
        @test source_location(metavar_y) == (1, 5)
        placeholders_unbind!(pattern)
        ## Second match.
        @test pattern_compare!(pattern, syntax_node.children[2])
        metavar_x = pattern.children[1]
        @test metavar_x.binding.val === :b
        @test source_location(metavar_x) == (2, 1)
        metavar_y = pattern.children[2]
        @test metavar_y.binding.val === 2
        @test source_location(metavar_y) == (2, 5)
        placeholders_unbind!(pattern)
        ## Third node -- no match.
        @test !pattern_compare!(pattern, syntax_node.children[3])
        ## Third match.
        @test pattern_compare!(pattern, syntax_node.children[4])
        metavar_x = pattern.children[1]
        @test isnothing(metavar_x.binding.val)
        @test kind(metavar_x.binding.raw) === K"call"
        @test source_location(metavar_x) == (4, 1)
        metavar_y = pattern.children[2]
        @test isnothing(metavar_y.binding.val)
        @test kind(metavar_y.binding.raw) === K"call"
        @test source_location(metavar_y) == (4, 11)
        placeholders_unbind!(pattern)
    end

end

@testset "Pattern matching" begin

    @testset "Without placeholders" begin
        # Single match.
        src = """
        function f(a, b)
            y = a + b
            return y
        end
        """
        syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, src)
        pattern = SyntaxPatternNode("a + b")
        matches = pattern_match!(pattern, syntax_node)
        @test length(matches) == 1
        m = matches[1]
        @test kind(m.ast) === K"call"
        @test length(children(m.ast)) == 3
        @test source_location(m) == (2, 9)
        # Multiple matches.
        src = """
        function f(a, b)
            y = a + b
            return y
        end

        a = 2
        b = 3
        f(a + b, b)

        let x = 2, y = 3
            z = x + y
            a = 1
            b = 2
            a + b
            g(b; a=2)
        end
        """
        syntax_node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src)
        pattern = SyntaxPatternNode("a + b")
        matches = pattern_match!(pattern, syntax_node)
        @test length(matches) == 3
        @test source_location(matches[1]) == (2, 9)
        @test source_location(matches[2]) == (8, 3)
        @test source_location(matches[3]) == (14, 5)
        pattern = SyntaxPatternNode("a = 2")
        matches = pattern_match!(pattern, syntax_node)
        @test length(matches) == 2
        @test source_location(matches[1]) == (6, 1)
        @test source_location(matches[2]) == (15, 10)
    end

    @testset "With placeholders" begin

        @testset "Metavariable" begin
            # Single metavariable, single match.
            src = """
            function f(a, b)
                y = a + b
                return y
            end
            """
            syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, src)
            pattern = SyntaxPatternNode("%A + b")
            metavar = pattern.children[1].pattern_data
            @test !has_binding(metavar)
            matches = pattern_match!(pattern, syntax_node)
            @test length(matches) == 1
            m = matches[1]
            @test source_location(m) == (2, 9)
            @test !isnothing(m.placeholders)
            @test length(m.placeholders) == 1
            mvar = m.placeholders[1]
            @test mvar !== placeholders(pattern)[1]  # Should be a copy.
            @test mvar.binding.val === :a
            @test mvar.binding.position == ncodeunits("function f(a, b)\n    y = ") + 1
            # Single metavariable, multiple matches.
            src = """
            function f(a, b)
                y = a + b
                return y
            end

            a = 2
            b = 3
            f(a + b, b)

            let x = 2, y = 3
                z = x + b
                a = 1
                b = 2
                b + a
            end
            """
            syntax_node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src)
            pattern = SyntaxPatternNode("%A + b")
            matches = pattern_match!(pattern, syntax_node)
            ## Check for three matches with one metavariable each.
            @test length(matches) == 3
            @test !any(m -> isnothing(m.placeholders), matches)
            @test !any(m -> length(m.placeholders) != 1, matches)
            ## Check each match.
            @test source_location(matches[1]) == (2, 9)
            metavar1 = matches[1].placeholders[1]
            @test metavar1.binding.val === :a
            @test source_location(matches[2]) == (8, 3)
            metavar2 = matches[2].placeholders[1]
            @test metavar2.binding.val === :a
            @test source_location(matches[3]) == (11, 9)
            metavar3 = matches[3].placeholders[1]
            @test metavar3.binding.val === :x
            # Match with file.
            # TODO: Make this a mock/fake?
            pattern = SyntaxPatternNode("const %a = %b = %_")
            matches = pattern_match!(pattern, "demo/chained_const_assignment.jl")
            @test length(matches) == 4
            @test all(m -> length(m.placeholders) == 3, matches)
            m1 = matches[1]
            @test source_location(m1) == (2, 1)
            a1 = m1.placeholders[1]
            b1 = m1.placeholders[2]
            _1 = m1.placeholders[3]
            @test a1.binding.val === :a
            @test b1.binding.val === :b
            @test _1.binding.val === 1
            m2 = matches[2]
            @test source_location(m2) == (5, 1)
            a2 = m2.placeholders[1]
            b2 = m2.placeholders[2]
            _2 = m2.placeholders[3]
            @test a2.binding.val === :a
            @test b2.binding.val === :b
            @test _2.binding.val === :c
            m3 = matches[3]
            @test source_location(m3) == (8, 1)
            a3 = m3.placeholders[1]
            b3 = m3.placeholders[2]
            _3 = m3.placeholders[3]
            @test a3.binding.val === :a
            @test b3.binding.val === :b
            @test isnothing(_3.binding.val)
            @test kind(_3.binding.raw) === K"="
            m4 = matches[4]
            @test source_location(m4) == (11, 1)
            a4 = m4.placeholders[1]
            b4 = m4.placeholders[2]
            _4 = m4.placeholders[3]
            @test a4.binding.val === :a
            @test b4.binding.val === :b
            @test isnothing(_4.binding.val)
            @test kind(_4.binding.raw) === K"string"
        end
        
    end

end
