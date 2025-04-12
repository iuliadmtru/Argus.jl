@testset "SyntaxPatternData" begin
    src = "x"
    syntax_data = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, src).data
    metavar = Metavariable(:x)
    @test SyntaxPatternData(syntax_data).val === :x
    @test SyntaxPatternData(metavar).name === :x
end

@testset "SyntaxPatternNode" begin

    @testset "Without placeholders" begin
        pattern_str = """
        begin
            a = "a"
            b = 1
            f(x) = x + 1
            println(a, f(b))
        end
        """
        syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, pattern_str)
        pattern = SyntaxPatternNode(syntax_node)
        @test pattern.data == syntax_node.data
        @test length(children(pattern)) == length(children(syntax_node))
        @test isnothing(pattern.parent)
        @test head(pattern) == head(syntax_node)
        b = children(children(syntax_node)[2])[1]
        @test children(children(pattern)[2])[1].data.val === :b
        @test isempty(placeholders(pattern))
    end

    @testset "With placeholders" begin
        pattern_str = "f(Metavariable(:x)) = Metavariable(:x) + 1"
        pattern = SyntaxPatternNode(pattern_str)
        @test contains_placeholders(pattern)
        @test kind(pattern) === K"function"

        metavar1 = children(children(pattern)[1])[2]
        @test is_placeholder(metavar1)
        m1 = placeholder(metavar1)
        @test !isnothing(m1)
        @test m1.name === :x
        @test isnothing(m1.binding)

        metavar2 = children(children(pattern)[2])[1]
        @test m1.name == placeholder(metavar2).name
    end

end

@testset "Compare" begin

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
        @test !pattern_compare!(pattern, children(syntax_node)[1])
        @test pattern_compare!(pattern, children(syntax_node)[3])
        @test !pattern_compare!(pattern, children(children(syntax_node)[4])[2])
    end

    @testset "With placeholders" begin
        src = """
        a = 1
        b = 2
        a + b
        f(x, y) = x + y
        """
        syntax_node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src)
        pattern = SyntaxPatternNode("Metavariable(:A) + Metavariable(:B)")
        @test contains_placeholders(pattern)
        @test !pattern_compare!(pattern, syntax_node)
        @test !pattern_compare!(pattern, children(syntax_node)[1])
        # First match.
        @test pattern_compare!(pattern, children(syntax_node)[3])
        metavar_a = children(pattern)[1]
        @test metavar_a.binding.val === :a
        @test source_location(metavar_a) == (3, 1)
        metavar_b = children(pattern)[3]
        @test metavar_b.data.binding.val === :b
        @test source_location(metavar_b) == (3, 5)
        placeholders_unbind!(pattern)
        # Second match.
        @test pattern_compare!(pattern, children(children(syntax_node)[4])[2])
        metavar_x = children(pattern)[1]
        @test metavar_x.binding.val === :x
        @test source_location(metavar_x) == (4, 11)
        metavar_y = children(pattern)[3]
        @test metavar_y.binding.val === :y
        @test source_location(metavar_y) == (4, 15)
    end

end

@testset "Match" begin

    @testset "Without placeholders" begin
        ## Single match.

        src = """
        function f(a, b)
            y = a + b
            return y
        end
        """
        syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, src)
        pattern = SyntaxPatternNode("a + b")
        @test !contains_placeholders(pattern)
        matches = pattern_match!(pattern, syntax_node)
        @test length(matches) == 1
        m = matches[1]
        @test kind(m.ast) === K"call"
        @test length(children(m.ast)) == 3
        @test source_location(m) == (2, 9)


        ## Multiple matches.

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
        end
        """
        syntax_node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src)
        pattern = SyntaxPatternNode("a + b")
        matches = pattern_match!(pattern, syntax_node)
        @test length(matches) == 3
        @test source_location(matches[1]) == (2, 9)
        @test source_location(matches[2]) == (8, 3)
        @test source_location(matches[3]) == (14, 5)

        src = """
        function f(a, b)
            y = a + b
            return y
        end

        a = 2
        b = 3
        f(a + b, b)

        g(x::T1; y::T2) = println(x, y)

        let x = 2, y = 3
            z = x + y
            a = 1
            b = 2
            g(b; a=2)
        end
        """
        syntax_node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src)
        pattern = SyntaxPatternNode("a = 2")
        matches = pattern_match!(pattern, syntax_node)
        @test length(matches) == 2
        @test source_location(matches[1]) == (6, 1)
        @test source_location(matches[2]) == (16, 10)
    end

    @testset "With placeholders" begin

        @testset "Metavariable" begin
            ## Single metavariable, single match.

            src = """
            function f(a, b)
                y = a + b
                return y
            end
            """
            syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, src)
            pattern = SyntaxPatternNode("Metavariable(:A) + b")
            metavar = children(pattern)[1].data
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


            ## Single metavariable, multiple matches.

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
            pattern = SyntaxPatternNode("Metavariable(:A) + b")
            matches = pattern_match!(pattern, syntax_node)
            # Check for three matches with one metavariable each.
            @test length(matches) == 3
            @test !any(m -> isnothing(m.placeholders), matches)
            @test !any(m -> length(m.placeholders) != 1, matches)
            # Check each match.
            @test source_location(matches[1]) == (2, 9)
            metavar1 = matches[1].placeholders[1]
            @test metavar1.binding.val === :a
            @test source_location(matches[2]) == (8, 3)
            metavar2 = matches[2].placeholders[1]
            @test metavar2.binding.val === :a
            @test source_location(matches[3]) == (11, 9)
            metavar3 = matches[3].placeholders[1]
            @test metavar3.binding.val === :x
        end
        
    end

end
