@testset "Compare" begin

    @testset "Without placeholders" begin
        src = """
        a = 1
        b = 2
        a + b
        f(x, y) = x + y
        """
        syntax_node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src)
        pattern = Pattern("a + b")
        template = pattern.template
        @test !contains_placeholders(template)
        @test !template_compare!(template, syntax_node)
        @test !template_compare!(template, children(syntax_node)[1])
        @test template_compare!(template, children(syntax_node)[3])
        @test !template_compare!(template, children(children(syntax_node)[4])[2])
    end

    @testset "With placeholders" begin
        src = """
        a = 1
        b = 2
        a + b
        f(x, y) = x + y
        """
        syntax_node = JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, src)
        pattern = Pattern("Metavariable(:A) + Metavariable(:B)")
        template = pattern.template
        @test contains_placeholders(template)
        @test !template_compare!(template, syntax_node)
        @test !template_compare!(template, children(syntax_node)[1])
        # First match.
        @test template_compare!(template, children(syntax_node)[3])
        metavar_a = children(template)[1]
        @test metavar_a.binding.val === :a
        @test source_location(metavar_a) == (3, 1)
        metavar_b = children(template)[3]
        @test metavar_b.data.binding.val === :b
        @test source_location(metavar_b) == (3, 5)
        placeholders_unbind!(template)
        # Second match.
        @test template_compare!(template, children(children(syntax_node)[4])[2])
        metavar_x = children(template)[1]
        @test metavar_x.binding.val === :x
        @test source_location(metavar_x) == (4, 11)
        metavar_y = children(template)[3]
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
        pattern = Pattern("a + b")
        template = pattern.template
        @test !contains_placeholders(template)
        matches = template_match!(template, syntax_node)
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
        pattern = Pattern("a + b")
        matches = template_match!(pattern.template, syntax_node)
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
        pattern = Pattern("a = 2")
        matches = template_match!(pattern.template, syntax_node)
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
            pattern = Pattern("Metavariable(:A) + b")
            template = pattern.template
            metavar = children(template)[1].data
            @test !has_binding(metavar)
            matches = template_match!(template, syntax_node)
            @test length(matches) == 1
            m = matches[1]
            @test source_location(m) == (2, 9)
            @test !isnothing(m.placeholders)
            @test length(m.placeholders) == 1
            mvar = m.placeholders[1]
            @test mvar !== placeholders(template)[1]  # Should be a copy.
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
            pattern = Pattern("Metavariable(:A) + b")
            template = pattern.template
            matches = template_match!(template, syntax_node)
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
