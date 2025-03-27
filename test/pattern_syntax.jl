@testset "Regular syntax" begin
    # Simple expression, single match.
    src = """
    function f(a, b)
        y = a + b
        return y
    end
    """
    src_ast = parsestmt(JuliaSyntax.SyntaxNode, src)
    rule = Pattern("a + b")
    matches = Argus.search_ast!(rule.ast, src_ast)
    @test length(matches) == 1
    match = matches[1]
    @test kind(match.ast) == K"call"
    @test length(match.ast.children) == 3
    @test source_location(match) == (2, 9)


    # Simple expression, multiple matches.
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
    src_ast = parseall(JuliaSyntax.SyntaxNode, src)
    rule = Pattern("a + b")
    matches = Argus.search_ast!(rule.ast, src_ast)
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
    src_ast = parseall(JuliaSyntax.SyntaxNode, src)
    rule = Pattern("a = 2")
    matches = Argus.search_ast!(rule.ast, src_ast)
    @test length(matches) == 2
    @test source_location(matches[1]) == (6, 1)
    @test source_location(matches[2]) == (16, 10)
end

@testset "Special syntax" begin
    @testset "Metavariable" begin
        # Single metavariable, single match.
        src = """
        function f(a, b)
            y = a + b
            return y
        end
        """
        src_ast = parsestmt(JuliaSyntax.SyntaxNode, src)
        rule = Pattern("Metavariable(:A) + b")
        rule_metavar = rule.ast.children[1].data.special_syntax
        @test isnothing(rule_metavar.binding)
        matches = Argus.search_ast!(rule.ast, src_ast)
        @test length(matches) == 1
        match = matches[1]
        @test source_location(match) == (2, 9)
        @test !isnothing(match.metavariables)
        @test length(match.metavariables) == 1
        metavar = match.metavariables[1]
        @test metavar.binding.val === :a
        @test metavar.binding.position == ncodeunits("function f(a, b)\n    y = ") + 1
        
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
        src_ast = parseall(JuliaSyntax.SyntaxNode, src)
        rule = Pattern("Metavariable(:A) + b")
        matches = Argus.search_ast!(rule.ast, src_ast)
        # Check for three matches with one metavariable each.
        @test length(matches) == 3
        @test !any(m -> isnothing(m.metavariables), matches)
        @test !any(m -> length(m.metavariables) != 1, matches)
        # Check each match.
        @test source_location(matches[1]) == (2, 9)
        metavar1 = matches[1].metavariables[1]
        @test metavar1.binding.val === :a
        @test source_location(matches[2]) == (8, 3)
        metavar2 = matches[2].metavariables[1]
        @test metavar2.binding.val === :a
        @test source_location(matches[3]) == (11, 9)
        metavar3 = matches[3].metavariables[1]
        @test metavar3.binding.val === :x
    end
end
