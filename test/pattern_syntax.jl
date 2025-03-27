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
    @test match.source_location == (2, 9)


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
    @test matches[1].source_location == (2, 9)
    @test matches[2].source_location == (8, 3)
    @test matches[3].source_location == (14, 5)

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
    @test matches[1].source_location == (6, 1)
    @test matches[2].source_location == (16, 10)
end

@testset "Special syntax" begin
    @testset "Metavariable" begin
        # Simple expression, single match.
        src = """
        function f(a, b)
            y = a + b
            return y
        end
        """
        src_ast = parsestmt(JuliaSyntax.SyntaxNode, src)
        rule = Pattern("Metavariable(:A) + b")
        metavar = rule.ast.children[1].data.special_syntax
        @test isnothing(metavar.binding)
        matches = Argus.search_ast!(rule.ast, src_ast)
        @test length(matches) == 1
        match = matches[1]
        @test match.source_location == (2, 9)
        @test metavar.name === :A
        @test !isnothing(metavar.binding)
        @test metavar.binding.val === :a
        @test metavar.binding.position == ncodeunits("function f(a, b)\n    y = ") + 1
        
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
            z = x + b
            a = 1
            b = 2
            b + a
        end
        """
        src_ast = parseall(JuliaSyntax.SyntaxNode, src)
        rule = Pattern("Metavariable(:A) + b")
        metavar = rule.ast.children[1].data.special_syntax
        @test isnothing(metavar.binding)
        matches = Argus.search_ast!(rule.ast, src_ast)
        @test length(matches) == 3
        @test matches[1].source_location == (2, 9)
        @test matches[2].source_location == (8, 3)
        @test matches[3].source_location == (11, 9)
        # TODO.
    end
end

