@testset "Pattern" begin

    @testset "Pattern forms" begin
        # `~var`.
        @test_nowarn Pattern(:( ~var(:_ex, :expr) ))
        @test_throws "Invalid pattern form argument `x` at (1, 7)" Pattern(:( ~var(x, :identifier) ))
        @test_throws "Invalid pattern variable name x" Pattern(:( ~var(:x, :identifier) ))

        # `~or`.
        fundef = @syntax_class "function definition" quote
            ~or(_f:::funcall = _, function (_g:::funcall) _ end)
        end
        match_first = syntax_match(fundef, parsestmt(SyntaxNode, "f() = begin 2 end"))
        @test collect(keys(match_first)) == [:_f]
        match_second = syntax_match(fundef, parsestmt(SyntaxNode, "function f() 2 end"))
        @test collect(keys(match_second)) == [:_g]

        # `~and`.
        conflicting = @pattern :( ~and(_x + 2, _x + 3) )
        match_result = syntax_match(conflicting, parsestmt(SyntaxNode, "1 + 2"))
        @test match_result == MatchFail("no match")

        # `~fail`.
        pattern = @pattern :( ~fail(_x, "") )
        err = "Binding context does not contain a binding for _x"
        @test_throws err syntax_match(pattern, parsestmt(SyntaxNode, "dummy"))
        pattern = @pattern :( ~fail(:(x + 1), "") )
        err = "Fail condition evaluated to non-Boolean value: x + 1 (::Expr)"
        @test_throws err syntax_match(pattern, parsestmt(SyntaxNode, "dummy"))
    end

    @testset "General" begin
        # Invalid syntax.
        @test_throws "Invalid pattern variable name x" @pattern :( x:::identifier )

        # Pattern matching.
        binary_funcall_pattern = @pattern :( (_f:::identifier)(_arg1, _) )
        ## Match.
        match_result =
            syntax_match(binary_funcall_pattern, parsestmt(SyntaxNode, "f(x, 1 + 2)"))
        @test isa(match_result, BindingSet)
        @test length(match_result) == 2  # No binding for the anonymous pattern variable.
        @test sort(collect(keys(match_result))) == [:_arg1, :_f]
        ## TODO: Sort by order of appearance and add tests.
        f_node = match_result[:_f].ast
        @test isa(f_node, JuliaSyntax.SyntaxNode)
        @test source_location(f_node) == (1, 1)
        x_node = match_result[:_arg1].ast
        @test source_location(x_node) == (1, 3)
        ## No match.
        match_result = syntax_match(binary_funcall_pattern, parsestmt(SyntaxNode, "f(x)"))
        @test isa(match_result, MatchFail)
        @test match_result == MatchFail("no match")

        # Display methods.
        buff = IOBuffer()
        show(buff, "text/plain", binary_funcall_pattern)
        show_str = String(take!(buff))
        @test show_str == """
        Pattern:
        [call]
          _f:::identifier                        :: ~var
          _arg1:::expr                           :: ~var
          _:::expr                               :: ~var
        """
        show(buff, binary_funcall_pattern)
        show_str_sexpr = String(take!(buff))
        @test show_str_sexpr == "(call (~var (quote-: _f) (quote-: identifier)) (~var (quote-: _arg1) (quote-: expr)) (~var (quote-: _) (quote-: expr)))"
    end
end
