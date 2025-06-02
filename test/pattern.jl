@testset "Pattern" begin

    @testset "Pattern forms" begin
        # `~var`.
        let
            @test_nowarn Pattern(SyntaxPatternNode(:( ~var(:_ex, :expr) )), Function[])
            @test_throws "Invalid pattern form argument `x`" Pattern(
                SyntaxPatternNode(:( ~var(x, :identifier) )),
                Function[]
            )
            @test_throws "Invalid pattern variable name x" Pattern(
                SyntaxPatternNode(:( ~var(:x, :identifier) )),
                Function[]
            )
        end

        # `~or`.
        let
            pattern = @pattern begin
                ~or(_x + 2,
                    ~and(_, _y + 3))
            end
            match_first = syntax_match(pattern, parsestmt(SyntaxNode, "1 + 2"))
            @test isa(match_first, BindingSet)
            @test length(match_first) == 1
            @test match_first[:_x].ast.val === 1
            match_second = syntax_match(pattern, parsestmt(SyntaxNode, "a + 3"))
            @test isa(match_second, BindingSet)
            @test length(match_second) == 1
            @test match_second[:_y].ast.val === :a
            @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "2 + 1")), MatchFail)
        end

        # `~and`.
        let
            conflicting = @pattern begin
                ~and(_x + 2, _x + 3)
            end
            match_result = syntax_match(conflicting, parsestmt(SyntaxNode, "1 + 2"))
            @test match_result == MatchFail("no match")
        end

        # `~fail`.
        let
            pattern = @pattern ~fail(_x, "")
            @test_throws BindingSetKeyError syntax_match(pattern,
                                                         parsestmt(SyntaxNode, "dummy"))
        end
        let
            pattern = @pattern ~fail(:(x + 1), "")
            @test_throws MatchError syntax_match(pattern,
                                                 parsestmt(SyntaxNode, "dummy"))
        end
    end

    @testset "General" begin
        # Invalid syntax.
        @test_throws "Invalid pattern variable name x" Pattern(
            SyntaxPatternNode(:( x:::identifier )),
            []
        )
        @test_throws SyntaxError @macroexpand @pattern quote _x:::expr = 2 end
        @test_throws "first expression cannot be a fail" @macroexpand @pattern begin
           @fail _ex.value == 2 "is two"
        end
        @test_throws "Only fail conditions" @macroexpand @pattern begin
            ex1
            ex2
        end

        # Pattern matching.
        binary_funcall_pattern = @pattern (_f:::identifier)(_arg1, _)
        let
            ## Match.
            let
                match_result =
                    syntax_match(binary_funcall_pattern,
                                 parsestmt(SyntaxNode, "f(x, 1 + 2)"))
                @test isa(match_result, BindingSet)
                # No binding for the anonymous pattern variable.
                @test length(match_result) == 2
                @test sort(collect(keys(match_result))) == [:_arg1, :_f]
                ## TODO: Sort by order of appearance and add tests.
                f_node = match_result[:_f].ast
                @test isa(f_node, JuliaSyntax.SyntaxNode)
                @test source_location(f_node) == (1, 1)
                x_node = match_result[:_arg1].ast
                @test source_location(x_node) == (1, 3)
            end
            ## No match.
            let
                match_result =
                    syntax_match(binary_funcall_pattern, parsestmt(SyntaxNode, "f(x)"))
                @test isa(match_result, MatchFail)
                @test match_result == MatchFail("no match")
            end
        end
        let
            even = @pattern begin
                _x
                @fail !iseven(_x.value) "not even"
            end
            match = syntax_match(even, parsestmt(SyntaxNode, "2"))
            @test isa(match, BindingSet)
            fail = syntax_match(even, parsestmt(SyntaxNode, "3"))
            @test fail == MatchFail("not even")
            err = syntax_match(even, parsestmt(SyntaxNode, "f"))
            @test err == MatchFail("binding `_x` has no field `value` " *
                "because the bound expression is not a literal")
        end
        let
            is_x = @pattern begin
                ~and((_f:::identifier)(),
                     ~fail(_f.__id.name != "x", "not x"))
            end
            match = syntax_match(is_x, parsestmt(SyntaxNode, "x()"))
            @test isa(match, BindingSet)
            fail_name = syntax_match(is_x, parsestmt(SyntaxNode, "b()"))
            @test fail_name == MatchFail("not x")
            fail_inner = syntax_match(is_x, parsestmt(SyntaxNode, "f()()"))
            @test fail_inner == MatchFail("not an identifier")
            fail = syntax_match(is_x, parsestmt(SyntaxNode, "2"))
            @test fail == MatchFail("no match")
        end

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
