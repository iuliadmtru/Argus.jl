@testset "Pattern" begin

    # TODO: Make `@test_throws` tests robust (probably after implementing error types).
    @testset "Pattern forms" begin
        # `~var`.
        let
            @test_nowarn @pattern :( ~var(:_ex, :expr) )
            @test_throws "Invalid pattern form argument `x` at (1, 7)" Pattern(:( ~var(x, :identifier) ))
            @test_throws "Invalid pattern variable name x" Pattern(:( ~var(:x, :identifier) ))
        end

        # `~or`.
        # TODO: Move this to `test/syntax-class.jl`. Write an explicit `@pattern` test here.
        let
            fundef = @syntax_class "function definition" begin
                @pattern :( _f:::funcall = _ )
                @pattern :( function (_g:::funcall) _ end )
            end
            match_first = syntax_match(fundef, parsestmt(SyntaxNode, "f() = begin 2 end"))
            @test isa(match_first, BindingSet)
            @test collect(keys(match_first)) == [:_f]
            match_second = syntax_match(fundef, parsestmt(SyntaxNode, "function f() 2 end"))
            @test isa(match_second, BindingSet)
            @test collect(keys(match_second)) == [:_g]
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
            pattern = @pattern :( ~fail(_x, "") )
            err = "Binding context does not contain a binding for _x."
            @test syntax_match(pattern, parsestmt(SyntaxNode, "dummy")) == MatchFail(err)
        end
        let
            pattern = @pattern :( ~fail(:(x + 1), "") )
            err = "Fail condition evaluated to non-Boolean value: x + 1 (::Expr)"
            @test syntax_match(pattern, parsestmt(SyntaxNode, "dummy")) == MatchFail(err)
        end
    end

    @testset "General" begin
        # Invalid syntax.
        @test_throws "Invalid pattern variable name x" Pattern(:( x:::identifier ))
        # @test_throws "Invalid `@pattern` syntax" @pattern quote x end
        # @test_throws "Invalid `@pattern` syntax" @pattern quote
        #     _x:::identifier
        #     @fail _x.value == 2 "not two"
        # end
        # @test_throws "The first expression cannot be a fail condition" @pattern begin
        #     @fail :true ""
        # end

        # Pattern matching.
        binary_funcall_pattern = @pattern :( (_f:::identifier)(_arg1, _) )
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
            is_x = @pattern :(
                ~and((_f:::identifier)(),
                     ~fail(_f.__id.name != "x", "not x"))
            )
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
