@testset "Bindings" begin

    # Special fields access.
    let
        pattern = @pattern begin
            {x}
            @fail [:x] x.name == "a" "is a"
        end
        @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "b")), BindingSet)
        match_result = syntax_match(pattern, parsestmt(SyntaxNode, "a"))
        @test !is_successful(match_result)
        @test match_result.message == "is a"
        @test source_location(match_result.src) == (1, 1)
        field_err_literal = syntax_match(pattern, parsestmt(SyntaxNode, "2"))
        @test isa(field_err_literal, MatchFail)
        @test field_err_literal.message ==
            """
            BindingFieldError: binding `x` has no field `name` because the bound \
            expression is not an identifier or a macro call.
            Available fields: `value`

            The following fields are internal, avoid using them in patterns: \
            `bname`, `src`, `bindings`"""
        field_err_expr = syntax_match(pattern, parsestmt(SyntaxNode, "x = y"))
        @test isa(field_err_literal, MatchFail)
        @test startswith(field_err_expr.message,
                         """
                         BindingFieldError: binding `x` has no field `name` because the \
                         bound expression is not an identifier or a macro call.
                         Available fields: none""")
    end
    let
        pattern = @pattern begin
            {x:::assign}
            @fail [:x] x.rhs.value == 2 "rhs is two"
        end
        @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "a = 3")), BindingSet)
        let
            fail = syntax_match(pattern, parsestmt(SyntaxNode, "3"))
            @test !is_successful(fail)
            @test fail.message == "expected assignment"
            @test source_location(fail.src) == (1, 1)
        end
        let
            fail = syntax_match(pattern, parsestmt(SyntaxNode, "x = 2"))
            @test !is_successful(fail)
            @test fail.message == "rhs is two"
            @test source_location(fail.src) == (1, 1)
        end
        field_err = syntax_match(pattern, parsestmt(SyntaxNode, "x = y"))
        @test isa(field_err, MatchFail)
        @test startswith(field_err.message,
                         """
                         BindingFieldError: binding `rhs` has no field `value` because \
                         the bound expression is not a literal.
                         Available fields: `name`
                         """)
    end
    let
        pattern = @pattern begin
            {x:::identifier}
            @fail [:x] x.abc.name == "abc" "is abc"
        end
        field_err = syntax_match(pattern, parsestmt(SyntaxNode, "a"))
        @test isa(field_err, MatchFail)
        @test startswith(field_err.message,
                         """
                         BindingFieldError: binding `x` has no field `abc` because `abc` \
                         is not a sub-binding of `x`.
                         Available fields: `name`
                         """)
    end
    let
        pattern = @pattern begin
            {x}
            @fail [:y] y.name == "y" "is y"
        end
        match_fail = syntax_match(pattern, parsestmt(SyntaxNode, "dummy"))
        @test !is_successful(match_fail)
        @test match_fail.message == "BindingSetKeyError: binding y not found"
    end

end
