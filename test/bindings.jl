@testset "Bindings" begin

    # Special fields access.
    let
        pattern = @pattern begin
            {x}
            @fail x.name == "a" "is a"
        end
        @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "b")), BindingSet)
        @test syntax_match(pattern, parsestmt(SyntaxNode, "a")) == MatchFail("is a")
        field_err_literal = syntax_match(pattern, parsestmt(SyntaxNode, "2"))
        @test isa(field_err_literal, MatchFail)
        @test field_err_literal.message ==
            """
            BindingFieldError: binding `x` has no field `name` because the bound expression is not an identifier or a macro call.
            Available fields: `value`

            The following fields are internal, avoid using them in patterns: `bname`, `src`, `bindings`
            """
        field_err_expr = syntax_match(pattern, parsestmt(SyntaxNode, "x = y"))
        @test isa(field_err_literal, MatchFail)
        @test startswith(field_err_expr.message,
                         """
                         BindingFieldError: binding `x` has no field `name` because the bound expression is not an identifier or a macro call.
                         Available fields: none
                         """)
    end
    let
        pattern = @pattern begin
            {x:::assign}
            @fail x.rhs.value == 2 "rhs is two"
        end
        @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "a = 3")), BindingSet)
        @test syntax_match(pattern, parsestmt(SyntaxNode, "3")) ==
            MatchFail("not an assignment")
        @test syntax_match(pattern, parsestmt(SyntaxNode, "x = 2")) ==
            MatchFail("rhs is two")
        field_err = syntax_match(pattern, parsestmt(SyntaxNode, "x = y"))
        @test isa(field_err, MatchFail)
        @test startswith(field_err.message,
                         """
                         BindingFieldError: binding `rhs` has no field `value` because the bound expression is not a literal.
                         Available fields: `name`
                         """)
    end
    let
        pattern = @pattern begin
            {x:::identifier}
            @fail x.abc.name == "abc" "is abc"
        end
        field_err = syntax_match(pattern, parsestmt(SyntaxNode, "a"))
        @test isa(field_err, MatchFail)
        @test startswith(field_err.message,
                         """
                         BindingFieldError: binding `x` has no field `abc` because `abc` is not a sub-binding of `x`.
                         Available fields: `_id`, `name`
                         """)
    end
    let
        pattern = @pattern begin
            {x}
            @fail y.name == "y" "is y"
        end
        dummy = parsestmt(SyntaxNode, "dummy")
        @test_throws "BindingSetKeyError: binding y not found" syntax_match(pattern, dummy)
    end

end
