@testset "Pattern" begin

    @testset "Pattern forms" begin
        # `~var`.
        let
            @test_nowarn Pattern(SyntaxPatternNode(:( ~var(:_ex, :expr) )))
            @test_throws "invalid pattern form argument `x`" Pattern(
                SyntaxPatternNode(:( ~var(x, :identifier) ))
            )
            @test_throws "invalid pattern variable name x" Pattern(
                SyntaxPatternNode(:( ~var(:x, :identifier) ))
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
            @test match_first[:_x].src.val === 1
            match_second = syntax_match(pattern, parsestmt(SyntaxNode, "a + 3"))
            @test isa(match_second, BindingSet)
            @test length(match_second) == 1
            @test match_second[:_y].src.val === :a
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
            @test try
                syntax_match(pattern, parsestmt(SyntaxNode, "dummy"))
            catch err
                isa(err, MatchError) &&
                    sprint(showerror, err) ==
                    "MatchError: Fail condition evaluated to Expr instead of Bool (`x + 1`)\n"
            else
                false
            end
        end

        # `~rep`
        let
            pattern = @pattern _x...
            match_result = syntax_match(pattern, parsestmt(SyntaxNode, "dummy"))
            @test isa(match_result, BindingSet)
            @test length(match_result) == 1
            @test isa(match_result[:_x].src, Vector{JuliaSyntax.SyntaxNode})
            @test length(match_result[:_x].src) == 1
            @test isa(match_result[:_x].bindings, Vector{BindingSet{Argus.AbstractBinding}})
            @test length(match_result[:_x].bindings) == 1
        end
        let
            pattern = @pattern (_x...)...
            match_result = syntax_match(pattern, parsestmt(SyntaxNode, "dummy"))
            @test isa(match_result, BindingSet)
            @test length(match_result) == 1
            @test isa(match_result[:_x].src, Vector{Vector{JuliaSyntax.SyntaxNode}})
            @test isa(match_result[:_x].bindings,
                      Vector{Vector{BindingSet{Argus.AbstractBinding}}})
        end
        let
            pattern = @pattern begin
                _f(_args...)
            end
            match_result = syntax_match(pattern, parsestmt(SyntaxNode, "f(a, b; c=2)"))
            @test isa(match_result, BindingSet)
            @test length(match_result) == 2
            args = match_result[:_args]
            @test length(args.src) == 3
        end
        let
            pattern = @pattern begin
                (_f(_args...) = _)...
            end
            # Match.
            src = """
            f() = begin 2 end
            g(x) = begin x + 1 end
            h(a; b::Int=2) = begin a - b end
            """
            match_result = syntax_match(pattern, parseall(SyntaxNode, src))
            @test isa(match_result, BindingSet)
            @test length(match_result) == 2
            args_bindings = match_result[:_args]
            f_bindings = match_result[:_f]
            @test length(args_bindings.src) == length(f_bindings.src) == 3
            src3 = args_bindings.src[3]
            @test source_location(src3[2]) == (3, 4)
            # Fail.
            src_fail = """
            f() = begin 2 end
            g(x)::Int = begin x + 1 end
            h(a; b::Int=2) = begin a - b end
            """
            fail_result = syntax_match(pattern, parseall(SyntaxNode, src_fail))
            @test isa(fail_result, MatchFail)
        end
        let
            pattern = @pattern [2, (_x:::literal)..., 2, (_y...)...]
            match_result = syntax_match(pattern, parsestmt(SyntaxNode, "[2, 2, 2, 2, 3, 4]"))
            @test isa(match_result, BindingSet)
            x = match_result[:_x]
            @test length(x.src) == length(x.bindings) == 2
            @test x.bindings[1][:_lit].src.val == 2
            @test source_location(x.bindings[2][:_lit].src) == (1, 8)
            y = match_result[:_y]
            @test length(y.src) == length(y.bindings) == 2
            @test length(y.src[1]) == length(y.bindings[1]) == 1
            @test length(y.src[2]) == length(y.bindings[2]) == 1
            @test y.src[1][1].val == 3
            @test y.src[2][1].val == 4
        end
    end

    @testset "General" begin

        @testset "Invalid syntax" begin
            @test_throws "invalid pattern variable name x" Pattern(
                SyntaxPatternNode(:( x:::identifier ))
            )
            @test_throws SyntaxError @macroexpand @pattern quote _x:::expr = 2 end
            @test_throws "first expression cannot be a fail" @macroexpand @pattern begin
                @fail _ex.value == 2 "is two"
            end
            @test_throws "intercalated" @macroexpand @pattern begin
                ex1
                @fail cond ""
                ex2
            end
        end

        # TODO: Move these to `test/bindings.jl`.
        # Bindings fields access.
        let
            pattern = @pattern begin
                _x
                @fail _x.name == "a" "is a"
            end
            @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "b")), BindingSet)
            @test syntax_match(pattern, parsestmt(SyntaxNode, "a")) == MatchFail("is a")
            field_err_literal = syntax_match(pattern, parsestmt(SyntaxNode, "2"))
            @test isa(field_err_literal, MatchFail)
            @test field_err_literal.message ==
                """
                BindingFieldError: binding `_x` has no field `name` because the bound expression is not an identifier.
                Available fields: `bname`, `src`, `bindings`, `value`
                """
            field_err_expr = syntax_match(pattern, parsestmt(SyntaxNode, "x = y"))
            @test isa(field_err_literal, MatchFail)
            @test endswith(field_err_expr.message,
                           "Available fields: `bname`, `src`, `bindings`\n")
        end
        let
            pattern = @pattern begin
                _x:::assign
                @fail _x._rhs.value == 2 "rhs is two"
            end
            @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "a = 3")), BindingSet)
            @test syntax_match(pattern, parsestmt(SyntaxNode, "3")) == MatchFail("no match")
            @test syntax_match(pattern, parsestmt(SyntaxNode, "x = 2")) ==
                MatchFail("rhs is two")
            field_err = syntax_match(pattern, parsestmt(SyntaxNode, "x = y"))
            @test isa(field_err, MatchFail)
            @test field_err.message ==
                """
                BindingFieldError: binding `_rhs` has no field `value` because the bound expression is not a literal.
                Available fields: `bname`, `src`, `bindings`, `name`
                """
        end
        let
            pattern = @pattern begin
                _x:::identifier
                @fail _x._abc.name == "abc" "is abc"
            end
            field_err = syntax_match(pattern, parsestmt(SyntaxNode, "a"))
            @test isa(field_err, MatchFail)
            @test field_err.message ==
                """
                BindingFieldError: binding `_x` has no field `_abc` because `_abc` is not a sub-binding of `_x`.
                Available fields: `bname`, `src`, `bindings`, `_id`, `name`
                """
        end

        @testset "Pattern matching" begin
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
                    # TODO: Sort by order of appearance and add tests.
                    f_node = match_result[:_f].src
                    @test isa(f_node, JuliaSyntax.SyntaxNode)
                    @test source_location(f_node) == (1, 1)
                    x_node = match_result[:_arg1].src
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
            end
            let
                is_x = @pattern begin
                    ~and((_f:::identifier)(),
                         ~fail(_f._id.name != "x", "not x"))
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
            ## Multiple pattern expressions.
            let
                pattern = @pattern begin
                    _a:::identifier = _
                    _a:::identifier = _
                    @fail _a.name == "x" "is x"
                end
                let
                    src = parseall(SyntaxNode, """
                    a = 2
                    x = 3
                    """)
                    @test syntax_match(pattern, src) ==
                        MatchFail("conflicting bindings for pattern variable _a")
                end
                let
                    src = parseall(SyntaxNode, """
                    x = 2
                    x = 3
                    """)
                    @test syntax_match(pattern, src) ==
                        MatchFail("is x")
                end
                let
                    src = parseall(SyntaxNode, """
                    a = 2
                    a = 3
                    """)
                    @test isa(syntax_match(pattern, src), BindingSet)
                end
                let
                    src = parseall(SyntaxNode, """
                    a = 2
                    a = 3
                    a + 1
                    """)
                    @test syntax_match(pattern, src) == MatchFail()
                end
            end
        end
    end
end
