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
            pattern = @pattern _...
            match_result = syntax_match(pattern, parsestmt(SyntaxNode, "dummy"))
            @test match_result == BindingSet()
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
            # @test_throws "Only fail conditions" @macroexpand @pattern begin
            #     ex1
            #     ex2
            # end
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
                @fail _x.__rhs.value == 2 "rhs is two"
            end
            @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "a = 3")), BindingSet)
            @test syntax_match(pattern, parsestmt(SyntaxNode, "3")) == MatchFail("no match")
            @test syntax_match(pattern, parsestmt(SyntaxNode, "x = 2")) ==
                MatchFail("rhs is two")
            field_err = syntax_match(pattern, parsestmt(SyntaxNode, "x = y"))
            @test isa(field_err, MatchFail)
            @test field_err.message ==
                """
                BindingFieldError: binding `__rhs` has no field `value` because the bound expression is not a literal.
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
                Available fields: `bname`, `src`, `bindings`, `__id`, `name`
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

        @testset "Display" begin
            pattern = @pattern begin
                function (_f:::identifier)(_args...)
                    (_...)...
                    return _f
                end
                @fail !startswith(_f.name, "_") "does not start with `_`"
            end
            buff = IOBuffer()
            show(buff, "text/plain", pattern)
            show_str = String(take!(buff))
            @test show_str == """
            Pattern:
            [~and]
              [function]
                [call]
                  _f:::identifier                    :: ~var
                  (_args:::expr)...                  :: ~rep
                [block]
                  ((_:::expr)...)...                 :: ~rep
                  [return]
                    _f:::expr                        :: ~var
              [~fail]
                [call-pre]
                  !                                  :: Identifier
                  [call]
                    startswith                       :: Identifier
                    [.]
                      _f                             :: Identifier
                      name                           :: Identifier
                    [string]
                      "_"                            :: String
                "does not start with `_`"            :: String
            """
            show(buff, pattern)
            show_str_sexpr = String(take!(buff))
            @test show_str_sexpr == "(~and (function (call (~var (quote-: _f) (quote-: identifier)) (~rep (~var (quote-: _args) (quote-: expr)) 1)) (block (~rep (~rep (~var (quote-: _) (quote-: expr)) 1) 2) (return (~var (quote-: _f) (quote-: expr))))) (~fail (call-pre ! (call startswith (. _f name) (string \"_\"))) \"does not start with `_`\"))"
            show(buff, "text/x.sexpression", pattern)
            @test show_str_sexpr == String(take!(buff))
        end
    end
end
