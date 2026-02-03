@testset "Pattern" begin

    @testset "Pattern forms" begin
        # `~var`.
        let
            @test_nowarn Pattern(SyntaxPatternNode(:( ~var(:ex, :expr) )))
            @test_throws "invalid pattern form argument `x`" Pattern(
                SyntaxPatternNode(:( ~var(x, :identifier) ))
            )
        end

        # `~or`.
        let
            pattern = @pattern begin
                ~or({x} + 2,
                    ~and({_}, {y} + 3))
            end
            match_first = syntax_match(pattern, parsestmt(SyntaxNode, "1 + 2"))
            @test isa(match_first, BindingSet)
            @test length(match_first) == 1
            @test match_first[:x].src.val === 1
            match_second = syntax_match(pattern, parsestmt(SyntaxNode, "a + 3"))
            @test isa(match_second, BindingSet)
            @test length(match_second) == 1
            @test match_second[:y].src.val === :a
            @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "2 + 1")), MatchFail)
        end

        # `~and`.
        let
            conflicting = @pattern begin
                ~and({x} + 2, {x} + 3)
            end
            match_result = syntax_match(conflicting, parsestmt(SyntaxNode, "1 + 2"))
            @test match_result == MatchFail("no match")
        end

        # `~fail`.
        let
            pattern = @pattern ~fail(x, "")
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
            pattern = @pattern {x}...
            match_result = syntax_match(pattern, parsestmt(SyntaxNode, "dummy"))
            @test isa(match_result, BindingSet)
            @test length(match_result) == 1
            @test isa(match_result[:x].src, Vector{JuliaSyntax.SyntaxNode})
            @test length(match_result[:x].src) == 1
            @test isa(match_result[:x].bindings, Vector{BindingSet{Argus.AbstractBinding}})
            @test length(match_result[:x].bindings) == 1
        end
        let
            pattern = @pattern ({x}...)...
            match_result = syntax_match(pattern, parsestmt(SyntaxNode, "dummy"))
            @test isa(match_result, BindingSet)
            @test length(match_result) == 1
            @test isa(match_result[:x].src, Vector{Vector{JuliaSyntax.SyntaxNode}})
            @test isa(match_result[:x].bindings,
                      Vector{Vector{BindingSet{Argus.AbstractBinding}}})
        end
        let
            pattern = @pattern begin
                {f}({args}...)
            end
            match_result = syntax_match(pattern, parsestmt(SyntaxNode, "f(a, b; c=2)"))
            @test isa(match_result, BindingSet)
            @test length(match_result) == 2
            args = match_result[:args]
            @test length(args.src) == 3
        end
        let
            pattern = @pattern begin
                ({f}({args}...) = {_})...
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
            args_bindings = match_result[:args]
            f_bindings = match_result[:f]
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
            pattern = @pattern [2, ({x:::literal})..., 2, ({y}...)...]
            match_result = syntax_match(pattern, parsestmt(SyntaxNode, "[2, 2, 2, 2, 3, 4]"))
            @test isa(match_result, BindingSet)
            x = match_result[:x]
            @test length(x.src) == length(x.bindings) == 2
            @test x.bindings[1][:_lit].src.val == 2
            @test source_location(x.bindings[2][:_lit].src) == (1, 8)
            y = match_result[:y]
            @test length(y.src) == length(y.bindings) == 2
            @test length(y.src[1]) == length(y.bindings[1]) == 1
            @test length(y.src[2]) == length(y.bindings[2]) == 1
            @test y.src[1][1].val == 3
            @test y.src[2][1].val == 4
        end
        let
            pattern = @pattern [{x:::identifier}..., 2]
            @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "[a, 2]")), BindingSet)
            @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "[2, 2]")), MatchFail)
        end
    end

    @testset "General" begin

        @testset "Invalid syntax" begin
            @test_throws SyntaxError @macroexpand @pattern quote {x:::expr} = 2 end
            @test_throws "first expression cannot be a fail" @macroexpand @pattern begin
                @fail ex.value == 2 "is two"
            end
            @test_throws "interspersed" @macroexpand @pattern begin
                ex1
                @fail cond ""
                ex2
            end
        end

        @testset "Escaping" begin
            let
                pattern = @pattern {x}...
                @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "x")), BindingSet)
                pattern_esc = @pattern @esc(x...)
                @test isa(syntax_match(pattern_esc, parsestmt(SyntaxNode, "x")), MatchFail)
            end
            let
                pattern = @pattern [{elems}...]...
                @test isa(syntax_match(pattern, parsestmt(SyntaxNode, "[1]")),
                          BindingSet)

                pattern_esc = @pattern @esc([{elems}...]..., 1)
                @test isa(syntax_match(pattern_esc, parsestmt(SyntaxNode, "[1]...")),
                          BindingSet)
                @test isa(syntax_match(pattern_esc, parsestmt(SyntaxNode, "[1]")),
                          MatchFail)

                pattern_esc2 = @pattern @esc([{elems}...]..., 3)
                @test isa(syntax_match(pattern_esc2, parsestmt(SyntaxNode, "[[1]...]...")),
                          BindingSet)
                @test isa(syntax_match(pattern_esc2, parsestmt(SyntaxNode, "[[1]]")),
                          MatchFail)
                m = syntax_match(pattern_esc2, parsestmt(SyntaxNode, "[{elems}...]..."))
                @test kind(m[:elems].src) === K"braces"

                pattern_esc9 = @pattern @esc([{elems}...]..., 9)
                @test isa(syntax_match(pattern_esc9,
                                       parsestmt(SyntaxNode, "[[1]...]...")),
                          MatchFail)
                @test isa(syntax_match(pattern_esc9,
                                       parsestmt(SyntaxNode, "[{1}...]...")),
                          MatchFail)

                pattern_esc_all = @pattern @esc([{elems}...]..., :all)
                @test isa(syntax_match(pattern_esc_all,
                                       parsestmt(SyntaxNode, "[[1]...]...")),
                          MatchFail)
                @test isa(syntax_match(pattern_esc_all,
                                       parsestmt(SyntaxNode, "[{elems}...]...")),
                          BindingSet)
            end
            let
                pattern = @pattern @esc(@esc)
                @test kind(pattern) === K"macrocall" && length(pattern.src.children) == 1
                @test pattern.src.children[1].val === Symbol("@esc")
            end
        end

        @testset "Pattern matching" begin
            let
                binary_funcall_pattern = @pattern ({f:::identifier})({arg1}, {_})
                ## Match.
                let
                    match_result =
                        syntax_match(binary_funcall_pattern,
                                     parsestmt(SyntaxNode, "f(x, 1 + 2)"))
                    @test isa(match_result, BindingSet)
                    # No binding for the anonymous pattern variable.
                    @test length(match_result) == 2
                    # Bindings should appear in insertion order => `f` should be first.
                    @test collect(keys(match_result)) == [:f, :arg1]
                    @test map(s -> source_location(s.src), values(match_result)) == [(1, 1), (1, 3)]
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
                    {x}
                    @fail !iseven(x.value) "not even"
                end
                match = syntax_match(even, parsestmt(SyntaxNode, "2"))
                @test isa(match, BindingSet)
                fail = syntax_match(even, parsestmt(SyntaxNode, "3"))
                @test fail == MatchFail("not even")
            end
            let
                is_x = @pattern begin
                    ~and(({f:::identifier})(),
                         ~fail(f._id.name != "x", "not x"))
                end
                match = syntax_match(is_x, parsestmt(SyntaxNode, "x()"))
                @test isa(match, BindingSet)
                fail_name = syntax_match(is_x, parsestmt(SyntaxNode, "b()"))
                @test fail_name == MatchFail("not x")
                fail_inner = syntax_match(is_x, parsestmt(SyntaxNode, "f()()"))
                @test fail_inner == MatchFail("expected identifier")
                fail = syntax_match(is_x, parsestmt(SyntaxNode, "2"))
                @test fail == MatchFail("no match")
            end
            let
                pattern = @pattern begin
                    ~or(
                        {b} || {_}...,
                        {_}... || {b}
                    )
                    @fail typeof(b.value) != Bool "not `Bool`"
                end
                src = parsestmt(SyntaxNode, "cond || true")
                match_result = syntax_match(pattern, src)
                @test isa(match_result, BindingSet)
                @test length(match_result) == 1
            end
            let
                pattern = @pattern begin
                    {ex:::assign}
                    {_}...
                    {ex}
                end
                src = parseall(SyntaxNode, """
                                           a = 1
                                           a = 2
                                           a = 1
                                           """)
                match_result = syntax_match(pattern, src)
                @test source_location(match_result[:ex].src) == (3, 1)
                src_fail = parseall(SyntaxNode, """
                                                a = 1
                                                a = 2
                                                """)
                @test syntax_match(pattern, src_fail) ==
                    MatchFail("conflicting bindings for pattern variable ex")
            end
            let
                pattern = @pattern begin
                    @show {x}
                    @fail x.name != "x" "not x"
                end
                src = parsestmt(SyntaxNode, "@show x")
                @test is_successful(syntax_match(pattern,
                                                 parsestmt(SyntaxNode, "@show x")))
                @test is_successful(syntax_match(pattern,
                                                 parsestmt(SyntaxNode, "@show(x)")))
                @test !is_successful(syntax_match(pattern,
                                                  parsestmt(SyntaxNode, "@show(y)")))
            end
            ## Multiple pattern expressions.
            let
                pattern = @pattern begin
                    {a:::identifier} = {_}
                    {a:::identifier} = {_}
                    @fail a.name == "x" "is x"
                end
                let
                    src = parseall(SyntaxNode, """
                    a = 2
                    x = 3
                    """)
                    @test syntax_match(pattern, src) ==
                        MatchFail("conflicting bindings for pattern variable a")
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
            let
                pattern = @pattern begin
                    function {f:::identifier}()
                        {_}...
                        a
                    end
                    @fail false ""
                end
                src = """
                      function f()
                          a
                          a
                          a
                      end
                      """
                match_result =
                    syntax_match(pattern, parsestmt(SyntaxNode, src); greedy=false)
                @test isa(match_result, BindingSet)
                @test length(match_result) == 1
            end
            let
                pattern = @pattern begin
                    function {f:::identifier}()
                        {_}...
                        a
                    end
                    @fail true "fail"
                end
                src = """
                      function f()
                          a
                          a
                          a
                      end
                      """
                match_result =
                    syntax_match(pattern, parsestmt(SyntaxNode, src); greedy=false)
                @test match_result == MatchFail("fail")
            end
            let
                pattern = @pattern begin
                    ~and(
                        function {f:::identifier}({args}...)
                            {_}...
                        end,
                        function {f}({args}...)
                            {_}...
                            a
                            {_}...
                        end
                    )
                end
                src = """
                      function f(x)
                          a
                          a
                          a
                      end
                      """
                match_result = syntax_match(pattern, parsestmt(SyntaxNode, src))
                @test length(match_result[:args].src) == 1
            end
            let
                pattern = @pattern {_} -> {_}
                src = "x -> begin y end"
                match_result = syntax_match(pattern, parsestmt(SyntaxNode, src))
                @test is_successful(match_result)
            end
            ## Templates.
            let
                p = @pattern {x}
                t = @template {x} + 1
                match_result = syntax_match(p, parsestmt(SyntaxNode, "abc"))
                @test is_successful(match_result)
                substitute = expand(t, match_result)
                @test kind(substitute) === K"call"
                @test length(substitute.children) == 3
                @test substitute.children[1].val === :abc
            end
            let
                p = @pattern function {f}({args}...)
                        return {ex}
                    end
                t = @template {f}({args}...) = {ex}
                src = """
                    function f(x, y)
                        return x + y
                    end
                    """
                match_result = syntax_match(p, parsestmt(SyntaxNode, src))
                @test is_successful(match_result)
                substitute = expand(t, match_result)
                @test flags(substitute) == JuliaSyntax.SHORT_FORM_FUNCTION_FLAG
            end
        end

        @testset "`Expr`-parsing compatibility" begin
            let
                p = @pattern f.$x
                @test is_successful(syntax_match(p, parsestmt(SyntaxNode, "f.\$x")))
            end
        end
    end
end
