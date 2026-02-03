@testset "Rules" begin

    @testset "Invalid syntax" begin
        # Invalid syntax.
        @test_throws SyntaxError @macroexpand @rule "" quote
            description = ""
            pattern = p
        end
        @test_throws "Expected 2 or 3 arguments, got 4" @macroexpand @rule "" begin
            description = ""
            pattern = p
            template = t
            arg4 = "bla"
        end
        @test_throws "invalid `@rule` argument syntax" @macroexpand @rule "" begin
            description => ""
            pattern = p
        end
        @test_throws "invalid `@rule` argument syntax" @macroexpand @rule "" begin
            description = ""
            pattern(p)
        end
        @test_throws "should be `description`" @macroexpand @rule "" begin
            other = ""
            pattern = p
        end
        @test_throws "should be `pattern`" @macroexpand @rule "" begin
            description = ""
            other = p
        end
        @test_throws "should be `template`" @macroexpand @rule "" begin
            description = ""
            pattern = p
            other = t
        end
    end

    @testset "Rule matching" begin
        let
            p = @pattern begin
                const {a:::identifier} = {b:::identifier} = {_}
            end
            chained_const_assignment = @rule "chained-const-assignment" begin
                description = """
                Do not chain assignments with const. The right hand side is not constant here.
                """

                pattern = p
            end
            # Match.
            let
                src = "f(a, b) = const a = b = 1 + 2"
                match_result =
                    rule_match(chained_const_assignment, parsestmt(SyntaxNode, src))
                @test isempty(match_result.failures)
                @test length(match_result.matches) == 1
                @test length(match_result.matches[1][1]) == 2
                @test all(isnothing, [m[2] for m in match_result.matches])
                @test match_result.matches[1][1].source_location == (1, 11)
                @test match_result.matches[1][1].file_name == ""
            end
            # No match.
            let
                src = "const a = b"
                match_result =
                    rule_match(chained_const_assignment,
                               parsestmt(SyntaxNode, src);
                               only_matches=false)
                @test length(match_result.failures) == 0
                @test isempty(match_result.matches)
            end
        end
        let
            rule = @rule "failures test" begin
                description = ""
                pattern = @pattern {_:::identifier}
            end
            match_result =
                rule_match(rule, parsestmt(SyntaxNode, "a = 2"); only_matches=false)
            @test length(match_result.matches) == 1
            @test length(match_result.failures) == 2
            @test match_result.failures[1] == match_result.failures[2] ==
                MatchFail("expected identifier")
        end
        let
            rule = @rule "" begin
                description = ""
                pattern = @pattern begin
                    {x} = 2
                    {y}()
                end
            end
            src = """
            x = 2
            function f(a)
                a = 2
                f()
                g()
            end
            for el in vec
                if c
                    el = 2
                    g(y) = 2
                    h()
                end
            end
            """
            match_result = rule_match(rule, parseall(SyntaxNode, src))
            @test length(match_result.matches) == 2
            matches = [m[1] for m in match_result.matches]
            first_match = matches[1]
            @test first_match[:x].name == "a"
            @test first_match[:y].name == "f"
            @test first_match.source_location == (3, 5)
            @test first_match.file_name == ""
            second_match = matches[2]
            @test kind(second_match[:x].src) === K"call"
            @test second_match[:y].name == "h"
            @test second_match.source_location == (10, 9)
            @test second_match.file_name == ""
        end
        let
            rule = @rule "" begin
                description = ""
                pattern = @pattern begin
                    {x:::identifier} = {_}
                    {_}...
                    {x} = {_}
                end
            end
            src = """
            a = :bla
            while false
                blu()
            end
            a = :bli
            b = :ble
            a = :blo
            """
            match_result = rule_match(rule, parseall(SyntaxNode, src))
            @test length(match_result.matches) == 3
        end
        let
            rule = @rule "test non-greedy" begin
                description = ""
                pattern = @pattern [1, {one1}..., 1, {one2}...]
            end
            src = "[1, 1, 1, 1]"
            match_result = rule_match(rule, parsestmt(SyntaxNode, src); greedy=false)
            @test length(match_result.matches) == 3
            matches = [m[1] for m in match_result.matches]
            one1 = map(m -> m[:one1], matches)
            @test length(one1[1].src) == 0
            @test length(one1[2].src) == 1
            @test length(one1[3].src) == 2
            one2 = map(m -> m[:one2], matches)
            @test length(one2[3].src) == 0
            @test length(one2[2].src) == 1
            @test length(one2[1].src) == 2
        end
        let
            rule = @rule "test no ellipses children" begin
                description = ""
                pattern = @pattern begin
                    f()
                    begin
                        {a1}...
                        a
                        {a2}...
                    end
                end
            end
            src = """
            f()
            begin
                a
                a
                a
            end
            """;
            @test length(rule_match(rule, parseall(SyntaxNode, src)).matches) == 3
        end
        let
            rule = @rule "test no ellipses children 2" begin
                description = ""
                pattern = @pattern begin
                    function {f:::identifier}()
                        {a1}...
                        a
                        {a2}...
                    end
                    @fail begin
                        println("Function name: ", f.name)
                        false
                    end ""
                end
            end
            src = """
                  function f()
                      a
                      a
                      a
                  end
                  """;
            original_stdout = stdout
            (read_pipe, write_pipe) = redirect_stdout()
            match_result = rule_match(rule, parseall(SyntaxNode, src))
            redirect_stdout(original_stdout)
            close(write_pipe)
            @test length(match_result.matches) == 3
            @test readline(read_pipe) == "Function name: f"
        end
        let
            rule = @rule "expr vs syntaxnode mismatch" begin
                description = ""
                pattern = @pattern in({_}, {_})
            end
            src = """
                in(a, b)
                a in b
                """;
            (tmp_file_path, _) = mktemp()
            write(tmp_file_path, src)
            match_result = rule_match(rule, tmp_file_path)
            @test length(match_result.matches) == 2
            @test match_result.matches[1][1].source_location == (1, 1)
            @test match_result.matches[2][1].source_location == (2, 1)
        end
    end

    @testset "`Expr`-parsing compatibility" begin
        function is_match(rule::Rule, src::String)
            src_node = parseall(SyntaxNode, src)
            src_node = Argus._normalise!(src_node)
            m = rule_match(rule, src_node)
            return length(m.matches) == 1
        end

        let
            rule = @rule "var" begin
                description = ""
                pattern = @pattern var"x"
            end
            @test is_match(rule, "var\"x\"")
        end
        let
            rule = @rule "?" begin
                description = ""
                pattern = @pattern x ? {_} : {_:::identifier}
            end
            @test is_match(rule, "x ? y : z")
        end
        let
            rule = @rule "cmd macro sugared" begin
                description = ""
                pattern = @pattern foo`x`
            end
            @test is_match(rule, "foo`x`")
        end
        let
            rule = @rule "cmd macro sugared with flags" begin
                description = ""
                pattern = @pattern foo`x`flags
            end
            @test is_match(rule, "foo`x`flags")
        end
        let
            rule = @rule "cmd macro" begin
                description = ""
                pattern = @pattern @foo_cmd `x`
            end
            @test is_match(rule, "@foo_cmd `x`")
        end
        # let
        #     rule = @rule "macro with do" begin
        #         description = ""
        #         pattern = @pattern @f(x) do y body end
        #     end
        #     @test is_match(rule, "@f(x) do y body end")
        # end
        let
            rule = @rule "doc" begin
                description = ""
                pattern = @pattern begin
                    """
                    docs
                    """
                    {_}
                end
            end
            @test is_match(rule, """
                \"""
                docs
                \"""
                x
                """)
        end
        let
            rule = @rule "infix" begin
                description = ""
                pattern = @pattern {_} + @esc({_}..., 1)
            end
            @test is_match(rule, "x + (y...)")
        end
        let
            rule = @rule "not infix" begin
                description = ""
                pattern = @pattern +({_}, {_}...)
            end
            @test is_match(rule, "+(x, y, z)")
        end
        let
            rule = @rule "trailing comma op" begin
                description = ""
                pattern = @pattern +({_},)
            end
            @test is_match(rule, "+(x,)")
            @test is_match(rule, "+(x)")
        end
        let
            rule = @rule "trailing comma .op" begin
                description = ""
                pattern = @pattern .+({_},)
            end
            @test is_match(rule, ".+(x,)")
            @test is_match(rule, ".+(x)")
        end
        let
            rule = @rule "trailing comma fun" begin
                description = ""
                pattern = @pattern f({_}, {_},)
            end
            @test is_match(rule, "f(x, y,)")
            @test is_match(rule, "f(x, y)")
        end
        let
            rule = @rule "trailing comma fun." begin
                description = ""
                pattern = @pattern f.({_}, {_},)
            end
            @test is_match(rule, "f.(x, y,)")
            @test is_match(rule, "f.(x, y)")
        end
        let
            rule = @rule "tuple assign one elem" begin
                description = ""
                pattern = @pattern x, = {_}
            end
            @test is_match(rule, "x, = 1, 2")
            @test is_match(rule, "(x,) = 1, 2")
        end
        let
            rule = @rule "tuple assign multiple elems" begin
                description = ""
                pattern = @pattern {_}, {_} = 1, 2
            end
            @test is_match(rule, "x, y = 1, 2")
            @test is_match(rule, "(x, y) = (1, 2)")
            @test is_match(rule, "(x, y) = 1, 2")
            @test is_match(rule, "x, y = (1, 2)")
            @test is_match(rule, "x, y, = 1, 2")
        end
        let
            rule = @rule "vect trailing comma" begin
                description = ""
                pattern = @pattern [x,]
            end
            @test is_match(rule, "[x,]")
            @test is_match(rule, "[x]")
        end
        let
            rule = @rule "braces one elem" begin
                description = ""
                pattern = @pattern {_} where {{_}}
            end
            @test is_match(rule, "x where {T}")
            @test is_match(rule, "x where {y for y in ys}")
        end
        let
            rule = @rule "braces one elem in macro" begin
                description = ""
                pattern = @pattern @m{{_}}
            end
            @test is_match(rule, "@m{x}")
        end
        let
            rule = @rule "braces multiple elem" begin
                description = ""
                pattern = @pattern {_} where {{_}, {_}...}
            end
            @test is_match(rule, "x where {S, T}")
        end
        let
            rule = @rule "braces multiple elem trailing comma" begin
                description = ""
                pattern = @pattern {_} where {{_}, {_},}
            end
            @test is_match(rule, "x where {S, T,}")
            @test is_match(rule, "x where {S, T}")
        end
        let
            rule = @rule "-> block in body" begin
                description = ""
                pattern = @pattern x -> {_}
            end
            @test is_match(rule, "x -> y")
            @test is_match(rule, "x -> begin y end")
        end
        let
            rule = @rule "-> tuple no elems" begin
                description = ""
                pattern = @pattern (;) -> {_}
            end
            @test is_match(rule, "(;) -> x")
        end
        let
            rule = @rule "-> tuple one elem" begin
                description = ""
                pattern = @pattern (x) -> {_}
            end
            @test is_match(rule, "(x) -> y")
            @test is_match(rule, "x -> y")
            @test !is_match(rule, "(x,) -> y")
        end
        let
            rule = @rule "-> tuple one elem trailing comma" begin
                description = ""
                pattern = @pattern (x,) -> {_}
            end
            @test is_match(rule, "(x,) -> y")
            @test !is_match(rule, "(x) -> y")
        end
        let
            rule = @rule "-> tuple params" begin
                description = ""
                pattern = @pattern (x; y={_}) -> {_}
            end
            @test is_match(rule, "(x; y=1) -> z")
        end
        let
            rule = @rule "-> where" begin
                description = ""
                pattern = @pattern (x where T) -> {_}
            end
            @test is_match(rule, "(x where T) -> y")
        end
        let
            rule = @rule "short form function def" begin
                description = ""
                pattern = @pattern f(x) = y
            end
            @test is_match(rule, "f(x) = y")
            @test is_match(rule, "f(x) = begin y end")
        end
        let
            rule = @rule "anonymous long form function one arg" begin
                description = ""
                pattern = @pattern function (x) {_} end
            end
            @test is_match(rule, "function (x) body end")
            @test is_match(rule, "function (x,) body end")
        end
        let
            rule = @rule "anonymous long form function multiple args trailing comma" begin
                description = ""
                pattern = @pattern function (x, y,) {_} end
            end
            @test is_match(rule, "function (x,y,) body end")
            @test is_match(rule, "function (x,y) body end")
        end
        let
            rule = @rule "weird anonymous long form function args" begin
                description = ""
                pattern = @pattern function (x*y) end
            end
            @test is_match(rule, "function (x*y) end")
        end
        # TODO: Fix parsing for pattern.
        let
            rule = @rule "local/global const" begin
                description = ""
                pattern = @pattern ~or(
                    ~or(local const {_} = {_}),
                    global const {_} = {_}
                )
            end
            @test is_match(rule, "local const x = 1")
            @test is_match(rule, "const global x = 1")
        end
        let
            rule = @rule "local/global tuple" begin
                description = ""
                pattern = @pattern ~or(
                    ~or(local ({_}...)),
                    global ({_}...)
                )
            end
            @test is_match(rule, "local (x, y)")
            @test is_match(rule, "global (x,)")
        end
        let
            rule = @rule "juxtapose" begin
                description = ""
                pattern = @pattern {_}'y
            end
            @test is_match(rule, "x'y")
            @test is_match(rule, "x' * y")
        end
        let
            rule = @rule "multi-line string" begin
                description = ""
                pattern = @pattern """
                a
                $x
                b
                c
                """
            end
            @test is_match(rule, """
                  \"""
                  a
                  \$x
                  b\nc
                  \"""
                  """)
            @test is_match(rule, "\"a\n\$x\nb\nc\n\"")
        end
        let
            rule = @rule "@." begin
                description = ""
                pattern = @pattern A.@.x
            end
            @test is_match(rule, "A.@.x")
            @test is_match(rule, "A.@__dot__(x)")
        end

        # (x for a in as, b in bs if z)     --- error
        # [@foo]                            --- macrocall-p vs macrocall
        # @S[a].b                           --- same
    end

end

function test_rule_in_group(rule_name, rule_group, test_dir, expected_matches)
    rule = rule_group[rule_name]
    test_file = joinpath(test_dir, rule_name * ".jl")
    match_result = rule_match(rule, test_file)
    @test length(match_result.matches) == expected_matches
end

@testset "Rule groups" begin
    dir = "../semgrep-to-argus"
    include(joinpath(dir, "lang-rules.jl"))
    test_rule_in_group("chained-const-assignment", lang_rules, dir, 4)
    test_rule_in_group("compare-nothing", lang_rules, dir, 6)
    test_rule_in_group("useless-equals", lang_rules, dir, 4)
    test_rule_in_group("useless-booleans", lang_rules, dir, 6)
    test_rule_in_group("open-tmp-path", lang_rules, dir, 3)
    test_rule_in_group("rand-bool", lang_rules, dir, 3)
    test_rule_in_group("invalid-module-name", lang_rules, dir, 1)

    rand_bool_rule_path = joinpath(dir, "rand-bool.jl")
    rule_group_match_result = rule_group_match(lang_rules, rand_bool_rule_path)
    rand_bool_rule_matches = rule_group_match_result["rand-bool"].matches
    @test length(rand_bool_rule_matches) == 3
    @test all(m -> m[1].file_name == joinpath(dir, "rand-bool.jl"), rand_bool_rule_matches)
    for (rule_name, result) in filter(p -> p.first != "rand-bool", rule_group_match_result)
        @test length(result.matches) == 0
    end

    # Run on directories.
    let
        g = RuleGroup("test")
        @define_rule_in_group g "test" begin
            description = ""
            pattern = @pattern highly_unlikely_variable_name_really
        end

        rule_group_match_result = rule_group_match(g, "../src")
        @test length(keys(rule_group_match_result)) == 1
        @test length(rule_group_match_result["test"].matches) == 0
        @test length(rule_group_match_result["test"].failures) == 0
    end

    # Refactoring.
    for m in rand_bool_rule_matches
        @test is_successful(m[1])
        @test Argus.compatible(m[2], SyntaxPatternNode(:( rand(Bool) )))
    end
end
