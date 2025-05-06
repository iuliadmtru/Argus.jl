import Argus: is_directive, is_or, is_and, _SyntaxPatternNode, _update_data_head,
    _unify_placeholders!, placeholder, placeholders, contains_placeholders, is_placeholder,
    placeholders_unbind!, _is_metavariable, _get_metavar_name, set_binding!,
    _desugar_metavariable, _is_metavariable_sugared, sugar, no_sugar, err

@testset "SyntaxPatternDirective" begin
    @test_throws "Unavailable pattern directive" SyntaxPatternDirective(:bla)
    pattern = SyntaxPatternNode(:or, :x, :y)
    @test is_directive(pattern)
    @test is_or(pattern)
    @test !is_and(pattern)
    @test_throws "must have at least one" SyntaxPatternNode(:and)
    @test_throws "Unavailable pattern directive" SyntaxPatternNode(:bla, :blu)
end

@testset "SyntaxPatternData" begin
    syntax_data = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "x").data
    wrapped_syntax_data = SyntaxPatternData(syntax_data)
    @test isa(wrapped_syntax_data, SyntaxPatternData{JuliaSyntax.SyntaxData})
    metavar = Metavariable(:x)
    wrapped_metavar = SyntaxPatternData(metavar)
    @test isa(wrapped_metavar, SyntaxPatternData{Metavariable})
    @test wrapped_metavar.name === wrapped_syntax_data.val

    # Data updating.
    syntax_data = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "f(2)").data
    wrapped = SyntaxPatternData(syntax_data)
    @test kind(wrapped) === K"call"
    keep_flags = JuliaSyntax.flags(wrapped.pattern_data.raw)
    updated = _update_data_head(wrapped, JuliaSyntax.SyntaxHead(K"=", keep_flags))
    @test kind(updated) === K"="
    unupdated = _update_data_head(updated, JuliaSyntax.SyntaxHead(K"call", keep_flags))
    @test isequal(wrapped, unupdated)
end

@testset "SyntaxPatternNode" begin

    @testset "Without placeholders" begin
        # Construct with `JuliaSyntax.SyntaxNode`.
        syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "x + 1")
        pattern_node1 = SyntaxPatternNode(syntax_node)
        @test isequal(pattern_node1, copy(pattern_node1))
        @test !is_directive(pattern_node1)
        # Construct with `String`.
        pattern_node2 = _SyntaxPatternNode("x + 1")
        @test repr("text/plain", pattern_node1) == repr("text/plain", pattern_node2)
        # Functionality inherited from JuliaSyntax.
        stream = JuliaSyntax.ParseStream("x + 1")
        JuliaSyntax.parse!(stream; rule=:statement)
        pattern_node3 = JuliaSyntax.build_tree(SyntaxPatternNode, stream)
        @test repr(pattern_node1) == repr(pattern_node3)
        @test !isnothing(head(pattern_node1))
        @test kind(pattern_node1) === K"call"
        @test !is_leaf(pattern_node1)
        @test is_leaf(pattern_node1.children[1])
        @test length(children(pattern_node1)) == 3
        @test source_location(pattern_node1) == source_location(syntax_node)
        # Placeholder utils.
        unprocessed = _SyntaxPatternNode(syntax_node)
        @test isempty(placeholders(unprocessed))
        processed = copy(unprocessed)
        _unify_placeholders!(processed)
        @test isequal(unprocessed, processed)
        @test isequal(unprocessed, placeholders_unbind!(processed))
        @test !contains_placeholders(processed)
        @test !_is_metavariable(syntax_node.children[1])
        @test_throws "non-Metavariable node" _get_metavar_name(syntax_node)
    end

    @testset "With placeholders" begin
        # Construct with `Expr`.
        pattern_node1 = SyntaxPatternNode(:(m"x" + 1))
        pattern_node2 = SyntaxPatternNode(:(m"y" + m"y"))
        # Functionality inherited from JuliaSyntax.
        @test !isnothing(head(pattern_node1))
        @test kind(pattern_node1) === K"call"
        @test isnothing(head(pattern_node1.children[1]))
        @test isnothing(head(pattern_node2.children[3]))
        @test !is_leaf(pattern_node1)
        @test length(children(pattern_node1)) == 3
        @test !is_leaf(pattern_node2)
        @test length(children(pattern_node2)) == 3
        # Placeholder utils.
        syntax_node1 = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "Metavariable(:x) + 1")
        syntax_node2 = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "m\"y\" + m\"y\"")
        x = syntax_node1.children[1]
        @test _is_metavariable(x)
        @test _is_metavariable_sugared(x) === no_sugar
        @test _get_metavar_name(x) === :x
        y1 = syntax_node2.children[1]
        @test !_is_metavariable(y1)
        @test _is_metavariable_sugared(y1) === sugar
        @test_throws "non-Metavariable node" _get_metavar_name(y1)
        y2 = syntax_node2.children[3]
        @test !_is_metavariable(y2)
        desugared = _desugar_metavariable(y2)
        @test _is_metavariable(desugared)
        @test _get_metavar_name(desugared) === :y
        @test !_is_metavariable(syntax_node2.children[2])
        unprocessed1 = _SyntaxPatternNode(syntax_node1)
        unprocessed2 = _SyntaxPatternNode(syntax_node2)
        @test !isempty(placeholders(unprocessed1))
        @test contains_placeholders(unprocessed1)
        @test !isempty(placeholders(unprocessed2))
        @test contains_placeholders(unprocessed2)
        @test length(placeholders(unprocessed1)) == 1
        @test length(placeholders(unprocessed2)) == 1
        @test is_placeholder(unprocessed1.children[1])
        @test !is_placeholder(unprocessed1.children[3])
        @test is_placeholder(unprocessed2.children[1])
        @test is_placeholder(unprocessed2.children[3])
        p1 = placeholder(unprocessed2.children[1])
        p3 = placeholder(unprocessed2.children[3])
        @test isequal(p1, p3)
        @test p1 !== p3
        @test isnothing(placeholder(unprocessed2.children[2]))
        ## Process nodes.
        processed1 = copy(unprocessed1)
        processed2 = copy(unprocessed2)
        ### Unify placeholders.
        _unify_placeholders!(processed1)
        @test isequal(unprocessed1, processed1)
        _unify_placeholders!(processed2)
        @test isequal(unprocessed2, processed2)
        p1 = placeholder(unprocessed2.children[1])
        p3 = placeholder(unprocessed2.children[3])
        @test p1 !== p3
        p1 = placeholder(processed2.children[1])
        p3 = placeholder(processed2.children[3])
        @test p1 === p3
        @test length(placeholders(processed2)) == 1
        @test length(placeholders(unprocessed2)) == 1
        ### Bind and unbind placeholders.
        @test isequal(processed1, placeholders_unbind!(processed1))
        @test isequal(processed2, placeholders_unbind!(processed2))
        binding_data = syntax_node1.children[3].data  # Node `1`.
        set_binding!(processed1.children[1].pattern_data, binding_data)
        set_binding!(processed2.children[1].pattern_data, binding_data)
        @test processed1.children[1].name === :x
        @test processed1.children[1].binding.val === 1
        @test processed2.children[1].name === :y
        @test processed2.children[1].binding.val === 1
        @test processed2.children[3].name === :y
        @test processed2.children[3].binding.val === 1
    end

end
