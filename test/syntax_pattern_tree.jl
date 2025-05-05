import Argus: _SyntaxPatternNode, _update_data_head, _unify_placeholders!, placeholder,
    placeholders, contains_placeholders, is_placeholder, placeholders_unbind!,
    _is_metavariable, _get_metavar_name, set_binding!

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
        pattern_node = SyntaxPatternNode(syntax_node)
        # Functionality inherited from JuliaSyntax.
        @test !isnothing(head(pattern_node))
        @test kind(pattern_node) === K"call"
        @test !is_leaf(pattern_node)
        @test is_leaf(pattern_node.children[1])
        @test length(children(pattern_node)) == 3
        @test source_location(pattern_node) == source_location(syntax_node)
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
        syntax_node1 = JuliaSyntax.parsestmt(
            JuliaSyntax.SyntaxNode,
            "Metavariable(:x) + 1";
            ignore_errors=true
        )
        syntax_node2 = JuliaSyntax.parsestmt(
            JuliaSyntax.SyntaxNode,
            "Metavariable(:y) + Metavariable(:y)";
            ignore_errors=true
        )
        @test _is_metavariable(syntax_node1.children[1])
        @test _get_metavar_name(syntax_node1.children[1]) === :x
        @test _is_metavariable(syntax_node2.children[3])
        @test _get_metavar_name(syntax_node2.children[1]) === :y
        @test _is_metavariable(syntax_node2.children[3])
        @test _get_metavar_name(syntax_node2.children[3]) === :y
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
        # Process nodes.
        processed1 = copy(unprocessed1)
        processed2 = copy(unprocessed2)
        ## Unify placeholders.
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
        ## Bind and unbind placeholders.
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
